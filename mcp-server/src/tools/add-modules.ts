import { readFile, writeFile } from "node:fs/promises";
import { findCabalFile, parseCabalModules } from "../parsers/cabal-parser.js";

export interface AddModulesResult {
  success: true;
  created: string[];
  alreadyExist: string[];
  cabalUpdated: string[];
}

export interface AddModulesError {
  success: false;
  error: string;
  hint?: string;
}

/**
 * Add module names to the `exposed-modules` section of a .cabal file,
 * preserving indentation. Returns the modules that were actually added
 * (those not already present).
 */
export async function addModulesToCabal(
  projectDir: string,
  modules: string[]
): Promise<string[]> {
  const cabalPath = await findCabalFile(projectDir);
  const content = await readFile(cabalPath, "utf-8");
  const lines = content.split("\n");

  let inLibrary = false;
  let exposedModulesLine = -1;
  let exposedModulesIndent = "";
  let moduleIndent = "";
  const existingModules = new Set<string>();
  let blockEnd = -1;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    if (/^library\b/i.test(line)) {
      inLibrary = true;
      continue;
    }
    if (/^[a-z]/i.test(line) && !line.startsWith(" ") && !line.startsWith("\t")) {
      if (inLibrary && /^(executable|test-suite|benchmark|common|flag|source-repository)\b/i.test(line)) {
        inLibrary = false;
      }
    }
    if (inLibrary && exposedModulesLine === -1) {
      const m = line.match(/^(\s*)exposed-modules:/i);
      if (m) {
        exposedModulesLine = i;
        exposedModulesIndent = m[1]!;
        continue;
      }
    }
    if (exposedModulesLine !== -1 && blockEnd === -1) {
      if (line.trim() === "") continue;
      const indentMatch = line.match(/^(\s+)\S/);
      if (indentMatch && indentMatch[1]!.length > exposedModulesIndent.length) {
        moduleIndent = indentMatch[1]!;
        existingModules.add(line.trim());
        continue;
      }
      blockEnd = i;
      break;
    }
  }

  if (exposedModulesLine === -1) {
    throw new Error("exposed-modules section not found in .cabal");
  }

  if (!moduleIndent) {
    moduleIndent = exposedModulesIndent + "    ";
  }

  const insertAt = blockEnd === -1 ? lines.length : blockEnd;
  const added: string[] = [];
  const toInsert: string[] = [];

  for (const mod of modules) {
    if (!existingModules.has(mod)) {
      toInsert.push(`${moduleIndent}${mod}`);
      added.push(mod);
    }
  }

  if (toInsert.length === 0) {
    return [];
  }

  const newLines = [...lines.slice(0, insertAt), ...toInsert, ...lines.slice(insertAt)];
  await writeFile(cabalPath, newLines.join("\n"), "utf-8");
  return added;
}

export async function handleAddModules(
  projectDir: string,
  args: {
    modules: string[];
    signatures?: Record<string, string[]>;
    update_cabal?: boolean;
  }
): Promise<string> {
  if (args.modules.length === 0) {
    const error: AddModulesError = {
      success: false,
      error: "No modules specified.",
      hint: "Provide `modules: [\"Foo.Bar\", ...]`.",
    };
    return JSON.stringify(error);
  }

  try {
    await findCabalFile(projectDir);
  } catch {
    const error: AddModulesError = {
      success: false,
      error: `No .cabal file found in ${projectDir}.`,
      hint: "Use ghci_create_project to start a new project.",
    };
    return JSON.stringify(error);
  }

  let cabalUpdated: string[] = [];
  if (args.update_cabal !== false) {
    try {
      cabalUpdated = await addModulesToCabal(projectDir, args.modules);
    } catch (err) {
      const error: AddModulesError = {
        success: false,
        error: `Failed to update .cabal: ${err instanceof Error ? err.message : String(err)}`,
      };
      return JSON.stringify(error);
    }
  }

  const existingCabalModules = await parseCabalModules(projectDir);
  const notInCabal = args.modules.filter((m) => !existingCabalModules.library.includes(m));
  if (notInCabal.length > 0 && args.update_cabal === false) {
    const error: AddModulesError = {
      success: false,
      error: `Modules not listed in cabal: ${notInCabal.join(", ")}`,
      hint: "Set update_cabal: true (default) or add them to exposed-modules manually.",
    };
    return JSON.stringify(error);
  }

  const { handleScaffold } = await import("./scaffold.js");
  const scaffoldResult = JSON.parse(await handleScaffold(projectDir, args.signatures));

  const result: AddModulesResult = {
    success: true,
    created: scaffoldResult.created ?? [],
    alreadyExist: scaffoldResult.alreadyExist ?? [],
    cabalUpdated,
  };
  return JSON.stringify({
    ...result,
    _nextStep:
      result.created.length > 0
        ? "Run ghci_load(module_path=\"...\") on the new modules (or ghci_session(restart) to reload everything)."
        : "No new stubs written (all modules already existed).",
  });
}
