import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors } from "../parsers/error-parser.js";

export interface ModuleDefinition {
  name: string;
  type: string;
  kind: "function" | "type" | "class" | "data" | "instance";
}

/**
 * Load a module and return a structured summary of its exports:
 * all definitions with their types, plus any compilation errors.
 */
export async function handleCheckModule(
  session: GhciSession,
  args: { module_path: string; module_name?: string }
): Promise<string> {
  // Step 1: Load the module
  const loadResult = await session.loadModule(args.module_path);
  const errors = parseGhcErrors(loadResult.output);
  const compileErrors = errors.filter((e) => e.severity === "error");
  const warnings = errors.filter((e) => e.severity === "warning");

  if (compileErrors.length > 0) {
    return JSON.stringify({
      success: false,
      compiled: false,
      errors: compileErrors,
      warnings,
      definitions: [],
      summary: `Module failed to compile: ${compileErrors.length} error(s)`,
    });
  }

  // Step 2: Determine module name from path if not given
  const moduleName =
    args.module_name ?? inferModuleName(args.module_path);

  // Step 3: Browse the module to get all exported definitions
  const browseResult = await session.execute(`:browse ${moduleName}`);
  const definitions = parseBrowseOutput(browseResult.output);

  // Step 4: Build summary
  const functions = definitions.filter((d) => d.kind === "function");
  const types = definitions.filter(
    (d) => d.kind === "type" || d.kind === "data"
  );
  const classes = definitions.filter((d) => d.kind === "class");

  return JSON.stringify({
    success: true,
    compiled: true,
    errors: [],
    warnings,
    definitions,
    summary: {
      total: definitions.length,
      functions: functions.length,
      types: types.length,
      classes: classes.length,
      warnings: warnings.length,
    },
    module: moduleName,
  });
}

/**
 * Infer a Haskell module name from a file path.
 * E.g. "src/HM/Infer.hs" -> "HM.Infer"
 */
function inferModuleName(filePath: string): string {
  return filePath
    .replace(/^src\//, "")
    .replace(/\.hs$/, "")
    .replace(/\//g, ".");
}

/**
 * Parse the output of GHCi :browse into structured definitions.
 *
 * Handles formats:
 *   functionName :: Type -> Type
 *   type TypeAlias :: Kind\ntype TypeAlias = Definition
 *   data DataType = Constructor1 | Constructor2
 *   class ClassName a where ...
 */
function parseBrowseOutput(output: string): ModuleDefinition[] {
  const definitions: ModuleDefinition[] = [];
  const lines = output.split("\n");

  let i = 0;
  while (i < lines.length) {
    const line = lines[i]!.trim();

    // Skip empty lines and GHCi noise
    if (line === "" || line.startsWith("ghci>") || line.startsWith("--")) {
      i++;
      continue;
    }

    // Kind annotation: "type TypeName :: Kind"
    // Followed by either "type TypeName = ..." or "data TypeName = ..."
    if (line.startsWith("type ") && line.includes("::")) {
      const nameMatch = line.match(/^type\s+(\S+)/);
      if (nameMatch) {
        const defLine = i + 1 < lines.length ? lines[i + 1]!.trim() : "";
        if (defLine.startsWith("type ")) {
          definitions.push({
            name: nameMatch[1]!,
            type: defLine,
            kind: "type",
          });
          i += 2;
        } else if (defLine.startsWith("data ")) {
          // Collect multiline data declaration
          const fullData = collectContinuation(lines, i + 1);
          const dataName = defLine.match(/^data\s+(\S+)/);
          definitions.push({
            name: dataName?.[1] ?? nameMatch[1]!,
            type: fullData.text,
            kind: "data",
          });
          i = fullData.nextIndex;
        } else {
          definitions.push({
            name: nameMatch[1]!,
            type: line,
            kind: "type",
          });
          i++;
        }
        continue;
      }
    }

    // data type: "data DataType" or "data DataType = ..."
    if (line.startsWith("data ")) {
      const nameMatch = line.match(/^data\s+(\S+)/);
      if (nameMatch) {
        const fullData = collectContinuation(lines, i);
        definitions.push({
          name: nameMatch[1]!,
          type: fullData.text,
          kind: "data",
        });
        i = fullData.nextIndex;
        continue;
      }
    }

    // class: "class ClassName ..."
    if (line.startsWith("class ")) {
      const nameMatch = line.match(/^class\s+(?:\(.*?\)\s+=>\s+)?(\S+)/);
      if (nameMatch) {
        definitions.push({
          name: nameMatch[1]!,
          type: line,
          kind: "class",
        });
        i++;
        continue;
      }
    }

    // Function signature: "name :: Type"
    const sigMatch = line.match(/^(\S+)\s+::\s+(.+)/);
    if (sigMatch) {
      // Type might span multiple lines (indented continuation)
      let fullType = sigMatch[2]!;
      while (
        i + 1 < lines.length &&
        lines[i + 1]!.match(/^\s+/) &&
        !lines[i + 1]!.trim().startsWith("type ") &&
        !lines[i + 1]!.trim().startsWith("data ") &&
        !lines[i + 1]!.trim().startsWith("class ")
      ) {
        i++;
        fullType += " " + lines[i]!.trim();
      }
      definitions.push({
        name: sigMatch[1]!,
        type: fullType,
        kind: "function",
      });
      i++;
      continue;
    }

    i++;
  }

  return definitions;
}

/**
 * Collect a multiline declaration starting at `startIndex`.
 * Continuation lines are indented (start with whitespace).
 * Returns the full text and the next index to process.
 */
function collectContinuation(
  lines: string[],
  startIndex: number
): { text: string; nextIndex: number } {
  let text = lines[startIndex]!.trim();
  let i = startIndex + 1;
  while (i < lines.length) {
    const raw = lines[i]!;
    // Continuation lines start with whitespace
    if (raw.match(/^\s/) && raw.trim() !== "") {
      text += " " + raw.trim();
      i++;
    } else {
      break;
    }
  }
  return { text, nextIndex: i };
}
