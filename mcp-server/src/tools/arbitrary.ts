import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseInfoOutput } from "../parsers/type-parser.js";
import { parseConstructors, type Constructor } from "../parsers/constructor-parser.js";
import type { ToolContext } from "./registry.js";

export interface ArbitraryResult {
  success: boolean;
  typeName: string;
  constructors: Constructor[];
  isRecursive: boolean;
  instance: string;
  hint: string;
  error?: string;
}

/**
 * Generate an Arbitrary instance for a Haskell data type.
 */
export async function handleArbitrary(
  session: GhciSession,
  args: { type_name: string }
): Promise<string> {
  const info = await session.infoOf(args.type_name);

  if (!info.success || info.output.includes("Not in scope")) {
    return JSON.stringify({
      success: false,
      typeName: args.type_name,
      constructors: [],
      isRecursive: false,
      instance: "",
      hint: "",
      error: info.output || `Type '${args.type_name}' not found`,
    });
  }

  const parsed = parseInfoOutput(info.output);

  // Handle type aliases: delegate to the underlying type's Arbitrary instance
  if (parsed.kind === "type-synonym") {
    // Extract RHS: "type Env = Map String Int" → "Map String Int"
    const aliasMatch = parsed.definition.match(/type\s+\S+(?:\s+\w+)*\s*=\s*(.+)/);
    const rhsType = aliasMatch ? aliasMatch[1]!.replace(/\s*--.*/, "").trim() : "";
    const inst = `instance Arbitrary ${args.type_name} where\n  arbitrary = arbitrary`;
    return JSON.stringify({
      success: true,
      typeName: args.type_name,
      constructors: [],
      isRecursive: false,
      instance: inst,
      hint: `Type alias — delegates to Arbitrary instance of underlying type (${rhsType}).`,
    });
  }

  if (parsed.kind !== "data" && parsed.kind !== "newtype") {
    return JSON.stringify({
      success: false,
      typeName: args.type_name,
      constructors: [],
      isRecursive: false,
      instance: "",
      hint: "",
      error: `'${args.type_name}' is a ${parsed.kind}, not a data or newtype. Arbitrary instances can only be generated for data types and newtypes.`,
    });
  }

  const constructors = parseConstructors(parsed.definition);

  if (constructors.length === 0) {
    return JSON.stringify({
      success: false,
      typeName: args.type_name,
      constructors: [],
      isRecursive: false,
      instance: "",
      hint: "",
      error: `Could not parse constructors for '${args.type_name}'. The type may use GADTs or existential types which are not supported.`,
    });
  }

  // Extract the base type name (without type variables) for recursion detection
  const baseTypeName = parsed.name;

  // Detect recursive constructors: any field type contains the base type name
  const isRecursive = constructors.some((ctor) =>
    ctor.fields.some((field) => fieldContainsType(field, baseTypeName))
  );

  // Detect type variables for constraints
  const typeVars = extractTypeVars(args.type_name, baseTypeName);

  let instance = isRecursive
    ? generateRecursiveInstance(args.type_name, baseTypeName, constructors, typeVars)
    : generateSimpleInstance(args.type_name, baseTypeName, constructors, typeVars);

  let hint = isRecursive
    ? "This type is recursive. The generated instance uses 'sized' to prevent infinite generation. " +
      "Base cases (non-recursive constructors) are used at size 0."
    : "This type is non-recursive. The generated instance uses 'oneof' to pick a random constructor.";

  // Validate the instance in GHCi — if it fails with missing constraints,
  // parse the error and add the required constraints automatically.
  const validated = await validateInstance(session, instance);
  if (validated.fixedInstance) {
    instance = validated.fixedInstance;
    hint += ` Added constraints: ${validated.addedConstraints!.join(", ")}.`;
  }

  return JSON.stringify({
    success: true,
    typeName: args.type_name,
    constructors,
    isRecursive,
    instance,
    hint,
  });
}

/**
 * Validate a generated Arbitrary instance by trying to evaluate it in GHCi.
 * If GHC reports missing constraints (e.g. "No instance for (Ord k)"),
 * parse them from the error and rebuild the instance with the extra constraints.
 */
async function validateInstance(
  session: GhciSession,
  instance: string
): Promise<{ fixedInstance?: string; addedConstraints?: string[] }> {
  // Try loading the instance in GHCi
  const lines = instance.split("\n");
  const testResult = await session.executeBlock(lines);

  if (testResult.success && !testResult.output.includes("No instance for")) {
    return {};
  }

  // Parse missing constraints from "No instance for (Constraint) arising from ..."
  const missingConstraints: string[] = [];
  const pattern = /No instance for \(([^)]+)\)/g;
  let match;
  while ((match = pattern.exec(testResult.output)) !== null) {
    missingConstraints.push(match[1]!.trim());
  }

  if (missingConstraints.length === 0) {
    return {};
  }

  // Rebuild instance with additional constraints
  const fixedInstance = instance.replace(
    /^instance\s+(?:\(([^)]*)\)\s*=>\s*)?/,
    (_fullMatch, existingConstraints?: string) => {
      const existing = existingConstraints
        ? existingConstraints.split(",").map((c: string) => c.trim()).filter(Boolean)
        : [];
      const all = [...new Set([...existing, ...missingConstraints])];
      return `instance (${all.join(", ")}) => `;
    }
  );

  return { fixedInstance, addedConstraints: missingConstraints };
}

/**
 * Exported for testing.
 *
 * Check if a field type string references the given type name as an actual
 * type reference (not as a module-path prefix).
 *
 * Problem: `\bExpr\b` matches inside `"Expr.Syntax.Name"` because `Expr` is
 * followed by `.` — which is NOT a word character, so `\b` fires.  This
 * causes constructors like `Var :: Expr.Syntax.Name -> Expr` to be misclassified
 * as recursive in `Expr` (because the `Name` field "contains" `Expr`), leading
 * to `Var <$> sub` in the generated instance where `sub :: Gen Expr` but
 * `Var :: String -> Expr` — a type error.
 *
 * Fix: require that the type name is NOT immediately followed by a dot, ruling
 * out qualified module-path prefixes like `Expr.Syntax.Name`.
 */
export function fieldContainsType(field: string, typeName: string): boolean {
  const escaped = typeName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // \b<Name>(?!\.) — word boundary before, negative lookahead for dot after.
  return new RegExp(`\\b${escaped}(?!\\.)\\b`).test(field);
}

/**
 * Extract type variables from the full type name.
 * e.g. "Maybe a" with baseName "Maybe" -> ["a"]
 * e.g. "Either a b" with baseName "Either" -> ["a", "b"]
 * e.g. "Lit" with baseName "Lit" -> []
 */
function extractTypeVars(fullTypeName: string, baseName: string): string[] {
  const parts = fullTypeName.trim().split(/\s+/);
  // Skip the base name (might be first token or might match)
  const baseIdx = parts.indexOf(baseName);
  if (baseIdx >= 0) {
    return parts.slice(baseIdx + 1).filter((p) => /^[a-z]/.test(p));
  }
  // Fallback: everything after the first token
  return parts.slice(1).filter((p) => /^[a-z]/.test(p));
}

/**
 * Generate a simple (non-recursive) Arbitrary instance using oneof.
 */
function generateSimpleInstance(
  fullTypeName: string,
  _baseName: string,
  constructors: Constructor[],
  typeVars: string[]
): string {
  const constraint = typeVars.length > 0
    ? `(${typeVars.map((v) => `Arbitrary ${v}`).join(", ")}) => `
    : "";

  const ctorGens = constructors.map((ctor) => {
    if (ctor.fields.length === 0) {
      return `    pure ${ctor.name}`;
    }
    const parts = [ctor.name, ...ctor.fields.map(() => "arbitrary")];
    return `    ${parts[0]} <$> ${parts.slice(1).join(" <*> ")}`;
  });

  const body =
    constructors.length === 1
      ? ctorGens[0]!.trim()
      : `oneof\n${ctorGens.map((g) => `  [ ${g.trim()} ]`).join("\n").replace(/\] \[/g, "]\n  , [").replace(/\[ {2}/g, "[ ").replace(/\]$/gm, " ]")}`;

  // For a single constructor, simpler output
  if (constructors.length === 1) {
    return (
      `instance ${constraint}Arbitrary (${fullTypeName}) where\n` +
      `  arbitrary = ${body}`
    );
  }

  const oneofItems = constructors.map((ctor) => {
    if (ctor.fields.length === 0) {
      return `pure ${ctor.name}`;
    }
    return `${ctor.name} <$> ${ctor.fields.map(() => "arbitrary").join(" <*> ")}`;
  });

  return (
    `instance ${constraint}Arbitrary (${fullTypeName}) where\n` +
    `  arbitrary = oneof\n` +
    oneofItems.map((item, i) => {
      const prefix = i === 0 ? "    [ " : "    , ";
      return `${prefix}${item}`;
    }).join("\n") +
    "\n    ]"
  );
}

/**
 * Generate a recursive Arbitrary instance using sized.
 */
function generateRecursiveInstance(
  fullTypeName: string,
  baseName: string,
  constructors: Constructor[],
  typeVars: string[]
): string {
  const constraint = typeVars.length > 0
    ? `(${typeVars.map((v) => `Arbitrary ${v}`).join(", ")}) => `
    : "";

  // Separate base (non-recursive) and recursive constructors
  const baseCases = constructors.filter(
    (ctor) => !ctor.fields.some((f) => fieldContainsType(f, baseName))
  );
  const recursiveCases = constructors;

  // Calculate K for resize: max number of recursive fields + 1
  let maxRecursiveFields = 1;
  for (const ctor of recursiveCases) {
    const count = ctor.fields.filter((f) => fieldContainsType(f, baseName)).length;
    if (count > maxRecursiveFields) maxRecursiveFields = count;
  }
  const k = maxRecursiveFields + 1;

  // If there are no base cases, use all constructors for size 0 too
  // (they'll just get very small via resize)
  const baseCaseCtors = baseCases.length > 0 ? baseCases : constructors;

  const formatCtor = (ctor: Constructor, useSub: boolean): string => {
    if (ctor.fields.length === 0) {
      return `pure ${ctor.name}`;
    }
    const fieldGens = ctor.fields.map((f) =>
      useSub && fieldContainsType(f, baseName) ? "sub" : "arbitrary"
    );
    return `${ctor.name} <$> ${fieldGens.join(" <*> ")}`;
  };

  const baseItems = baseCaseCtors.map((ctor) => formatCtor(ctor, false));
  const recursiveItems = recursiveCases.map((ctor) => formatCtor(ctor, true));

  const formatOneof = (items: string[]): string => {
    if (items.length === 1) return items[0]!;
    return (
      "oneof\n" +
      items
        .map((item, i) => {
          const prefix = i === 0 ? "          [ " : "          , ";
          return `${prefix}${item}`;
        })
        .join("\n") +
      "\n          ]"
    );
  };

  return (
    `instance ${constraint}Arbitrary (${fullTypeName}) where\n` +
    `  arbitrary = sized go\n` +
    `    where\n` +
    `      go 0 = ${formatOneof(baseItems)}\n` +
    `      go n = let sub = resize (n \`div\` ${k}) arbitrary\n` +
    `             in ${formatOneof(recursiveItems)}`
  );
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_arbitrary",
    "Generate a QuickCheck Arbitrary instance for a Haskell data type. " +
      "Uses GHCi :i to inspect the type, then generates an appropriate Arbitrary instance. " +
      "Handles recursive types with 'sized' and non-recursive types with 'oneof'.",
    {
      type_name: z.string().describe(
        'The type name to generate an Arbitrary instance for. Examples: "Lit", "Expr", "Maybe a"'
      ),
    },
    async ({ type_name }) => {
      const session = await ctx.getSession();
      const result = await handleArbitrary(session, { type_name });
      // Mark that Arbitrary instances have been defined for the active module
      try {
        const parsed = JSON.parse(result);
        if (parsed.success) {
          const activeModule = ctx.getWorkflowState().activeModule;
          if (activeModule) {
            ctx.updateModuleProgress(activeModule, { arbitraryInstancesDefined: true });
          }
        }
      } catch { /* non-fatal */ }
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
