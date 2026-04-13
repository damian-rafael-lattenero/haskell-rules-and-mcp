/**
 * Parse the output of GHCi's :t command into a clean type string.
 *
 * GHCi :t output format:
 *   expression :: Type
 *   or for operators:
 *   (++) :: forall a. [a] -> [a] -> [a]
 */
export function parseTypeOutput(output: string): {
  expression: string;
  type: string;
} | null {
  // Match "expr :: type" pattern, handling multiline types
  const match = output.match(/^(.+)\s+::\s+([\s\S]+)$/m);
  if (!match) return null;

  const expression = match[1]!.trim();
  // Collapse multiline types into a single line
  const type = match[2]!.replace(/\s+/g, " ").trim();

  return { expression, type };
}

/**
 * Parse the output of GHCi's :i command.
 */
export function parseInfoOutput(output: string): {
  kind: "type" | "class" | "data" | "newtype" | "type-synonym" | "function" | "unknown";
  name: string;
  definition: string;
  instances?: string[];
} {
  const lines = output.trim().split("\n");
  if (lines.length === 0) {
    return { kind: "unknown", name: "", definition: output };
  }

  const firstLine = lines[0]!;
  let kind: "type" | "class" | "data" | "newtype" | "type-synonym" | "function" | "unknown" = "unknown";

  if (firstLine.startsWith("class ")) kind = "class";
  else if (firstLine.startsWith("data ")) kind = "data";
  else if (firstLine.startsWith("newtype ")) kind = "newtype";
  else if (firstLine.startsWith("type role ")) kind = "unknown";
  else if (firstLine.startsWith("type ")) kind = "type-synonym";
  else if (firstLine.includes(" :: ")) kind = "function";

  // When the first line is a kind annotation ("type X :: Kind") or a role annotation
  // ("type role X ..."), check subsequent lines to determine the actual definition kind
  if ((kind === "type-synonym" && firstLine.includes(" :: ")) || kind === "unknown") {
    for (let i = 1; i < lines.length; i++) {
      const l = lines[i]!.trimStart();
      if (l.startsWith("data ")) { kind = "data"; break; }
      if (l.startsWith("newtype ")) { kind = "newtype"; break; }
      if (l.startsWith("class ")) { kind = "class"; break; }
      if (l.startsWith("type ") && l.includes(" = ")) break; // actual type synonym
      if (l.startsWith("instance ") || l.startsWith("-- ")) break;
    }
  }

  // Extract instances if present
  const instanceLines = lines.filter((l) => l.trimStart().startsWith("instance "));
  const instances = instanceLines.length > 0 ? instanceLines.map((l) => l.trim()) : undefined;

  // Extract name (handle "type role X ..." specially)
  const roleMatch = firstLine.match(/^type role\s+(\S+)/);
  const nameMatch = firstLine.match(
    /^(?:class|data|newtype|type)\s+(?:\([^)]+\)\s+=>\s+)?(\S+)/
  );
  const funcNameMatch = firstLine.match(/^(\S+)\s+::/);
  const name = roleMatch?.[1] ?? nameMatch?.[1] ?? funcNameMatch?.[1] ?? firstLine.split(/\s/)[0] ?? "";

  return {
    kind,
    name,
    definition: output.trim(),
    instances,
  };
}
