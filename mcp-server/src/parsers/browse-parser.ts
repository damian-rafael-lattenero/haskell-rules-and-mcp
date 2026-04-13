/**
 * Parser for GHCi :browse output.
 *
 * Extracts structured module definitions: functions, types, classes, data types.
 */

export interface ModuleDefinition {
  name: string;
  type: string;
  kind: "function" | "type" | "class" | "data" | "instance";
}

/**
 * Infer a Haskell module name from a file path.
 * E.g. "src/HM/Infer.hs" -> "HM.Infer"
 */
export function inferModuleName(filePath: string): string {
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
export function parseBrowseOutput(output: string): ModuleDefinition[] {
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
        // Consume indented class body and extract method signatures
        while (
          i + 1 < lines.length &&
          lines[i + 1]!.match(/^\s/) &&
          lines[i + 1]!.trim() !== ""
        ) {
          i++;
          const bodyLine = lines[i]!.trim();
          if (bodyLine.startsWith("{-#")) continue;
          const methodMatch = bodyLine.match(/^(\S+)\s+::\s+(.+)/);
          if (methodMatch) {
            definitions.push({
              name: methodMatch[1]!,
              type: methodMatch[2]!,
              kind: "function",
            });
          }
        }
        i++;
        continue;
      }
    }

    // Function signature: "name :: Type"
    const sigMatch = line.match(/^(\S+)\s+::\s+(.+)/);
    if (sigMatch) {
      let fullType = sigMatch[2]!;
      while (
        i + 1 < lines.length &&
        lines[i + 1]!.match(/^\s+/) &&
        !lines[i + 1]!.trim().startsWith("type ") &&
        !lines[i + 1]!.trim().startsWith("data ") &&
        !lines[i + 1]!.trim().startsWith("class ") &&
        !lines[i + 1]!.trim().startsWith("{-#") &&
        !/^\S+\s+::/.test(lines[i + 1]!.trim())
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
 */
function collectContinuation(
  lines: string[],
  startIndex: number
): { text: string; nextIndex: number } {
  let text = lines[startIndex]!.trim();
  let i = startIndex + 1;
  while (i < lines.length) {
    const raw = lines[i]!;
    if (raw.match(/^\s/) && raw.trim() !== "") {
      text += " " + raw.trim();
      i++;
    } else {
      break;
    }
  }
  return { text, nextIndex: i };
}
