import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";

export interface FixResult {
  success: boolean;
  patch?: string;
  message: string;
  applied?: boolean;
}

/**
 * Auto-fix common GHC warnings.
 */
export async function fixWarning(
  projectDir: string,
  file: string,
  line: number,
  code: string,
  apply: boolean = false
): Promise<FixResult> {
  const absPath = path.resolve(projectDir, file);
  
  try {
    const content = await readFile(absPath, "utf-8");
    const lines = content.split("\n");

    let fixResult: FixResult;

    switch (code) {
      case "GHC-40910": // unused-matches
        fixResult = fixUnusedMatches(lines, line);
        break;
      case "GHC-38417": // unused-import
        fixResult = fixUnusedImport(lines, line);
        break;
      case "GHC-38417": // unused-top-bind
        fixResult = fixUnusedTopBind(lines, line);
        break;
      default:
        return {
          success: false,
          message: `No auto-fix available for warning code ${code}`
        };
    }

    if (fixResult.success && apply && fixResult.patch) {
      // Apply the fix
      const fixedContent = applyPatchToContent(lines, fixResult.patch);
      await writeFile(absPath, fixedContent);
      return {
        ...fixResult,
        applied: true,
        message: `Applied fix for ${code} at ${file}:${line}`
      };
    }

    return fixResult;
  } catch (error) {
    return {
      success: false,
      message: `Failed to fix warning: ${error instanceof Error ? error.message : String(error)}`
    };
  }
}

/**
 * Fix unused-matches warning by replacing the unused variable with _.
 * Example: "eval env (Lit n)" -> "eval _ (Lit n)"
 */
function fixUnusedMatches(lines: string[], line: number): FixResult {
  const lineContent = lines[line - 1];
  if (!lineContent) {
    return { success: false, message: "Line not found" };
  }

  // Pattern: find unused variable in pattern match
  // Look for word boundaries to avoid replacing parts of other identifiers
  const pattern = /\b(\w+)\b(?=\s+\()/;
  const match = lineContent.match(pattern);

  if (match) {
    const unusedVar = match[1];
    const fixed = lineContent.replace(new RegExp(`\\b${unusedVar}\\b`), "_");
    
    return {
      success: true,
      patch: JSON.stringify({
        line,
        old: lineContent,
        new: fixed
      }),
      message: `Replace '${unusedVar}' with '_' at line ${line}`
    };
  }

  return {
    success: false,
    message: "Could not detect unused variable pattern"
  };
}

/**
 * Fix unused-import warning by removing or commenting out the import.
 */
function fixUnusedImport(lines: string[], line: number): FixResult {
  const lineContent = lines[line - 1];
  if (!lineContent) {
    return { success: false, message: "Line not found" };
  }

  // Check if it's an import line
  if (!lineContent.trim().startsWith("import")) {
    return { success: false, message: "Line is not an import statement" };
  }

  // Comment out the import
  const fixed = "-- " + lineContent;

  return {
    success: true,
    patch: JSON.stringify({
      line,
      old: lineContent,
      new: fixed
    }),
    message: `Comment out unused import at line ${line}`
  };
}

/**
 * Fix unused-top-bind warning by prefixing with underscore.
 */
function fixUnusedTopBind(lines: string[], line: number): FixResult {
  const lineContent = lines[line - 1];
  if (!lineContent) {
    return { success: false, message: "Line not found" };
  }

  // Pattern: function definition like "helper = ..."
  const pattern = /^(\s*)(\w+)(\s*=)/;
  const match = lineContent.match(pattern);

  if (match) {
    const [, indent, name, equals] = match;
    const fixed = `${indent}_${name}${equals}${lineContent.substring(match[0].length)}`;
    
    return {
      success: true,
      patch: JSON.stringify({
        line,
        old: lineContent,
        new: fixed
      }),
      message: `Prefix unused binding '${name}' with underscore at line ${line}`
    };
  }

  return {
    success: false,
    message: "Could not detect unused top-level binding pattern"
  };
}

/**
 * Apply a patch to the content.
 */
function applyPatchToContent(lines: string[], patchJson: string): string {
  const patch = JSON.parse(patchJson);
  const newLines = [...lines];
  newLines[patch.line - 1] = patch.new;
  return newLines.join("\n");
}

/**
 * Check if a warning code can be auto-fixed.
 */
export function canAutoFix(code: string): boolean {
  return ["GHC-40910", "GHC-38417"].includes(code);
}

/**
 * Get a description of what the fix will do.
 */
export function getFixDescription(code: string): string {
  switch (code) {
    case "GHC-40910":
      return "Replace unused variable with underscore";
    case "GHC-38417":
      return "Comment out unused import";
    default:
      return "No auto-fix available";
  }
}
