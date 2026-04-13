/**
 * Parser for GHCi :show imports output.
 *
 * Formats:
 *   import Prelude -- implicit
 *   import Data.Map.Strict qualified as Map
 *   import Data.List ( sort, nub )
 *   import TestLib
 */

export interface ImportInfo {
  module: string;
  qualified: boolean;
  alias?: string;
  items?: string[];
  implicit: boolean;
}

export function parseImportsOutput(output: string): ImportInfo[] {
  const imports: ImportInfo[] = [];
  const lines = output.trim().split("\n");

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed.startsWith("import ")) continue;

    const implicit = trimmed.includes("-- implicit");
    // Remove trailing comment
    const clean = trimmed.replace(/\s*--.*$/, "").trim();

    // Parse: import [qualified] ModuleName [as Alias] [(items)]
    const match = clean.match(
      /^import\s+(qualified\s+)?(\S+)(?:\s+qualified)?(?:\s+as\s+(\S+))?(?:\s*\(\s*(.*?)\s*\))?/
    );
    if (!match) continue;

    const qualified = !!match[1] || clean.includes(" qualified");
    const moduleName = match[2]!;
    const alias = match[3];
    const itemsStr = match[4];

    const items = itemsStr
      ? itemsStr.split(",").map((s) => s.trim()).filter((s) => s.length > 0)
      : undefined;

    imports.push({
      module: moduleName,
      qualified,
      alias,
      items,
      implicit,
    });
  }

  return imports;
}
