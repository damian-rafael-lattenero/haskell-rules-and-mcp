/**
 * Property Store — persists QuickCheck properties to disk so they survive
 * across conversations. Enables regression testing of algebraic contracts.
 *
 * Store file: <projectDir>/.haskell-flows/properties.json
 */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import path from "node:path";

export interface PropertyRecord {
  property: string;
  /** Module path used as load context when running the property. */
  module: string;
  /**
   * Module path this property is semantically testing.
   * Distinct from `module` (load context): e.g. a property for Expr.Eval loaded
   * via Expr.Syntax (where Arbitrary lives) has module="src/Expr/Syntax.hs" but
   * tests_module="src/Expr/Eval.hs".
   * When present, regression filtering uses this field instead of `module`.
   */
  tests_module?: string;
  functionName?: string;
  law?: string;
  lastPassed: string; // ISO date
  passCount: number;
}

export interface PropertyStore {
  version: 1;
  properties: PropertyRecord[];
}

function storePath(projectDir: string): string {
  return path.join(projectDir, ".haskell-flows", "properties.json");
}

export async function loadStore(projectDir: string): Promise<PropertyStore> {
  try {
    const data = await readFile(storePath(projectDir), "utf-8");
    return JSON.parse(data) as PropertyStore;
  } catch {
    return { version: 1, properties: [] };
  }
}

export async function saveStore(projectDir: string, store: PropertyStore): Promise<void> {
  const dir = path.dirname(storePath(projectDir));
  await mkdir(dir, { recursive: true });
  await writeFile(storePath(projectDir), JSON.stringify(store, null, 2), "utf-8");
}

export async function saveProperty(
  projectDir: string,
  record: {
    property: string;
    module: string;
    functionName?: string;
    law?: string;
    /** Semantic module being tested — used for regression filtering. */
    tests_module?: string;
  }
): Promise<void> {
  const store = await loadStore(projectDir);
  // Deduplicate by property string — a property is unique regardless of which
  // module it was run from. Prevents duplicates from batch vs individual runs.
  const existing = store.properties.find(
    (p) => p.property === record.property
  );
  if (existing) {
    existing.lastPassed = new Date().toISOString();
    existing.passCount++;
    // Update module to the most specific one (prefer non-"unknown")
    if (record.module !== "unknown" && existing.module === "unknown") {
      existing.module = record.module;
    }
    // Update tests_module if we now have one and didn't before
    if (record.tests_module && !existing.tests_module) {
      existing.tests_module = record.tests_module;
    }
  } else {
    store.properties.push({
      ...record,
      lastPassed: new Date().toISOString(),
      passCount: 1,
    });
  }
  await saveStore(projectDir, store);
}

/**
 * Return properties associated with a module.
 * Filters by `tests_module` when present (semantic target), falls back to
 * `module` (load context) for records that pre-date the tests_module field.
 */
export async function getModuleProperties(
  projectDir: string,
  modulePath: string
): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties.filter(
    (p) => (p.tests_module ?? p.module) === modulePath
  );
}

export async function getAllProperties(projectDir: string): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties;
}
