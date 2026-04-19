/**
 * Property Store — persists QuickCheck properties to disk so they survive
 * across conversations. Enables regression testing of algebraic contracts.
 *
 * Store file: <projectDir>/.haskell-flows/properties.json
 */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import path from "node:path";
import { validatePropertyText } from "./parsers/property-validator.js";

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
  /**
   * Human-friendly identifier used by the exporter as the test suite label.
   * When present, takes priority over `law`/`functionName`/positional index
   * when generating `test/Spec.hs`. Sanitized before use (no quotes, no
   * newlines); duplicates get a `_2`, `_3` suffix at export time.
   */
  label?: string;
  lastPassed: string; // ISO date
  passCount: number;
  /** Property version for tracking replacements. Defaults to 1. */
  version?: number;
  /** Property ID that replaces this one (if deprecated). */
  replaced_by?: string;
  /** Whether this property is deprecated and should not be exported. */
  deprecated?: boolean;
  /** Reason for deprecation (if deprecated=true). */
  deprecation_reason?: string;
}

export interface PropertyStore {
  version: 1;
  properties: PropertyRecord[];
}

function storePath(projectDir: string): string {
  return path.join(projectDir, ".haskell-flows", "properties.json");
}

/**
 * Load the property store. Distinguishes three cases:
 *   1. File does not exist (ENOENT) → fresh empty store.
 *   2. File exists but is not valid JSON → back up the corrupt file with a
 *      `.corrupt-<timestamp>` suffix and return an empty store.  This is
 *      a **deliberate non-destructive recovery** — we never silently drop
 *      a user's saved properties without leaving forensic evidence on disk.
 *   3. File is valid JSON but does not match the expected shape → treated
 *      like (2): back up, start fresh.
 *
 * Historical bug: previous implementation caught every error as "file not
 * present" and silently returned an empty store, losing every saved
 * property on a single malformed byte. Fixed in Fase 5.
 */
export async function loadStore(projectDir: string): Promise<PropertyStore> {
  const file = storePath(projectDir);
  let data: string;
  try {
    data = await readFile(file, "utf-8");
  } catch (err) {
    const code = (err as NodeJS.ErrnoException | undefined)?.code;
    if (code === "ENOENT") {
      return { version: 1, properties: [] };
    }
    throw err; // permission / IO error — let the caller see it
  }

  try {
    const parsed = JSON.parse(data) as unknown;
    if (!parsed || typeof parsed !== "object" || !Array.isArray((parsed as { properties?: unknown }).properties)) {
      throw new Error("property-store: JSON parsed but schema mismatch (missing/invalid 'properties' array)");
    }
    return parsed as PropertyStore;
  } catch (parseErr) {
    // Preserve the corrupt file — never silently drop saved state.
    const backup = `${file}.corrupt-${Date.now()}`;
    try {
      await writeFile(backup, data, "utf-8");
    } catch {
      // If we can't even write the backup, continue anyway — the corrupt
      // content is still on disk at `file`, untouched.
    }
    // eslint-disable-next-line no-console
    console.warn(
      `[property-store] Could not parse ${file} (${(parseErr as Error).message}). ` +
      `Backed up to ${backup}. Starting with an empty store.`
    );
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
    /** Optional human-friendly label; used by the exporter for Spec.hs names. */
    label?: string;
  }
): Promise<void> {
  const validation = validatePropertyText(record.property);
  if (!validation.ok) {
    throw new Error(
      `Refusing to save invalid property: ${validation.issues.map((i) => i.message).join("; ")}`
    );
  }
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
    // Promote label if previously absent. We prefer the first label we see
    // over silent overwrite, so an explicit label on run #1 survives re-runs
    // that forget to pass it.
    if (record.label && !existing.label) {
      existing.label = record.label;
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

/**
 * Remove a property from the store by exact match.
 * Returns true if the property was found and removed.
 */
export async function removeProperty(
  projectDir: string,
  propertyText: string
): Promise<boolean> {
  const store = await loadStore(projectDir);
  const initialLength = store.properties.length;
  store.properties = store.properties.filter((p) => p.property !== propertyText);
  const removed = store.properties.length < initialLength;
  if (removed) {
    await saveStore(projectDir, store);
  }
  return removed;
}

/**
 * Deprecate a property by marking it as deprecated and optionally linking to a replacement.
 * The property remains in the store but will be filtered out of exports.
 */
export async function deprecateProperty(
  projectDir: string,
  propertyText: string,
  options?: {
    replaced_by?: string;
    reason?: string;
  }
): Promise<boolean> {
  const store = await loadStore(projectDir);
  const prop = store.properties.find((p) => p.property === propertyText);
  if (!prop) return false;

  prop.deprecated = true;
  if (options?.replaced_by) prop.replaced_by = options.replaced_by;
  if (options?.reason) prop.deprecation_reason = options.reason;

  await saveStore(projectDir, store);
  return true;
}

/**
 * Get all active (non-deprecated) properties.
 * Used by export-tests to filter out deprecated properties.
 */
export async function getActiveProperties(projectDir: string): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties.filter((p) => !p.deprecated);
}

/**
 * Get all active properties for a specific module.
 */
export async function getActiveModuleProperties(
  projectDir: string,
  modulePath: string
): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties.filter(
    (p) => !p.deprecated && (p.tests_module ?? p.module) === modulePath
  );
}
