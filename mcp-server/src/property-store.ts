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
  module: string;
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
  record: { property: string; module: string; functionName?: string; law?: string }
): Promise<void> {
  const store = await loadStore(projectDir);
  const existing = store.properties.find(
    (p) => p.property === record.property && p.module === record.module
  );
  if (existing) {
    existing.lastPassed = new Date().toISOString();
    existing.passCount++;
  } else {
    store.properties.push({
      ...record,
      lastPassed: new Date().toISOString(),
      passCount: 1,
    });
  }
  await saveStore(projectDir, store);
}

export async function getModuleProperties(
  projectDir: string,
  modulePath: string
): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties.filter((p) => p.module === modulePath);
}

export async function getAllProperties(projectDir: string): Promise<PropertyRecord[]> {
  const store = await loadStore(projectDir);
  return store.properties;
}
