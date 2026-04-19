/**
 * BUG-2 coverage: `loadStore` MUST distinguish ENOENT (fresh project) from
 * JSON parse errors (corrupt file) — the old implementation dropped every
 * saved property on a single malformed byte by treating every error as
 * "file not present".
 */
import { describe, it, expect } from "vitest";
import { mkdtemp, writeFile, readFile, rm, mkdir, readdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import { loadStore, saveStore } from "../property-store.js";

describe("property-store corruption recovery (BUG-2)", () => {
  it("returns an empty store when properties.json does not exist (ENOENT)", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "ps-enoent-"));
    try {
      const store = await loadStore(dir);
      expect(store).toEqual({ version: 1, properties: [] });
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("backs up a corrupt properties.json and returns empty store", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "ps-corrupt-"));
    try {
      const file = path.join(dir, ".haskell-flows", "properties.json");
      await mkdir(path.dirname(file), { recursive: true });
      await writeFile(file, "this is not { valid JSON", "utf-8");

      // Silence the console.warn from loadStore for this test — we assert
      // on the filesystem side-effect, not stderr noise.
      const originalWarn = console.warn;
      console.warn = () => {};
      try {
        const store = await loadStore(dir);
        expect(store).toEqual({ version: 1, properties: [] });
      } finally {
        console.warn = originalWarn;
      }

      const names = await readdir(path.dirname(file));
      const backups = names.filter((n) => n.startsWith("properties.json.corrupt-"));
      expect(backups.length).toBe(1);
      const backupContent = await readFile(path.join(path.dirname(file), backups[0]!), "utf-8");
      expect(backupContent).toBe("this is not { valid JSON");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("treats schema-mismatched JSON as corruption (no silent data loss)", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "ps-schema-"));
    try {
      const file = path.join(dir, ".haskell-flows", "properties.json");
      await mkdir(path.dirname(file), { recursive: true });
      // Parsable JSON but missing `properties` array — must be quarantined,
      // not silently returned as "empty".
      await writeFile(file, JSON.stringify({ version: 1, items: [] }), "utf-8");

      const originalWarn = console.warn;
      console.warn = () => {};
      try {
        const store = await loadStore(dir);
        expect(store.properties).toEqual([]);
      } finally {
        console.warn = originalWarn;
      }

      const names = await readdir(path.dirname(file));
      expect(names.some((n) => n.startsWith("properties.json.corrupt-"))).toBe(true);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("preserves saved properties across save/load roundtrip (no regression)", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "ps-roundtrip-"));
    try {
      await saveStore(dir, {
        version: 1,
        properties: [
          {
            property: "\\x -> x == (x :: Int)",
            module: "src/Foo.hs",
            lastPassed: new Date().toISOString(),
            passCount: 3,
          },
        ],
      });
      const reloaded = await loadStore(dir);
      expect(reloaded.properties).toHaveLength(1);
      expect(reloaded.properties[0]?.passCount).toBe(3);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
