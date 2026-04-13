import { describe, it, expect } from "vitest";
import { handleReferences } from "../tools/references.js";
import path from "node:path";

const FIXTURE_DIR = path.resolve(
  import.meta.dirname,
  "fixtures/test-project"
);

describe("handleReferences", () => {
  it("finds references to a function in the fixture project", async () => {
    const result = JSON.parse(await handleReferences(FIXTURE_DIR, { name: "add" }));
    expect(result.success).toBe(true);
    expect(result.count).toBeGreaterThan(0);
    // "add" should be found in TestLib.hs
    const files = result.references.map((r: any) => r.file);
    expect(files.some((f: string) => f.includes("TestLib.hs"))).toBe(true);
  });

  it("returns empty for non-existent name", async () => {
    const result = JSON.parse(await handleReferences(FIXTURE_DIR, { name: "totallyNonExistentXYZ123" }));
    expect(result.success).toBe(true);
    expect(result.count).toBe(0);
  });

  it("each reference has file, line, context", async () => {
    const result = JSON.parse(await handleReferences(FIXTURE_DIR, { name: "add" }));
    if (result.count > 0) {
      const ref = result.references[0];
      expect(ref).toHaveProperty("file");
      expect(ref).toHaveProperty("line");
      expect(ref).toHaveProperty("context");
      expect(typeof ref.line).toBe("number");
    }
  });
});
