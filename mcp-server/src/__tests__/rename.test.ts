import { describe, it, expect } from "vitest";
import { handleRename } from "../tools/rename.js";
import path from "node:path";

const FIXTURE_DIR = path.resolve(
  import.meta.dirname,
  "fixtures/test-project"
);

describe("handleRename", () => {
  it("previews rename with references found", async () => {
    const result = JSON.parse(
      await handleRename(FIXTURE_DIR, { oldName: "add", newName: "addInts" })
    );
    expect(result.success).toBe(true);
    expect(result.oldName).toBe("add");
    expect(result.newName).toBe("addInts");
    expect(result.totalReferences).toBeGreaterThan(0);
    expect(result.files.length).toBeGreaterThan(0);
    expect(result.message).toContain("reference");
  });

  it("rejects invalid Haskell identifier", async () => {
    const result = JSON.parse(
      await handleRename(FIXTURE_DIR, { oldName: "add", newName: "123bad" })
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("Invalid");
  });

  it("returns error for non-existent name", async () => {
    const result = JSON.parse(
      await handleRename(FIXTURE_DIR, { oldName: "nonExistentXYZ999", newName: "newName" })
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("No references");
  });

  it("accepts valid Haskell names with primes", async () => {
    const result = JSON.parse(
      await handleRename(FIXTURE_DIR, { oldName: "add", newName: "add'" })
    );
    // Should succeed (the name is valid even if it has a prime)
    expect(result.success).toBe(true);
  });
});
