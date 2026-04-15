/**
 * Unit tests for ghci_property_lifecycle tool.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { handlePropertyLifecycle } from "../tools/property-lifecycle.js";
import {
  saveProperty,
  getAllProperties,
  getActiveProperties,
  type PropertyStore,
} from "../property-store.js";

const TEST_PROJECT_DIR = path.resolve(import.meta.dirname, "../../test-fixtures/property-lifecycle-test");

beforeEach(async () => {
  await mkdir(TEST_PROJECT_DIR, { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, ".haskell-flows"), { recursive: true });
});

afterEach(async () => {
  await rm(TEST_PROJECT_DIR, { recursive: true, force: true });
});

describe("ghci_property_lifecycle", () => {
  describe("action=list", () => {
    it("should list all properties when no module filter", async () => {
      // Setup: save 3 properties
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
        functionName: "identity",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x y -> x + y == y + x",
        module: "src/B.hs",
        functionName: "commutative",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\xs -> reverse (reverse xs) == xs",
        module: "src/A.hs",
        functionName: "doubleReverse",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "list" })
      );

      expect(result.success).toBe(true);
      expect(result.count).toBe(3);
      expect(result.active).toBe(3);
      expect(result.deprecated).toBe(0);
      expect(result.modules).toHaveLength(2);
    });

    it("should filter properties by module", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x y -> x + y == y + x",
        module: "src/B.hs",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "list",
          module: "src/A.hs",
        })
      );

      expect(result.success).toBe(true);
      expect(result.count).toBe(1);
      expect(result.modules[0].module).toBe("src/A.hs");
    });

    it("should show deprecated properties separately", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> True",
        module: "src/A.hs",
      });

      // Deprecate one
      await handlePropertyLifecycle(TEST_PROJECT_DIR, {
        action: "deprecate",
        property: "\\x -> True",
        reason: "Trivial property",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "list" })
      );

      expect(result.success).toBe(true);
      expect(result.count).toBe(2);
      expect(result.active).toBe(1);
      expect(result.deprecated).toBe(1);
    });
  });

  describe("action=remove", () => {
    it("should remove a property by exact match", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x y -> x + y == y + x",
        module: "src/B.hs",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "remove",
          property: "\\x -> x == x",
        })
      );

      expect(result.success).toBe(true);
      expect(result.message).toContain("removed");

      const remaining = await getAllProperties(TEST_PROJECT_DIR);
      expect(remaining).toHaveLength(1);
      expect(remaining[0].property).toBe("\\x y -> x + y == y + x");
    });

    it("should return false when property not found", async () => {
      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "remove",
          property: "\\x -> nonexistent",
        })
      );

      expect(result.success).toBe(false);
      expect(result.message).toContain("not found");
    });

    it("should require property parameter", async () => {
      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "remove" })
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain("required");
    });
  });

  describe("action=deprecate", () => {
    it("should mark property as deprecated", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "deprecate",
          property: "\\x -> x == x",
          reason: "Replaced with better version",
        })
      );

      expect(result.success).toBe(true);
      expect(result.message).toContain("deprecated");

      const all = await getAllProperties(TEST_PROJECT_DIR);
      expect(all[0].deprecated).toBe(true);
      expect(all[0].deprecation_reason).toBe("Replaced with better version");

      const active = await getActiveProperties(TEST_PROJECT_DIR);
      expect(active).toHaveLength(0);
    });

    it("should link to replacement property", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "deprecate",
          property: "\\x -> x == x",
          replaced_by: "\\x -> normalize x == normalize x",
        })
      );

      expect(result.success).toBe(true);
      expect(result.replaced_by).toBe("\\x -> normalize x == normalize x");

      const all = await getAllProperties(TEST_PROJECT_DIR);
      expect(all[0].replaced_by).toBe("\\x -> normalize x == normalize x");
    });
  });

  describe("action=replace", () => {
    it("should deprecate old and link to new", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> deserialize (serialize x) == Just x",
        module: "src/TestModule.hs",
      });

      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "replace",
          property: "\\x -> deserialize (serialize x) == Just x",
          replaced_by: "\\x -> isValid x ==> deserialize (serialize x) == Just x",
          reason: "Added precondition for valid inputs",
        })
      );

      expect(result.success).toBe(true);
      expect(result.old_property).toBe("\\x -> deserialize (serialize x) == Just x");
      expect(result.new_property).toBe("\\x -> isValid x ==> deserialize (serialize x) == Just x");

      const all = await getAllProperties(TEST_PROJECT_DIR);
      expect(all[0].deprecated).toBe(true);
      expect(all[0].replaced_by).toBe("\\x -> isValid x ==> deserialize (serialize x) == Just x");
      expect(all[0].deprecation_reason).toContain("Added precondition");
    });

    it("should require both property and replaced_by", async () => {
      const result = JSON.parse(
        await handlePropertyLifecycle(TEST_PROJECT_DIR, {
          action: "replace",
          property: "\\x -> x",
        })
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain("Both property and replaced_by");
    });
  });

  describe("integration with export", () => {
    it("should filter deprecated properties from active list", async () => {
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> x == x",
        module: "src/A.hs",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x -> True",
        module: "src/A.hs",
      });
      await saveProperty(TEST_PROJECT_DIR, {
        property: "\\x y -> x + y == y + x",
        module: "src/A.hs",
      });

      // Deprecate the trivial one
      await handlePropertyLifecycle(TEST_PROJECT_DIR, {
        action: "deprecate",
        property: "\\x -> True",
        reason: "Trivial",
      });

      const active = await getActiveProperties(TEST_PROJECT_DIR);
      expect(active).toHaveLength(2);
      expect(active.map((p) => p.property)).not.toContain("\\x -> True");
    });
  });
});
