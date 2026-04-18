/**
 * Unit coverage for the `label` field added to the property store (P2b) and
 * the sanitization/dedup helper used by ghci_quickcheck_export when emitting
 * `putStr "<label>: "` into the generated Spec.hs.
 */
import { describe, it, expect } from "vitest";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import { sanitizeLabel } from "../tools/export-tests.js";
import { saveProperty, loadStore } from "../property-store.js";

describe("sanitizeLabel", () => {
  it("keeps alnum, underscore and hyphen untouched", () => {
    expect(sanitizeLabel("addRightIdentity")).toBe("addRightIdentity");
    expect(sanitizeLabel("roundtrip_pretty-parse")).toBe("roundtrip_pretty-parse");
  });

  it("replaces whitespace with underscores", () => {
    expect(sanitizeLabel("add right identity")).toBe("add_right_identity");
  });

  it("strips newlines so the label never breaks the Haskell string literal", () => {
    expect(sanitizeLabel("foo\nbar")).toBe("foo_bar");
    expect(sanitizeLabel("foo\r\nbar")).toBe("foo_bar");
  });

  it("replaces unsafe characters (quotes, slashes, emoji) with underscore", () => {
    expect(sanitizeLabel('say "hi"')).toBe("say__hi");
    expect(sanitizeLabel("pretty/parse")).toBe("pretty_parse");
  });

  it("falls back to 'property' for empty or all-unsafe input", () => {
    expect(sanitizeLabel("")).toBe("property");
    expect(sanitizeLabel("???")).toBe("property");
    expect(sanitizeLabel("   ")).toBe("property");
  });

  it("strips leading/trailing underscores produced by the collapse step", () => {
    expect(sanitizeLabel("  foo  ")).toBe("foo");
    expect(sanitizeLabel("_bar_")).toBe("bar");
  });
});

describe("PropertyRecord.label end-to-end save/load", () => {
  it("persists the label and surfaces it on loadStore", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "label-store-"));
    try {
      await saveProperty(dir, {
        property: "\\x -> x == (x :: Int)",
        module: "src/Foo.hs",
        label: "identityOnInt",
      });
      const store = await loadStore(dir);
      expect(store.properties).toHaveLength(1);
      expect(store.properties[0]?.label).toBe("identityOnInt");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("promotes an absent label on a second save (label sticks)", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "label-promote-"));
    try {
      await saveProperty(dir, {
        property: "\\xs -> reverse (reverse xs) == (xs :: [Int])",
        module: "src/Foo.hs",
      });
      await saveProperty(dir, {
        property: "\\xs -> reverse (reverse xs) == (xs :: [Int])",
        module: "src/Foo.hs",
        label: "reverseInvolution",
      });
      const store = await loadStore(dir);
      expect(store.properties).toHaveLength(1);
      expect(store.properties[0]?.label).toBe("reverseInvolution");
      expect(store.properties[0]?.passCount).toBe(2);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("prefers the first label seen over a later conflicting one", async () => {
    // A run #1 with `label="first"` should win over a run #2 with `label="second"`
    // — this prevents silent renaming. Explicit deprecation goes through
    // ghci_property_lifecycle instead.
    const dir = await mkdtemp(path.join(tmpdir(), "label-sticky-"));
    try {
      await saveProperty(dir, {
        property: "\\x -> x + 0 == (x :: Int)",
        module: "src/Foo.hs",
        label: "first",
      });
      await saveProperty(dir, {
        property: "\\x -> x + 0 == (x :: Int)",
        module: "src/Foo.hs",
        label: "second",
      });
      const store = await loadStore(dir);
      expect(store.properties[0]?.label).toBe("first");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
