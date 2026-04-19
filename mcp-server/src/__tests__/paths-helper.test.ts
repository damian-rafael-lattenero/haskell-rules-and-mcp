/**
 * REFACTOR-3 coverage: `resolveModulePath` is the single source of truth
 * for turning a caller-supplied `module_path` into an absolute path, and
 * MUST reject path-traversal attempts that escape the project root.
 */
import { describe, it, expect } from "vitest";
import path from "node:path";
import {
  resolveModulePath,
  tryResolveModulePath,
  PathTraversalError,
} from "../helpers/paths.js";

describe("resolveModulePath", () => {
  const ROOT = "/project";

  it("resolves a simple relative module path", () => {
    expect(resolveModulePath(ROOT, "src/Foo.hs")).toBe(path.join(ROOT, "src/Foo.hs"));
  });

  it("resolves nested paths", () => {
    expect(resolveModulePath(ROOT, "src/A/B/C.hs")).toBe(path.join(ROOT, "src/A/B/C.hs"));
  });

  it("accepts an absolute path that is inside the project", () => {
    expect(resolveModulePath(ROOT, path.join(ROOT, "src/Foo.hs"))).toBe(
      path.join(ROOT, "src/Foo.hs")
    );
  });

  it("accepts the project root itself (no escape)", () => {
    expect(resolveModulePath(ROOT, ".")).toBe(path.normalize(ROOT));
  });

  it("rejects `../` traversal to sibling", () => {
    expect(() => resolveModulePath(ROOT, "../sibling/Foo.hs")).toThrow(PathTraversalError);
  });

  it("rejects `../../` traversal to parent of parent", () => {
    expect(() => resolveModulePath(ROOT, "../../etc/passwd")).toThrow(PathTraversalError);
  });

  it("rejects absolute path outside the project", () => {
    expect(() => resolveModulePath(ROOT, "/etc/passwd")).toThrow(PathTraversalError);
  });

  it("rejects traversal that looks safe until resolved", () => {
    // `src/../../escape/Foo.hs` resolves to `../escape/Foo.hs` which is out.
    expect(() => resolveModulePath(ROOT, "src/../../escape/Foo.hs")).toThrow(PathTraversalError);
  });

  it("does NOT reject traversal that stays inside (src/../src/Foo)", () => {
    // Normalizes back into the root.
    expect(resolveModulePath(ROOT, "src/../src/Foo.hs")).toBe(path.join(ROOT, "src/Foo.hs"));
  });
});

describe("tryResolveModulePath", () => {
  it("returns ok:true with absPath for valid input", () => {
    const r = tryResolveModulePath("/root", "src/Foo.hs");
    expect(r.ok).toBe(true);
    if (r.ok) expect(r.absPath).toBe(path.join("/root", "src/Foo.hs"));
  });

  it("returns ok:false with a descriptive error for traversal", () => {
    const r = tryResolveModulePath("/root", "../escape.hs");
    expect(r.ok).toBe(false);
    if (!r.ok) {
      expect(r.error).toContain("escapes the project root");
      expect(r.error).toContain("../escape.hs");
    }
  });
});
