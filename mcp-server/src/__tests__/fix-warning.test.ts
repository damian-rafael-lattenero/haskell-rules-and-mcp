import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { fixWarning, canAutoFix, getFixDescription } from "../tools/fix-warning.js";
import { writeFile, mkdir, rm } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const TEST_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "fixtures", "fix-warning-test");

describe("fixWarning", () => {
  beforeEach(async () => {
    await mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it("fixes unused-matches (GHC-40910)", async () => {
    const testFile = path.join(TEST_DIR, "Test.hs");
    const content = `module Test where

eval env (Lit n) = Right n
eval env (Add e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  return (v1 + v2)
`;
    await writeFile(testFile, content);

    const result = await fixWarning(TEST_DIR, "Test.hs", 3, "GHC-40910", false);
    
    expect(result.success).toBe(true);
    expect(result.patch).toBeDefined();
    expect(result.message).toContain("Replace");
    expect(result.message).toContain("_");
  });

  it("fixes unused-import (GHC-38417)", async () => {
    const testFile = path.join(TEST_DIR, "Test.hs");
    const content = `module Test where

import Data.List (sort)
import Data.Map (Map)

foo :: Int
foo = 42
`;
    await writeFile(testFile, content);

    const result = await fixWarning(TEST_DIR, "Test.hs", 4, "GHC-38417", false);
    
    expect(result.success).toBe(true);
    expect(result.patch).toBeDefined();
    expect(result.message).toContain("Comment out");
  });

  it("applies fix when apply=true", async () => {
    const testFile = path.join(TEST_DIR, "Test.hs");
    const content = `module Test where

eval env (Lit n) = Right n
`;
    await writeFile(testFile, content);

    const result = await fixWarning(TEST_DIR, "Test.hs", 3, "GHC-40910", true);
    
    expect(result.success).toBe(true);
    expect(result.applied).toBe(true);
    expect(result.message).toContain("Applied fix");
  });

  it("returns error for unsupported warning code", async () => {
    const testFile = path.join(TEST_DIR, "Test.hs");
    await writeFile(testFile, "module Test where\n");

    const result = await fixWarning(TEST_DIR, "Test.hs", 1, "GHC-99999", false);
    
    expect(result.success).toBe(false);
    expect(result.message).toContain("No auto-fix available");
  });
});

describe("canAutoFix", () => {
  it("returns true for supported codes", () => {
    expect(canAutoFix("GHC-40910")).toBe(true);
    expect(canAutoFix("GHC-38417")).toBe(true);
  });

  it("returns false for unsupported codes", () => {
    expect(canAutoFix("GHC-99999")).toBe(false);
    expect(canAutoFix("GHC-12345")).toBe(false);
  });
});

describe("getFixDescription", () => {
  it("returns description for supported codes", () => {
    expect(getFixDescription("GHC-40910")).toContain("underscore");
    expect(getFixDescription("GHC-38417")).toContain("Comment");
  });

  it("returns default message for unsupported codes", () => {
    expect(getFixDescription("GHC-99999")).toContain("No auto-fix");
  });
});
