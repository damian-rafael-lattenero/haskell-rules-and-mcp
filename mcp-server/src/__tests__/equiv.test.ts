import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { GhciSession } from "../ghci-session.js";
import { checkEquivalence } from "../tools/equiv.js";
import path from "node:path";

const TEST_PROJECT = path.resolve(import.meta.dirname, "fixtures", "test-project");

describe("ghci_equiv", () => {
  let session: GhciSession;

  beforeEach(async () => {
    session = new GhciSession(TEST_PROJECT);
    await session.start();
  });

  afterEach(async () => {
    if (session.isAlive()) {
      await session.kill();
    }
  });

  it("detects equivalent expressions", async () => {
    const result = await checkEquivalence(session, "1 + 1", "2", {});
    
    expect(result.equivalent).toBe(true);
    expect(result.expr1Result).toBe("2");
    expect(result.expr2Result).toBe("2");
    expect(result.reason).toBeUndefined();
  });

  it("detects non-equivalent expressions", async () => {
    const result = await checkEquivalence(session, "1 + 1", "3", {});
    
    expect(result.equivalent).toBe(false);
    expect(result.reason).toBeDefined();
    expect(result.reason).toContain("≠");
  });

  it("uses context for evaluation", async () => {
    const result = await checkEquivalence(
      session,
      "x + 1",
      "6",
      { x: "5" }
    );
    
    expect(result.equivalent).toBe(true);
    expect(result.expr1Result).toBe("6");
    expect(result.expr2Result).toBe("6");
  });

  it("handles multiple variables in context", async () => {
    const result = await checkEquivalence(
      session,
      "x + y",
      "15",
      { x: "5", y: "10" }
    );
    
    expect(result.equivalent).toBe(true);
  });

  it("detects evaluation errors", async () => {
    const result = await checkEquivalence(
      session,
      "undefined",
      "5",
      {}
    );
    
    expect(result.equivalent).toBe(false);
    expect(result.reason).toContain("failed to evaluate");
  });

  it("compares complex expressions", async () => {
    const result = await checkEquivalence(
      session,
      "map (\\x -> x + 1) [1,2,3]",
      "[2,3,4]",
      {}
    );
    
    expect(result.equivalent).toBe(true);
  });

  it("handles string comparisons", async () => {
    const result = await checkEquivalence(
      session,
      '"hello" ++ " " ++ "world"',
      '"hello world"',
      {}
    );
    
    expect(result.equivalent).toBe(true);
  });

  it("detects semantic differences in lists", async () => {
    const result = await checkEquivalence(
      session,
      "[1,2,3]",
      "[3,2,1]",
      {}
    );
    
    expect(result.equivalent).toBe(false);
  });
});
