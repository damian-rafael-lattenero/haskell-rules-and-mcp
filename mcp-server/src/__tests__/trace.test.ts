import { describe, it, expect, vi } from "vitest";
import { parseTraceOutput } from "../parsers/trace-parser.js";
import { handleTrace } from "../tools/trace.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("parseTraceOutput", () => {
  it("parses output with trace lines and result", () => {
    const parsed = parseTraceOutput(">> x = 42\n>> y = 10\n52\n");
    expect(parsed.traceLines).toEqual(["x = 42", "y = 10"]);
    expect(parsed.result).toBe("52");
    expect(parsed.error).toBeUndefined();
  });

  it("parses output with no trace lines", () => {
    const parsed = parseTraceOutput("42\n");
    expect(parsed.traceLines).toEqual([]);
    expect(parsed.result).toBe("42");
    expect(parsed.error).toBeUndefined();
  });

  it("parses output with exception", () => {
    const parsed = parseTraceOutput("*** Exception: Prelude.undefined\n");
    expect(parsed.error).toBeDefined();
    expect(parsed.error).toContain("*** Exception:");
  });

  it("parses empty output", () => {
    const parsed = parseTraceOutput("");
    expect(parsed.traceLines).toEqual([]);
    expect(parsed.result).toBe("");
    expect(parsed.error).toBeUndefined();
  });

  it("parses output with Show error", () => {
    const parsed = parseTraceOutput("No instance for (Show Foo)\n");
    expect(parsed.error).toBeDefined();
    expect(parsed.error).toContain("No instance for (Show");
  });
});

describe("handleTrace", () => {
  it("wraps expression with traceShowId when no trace_points", async () => {
    const session = createMockSession({
      execute: vi.fn()
        .mockResolvedValueOnce({ output: "", success: true })          // import Debug.Trace
        .mockResolvedValueOnce({ output: ">> 42\n42\n", success: true }), // traceShowId
    });

    const result = JSON.parse(await handleTrace(session, { expression: "21 + 21" }));
    expect(result.success).toBe(true);
    expect(result.expression).toBe("traceShowId (21 + 21)");
    expect(session.execute).toHaveBeenCalledWith("import Debug.Trace");
    expect(session.execute).toHaveBeenCalledWith("traceShowId (21 + 21)");
  });

  it("wraps each trace_point with trace", async () => {
    const session = createMockSession({
      execute: vi.fn()
        .mockResolvedValueOnce({ output: "", success: true })                        // import
        .mockResolvedValueOnce({ output: ">> x = 5\n>> y = 3\n8\n", success: true }), // trace
    });

    const result = JSON.parse(
      await handleTrace(session, {
        expression: "x + y",
        trace_points: ["x", "y"],
      })
    );
    expect(result.success).toBe(true);
    expect(result.traceLines).toEqual(["x = 5", "y = 3"]);
    expect(result.result).toBe("8");
    // The expression should be nested: y wraps (x wraps (expr))
    expect(result.expression).toContain('trace (">> x = "');
    expect(result.expression).toContain('trace (">> y = "');
  });

  it("imports Debug.Trace before executing", async () => {
    const executeFn = vi.fn()
      .mockResolvedValueOnce({ output: "", success: true })
      .mockResolvedValueOnce({ output: "42\n", success: true });

    const session = createMockSession({ execute: executeFn });

    await handleTrace(session, { expression: "42" });

    // First call should be the import
    expect(executeFn.mock.calls[0][0]).toBe("import Debug.Trace");
    // Second call should be the wrapped expression
    expect(executeFn.mock.calls[1][0]).toContain("traceShowId");
  });

  it("returns parsed trace output", async () => {
    const session = createMockSession({
      execute: vi.fn()
        .mockResolvedValueOnce({ output: "", success: true })
        .mockResolvedValueOnce({ output: ">> val = 10\n10\n", success: true }),
    });

    const result = JSON.parse(await handleTrace(session, { expression: "let val = 10 in val" }));
    expect(result.success).toBe(true);
    expect(result.traceLines).toEqual(["val = 10"]);
    expect(result.result).toBe("10");
  });
});
