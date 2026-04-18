/**
 * Unit coverage for the Fase 4 boundary-tolerant Zod coercers
 * (`zBool` / `zNum` / `zArray` / `zRecord`). Each helper must:
 *   1. Accept the canonical JSON shape unchanged.
 *   2. Accept the stringified form the Claude↔MCP boundary tends to emit.
 *   3. Reject inputs that are neither.
 *   4. NOT weaken `.strict()` unknown-key rejection — that's tested separately
 *      against the full `registerStrictTool` schema.
 */
import { describe, it, expect } from "vitest";
import { z } from "zod";
import { zArray, zBool, zNum, zRecord } from "../tools/registry.js";

describe("zBool()", () => {
  const schema = zBool();

  it("accepts canonical booleans", () => {
    expect(schema.parse(true)).toBe(true);
    expect(schema.parse(false)).toBe(false);
  });

  it("accepts string forms sent by the Claude↔MCP bridge", () => {
    expect(schema.parse("true")).toBe(true);
    expect(schema.parse("false")).toBe(false);
    expect(schema.parse("1")).toBe(true);
    expect(schema.parse("0")).toBe(false);
    // Case-insensitive
    expect(schema.parse("TRUE")).toBe(true);
    expect(schema.parse("False")).toBe(false);
  });

  it("rejects nonsensical strings", () => {
    expect(() => schema.parse("maybe")).toThrow();
    expect(() => schema.parse("")).toThrow();
  });

  it("rejects numbers and other primitives", () => {
    expect(() => schema.parse(1)).toThrow();
    expect(() => schema.parse(null as unknown as string)).toThrow();
    expect(() => schema.parse(undefined as unknown as string)).toThrow();
  });
});

describe("zNum()", () => {
  const schema = zNum();

  it("accepts canonical numbers", () => {
    expect(schema.parse(42)).toBe(42);
    expect(schema.parse(-3.14)).toBe(-3.14);
    expect(schema.parse(0)).toBe(0);
  });

  it("accepts numeric strings", () => {
    expect(schema.parse("42")).toBe(42);
    expect(schema.parse("-3.14")).toBe(-3.14);
    expect(schema.parse("0")).toBe(0);
  });

  it("rejects NaN / Infinity from either form", () => {
    expect(() => schema.parse(Number.NaN)).toThrow();
    expect(() => schema.parse(Number.POSITIVE_INFINITY)).toThrow();
    expect(() => schema.parse("NaN")).toThrow();
    expect(() => schema.parse("Infinity")).toThrow();
  });

  it("rejects non-numeric strings", () => {
    expect(() => schema.parse("abc")).toThrow();
    expect(() => schema.parse("1e")).toThrow();
  });
});

describe("zArray(T)", () => {
  const schema = zArray(z.string());

  it("accepts canonical arrays", () => {
    expect(schema.parse(["a", "b"])).toEqual(["a", "b"]);
    expect(schema.parse([])).toEqual([]);
  });

  it("accepts JSON-stringified arrays", () => {
    expect(schema.parse('["a","b"]')).toEqual(["a", "b"]);
    expect(schema.parse("[]")).toEqual([]);
  });

  it("rejects stringified arrays whose items fail the inner schema", () => {
    expect(() => schema.parse("[1, 2]")).toThrow();
  });

  it("rejects non-JSON strings", () => {
    expect(() => schema.parse("not json")).toThrow();
  });

  it("composes with complex inner schemas (objects)", () => {
    const objArr = zArray(z.object({ name: z.string(), count: z.number() }));
    expect(objArr.parse('[{"name":"a","count":1}]')).toEqual([
      { name: "a", count: 1 },
    ]);
    expect(objArr.parse([{ name: "b", count: 2 }])).toEqual([
      { name: "b", count: 2 },
    ]);
  });
});

describe("zRecord(T)", () => {
  const schema = zRecord(zArray(z.string()));

  it("accepts canonical records", () => {
    expect(schema.parse({ foo: ["a", "b"] })).toEqual({ foo: ["a", "b"] });
  });

  it("accepts JSON-stringified records", () => {
    expect(schema.parse('{"foo":["a","b"]}')).toEqual({ foo: ["a", "b"] });
  });

  it("rejects malformed string inputs", () => {
    expect(() => schema.parse("not-json")).toThrow();
    // Nested schema mismatch: values must be arrays of string
    expect(() => schema.parse('{"foo":"bar"}')).toThrow();
  });
});

describe("coercers preserve .strict() unknown-keys rejection", () => {
  it("a strict object schema using coerced fields still rejects unknown keys", () => {
    const shape = {
      flag: zBool().optional(),
      count: zNum().optional(),
      items: zArray(z.string()).optional(),
    };
    const strict = z.object(shape).strict();
    // Valid input passes
    expect(strict.parse({ flag: "true", count: "5", items: '["a"]' })).toEqual({
      flag: true,
      count: 5,
      items: ["a"],
    });
    // Unknown key rejected — strict() semantics intact
    expect(() =>
      strict.parse({ flag: true, unknown_key: "x" } as unknown)
    ).toThrow(/unrecognized_keys|Unrecognized key/);
  });
});
