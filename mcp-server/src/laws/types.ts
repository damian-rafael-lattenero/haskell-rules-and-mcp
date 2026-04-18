/**
 * Pluggable law-suggestion engine interface.
 *
 * Each engine inspects a function's type signature (and optionally its
 * siblings in the same module) and returns a list of laws expressible as
 * QuickCheck properties. The suggest tool runs every engine in the registry
 * and aggregates results, deduplicating by property string.
 *
 * Design: engines MUST be pure. They do not consult GHCi, the property
 * store, or the filesystem. They transform a `LawContext` into `Law[]`. This
 * lets us unit-test every engine cheaply and run them without side effects.
 */

export interface Sibling {
  name: string;
  type: string;
}

export interface LawContext {
  /** Name of the function being analyzed. */
  functionName: string;
  /** Type signature (may include the leading `fn ::`; engines should strip). */
  type: string;
  /** Other definitions in the same module with their types. Enables cross-fn
   *  laws like roundtrip pairs and evaluator-preservation. */
  siblings?: Sibling[];
}

export interface Law {
  /** Short human-readable tag: "idempotence", "roundtrip", "evaluator preservation", etc. */
  law: string;
  /** QuickCheck property expression (a valid Haskell lambda). */
  property: string;
  /** How much trust to place in the suggestion. High = mathematically expected,
   *  medium = likely, low = speculative/optional. */
  confidence: "high" | "medium" | "low";
  /** Optional one-line explanation shown to the caller. */
  rationale?: string;
}

export interface LawEngine {
  /** Stable identifier used in telemetry and deduplication. */
  name: string;
  /** Human-readable description for error messages and docs. */
  description: string;
  /**
   * Match the context against this engine's shape. Returns zero or more laws.
   * Must NOT throw — on any parse ambiguity the engine returns `[]`.
   */
  match(ctx: LawContext): Law[];
}

/** Strip the leading `name ::` if present and collapse whitespace. */
export function cleanTypeSignature(typeStr: string): string {
  return typeStr.replace(/^\S+\s*::\s*/, "").replace(/\s+/g, " ").trim();
}

/** Split a type string by top-level arrows, respecting parens and brackets. */
export function splitTopLevelArrows(typeStr: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let current = "";

  for (let i = 0; i < typeStr.length; i++) {
    const ch = typeStr[i]!;
    if (ch === "(" || ch === "[") depth++;
    else if (ch === ")" || ch === "]") depth--;
    else if (depth === 0 && ch === "-" && typeStr[i + 1] === ">") {
      parts.push(current.trim());
      current = "";
      i++; // skip '>'
      continue;
    }
    current += ch;
  }
  if (current.trim()) parts.push(current.trim());

  return parts;
}

export function normalizeType(t: string): string {
  return t.replace(/\s+/g, " ").trim();
}
