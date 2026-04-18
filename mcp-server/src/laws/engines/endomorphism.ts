/**
 * EndomorphismEngine — functions of type `X -> X`.
 *
 * Proposes two speculative laws (confidence: low):
 *   • idempotence: f (f x) == f x
 *   • involution:  f (f x) == x
 *
 * Both are `low` confidence because most `X -> X` functions satisfy NEITHER
 * law (think `(+1)` or `reverse . tail`). The goal is to surface the question
 * to the agent so they check if the function's semantics imply one of these.
 */
import type { LawContext, Law, LawEngine } from "../types.js";
import { cleanTypeSignature } from "../types.js";

function matchEndomorphism(cleanedType: string): string | null {
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

export const endomorphismEngine: LawEngine = {
  name: "endomorphism",
  description: "X -> X signatures: proposes idempotence and involution (speculative).",
  match(ctx: LawContext): Law[] {
    const cleaned = cleanTypeSignature(ctx.type);
    const monoType = matchEndomorphism(cleaned);
    if (!monoType) return [];
    const laws: Law[] = [
      {
        law: "idempotence",
        property: `\\x -> ${ctx.functionName} (${ctx.functionName} x) == ${ctx.functionName} (x :: ${monoType})`,
        confidence: "low",
        rationale: "f applied twice == f applied once (holds for e.g. normalize/sort)",
      },
      {
        law: "involution",
        property: `\\x -> ${ctx.functionName} (${ctx.functionName} x) == (x :: ${monoType})`,
        confidence: "low",
        rationale: "f . f == id (holds for e.g. negate, reverse)",
      },
    ];
    return laws;
  },
};
