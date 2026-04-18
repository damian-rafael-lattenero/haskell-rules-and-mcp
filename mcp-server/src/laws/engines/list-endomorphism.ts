/**
 * ListEndomorphismEngine — functions of type `[a] -> [a]`.
 *
 *   • length preservation: length (f xs) == length xs   [low]
 *
 * Speculative because many list endos change length (filter, take, drop).
 * Surfacing the question is still useful — it prompts the agent to decide if
 * THEIR particular transformation preserves length.
 */
import type { LawContext, Law, LawEngine } from "../types.js";
import { cleanTypeSignature } from "../types.js";

function matchListEndomorphism(cleanedType: string): string | null {
  const match = cleanedType.match(/^\[(\w+)\]\s*->\s*\[(\w+)\]$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

export const listEndomorphismEngine: LawEngine = {
  name: "list-endomorphism",
  description: "[a] -> [a] signatures: length preservation (speculative).",
  match(ctx: LawContext): Law[] {
    const cleaned = cleanTypeSignature(ctx.type);
    const elt = matchListEndomorphism(cleaned);
    if (!elt) return [];
    return [
      {
        law: "length preservation",
        property: `\\xs -> length (${ctx.functionName} xs) == length (xs :: [${elt}])`,
        confidence: "low",
        rationale: "Holds for map/reverse/sort; fails for filter/nub/take",
      },
    ];
  },
};
