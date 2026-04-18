/**
 * BinaryOpEngine — functions of type `a -> a -> a`.
 *
 *   • associativity: f (f x y) z == f x (f y z)   [medium]
 *   • commutativity: f x y       == f y x         [low, often wrong]
 */
import type { LawContext, Law, LawEngine } from "../types.js";
import { cleanTypeSignature } from "../types.js";

function matchBinaryOp(cleanedType: string): string | null {
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2] && match[2] === match[3]) {
    return match[1]!;
  }
  return null;
}

export const binaryOpEngine: LawEngine = {
  name: "binary-op",
  description: "a -> a -> a signatures: associativity (medium) and commutativity (low).",
  match(ctx: LawContext): Law[] {
    const cleaned = cleanTypeSignature(ctx.type);
    const t = matchBinaryOp(cleaned);
    if (!t) return [];
    return [
      {
        law: "associativity",
        property: `\\x y z -> ${ctx.functionName} (${ctx.functionName} x y) z == ${ctx.functionName} x (${ctx.functionName} y z :: ${t})`,
        confidence: "medium",
        rationale: "Binary ops over one type often associate (e.g. +, *, ++)",
      },
      {
        law: "commutativity",
        property: `\\x y -> ${ctx.functionName} x y == ${ctx.functionName} y (x :: ${t})`,
        confidence: "low",
        rationale: "Commutativity holds for +, * but NOT for -, /, or ++ — verify first",
      },
    ];
  },
};
