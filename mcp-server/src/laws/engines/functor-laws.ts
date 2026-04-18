/**
 * FunctorLawsEngine — detects when a function has the shape of `fmap`
 * (either literally named `fmap` or a user-defined map over a container) and
 * proposes the two functor laws:
 *
 *   • identity:    fmap id == id
 *   • composition: fmap (f . g) == fmap f . fmap g
 *
 * Confidence: high. Any real functor instance MUST satisfy these; a failure
 * indicates a bug in the instance. This engine is most useful when a dev has
 * written a custom `mapX` without deriving `Functor`.
 */
import type { LawContext, Law, LawEngine } from "../types.js";
import { cleanTypeSignature } from "../types.js";

function matchFunctorShape(
  cleanedType: string
): { containerPrefix: string; elementA: string; elementB: string } | null {
  // Match: `(a -> b) -> F a -> F b`
  // We're permissive: F can be `Maybe`, `[]` rendered as `[a]`, or a user
  // constructor `Tree a`.
  const arrowStripped = cleanedType.replace(/^\(([^)]+)\)\s*->\s*/, "HOF:");
  if (!arrowStripped.startsWith("HOF:")) return null;

  // HOF: was `a -> b`
  const hofBody = arrowStripped.match(/^HOF:([^()]+?)\s*->\s*([^()]+?)\s*->\s*(.+)$/);
  if (!hofBody) return null;
  const [, beforeArrowTwo, afterArrowTwo, tail] = hofBody;
  // `beforeArrowTwo -> afterArrowTwo -> tail`   (we are parsing backwards a bit)
  // In the original text the shape is `(a -> b) -> F a -> F b`, so:
  //   HOF:<content of parens>   => `a -> b`
  //   then ` -> F a -> F b`
  // My regex captured `a -> b` as (beforeArrowTwo, afterArrowTwo), and the
  // remaining `F a -> F b` as `tail`.
  // Re-split `tail` on the outermost arrow:
  const tailParts = (tail ?? "").split(/\s*->\s*/);
  if (tailParts.length !== 2) return null;
  const [fA, fB] = tailParts;
  const a = beforeArrowTwo?.trim();
  const b = afterArrowTwo?.trim();
  if (!a || !b || !fA || !fB) return null;

  // Extract container prefix (everything before the final " a"/" b") from each side
  const prefA = stripTrailingVar(fA.trim(), a);
  const prefB = stripTrailingVar(fB.trim(), b);
  if (!prefA || !prefB || prefA !== prefB) return null;
  return { containerPrefix: prefA, elementA: a, elementB: b };
}

function stripTrailingVar(typeStr: string, variable: string): string | null {
  // Handle `[a]` form
  if (typeStr === `[${variable}]`) return "[]";
  // Handle `F a` form
  const suffix = ` ${variable}`;
  if (typeStr.endsWith(suffix)) return typeStr.slice(0, -suffix.length).trim();
  return null;
}

export const functorLawsEngine: LawEngine = {
  name: "functor-laws",
  description:
    "(a -> b) -> F a -> F b signatures: proposes identity and composition laws.",
  match(ctx: LawContext): Law[] {
    const cleaned = cleanTypeSignature(ctx.type);
    const shape = matchFunctorShape(cleaned);
    if (!shape) return [];
    const { containerPrefix, elementA } = shape;
    const containerType =
      containerPrefix === "[]" ? `[${elementA}]` : `${containerPrefix} ${elementA}`;
    return [
      {
        law: "functor identity",
        property: `\\x -> ${ctx.functionName} id (x :: ${containerType}) == x`,
        confidence: "high",
        rationale: "Every lawful functor satisfies fmap id == id",
      },
      {
        law: "functor composition",
        property: `\\x -> ${ctx.functionName} ((+1) . (+2)) (x :: ${containerType}) == ${ctx.functionName} (+1) (${ctx.functionName} (+2) x)`,
        confidence: "high",
        rationale: "Every lawful functor satisfies fmap (f . g) == fmap f . fmap g (concrete f, g shown)",
      },
    ];
  },
};
