/**
 * ConstantFoldingSoundnessEngine — specialization of EvaluatorPreservation
 * for transforms whose name strongly implies optimization semantics:
 * `simplify`, `normalize`, `fold`, `rewrite`, `optimize`, `canonicalize`.
 *
 * When one of those names appears on a `X -> X` transform in a module that
 * also has an evaluator `X -> Y` (possibly with context params), we surface
 * the evaluation-preservation law with even higher confidence, framed as
 * "constant-folding soundness": the transformation must not change observable
 * behavior.
 *
 * This is strictly additive to evaluator-preservation — the two engines both
 * fire on the same context and the registry deduplicates by property text.
 * Having a dedicated engine lets us bump the wording and confidence
 * specifically for the optimization sub-case without special-casing the
 * general engine.
 */
import type { LawContext, Law, LawEngine } from "../types.js";
import { cleanTypeSignature, splitTopLevelArrows, normalizeType } from "../types.js";

const OPTIMIZATION_NAMES = new Set([
  "simplify",
  "normalize",
  "fold",
  "constantFold",
  "constant_fold",
  "rewrite",
  "optimize",
  "canonicalize",
  "reduce",
]);

export const constantFoldingSoundnessEngine: LawEngine = {
  name: "constant-folding-soundness",
  description:
    "Transforms named simplify/normalize/fold/rewrite/optimize/canonicalize that have an interpreter sibling → optimization soundness law.",
  match(ctx: LawContext): Law[] {
    if (!OPTIMIZATION_NAMES.has(ctx.functionName)) return [];
    if (!ctx.siblings || ctx.siblings.length === 0) return [];

    const cleaned = cleanTypeSignature(ctx.type);
    const parts = splitTopLevelArrows(cleaned);
    if (parts.length !== 2) return [];
    const [input, output] = parts;
    if (normalizeType(input!) !== normalizeType(output!)) return [];

    const normTarget = normalizeType(input!);
    const laws: Law[] = [];

    for (const sib of ctx.siblings) {
      const sibParts = splitTopLevelArrows(cleanTypeSignature(sib.type));
      if (sibParts.length < 2) continue;
      const sibFinalInput = sibParts[sibParts.length - 2]!;
      const sibFinalOutput = sibParts[sibParts.length - 1]!;
      if (normalizeType(sibFinalInput) !== normTarget) continue;
      if (normalizeType(sibFinalOutput) === normTarget) continue;

      const extraParams = sibParts.length > 2
        ? Array.from({ length: sibParts.length - 2 }, (_, i) => `p${i + 1}`)
        : [];
      const paramsSig = extraParams.length > 0 ? extraParams.join(" ") + " " : "";
      const paramArgs = extraParams.join(" ");
      laws.push({
        law: `constant-folding soundness (${sib.name} / ${ctx.functionName})`,
        property: `\\${paramsSig}x -> ${sib.name}${paramArgs ? " " + paramArgs : ""} (${ctx.functionName} x) == ${sib.name}${paramArgs ? " " + paramArgs : ""} (x :: ${input})`,
        confidence: "high",
        rationale: `${ctx.functionName} is an optimization pass; ${sib.name} must agree on original and transformed input.`,
      });
    }
    return laws;
  },
};
