/**
 * EvaluatorPreservationEngine — when a module exposes both an "interpreter"
 * (X -> Y) and a "transformation" (X -> X), propose that the transformation
 * is semantics-preserving: `eval (transform x) == eval x`.
 *
 * This is the canonical optimization-soundness law:
 *   eval . simplify   ≡ eval
 *   interp . normalize ≡ interp
 *   run . rewrite      ≡ run
 *
 * Confidence: high when the pair co-exists in the same module with a
 * recognizable shape. It's one of the most common correctness invariants in
 * DSL/evaluator code, and the template is rarely wrong for that shape.
 *
 * This engine was missing before P1a — the previous suggestion set only
 * covered endomorphism / binary-op / list-endo / roundtrip, leaving
 * evaluator-shaped modules with zero suggestions.
 */
import type { LawContext, Law, LawEngine, Sibling } from "../types.js";
import { cleanTypeSignature, splitTopLevelArrows, normalizeType } from "../types.js";

interface InterpreterSibling {
  name: string;
  inputType: string;
  outputType: string;
  /** Number of parameters before the final return. `eval :: Env -> Expr -> r`
   *  has 2, `run :: Expr -> r` has 1. Treat all but the last as "context"
   *  parameters that we do not need to match against the transform. */
  paramCount: number;
}

/** Find siblings that look like interpreters: *any* function whose FINAL
 *  argument type equals the transform's input type, and whose output type
 *  differs from that input type. Extra params (env, store, config) are OK. */
function findInterpreters(
  targetInputType: string,
  siblings: Sibling[]
): InterpreterSibling[] {
  const out: InterpreterSibling[] = [];
  const normTarget = normalizeType(targetInputType);
  for (const sib of siblings) {
    const cleaned = cleanTypeSignature(sib.type);
    const parts = splitTopLevelArrows(cleaned);
    if (parts.length < 2) continue;
    const finalInput = parts[parts.length - 2]!;
    const finalOutput = parts[parts.length - 1]!;
    if (normalizeType(finalInput) !== normTarget) continue;
    if (normalizeType(finalOutput) === normTarget) continue; // that's an endomorphism, not an interpreter
    out.push({
      name: sib.name,
      inputType: finalInput,
      outputType: finalOutput,
      paramCount: parts.length - 1,
    });
  }
  return out;
}

export const evaluatorPreservationEngine: LawEngine = {
  name: "evaluator-preservation",
  description:
    "When an interpreter `f : X -> Y` and a transformation `t : X -> X` coexist, propose `f (t x) == f x`.",
  match(ctx: LawContext): Law[] {
    if (!ctx.siblings || ctx.siblings.length === 0) return [];
    const cleaned = cleanTypeSignature(ctx.type);
    const parts = splitTopLevelArrows(cleaned);

    // Only consider the target as a transformation if it has shape `X -> X`
    // (one arrow, same input and output type, ignoring inner spaces).
    if (parts.length !== 2) return [];
    const [input, output] = parts;
    if (normalizeType(input!) !== normalizeType(output!)) return [];

    const interpreters = findInterpreters(input!, ctx.siblings);
    return interpreters.map<Law>((interp) => {
      // Synthesize a preservation property that respects extra interpreter
      // parameters. For `eval :: Env -> Expr -> r` + `simplify :: Expr -> Expr`,
      // emit `\env x -> eval env (simplify x) == eval env x`.
      const extraParams = Array.from({ length: interp.paramCount - 1 }, (_, i) => `p${i + 1}`);
      const paramsSig = extraParams.length > 0 ? extraParams.join(" ") + " " : "";
      const paramArgs = extraParams.join(" ");
      const lhs = `${interp.name}${paramArgs ? " " + paramArgs : ""} (${ctx.functionName} x)`;
      const rhs = `${interp.name}${paramArgs ? " " + paramArgs : ""} (x :: ${input})`;
      return {
        law: `evaluator preservation (${interp.name} / ${ctx.functionName})`,
        property: `\\${paramsSig}x -> ${lhs} == ${rhs}`,
        confidence: "high",
        rationale: `${interp.name} interprets ${input}; if ${ctx.functionName} is semantics-preserving, interpretation must agree on original vs transformed.`,
      };
    });
  },
};
