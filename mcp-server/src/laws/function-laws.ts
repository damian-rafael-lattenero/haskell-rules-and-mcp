/**
 * Generic property suggestion engine based on function type signatures.
 *
 * Backward-compatibility facade: `suggestFunctionProperties` keeps its
 * original signature and return shape, but internally delegates to the
 * pluggable registry (`./registry.ts`). New engines (evaluator-preservation,
 * constant-folding-soundness, functor-laws) come through for free.
 *
 * Silence > wrong suggestion: tautologies are never emitted. The registry
 * dedupes by `property` text so overlapping engines do not double-report.
 */

import { runLawEngines } from "./registry.js";
import type { Sibling } from "./types.js";

export type { Sibling } from "./types.js";
export { splitTopLevelArrows } from "./types.js";

export interface FunctionLaw {
  law: string;
  property: string;
  confidence: "high" | "medium" | "low";
  /** Optional rationale surfaced by newer engines. Legacy callers ignore it. */
  rationale?: string;
}

/**
 * Suggest QuickCheck properties based on a function's type signature.
 *
 * @param funcName - Name of the function
 * @param typeStr - Type signature (e.g. "foo :: Int -> Int -> Int")
 * @param siblings - Other functions in the same module (for roundtrip and
 *                   evaluator-preservation detection)
 */
export function suggestFunctionProperties(
  funcName: string,
  typeStr: string,
  siblings?: Sibling[]
): FunctionLaw[] {
  const laws = runLawEngines({
    functionName: funcName,
    type: typeStr,
    siblings,
  });
  return laws.map((law) => ({
    law: law.law,
    property: law.property,
    confidence: law.confidence,
    ...(law.rationale ? { rationale: law.rationale } : {}),
  }));
}
