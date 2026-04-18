/**
 * Central registry of active LawEngines. The suggest tool iterates this list
 * in order and deduplicates laws by `property` text — so adding an engine
 * whose output overlaps with an existing one is safe (the registered order
 * wins the tie-breaker).
 */
import type { Law, LawContext, LawEngine } from "./types.js";

import { endomorphismEngine } from "./engines/endomorphism.js";
import { binaryOpEngine } from "./engines/binary-op.js";
import { listEndomorphismEngine } from "./engines/list-endomorphism.js";
import { roundtripEngine } from "./engines/roundtrip.js";
import { evaluatorPreservationEngine } from "./engines/evaluator-preservation.js";
import { constantFoldingSoundnessEngine } from "./engines/constant-folding-soundness.js";
import { functorLawsEngine } from "./engines/functor-laws.js";

/**
 * Order matters for tie-breaking: when two engines produce the same property
 * text, the FIRST one in this list wins. `constant-folding-soundness` goes
 * before generic `evaluator-preservation` so the more specific wording takes
 * precedence when a transform is named `simplify`/`normalize`/etc.
 */
export const DEFAULT_ENGINES: readonly LawEngine[] = [
  constantFoldingSoundnessEngine,
  evaluatorPreservationEngine,
  roundtripEngine,
  functorLawsEngine,
  binaryOpEngine,
  listEndomorphismEngine,
  endomorphismEngine,
];

export function runLawEngines(
  ctx: LawContext,
  engines: readonly LawEngine[] = DEFAULT_ENGINES
): Law[] {
  const seen = new Set<string>();
  const out: Law[] = [];
  for (const engine of engines) {
    let laws: Law[];
    try {
      laws = engine.match(ctx);
    } catch {
      // Engines must be total; any throw is a bug. Skip silently rather than
      // poisoning unrelated suggestions.
      laws = [];
    }
    for (const law of laws) {
      const key = law.property;
      if (seen.has(key)) continue;
      seen.add(key);
      out.push(law);
    }
  }
  return out;
}
