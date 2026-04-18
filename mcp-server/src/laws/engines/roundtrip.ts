/**
 * RoundtripEngine — detects sibling pairs with inverse type shapes.
 *
 * If module exposes `pretty :: X -> String` and `parse :: String -> Maybe X`,
 * propose `\x -> parse (pretty x) == Just x`.
 *
 * Confidence: high. This pattern is nearly always the intent when such a
 * pair exists — serialization, parsing, encoding.
 */
import type { LawContext, Law, LawEngine, Sibling } from "../types.js";
import { cleanTypeSignature, normalizeType, splitTopLevelArrows } from "../types.js";

function findRoundtripPairs(
  funcName: string,
  cleanedType: string,
  siblings: Sibling[]
): Array<{ inverse: string; property: string; inputType: string; outputType: string }> {
  const results: Array<{ inverse: string; property: string; inputType: string; outputType: string }> = [];
  const arrowParts = splitTopLevelArrows(cleanedType);
  if (arrowParts.length !== 2) return results;

  const [inputType, outputType] = arrowParts;

  for (const sib of siblings) {
    if (sib.name === funcName) continue;
    const sibCleaned = cleanTypeSignature(sib.type);
    const sibParts = splitTopLevelArrows(sibCleaned);
    if (sibParts.length !== 2) continue;

    const [sibInput, sibOutput] = sibParts;

    // Direct roundtrip: sib's input matches our output AND sib's output matches our input.
    if (
      normalizeType(sibInput!) === normalizeType(outputType!) &&
      normalizeType(sibOutput!) === normalizeType(inputType!)
    ) {
      results.push({
        inverse: sib.name,
        property: `\\x -> ${sib.name} (${funcName} x) == (x :: ${inputType})`,
        inputType: inputType!,
        outputType: outputType!,
      });
      continue;
    }

    // "Maybe wrapper" roundtrip: serialize :: X -> Y, parse :: Y -> Maybe X
    //   → parse (serialize x) == Just x
    const maybeOfInput = new RegExp(`^Maybe\\s+${escapeRegex(normalizeType(inputType!))}$`);
    if (
      normalizeType(sibInput!) === normalizeType(outputType!) &&
      maybeOfInput.test(normalizeType(sibOutput!))
    ) {
      results.push({
        inverse: sib.name,
        property: `\\x -> ${sib.name} (${funcName} x) == Just (x :: ${inputType})`,
        inputType: inputType!,
        outputType: outputType!,
      });
    }
  }

  return results;
}

function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

export const roundtripEngine: LawEngine = {
  name: "roundtrip",
  description: "Detect inverse sibling pairs (pretty/parse, encode/decode, etc.) and propose roundtrip laws.",
  match(ctx: LawContext): Law[] {
    if (!ctx.siblings || ctx.siblings.length === 0) return [];
    const cleaned = cleanTypeSignature(ctx.type);
    const pairs = findRoundtripPairs(ctx.functionName, cleaned, ctx.siblings);
    return pairs.map((rt) => ({
      law: `roundtrip (${ctx.functionName} / ${rt.inverse})`,
      property: rt.property,
      confidence: "high" as const,
      rationale: `${rt.inverse}'s signature inverts ${ctx.functionName}'s, suggesting a serialization/parser pair`,
    }));
  },
};
