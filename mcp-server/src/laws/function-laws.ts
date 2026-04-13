/**
 * Generic property suggestion engine based on function type signatures.
 *
 * Analyzes a function's type and suggests QuickCheck properties using heuristics.
 * Each suggestion is validated by the caller (type-checked with :t) before being presented.
 */

export interface FunctionLaw {
  law: string;
  property: string;
  confidence: "high" | "medium" | "low";
}

export interface Sibling {
  name: string;
  type: string;
}

/**
 * Suggest QuickCheck properties based on a function's type signature.
 *
 * @param funcName - Name of the function
 * @param typeStr - Type signature (e.g. "foo :: Int -> Int -> Int")
 * @param siblings - Other functions in the same module (for roundtrip detection)
 */
export function suggestFunctionProperties(
  funcName: string,
  typeStr: string,
  siblings?: Sibling[]
): FunctionLaw[] {
  const suggestions: FunctionLaw[] = [];
  const cleaned = cleanTypeSignature(typeStr);

  // --- Endomorphism: a -> a ---
  const endoMatch = matchEndomorphism(cleaned);
  if (endoMatch) {
    suggestions.push({
      law: "idempotence",
      property: `\\x -> ${funcName} (${funcName} x) == ${funcName} (x :: ${endoMatch})`,
      confidence: "low",
    });
    suggestions.push({
      law: "involution",
      property: `\\x -> ${funcName} (${funcName} x) == (x :: ${endoMatch})`,
      confidence: "low",
    });
  }

  // --- Binary operator: a -> a -> a ---
  const binOpMatch = matchBinaryOp(cleaned);
  if (binOpMatch) {
    suggestions.push({
      law: "associativity",
      property: `\\x y z -> ${funcName} (${funcName} x y) z == ${funcName} x (${funcName} y z :: ${binOpMatch})`,
      confidence: "medium",
    });
    suggestions.push({
      law: "commutativity",
      property: `\\x y -> ${funcName} x y == ${funcName} y (x :: ${binOpMatch})`,
      confidence: "low",
    });
  }

  // --- Roundtrip: look for sibling with inverse type ---
  if (siblings) {
    const roundtrips = findRoundtripPairs(funcName, cleaned, siblings);
    for (const rt of roundtrips) {
      suggestions.push({
        law: `roundtrip (${funcName} / ${rt.inverse})`,
        property: rt.property,
        confidence: "high",
      });
    }
  }

  // --- List endomorphism: [a] -> [a] ---
  const listEndoMatch = matchListEndomorphism(cleaned);
  if (listEndoMatch) {
    suggestions.push({
      law: "length preservation",
      property: `\\xs -> length (${funcName} xs) == length (xs :: [${listEndoMatch}])`,
      confidence: "low",
    });
  }

  return suggestions;
}

// --- Type signature helpers ---

/**
 * Strip the "name :: " prefix and normalize whitespace.
 */
function cleanTypeSignature(typeStr: string): string {
  return typeStr.replace(/^\S+\s*::\s*/, "").replace(/\s+/g, " ").trim();
}

/**
 * Match `a -> a` where a is a concrete type (not a type variable).
 * Returns the type name if matched.
 */
function matchEndomorphism(cleanedType: string): string | null {
  // Match: TypeName -> TypeName (at end, accounting for possible parens)
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

/**
 * Match `a -> a -> a` (binary operator on same type).
 * Returns the type name if matched.
 */
function matchBinaryOp(cleanedType: string): string | null {
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2] && match[2] === match[3]) {
    return match[1]!;
  }
  return null;
}

/**
 * Match `[a] -> [a]` (list endomorphism).
 * Returns the element type.
 */
function matchListEndomorphism(cleanedType: string): string | null {
  const match = cleanedType.match(/^\[(\w+)\]\s*->\s*\[(\w+)\]$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

/**
 * Find sibling functions that form roundtrip pairs.
 * E.g., if funcName has type `a -> b` and a sibling has `b -> a`,
 * suggest `\x -> sibling (funcName x) == x`.
 */
function findRoundtripPairs(
  funcName: string,
  cleanedType: string,
  siblings: Sibling[]
): Array<{ inverse: string; property: string }> {
  const results: Array<{ inverse: string; property: string }> = [];

  // Extract A -> B pattern (simplified: last arrow splits output from input chain)
  const arrowParts = splitTopLevelArrows(cleanedType);
  if (arrowParts.length !== 2) return results; // Only handle simple A -> B

  const [inputType, outputType] = arrowParts;

  for (const sib of siblings) {
    if (sib.name === funcName) continue;
    const sibCleaned = cleanTypeSignature(sib.type);
    const sibParts = splitTopLevelArrows(sibCleaned);
    if (sibParts.length !== 2) continue;

    const [sibInput, sibOutput] = sibParts;

    // Check if sibling is the inverse: B -> A
    if (
      normalizeType(sibInput!) === normalizeType(outputType!) &&
      normalizeType(sibOutput!) === normalizeType(inputType!)
    ) {
      results.push({
        inverse: sib.name,
        property: `\\x -> ${sib.name} (${funcName} x) == (x :: ${inputType})`,
      });
    }
  }

  return results;
}

/**
 * Split a type string by top-level arrows (respecting parens/brackets).
 */
function splitTopLevelArrows(typeStr: string): string[] {
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

function normalizeType(t: string): string {
  return t.replace(/\s+/g, " ").trim();
}
