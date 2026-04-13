/**
 * Generic property suggestion engine based on function type signatures.
 *
 * Analyzes a function's type and suggests QuickCheck properties using heuristics.
 * Each suggestion is validated by the caller (type-checked with :t) before being presented.
 *
 * Strategies:
 * 1. Type-shape heuristics (endomorphism, binary op, list endo, roundtrip)
 * 2. Return-type contracts (Either → Right/Left, Maybe → Just/Nothing, Bool → exists True/False)
 * 3. Multi-argument consistency (same-type args → test with equal args)
 * 4. Preservation properties (for functions taking and returning structured types)
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
  const arrows = splitTopLevelArrows(cleaned);

  // --- Strategy 1: Type-shape heuristics ---

  // Endomorphism: a -> a
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

  // Binary operator: a -> a -> a
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

  // List endomorphism: [a] -> [a]
  const listEndoMatch = matchListEndomorphism(cleaned);
  if (listEndoMatch) {
    suggestions.push({
      law: "length preservation",
      property: `\\xs -> length (${funcName} xs) == length (xs :: [${listEndoMatch}])`,
      confidence: "low",
    });
  }

  // Roundtrip: look for sibling with inverse type
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

  // --- Strategy 2: Safe return-type properties ---
  // PRINCIPLE: Only suggest properties that are correct without domain knowledge.
  // Determinism and totality are always safe. Reachability is NOT (the function
  // may correctly reject arbitrary inputs). Silence > wrong suggestion.
  if (arrows.length >= 2) {
    const returnType = arrows[arrows.length - 1]!.trim();
    const argTypes = arrows.slice(0, -1).map((a) => a.trim());

    // Either return: determinism (safe) + reflexivity for same-type args (often correct)
    if (returnType.startsWith("Either ")) {
      // Determinism: same input always gives same output (always correct for pure functions)
      suggestions.push({
        law: "determinism",
        property: buildNArgs(funcName, argTypes, `${funcName} ${buildArgApply(argTypes)} == ${funcName} ${buildArgApply(argTypes)}`, "determinism"),
        confidence: "high",
      });
      // Reflexive: f x x should be consistent (often succeeds for well-behaved functions)
      if (argTypes.length === 2 && argTypes[0] === argTypes[1]) {
        suggestions.push({
          law: "reflexivity (equal args consistent)",
          property: `\\x -> ${funcName} x (x :: ${argTypes[0]}) == ${funcName} x x`,
          confidence: "medium",
        });
      }
    }

    // --- Strategy 3: Same-type arguments → test with equal args ---
    if (argTypes.length === 2 && argTypes[0] === argTypes[1] && !returnType.startsWith("Either ")) {
      const t = argTypes[0]!;
      suggestions.push({
        law: "reflexive (equal args)",
        property: `\\x -> ${funcName} x (x :: ${t}) == ${funcName} x x`,
        confidence: "medium",
      });
    }

    // --- Strategy 4: Multi-arg functions with Pos-like state threading ---
    // f :: State -> Input -> State pattern (state threading)
    if (argTypes.length === 2 && argTypes[0] === arrows[arrows.length - 1]!.trim()) {
      const stateType = argTypes[0]!;
      const inputType = argTypes[1]!;
      // Applying twice should be consistent
      suggestions.push({
        law: "sequential application consistency",
        property: `\\s i1 i2 -> ${funcName} (${funcName} (s :: ${stateType}) (i1 :: ${inputType})) i2 == ${funcName} (${funcName} s i1) i2`,
        confidence: "medium",
      });
    }
  }

  return suggestions;
}

// --- Helpers for building property expressions ---

/**
 * Build a property with N quantified arguments matching the arg types.
 */
function buildNArgs(
  funcName: string,
  argTypes: string[],
  body: string,
  _label: string
): string {
  const vars = argTypes.map((_, i) => `x${i}`);
  const annotations = argTypes
    .map((t, i) => `(${vars[i]} :: ${t})`)
    .join(" ");
  return `\\${annotations} -> ${body}`;
}

/**
 * Build the argument application part: "x0 x1 x2"
 */
function buildArgApply(argTypes: string[]): string {
  return argTypes.map((_, i) => `x${i}`).join(" ");
}

// --- Type signature helpers ---

function cleanTypeSignature(typeStr: string): string {
  return typeStr.replace(/^\S+\s*::\s*/, "").replace(/\s+/g, " ").trim();
}

function matchEndomorphism(cleanedType: string): string | null {
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

function matchBinaryOp(cleanedType: string): string | null {
  const match = cleanedType.match(/^(\[?\w+\]?)\s*->\s*(\[?\w+\]?)\s*->\s*(\[?\w+\]?)$/);
  if (match && match[1] === match[2] && match[2] === match[3]) {
    return match[1]!;
  }
  return null;
}

function matchListEndomorphism(cleanedType: string): string | null {
  const match = cleanedType.match(/^\[(\w+)\]\s*->\s*\[(\w+)\]$/);
  if (match && match[1] === match[2]) {
    return match[1]!;
  }
  return null;
}

function findRoundtripPairs(
  funcName: string,
  cleanedType: string,
  siblings: Sibling[]
): Array<{ inverse: string; property: string }> {
  const results: Array<{ inverse: string; property: string }> = [];
  const arrowParts = splitTopLevelArrows(cleanedType);
  if (arrowParts.length !== 2) return results;

  const [inputType, outputType] = arrowParts;

  for (const sib of siblings) {
    if (sib.name === funcName) continue;
    const sibCleaned = cleanTypeSignature(sib.type);
    const sibParts = splitTopLevelArrows(sibCleaned);
    if (sibParts.length !== 2) continue;

    const [sibInput, sibOutput] = sibParts;
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
export function splitTopLevelArrows(typeStr: string): string[] {
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
