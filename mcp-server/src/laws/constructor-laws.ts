/**
 * Constructor-aware QuickCheck property suggestions.
 *
 * Inspects the constructors of a function's input ADT and generates
 * per-constructor properties:
 * - Totality: function doesn't crash on any constructor
 * - Identity: base-case constructors wrapping a value produce Right/Just of that value
 * - Homomorphism: recursive constructors distribute over the function
 *
 * All suggestions are validated by :t in GHCi before presenting — bad guesses
 * (wrong operator, wrong wrapping) are filtered automatically.
 */

import type { FunctionLaw } from "./function-laws.js";
import type { Constructor } from "../parsers/constructor-parser.js";

/**
 * Generate per-constructor QuickCheck properties for a function that takes
 * an algebraic data type as input.
 *
 * @param funcName - The function name (e.g. "eval")
 * @param returnType - The return type (e.g. "Either Error Int")
 * @param otherArgs - Arg types besides the ADT (e.g. ["Env"])
 * @param constructors - Parsed constructors of the ADT
 * @param inputTypeName - Name of the ADT (e.g. "Expr")
 * @param adtArgPosition - Which argument position the ADT is in (0-indexed)
 */
export function suggestConstructorProperties(
  funcName: string,
  returnType: string,
  otherArgs: string[],
  constructors: Constructor[],
  inputTypeName: string,
  adtArgPosition: number = 0
): FunctionLaw[] {
  const laws: FunctionLaw[] = [];
  const retTrimmed = returnType.trim();
  const isEitherReturn = retTrimmed.startsWith("Either ");
  const isMaybeReturn = retTrimmed.startsWith("Maybe ");

  // Extract the inner success type for Either/Maybe
  let innerReturnType = retTrimmed;
  if (isEitherReturn) {
    // "Either Error Int" → need to get "Int" (skip "Either Error ")
    const parts = retTrimmed.replace(/^Either\s+/, "").split(/\s+/);
    innerReturnType = parts.length > 1 ? parts.slice(1).join(" ") : parts[0] ?? retTrimmed;
  } else if (isMaybeReturn) {
    innerReturnType = retTrimmed.replace(/^Maybe\s+/, "").trim();
  }

  for (const ctor of constructors) {
    const ctorVars = ctor.fields.map((_, i) => `a${i}`);
    const otherVars = otherArgs.map((_, i) => `e${i}`);

    // Build the constructor expression: (Lit a0) or (Add a0 a1) or Nil
    const ctorExpr = ctorVars.length > 0
      ? `(${ctor.name} ${ctorVars.join(" ")})`
      : ctor.name;

    // Build the function call with args in the right position
    const allArgs = [...otherVars];
    allArgs.splice(adtArgPosition, 0, ctorExpr);
    const funcCall = `${funcName} ${allArgs.join(" ")}`;

    // Build type annotations for the lambda
    const ctorAnnotations = ctorVars.map((v, i) => `(${v} :: ${ctor.fields[i]})`);
    const otherAnnotations = otherVars.map((v, i) => `(${v} :: ${otherArgs[i]})`);
    const annotations = [...ctorAnnotations, ...otherAnnotations].join(" ");

    const isRecursive = ctor.fields.some(f => f.includes(inputTypeName));

    // --- A. Totality: function completes without exception ---
    laws.push({
      law: `totality: ${ctor.name}`,
      property: `\\${annotations} -> seq (${funcCall}) True`,
      confidence: "high",
    });

    // --- B. Identity for base-case constructors ---
    // If constructor has 1 field whose type matches the inner return type,
    // suggest that the function wraps it in Right/Just
    if (!isRecursive && ctor.fields.length === 1) {
      const fieldType = ctor.fields[0]!.trim();
      if (fieldType === innerReturnType) {
        if (isEitherReturn) {
          laws.push({
            law: `identity: ${ctor.name} → Right`,
            property: `\\${annotations} -> ${funcCall} == Right a0`,
            confidence: "medium",
          });
        } else if (isMaybeReturn) {
          laws.push({
            law: `identity: ${ctor.name} → Just`,
            property: `\\${annotations} -> ${funcCall} == Just a0`,
            confidence: "medium",
          });
        } else if (fieldType === retTrimmed) {
          // Direct return (not wrapped)
          laws.push({
            law: `identity: ${ctor.name} unwraps`,
            property: `\\${annotations} -> ${funcCall} == a0`,
            confidence: "medium",
          });
        }
      }
    }

    // --- C. Homomorphism for binary recursive constructors ---
    // e.g. Add Expr Expr → f env (Add a b) == liftA2 op (f env a) (f env b)
    if (isRecursive && ctor.fields.length === 2 &&
        ctor.fields.every(f => f.includes(inputTypeName))) {
      const recArgs0 = [...otherVars];
      recArgs0.splice(adtArgPosition, 0, "a0");
      const recArgs1 = [...otherVars];
      recArgs1.splice(adtArgPosition, 0, "a1");
      const recCall0 = `${funcName} ${recArgs0.join(" ")}`;
      const recCall1 = `${funcName} ${recArgs1.join(" ")}`;

      if (isEitherReturn || isMaybeReturn) {
        // Suggest liftA2 (+) as a common arithmetic homomorphism
        laws.push({
          law: `homomorphism: ${ctor.name} distributes (+)`,
          property: `\\${annotations} -> ${funcCall} == liftA2 (+) (${recCall0}) (${recCall1})`,
          confidence: "low",
        });
        // Also suggest liftA2 (*) for Mul-like constructors
        laws.push({
          law: `homomorphism: ${ctor.name} distributes (*)`,
          property: `\\${annotations} -> ${funcCall} == liftA2 (*) (${recCall0}) (${recCall1})`,
          confidence: "low",
        });
      } else {
        // Direct return type
        laws.push({
          law: `homomorphism: ${ctor.name} distributes (+)`,
          property: `\\${annotations} -> ${funcCall} == (${recCall0}) + (${recCall1})`,
          confidence: "low",
        });
      }
    }

    // --- D. Homomorphism for unary recursive constructors ---
    // e.g. Neg Expr → f env (Neg a) == fmap negate (f env a)
    if (isRecursive && ctor.fields.length === 1 &&
        ctor.fields[0]!.includes(inputTypeName)) {
      const recArgs = [...otherVars];
      recArgs.splice(adtArgPosition, 0, "a0");
      const recCall = `${funcName} ${recArgs.join(" ")}`;

      if (isEitherReturn || isMaybeReturn) {
        laws.push({
          law: `homomorphism: ${ctor.name} maps negate`,
          property: `\\${annotations} -> ${funcCall} == fmap negate (${recCall})`,
          confidence: "low",
        });
      }
    }
  }

  return laws;
}
