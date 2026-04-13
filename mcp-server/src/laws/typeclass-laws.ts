/**
 * Curated database of algebraic laws for common Haskell typeclasses.
 * Used by ghci_quickcheck(property="suggest") to auto-suggest QuickCheck properties.
 */

export interface AlgebraicLaw {
  typeclass: string;
  lawName: string;
  description: string;
  /** Property template. {T} is replaced with the concrete type name. */
  propertyTemplate: string;
  requiredInstances: string[];
}

export const TYPECLASS_LAWS: AlgebraicLaw[] = [
  // === Eq ===
  {
    typeclass: "Eq",
    lawName: "reflexivity",
    description: "x == x for all x",
    propertyTemplate: "\\(x :: {T}) -> x == x",
    requiredInstances: ["Eq", "Arbitrary"],
  },
  {
    typeclass: "Eq",
    lawName: "symmetry",
    description: "x == y implies y == x",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) -> (x == y) == (y == x)",
    requiredInstances: ["Eq", "Arbitrary"],
  },
  {
    typeclass: "Eq",
    lawName: "transitivity",
    description: "x == y && y == z implies x == z",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) (z :: {T}) -> (x == y && y == z) ==> (x == z)",
    requiredInstances: ["Eq", "Arbitrary"],
  },

  // === Ord ===
  {
    typeclass: "Ord",
    lawName: "antisymmetry",
    description: "x <= y && y <= x implies x == y",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) -> (x <= y && y <= x) ==> (x == y)",
    requiredInstances: ["Ord", "Eq", "Arbitrary"],
  },
  {
    typeclass: "Ord",
    lawName: "transitivity",
    description: "x <= y && y <= z implies x <= z",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) (z :: {T}) -> (x <= y && y <= z) ==> (x <= z)",
    requiredInstances: ["Ord", "Arbitrary"],
  },
  {
    typeclass: "Ord",
    lawName: "totality",
    description: "x <= y || y <= x",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) -> x <= y || y <= x",
    requiredInstances: ["Ord", "Arbitrary"],
  },

  // === Semigroup ===
  {
    typeclass: "Semigroup",
    lawName: "associativity",
    description: "(x <> y) <> z == x <> (y <> z)",
    propertyTemplate: "\\(x :: {T}) (y :: {T}) (z :: {T}) -> (x <> y) <> z == x <> (y <> z)",
    requiredInstances: ["Semigroup", "Eq", "Arbitrary"],
  },

  // === Monoid ===
  {
    typeclass: "Monoid",
    lawName: "left-identity",
    description: "mempty <> x == x",
    propertyTemplate: "\\(x :: {T}) -> mempty <> x == x",
    requiredInstances: ["Monoid", "Eq", "Arbitrary"],
  },
  {
    typeclass: "Monoid",
    lawName: "right-identity",
    description: "x <> mempty == x",
    propertyTemplate: "\\(x :: {T}) -> x <> mempty == x",
    requiredInstances: ["Monoid", "Eq", "Arbitrary"],
  },

  // === Functor ===
  {
    typeclass: "Functor",
    lawName: "identity",
    description: "fmap id == id",
    propertyTemplate: "\\(x :: {T}) -> fmap id x == x",
    requiredInstances: ["Functor", "Eq", "Arbitrary"],
  },

  // === Applicative ===
  {
    typeclass: "Applicative",
    lawName: "identity",
    description: "pure id <*> v == v",
    propertyTemplate: "\\(x :: {T}) -> (pure id <*> x) == x",
    requiredInstances: ["Applicative", "Eq", "Arbitrary"],
  },

  // === Monad ===
  {
    typeclass: "Monad",
    lawName: "right-identity",
    description: "m >>= return == m",
    propertyTemplate: "\\(m :: {T}) -> (m >>= return) == m",
    requiredInstances: ["Monad", "Eq", "Arbitrary"],
  },
];

/**
 * Given a list of typeclass instance strings (from :i output),
 * extract the typeclass names.
 */
export function extractTypeclasses(instances: string[]): string[] {
  return instances
    .map((i) => {
      // Handle: "instance Eq X", "instance Eq a => Eq (X a)", "instance [safe] Eq X"
      const match = i.match(/instance\s+(?:\[safe\]\s+)?(?:.*=>\s+)?(\w+)\s/);
      return match?.[1] ?? "";
    })
    .filter(Boolean);
}

/**
 * Find applicable laws for a type given its typeclass instances.
 */
export function findApplicableLaws(
  typeName: string,
  typeclasses: string[]
): Array<{ typeclass: string; lawName: string; description: string; property: string }> {
  const tcSet = new Set(typeclasses);

  return TYPECLASS_LAWS
    .filter((law) =>
      law.requiredInstances.every((req) => req === "Arbitrary" || tcSet.has(req))
    )
    .map((law) => ({
      typeclass: law.typeclass,
      lawName: law.lawName,
      description: law.description,
      property: law.propertyTemplate.replace(/\{T\}/g, typeName),
    }));
}
