import { GhciSession } from "../ghci-session.js";

export interface HoleFit {
  name: string;
  type: string;
  specialization?: string; // "with map @a @b" etc.
  source?: string; // "imported from 'Prelude'" or "bound at ..."
}

export interface RelevantBinding {
  name: string;
  type: string;
  location: string;
}

export interface TypedHole {
  hole: string; // "_" or "_name"
  expectedType: string;
  location: { file: string; line: number; column: number };
  expression?: string; // "In the expression: ..."
  equation?: string; // "In an equation for ..."
  relevantBindings: RelevantBinding[];
  validFits: HoleFit[];
  suppressed: boolean; // true if "(Some hole fits suppressed...)"
}

/**
 * Load a module and extract structured information about all typed holes.
 * Typed holes use -fdefer-type-errors (already set in .ghci) so GHC reports
 * them as warnings with [GHC-88464] and includes valid hole fits.
 */
export async function handleHoleFits(
  session: GhciSession,
  args: { module_path: string; max_fits?: number }
): Promise<string> {
  // Optionally increase the max valid hole fits shown
  const maxFits = args.max_fits ?? 10;
  await session.execute(`:set -fmax-valid-hole-fits=${maxFits}`);

  // Load the module — with -fdefer-type-errors, holes become warnings
  const loadResult = await session.loadModule(args.module_path);

  // Reset to default
  await session.execute(":set -fmax-valid-hole-fits=6");

  const holes = parseHoleWarnings(loadResult.output);

  if (holes.length === 0) {
    return JSON.stringify({
      success: true,
      holes: [],
      summary: "No typed holes found in module",
    });
  }

  return JSON.stringify({
    success: true,
    holes,
    summary: `Found ${holes.length} typed hole(s)`,
  });
}

/**
 * Parse GHC output for typed-hole warnings [GHC-88464].
 *
 * Each hole warning looks like:
 *   src/Foo.hs:5:9: warning: [GHC-88464] [-Wtyped-holes]
 *       • Found hole: _ :: String
 *       • In an equation for 'foo': foo x = _
 *       • Relevant bindings include
 *           x :: Int (bound at src/Foo.hs:5:5)
 *           foo :: Int -> String (bound at src/Foo.hs:5:1)
 *         Valid hole fits include
 *           [] :: forall a. [a]
 *             with [] @Char
 *             (bound at <wired into compiler>)
 */
function parseHoleWarnings(output: string): TypedHole[] {
  const holes: TypedHole[] = [];

  // Split into warning blocks: each starts with a file:line:col pattern
  const warningBlocks = splitIntoBlocks(output);

  for (const block of warningBlocks) {
    // Only process typed-hole warnings
    if (!block.includes("GHC-88464") && !block.includes("-Wtyped-holes")) {
      continue;
    }

    const hole = parseOneHole(block);
    if (hole) {
      holes.push(hole);
    }
  }

  return holes;
}

/**
 * Split GHC output into blocks, each starting with a location header.
 */
function splitIntoBlocks(output: string): string[] {
  const blocks: string[] = [];
  const lines = output.split("\n");
  let current: string[] = [];

  for (const line of lines) {
    // New block starts with file:line:col: severity:
    if (/^\S+:\d+:\d+/.test(line) && current.length > 0) {
      blocks.push(current.join("\n"));
      current = [];
    }
    current.push(line);
  }
  if (current.length > 0) {
    blocks.push(current.join("\n"));
  }

  return blocks;
}

/**
 * Parse a single typed-hole warning block into a TypedHole structure.
 */
function parseOneHole(block: string): TypedHole | null {
  // Extract location from the first line
  const locMatch = block.match(/^(.+?):(\d+):(\d+)/);
  if (!locMatch) return null;

  const location = {
    file: locMatch[1]!,
    line: parseInt(locMatch[2]!, 10),
    column: parseInt(locMatch[3]!, 10),
  };

  // Extract hole name and type: "Found hole: _name :: Type"
  const holeMatch = block.match(/Found hole:\s+(\S+)\s+::\s+(.+?)(?:\n|$)/);
  if (!holeMatch) return null;

  const holeName = holeMatch[1]!;

  // The type may reference "Where:" on the next line, clean up
  let expectedType = holeMatch[2]!.trim();
  // Remove trailing context like "Where: ..."
  expectedType = expectedType.replace(/\s*Where:.*$/, "");

  // Extract expression context
  const exprMatch = block.match(/In the expression:\s+(.+?)(?:\n\s{4,}\S|$)/s);
  const expression = exprMatch ? exprMatch[1]!.trim() : undefined;

  // Extract equation context
  const eqMatch = block.match(
    /In an equation for '([^']+)':\s+(.+?)(?:\n\s{4,}•|$)/s
  );
  const equation = eqMatch ? `${eqMatch[1]} = ${eqMatch[2]!.trim()}` : undefined;

  // Extract relevant bindings
  const relevantBindings = parseRelevantBindings(block);

  // Extract valid hole fits
  const { fits, suppressed } = parseValidFits(block);

  return {
    hole: holeName,
    expectedType,
    location,
    expression,
    equation,
    relevantBindings,
    validFits: fits,
    suppressed,
  };
}

/**
 * Parse "Relevant bindings include" section.
 * Format:
 *   x :: Int (bound at src/Foo.hs:5:5)
 *   foo :: Int -> String (bound at src/Foo.hs:5:1)
 */
function parseRelevantBindings(block: string): RelevantBinding[] {
  const bindings: RelevantBinding[] = [];

  const section = block.match(
    /Relevant bindings include\n([\s\S]*?)(?:Valid hole fits|$)/
  );
  if (!section) return bindings;

  const lines = section[1]!.split("\n");
  for (const line of lines) {
    const trimmed = line.trim();
    // Match: name :: Type (bound at location)
    const match = trimmed.match(/^(\S+)\s+::\s+(.+?)\s+\(bound at (.+?)\)\s*$/);
    if (match) {
      bindings.push({
        name: match[1]!,
        type: match[2]!,
        location: match[3]!,
      });
    }
  }

  return bindings;
}

/**
 * Parse "Valid hole fits include" section.
 * Format:
 *   functionName :: forall a. SomeType
 *     with functionName @ConcreteType
 *     (imported from 'Module' at location)
 * Or:
 *   localName :: LocalType (bound at location)
 */
function parseValidFits(block: string): {
  fits: HoleFit[];
  suppressed: boolean;
} {
  const fits: HoleFit[] = [];
  let suppressed = false;

  const section = block.match(/Valid hole fits include\n([\s\S]*?)(?:\s*\||$)/);
  if (!section) return { fits, suppressed };

  const lines = section[1]!.split("\n");
  let i = 0;

  while (i < lines.length) {
    const line = lines[i]!;
    const trimmed = line.trim();

    // Check for suppression notice
    if (trimmed.includes("Some hole fits suppressed")) {
      suppressed = true;
      i++;
      continue;
    }

    // Match a fit: "name :: Type" or "name :: Type (bound at ...)"
    const fitMatch = trimmed.match(/^(\S+)\s+::\s+(.+?)(?:\s+\(bound at (.+?)\))?\s*$/);
    if (fitMatch) {
      const fit: HoleFit = {
        name: fitMatch[1]!,
        type: fitMatch[2]!,
      };

      if (fitMatch[3]) {
        fit.source = `bound at ${fitMatch[3]}`;
      }

      // Look ahead for "with ..." and "(imported from ...)" lines
      while (i + 1 < lines.length) {
        const nextTrimmed = lines[i + 1]!.trim();

        if (nextTrimmed.startsWith("with ")) {
          fit.specialization = nextTrimmed;
          i++;
        } else if (nextTrimmed.startsWith("(imported from ")) {
          // May span two lines: "(imported from 'Prelude' at ...\n   (and originally defined in ...))"
          let source = nextTrimmed;
          while (
            i + 2 < lines.length &&
            lines[i + 2]!.trim().startsWith("(and originally")
          ) {
            i++;
            source += " " + lines[i + 1]!.trim();
          }
          fit.source = source.replace(/^\(/, "").replace(/\)$/, "");
          i++;
        } else {
          break;
        }
      }

      fits.push(fit);
    }

    i++;
  }

  return { fits, suppressed };
}
