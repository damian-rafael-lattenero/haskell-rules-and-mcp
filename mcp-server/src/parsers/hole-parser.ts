/**
 * Parser for GHC typed-hole warnings [GHC-88464].
 *
 * Extracts structured information about each hole: expected type,
 * relevant bindings, valid hole fits, and suppression status.
 */

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
 * Lightweight hole summary used by ghci_load (less detail than TypedHole).
 */
export interface HoleSummary {
  hole: string;
  expectedType: string;
  line: number;
  column: number;
  relevantBindings: string[];
  topFits: string[];
}

/**
 * Split GHC output into diagnostic blocks, each starting with a location header.
 * Reusable for any GHC warning/error block splitting.
 */
export function splitGhcDiagnosticBlocks(output: string): string[] {
  const blocks: string[] = [];
  const lines = output.split("\n");
  let current: string[] = [];

  for (const line of lines) {
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
 * Parse GHC output for typed-hole warnings [GHC-88464].
 * Returns detailed TypedHole structures with fits, bindings, etc.
 */
export function parseTypedHoles(output: string): TypedHole[] {
  const holes: TypedHole[] = [];
  const warningBlocks = splitGhcDiagnosticBlocks(output);

  for (const block of warningBlocks) {
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
 * Parse GHC output for typed-hole warnings, returning lightweight summaries.
 * Used by ghci_load for quick hole reporting without full fit details.
 */
export function parseHoleSummaries(output: string): HoleSummary[] {
  const holes: HoleSummary[] = [];
  const lines = output.split("\n");

  let i = 0;
  while (i < lines.length) {
    const line = lines[i]!;

    const headerMatch = line.match(
      /^.+?:(\d+):(\d+).*?warning:.*?\[GHC-88464\]/
    );
    if (!headerMatch) {
      i++;
      continue;
    }

    const holeLine = parseInt(headerMatch[1]!, 10);
    const holeCol = parseInt(headerMatch[2]!, 10);

    let block = line + "\n";
    i++;
    while (i < lines.length) {
      const next = lines[i]!;
      if (/^\S+:\d+:\d+/.test(next)) break;
      block += next + "\n";
      i++;
    }

    const holeMatch = block.match(/Found hole:\s+(\S+)\s+::\s+(.+?)(?:\n|$)/);
    const holeName = holeMatch ? holeMatch[1]! : "_";
    const expectedType = holeMatch
      ? holeMatch[2]!.trim().replace(/\s*Where:.*$/, "")
      : "unknown";

    const bindings: string[] = [];
    const bindSection = block.match(
      /Relevant bindings include\n([\s\S]*?)(?:Valid (?:refinement )?hole fits|$)/
    );
    if (bindSection) {
      for (const bLine of bindSection[1]!.split("\n")) {
        const bMatch = bLine.trim().match(/^(\S+)\s+::\s+(.+?)\s+\(bound/);
        if (bMatch) {
          bindings.push(`${bMatch[1]} :: ${bMatch[2]}`);
        }
      }
    }

    const fits: string[] = [];
    const fitSection = block.match(
      /Valid (?:refinement )?hole fits include\n([\s\S]*?)(?:\s*\||\s*$)/
    );
    if (fitSection) {
      for (const fLine of fitSection[1]!.split("\n")) {
        const trimmedFit = fLine.trim();
        if (trimmedFit.startsWith("with ") || trimmedFit.startsWith("(") || trimmedFit.startsWith("where ")) {
          continue;
        }
        // Standard fit: "name :: Type"
        const fMatch = trimmedFit.match(/^(\S+)\s+::\s+(.+?)(?:\s+\(bound|\s*$)/);
        // Refinement fit: "Name (_ :: ArgType)"
        const rMatch = !fMatch ? trimmedFit.match(/^(\S+)\s+\(_\s+::\s+(.+?)\)\s*$/) : null;
        if (fMatch) {
          fits.push(`${fMatch[1]} :: ${fMatch[2]}`);
        } else if (rMatch) {
          fits.push(`${rMatch[1]} (_ :: ${rMatch[2]})`);
        }
      }
    }

    holes.push({
      hole: holeName,
      expectedType,
      line: holeLine,
      column: holeCol,
      relevantBindings: bindings,
      topFits: fits,
    });
  }

  return holes;
}

// --- Internal helpers ---

function parseOneHole(block: string): TypedHole | null {
  const locMatch = block.match(/^(.+?):(\d+):(\d+)/);
  if (!locMatch) return null;

  const location = {
    file: locMatch[1]!,
    line: parseInt(locMatch[2]!, 10),
    column: parseInt(locMatch[3]!, 10),
  };

  const holeMatch = block.match(/Found hole:\s+(\S+)\s+::\s+(.+?)(?:\n|$)/);
  if (!holeMatch) return null;

  const holeName = holeMatch[1]!;
  let expectedType = holeMatch[2]!.trim();
  expectedType = expectedType.replace(/\s*Where:.*$/, "");

  const exprMatch = block.match(/In the expression:\s+(.+?)(?:\n\s{4,}\S|$)/s);
  const expression = exprMatch ? exprMatch[1]!.trim() : undefined;

  const eqMatch = block.match(
    /In an equation for '([^']+)':\s+(.+?)(?:\n\s{4,}•|$)/s
  );
  const equation = eqMatch ? `${eqMatch[1]} = ${eqMatch[2]!.trim()}` : undefined;

  const relevantBindings = parseRelevantBindings(block);
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

function parseRelevantBindings(block: string): RelevantBinding[] {
  const bindings: RelevantBinding[] = [];

  const section = block.match(
    /Relevant bindings include\n([\s\S]*?)(?:Valid (?:refinement )?hole fits|$)/
  );
  if (!section) return bindings;

  const lines = section[1]!.split("\n");
  for (const line of lines) {
    const trimmed = line.trim();
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

function parseValidFits(block: string): {
  fits: HoleFit[];
  suppressed: boolean;
} {
  const fits: HoleFit[] = [];
  let suppressed = false;

  const section = block.match(/Valid (?:refinement )?hole fits include\n([\s\S]*?)(?:\s*\||$)/);
  if (!section) return { fits, suppressed };

  const lines = section[1]!.split("\n");
  let i = 0;

  while (i < lines.length) {
    const line = lines[i]!;
    const trimmed = line.trim();

    if (trimmed.includes("Some hole fits suppressed")) {
      suppressed = true;
      i++;
      continue;
    }

    // Standard fit: "name :: Type (bound at ...)"
    const fitMatch = trimmed.match(/^(\S+)\s+::\s+(.+?)(?:\s+\(bound at (.+?)\))?\s*$/);
    // Refinement fit: "Name (_ :: ArgType)" — type comes from the "where" line
    const refinementMatch = !fitMatch ? trimmed.match(/^(\S+)\s+\(_\s+::\s+(.+?)\)\s*$/) : null;

    if (fitMatch || refinementMatch) {
      const match = fitMatch ?? refinementMatch!;
      const fit: HoleFit = {
        name: match[1]!,
        type: match[2]!,
      };

      if (fitMatch && fitMatch[3]) {
        fit.source = `bound at ${fitMatch[3]}`;
      }

      while (i + 1 < lines.length) {
        const nextTrimmed = lines[i + 1]!.trim();

        if (nextTrimmed.startsWith("where ")) {
          // "where Name :: full type" — extract full type for refinement fits
          const whereMatch = nextTrimmed.match(/^where\s+\S+\s+::\s+(.+)$/);
          if (whereMatch && refinementMatch) {
            fit.type = whereMatch[1]!;
          }
          i++;
        } else if (nextTrimmed.startsWith("with ")) {
          fit.specialization = nextTrimmed;
          i++;
        } else if (nextTrimmed.startsWith("(imported from ")) {
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
