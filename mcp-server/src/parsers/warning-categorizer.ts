import { GhcError } from "./error-parser.js";

export interface WarningAction {
  warning: GhcError;
  category: string;
  suggestedAction: string;
  confidence: "high" | "medium";
}

/**
 * Categorize a GHC warning and suggest a concrete fix action.
 * Returns null for warnings we don't know how to categorize.
 */
export function categorizeWarning(w: GhcError): WarningAction | null {
  const flag = w.warningFlag ?? "";
  const msg = w.message;

  switch (flag) {
    case "-Wunused-imports": {
      // "The import of 'Data.List' is redundant"
      // "The import of 'sort' from module 'Data.List' is redundant"
      // GHC 9.12 uses Unicode quotes (\u2018/\u2019)
      const modMatch = msg.match(
        /The import of ['\u2018](.+?)['\u2019] (?:from module ['\u2018](.+?)['\u2019] )?is redundant/
      );
      if (modMatch) {
        const name = modMatch[1]!;
        return {
          warning: w,
          category: "unused-import",
          suggestedAction: `Remove unused import: ${name} (line ${w.line})`,
          confidence: "high",
        };
      }
      return {
        warning: w,
        category: "unused-import",
        suggestedAction: `Remove redundant import at line ${w.line}`,
        confidence: "medium",
      };
    }

    case "-Wmissing-signatures": {
      // GHC format — type may span multiple lines:
      //   Top-level binding with no type signature:
      //       foo :: forall a. Num a =>
      //            a -> a
      //   |
      //   3 | foo x = x + 1
      const sigMatch = msg.match(
        /Top-level binding with no type signature:\s*\n([\s\S]+?)(?:\s*\n\s*\||\s*$)/
      );
      if (sigMatch) {
        const sig = sigMatch[1]!.replace(/\s+/g, " ").trim();
        return {
          warning: w,
          category: "missing-signature",
          suggestedAction: `Add type signature: ${sig}`,
          confidence: "high",
        };
      }
      return null;
    }

    case "-Wunused-matches":
    case "-Wunused-local-binds": {
      // "Defined but not used: 'x'"
      const nameMatch = msg.match(/Defined but not used: ['\u2018](.+?)['\u2019]/);
      if (nameMatch) {
        return {
          warning: w,
          category: "unused-binding",
          suggestedAction: `Prefix with underscore: _${nameMatch[1]!} (line ${w.line})`,
          confidence: "high",
        };
      }
      return null;
    }

    case "-Wincomplete-patterns": {
      // "Patterns of type \u2018Maybe Int\u2019 not matched: Nothing"
      // GHC 9.12 uses Unicode quotes (\u2018/\u2019)
      const patMatch = msg.match(
        /Patterns? (?:of type ['\u2018].+?['\u2019] )?not matched:\s*([\s\S]+?)(?:\s*\||\s*$)/
      );
      if (patMatch) {
        const missing = patMatch[1]!.trim().split(/\n/).map(l => l.trim()).join(", ");
        return {
          warning: w,
          category: "incomplete-patterns",
          suggestedAction: `Add missing pattern(s): ${missing}`,
          confidence: "high",
        };
      }
      return null;
    }

    case "-Wname-shadowing": {
      const shadowMatch = msg.match(
        /This binding for ['\u2018](.+?)['\u2019] shadows the existing binding/
      );
      if (shadowMatch) {
        return {
          warning: w,
          category: "name-shadowing",
          suggestedAction: `Rename '${shadowMatch[1]!}' to avoid shadowing (line ${w.line})`,
          confidence: "medium",
        };
      }
      return null;
    }

    case "-Wredundant-constraints": {
      return {
        warning: w,
        category: "redundant-constraint",
        suggestedAction: `Remove redundant constraint from type signature at line ${w.line}`,
        confidence: "high",
      };
    }

    case "-Wunused-do-bind": {
      return {
        warning: w,
        category: "unused-do-bind",
        suggestedAction: `Add 'void $' or '_ <-' before expression at line ${w.line}`,
        confidence: "high",
      };
    }

    case "-Wtyped-holes": {
      // Informational — not auto-fixable, but categorized
      return {
        warning: w,
        category: "typed-hole",
        suggestedAction: `Typed hole at line ${w.line} — read hole fits to determine implementation`,
        confidence: "medium",
      };
    }

    case "-Wtype-defaults": {
      return {
        warning: w,
        category: "type-defaults",
        suggestedAction: `Add explicit type annotation to avoid defaulting at line ${w.line}`,
        confidence: "medium",
      };
    }

    case "-Wdeferred-type-errors":
    case "-Wdeferred-out-of-scope-variables": {
      return {
        warning: w,
        category: "deferred-type-error",
        suggestedAction: `Deferred type error at line ${w.line} — fix the type mismatch${
          w.expected && w.actual
            ? `: expected ${w.expected}, actual ${w.actual}`
            : ""
        }`,
        confidence: "high",
      };
    }

    default:
      return null;
  }
}

/**
 * Categorize all warnings from a compilation.
 * Returns auto-fixable actions and uncategorized warnings separately.
 */
export function categorizeWarnings(warnings: GhcError[]): {
  actions: WarningAction[];
  uncategorized: GhcError[];
} {
  const actions: WarningAction[] = [];
  const uncategorized: GhcError[] = [];

  for (const w of warnings) {
    const action = categorizeWarning(w);
    if (action) {
      actions.push(action);
    } else {
      uncategorized.push(w);
    }
  }

  return { actions, uncategorized };
}
