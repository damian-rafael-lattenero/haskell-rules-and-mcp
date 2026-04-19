/**
 * Central configuration constants for the haskell-flows MCP.
 *
 * Rule of thumb: any literal used in >1 file that is NOT a tool-specific
 * implementation detail should live here. Timeouts, well-known filenames,
 * safety caps. Tool-specific constants (e.g. a regex pattern only used
 * inside a single parser) stay at the call site.
 *
 * Re-exports are deliberately sparse — each module imports exactly what it
 * needs via `import { TIMEOUTS } from "../config.js"`.
 */

/** All timeouts in milliseconds. */
export const TIMEOUTS = {
  /** Default GHCi command execution. */
  GHCI_COMMAND: 30_000,
  /** Cabal build/test (can be long on first compile). */
  CABAL_BUILD: 180_000,
  CABAL_TEST: 180_000,
  /** Single external process (fourmolu/ormolu/hlint formatting of one file). */
  FORMATTER: 30_000,
  LINTER: 30_000,
  /** Invocation of `--version` to discover a tool. */
  TOOL_VERSION_PROBE: 10_000,
  /** Network download of a vendored binary (large). */
  TOOL_DOWNLOAD: 5 * 60 * 1000,
  /** Health probe on a running GHCi. */
  GHCI_HEALTH_PROBE: 5_000,
} as const;

/** Well-known filesystem paths relative to a project root. */
export const PROJECT_DIRS = {
  /** Property store and other local state. */
  LOCAL_STATE: ".haskell-flows",
  PROPERTY_STORE: ".haskell-flows/properties.json",
  /** Cabal build output — git-ignored. */
  CABAL_BUILD_OUTPUT: "dist-newstyle",
} as const;

/** Paths relative to the mcp-server package root. */
export const PACKAGE_DIRS = {
  VENDOR_TOOLS: "vendor-tools",
  BUNDLED_MANIFEST: "vendor-tools/bundled-tools-manifest.json",
  RULES: "rules",
} as const;

/** Size caps to prevent runaway memory usage from untrusted output. */
export const OUTPUT_LIMITS = {
  /** `:browse <module>` can be huge for libraries — truncate at this size. */
  BROWSE_OUTPUT_BYTES: 50_000,
  /** Raw GHCi output attached to error responses. */
  GHCI_RAW_SNIPPET_BYTES: 10_000,
} as const;
