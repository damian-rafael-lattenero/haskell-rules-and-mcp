/**
 * Path resolution helpers shared across every MCP tool that accepts a
 * caller-supplied module path. Two responsibilities:
 *
 *   1. Consistency — a single place that resolves `module_path` against
 *      `projectDir`, so we never have subtle differences in how tools
 *      canonicalize paths.
 *   2. Safety — rejects path traversal attempts that would escape the
 *      project root (`../../../etc/passwd`). Although the MCP runs locally,
 *      preventing traversal is defense-in-depth; a misbehaving client must
 *      not be able to coerce the server into reading arbitrary host files.
 */
import path from "node:path";

export class PathTraversalError extends Error {
  readonly requested: string;
  readonly projectDir: string;
  constructor(requested: string, projectDir: string) {
    super(
      `Module path '${requested}' escapes the project root '${projectDir}'. ` +
      "Use a path relative to the project root (e.g. 'src/Expr/Eval.hs')."
    );
    this.name = "PathTraversalError";
    this.requested = requested;
    this.projectDir = projectDir;
  }
}

/**
 * Resolve a user-provided module path against the project root and verify
 * the result is inside that root. Throws `PathTraversalError` on escape.
 *
 * Uses `path.resolve` + `startsWith` on normalized absolute paths. We do
 * NOT call `fs.realpath` because (a) it synchronously hits the filesystem
 * for every tool invocation, (b) a non-existent module path is a valid
 * case we want to pass through to the compile step. Symlinks therefore
 * remain an explicit risk — but project trees under the MCP's control
 * rarely use them.
 */
export function resolveModulePath(projectDir: string, modulePath: string): string {
  const normalizedRoot = path.resolve(projectDir);
  const resolved = path.resolve(normalizedRoot, modulePath);
  // Guard against equality being misinterpreted as an escape (root itself
  // is allowed, e.g. when the tool accepts empty module_path).
  if (resolved !== normalizedRoot && !resolved.startsWith(normalizedRoot + path.sep)) {
    throw new PathTraversalError(modulePath, normalizedRoot);
  }
  return resolved;
}

/**
 * Non-throwing variant that returns an Either-style result. Use when a
 * tool wants to surface the violation in its JSON envelope rather than
 * propagating an exception.
 */
export type ResolveModulePathResult =
  | { ok: true; absPath: string }
  | { ok: false; error: string };

export function tryResolveModulePath(
  projectDir: string,
  modulePath: string
): ResolveModulePathResult {
  try {
    return { ok: true, absPath: resolveModulePath(projectDir, modulePath) };
  } catch (err) {
    return {
      ok: false,
      error: err instanceof Error ? err.message : String(err),
    };
  }
}
