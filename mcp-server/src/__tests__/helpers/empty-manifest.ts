/**
 * Writes an empty-releases `bundled-tools-manifest.json` to a temp file and
 * returns the path so e2e tests can pass it as
 * `HASKELL_FLOWS_MANIFEST_PATH` to the MCP subprocess.
 *
 * Why: when the MCP is spawned as a child (StdioClientTransport) the
 * in-process `setManifestPathForTests` does not cross the process boundary.
 * Tools that trigger auto-download (`ghci_format`, `ghci_lint`,
 * `ghci_toolchain_status`) then race on `vendor-tools/<tool>/<platform>/`
 * under parallel `test:e2e` load, producing timeouts that look like
 * flakes. Pointing the subprocess at an empty-releases manifest forces
 * `canAutoDownload()` to return false for every tool, making the
 * resolution chain deterministic: host → bundled → unavailable, no
 * network. Tests that already accept the `unavailable` branch keep
 * working; tests that hard-required a successful formatter continue to
 * see the real host/bundled tool.
 */
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

export interface EmptyManifestHandle {
  /** Absolute path to the temp manifest — assign to HASKELL_FLOWS_MANIFEST_PATH. */
  path: string;
  /** Clean up the temp directory. */
  cleanup(): Promise<void>;
}

export async function writeEmptyManifest(prefix = "e2e-empty-manifest-"): Promise<EmptyManifestHandle> {
  const dir = await mkdtemp(path.join(tmpdir(), prefix));
  const file = path.join(dir, "bundled-tools-manifest.json");
  await writeFile(
    file,
    JSON.stringify({
      manifestVersion: 2,
      updatedAt: "test",
      releases: {
        hlint: { binaryName: "hlint", platforms: {} },
        fourmolu: { binaryName: "fourmolu", platforms: {} },
        ormolu: { binaryName: "ormolu", platforms: {} },
        hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
      },
      tools: [],
    }),
    "utf-8"
  );
  return {
    path: file,
    async cleanup() {
      await rm(dir, { recursive: true, force: true });
    },
  };
}
