/**
 * Phase-D coverage: the manifest MUST carry upstream trust-anchor metadata
 * for every configured tool, and `getUpstreamDirectBinary` MUST return a
 * valid URL + sha256 for tools whose upstream publishes a direct
 * executable binary.
 *
 * Invariants pinned here:
 *   (1) Every tool has an `upstream` entry with a releases page URL + a
 *       recommended-install command. Without this, `ghci_toolchain_status`
 *       cannot point users at the canonical source.
 *   (2) When `upstream.distributionShape === "directBinary"`, every
 *       platform listed under `upstream.platforms` has BOTH a URL and a
 *       sha256. Partial entries would make `auto-download.ts` try an
 *       unverifiable download.
 *   (3) `getUpstreamDirectBinary` returns `null` for tools whose upstream
 *       distribution requires extraction (tarball / zip) — those MUST
 *       fall through to the mirror.
 */
import { describe, it, expect } from "vitest";
import {
  getUpstreamDirectBinary,
  getUpstreamMeta,
} from "../vendor-tools/manifest.js";

describe("Phase-D upstream trust-anchor metadata", () => {
  const TOOLS = ["hlint", "fourmolu", "ormolu", "hls"] as const;

  it("every tool declares releases page URL + recommended install", async () => {
    for (const tool of TOOLS) {
      const m = await getUpstreamMeta(tool);
      expect(m, `${tool} must have upstream metadata`).not.toBeNull();
      expect(m!.releasesPageUrl).toMatch(/^https:\/\/github\.com\//);
      expect(m!.recommendedInstall).toMatch(/ghcup/);
      expect(["directBinary", "tarball", "zip", "source"]).toContain(
        m!.distributionShape
      );
    }
  });

  it("fourmolu publishes a direct-binary upstream for darwin-arm64 with sha256", async () => {
    // fourmolu is the only tool whose upstream ships a drop-in executable
    // binary for our primary dev target. Auto-download should see it as the
    // preferred source.
    const entry = await getUpstreamDirectBinary("fourmolu", "darwin-arm64");
    expect(entry).not.toBeNull();
    expect(entry!.url).toContain("github.com/fourmolu/fourmolu");
    expect(entry!.url).toContain("v0.19.0.1");
    expect(entry!.sha256).toMatch(/^[a-f0-9]{64}$/);
  });

  it("hlint, ormolu, hls return null from getUpstreamDirectBinary (require extraction)", async () => {
    // hlint upstream ships .tar.gz, ormolu ships .zip, hls ships .tar.xz —
    // none are drop-in binaries. Auto-download must fall through to the
    // mirror for these tools until extraction infra exists.
    for (const tool of ["hlint", "ormolu", "hls"] as const) {
      const entry = await getUpstreamDirectBinary(tool, "darwin-arm64");
      expect(entry, `${tool} should NOT have a direct-binary upstream`).toBeNull();
    }
  });

  it("every direct-binary upstream entry has BOTH url and sha256 (no partial configs)", async () => {
    for (const tool of TOOLS) {
      const meta = await getUpstreamMeta(tool);
      if (meta?.distributionShape !== "directBinary") continue;
      for (const [target, entry] of Object.entries(meta.platforms ?? {})) {
        expect(entry?.url, `${tool} ${target} url`).toBeTruthy();
        expect(entry?.sha256, `${tool} ${target} sha256`).toMatch(/^[a-f0-9]{64}$/);
      }
    }
  });

  it("getUpstreamDirectBinary returns null on unsupported platforms", async () => {
    // Linux/Windows darwin-arm64 config shouldn't leak to other targets.
    const entry = await getUpstreamDirectBinary("fourmolu", "linux-arm64");
    expect(entry).toBeNull();
  });
});
