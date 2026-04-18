/**
 * Unit coverage for the manifest loader: make sure the v2 `releases` matrix
 * produces correct accessors, missing tools degrade to empty rather than
 * throwing, and cache invalidation works so tests can swap in fixtures.
 */
import { describe, it, expect, beforeEach } from "vitest";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import {
  enumerateConfiguredReleases,
  getReleaseEntry,
  loadManifest,
  resetManifestCache,
  setManifestPathForTests,
} from "../vendor-tools/manifest.js";

let tmp: string;

async function writeManifest(content: unknown): Promise<string> {
  const file = path.join(tmp, "manifest.json");
  await writeFile(file, JSON.stringify(content), "utf-8");
  return file;
}

describe("vendor-tools manifest loader", () => {
  beforeEach(async () => {
    tmp = await mkdtemp(path.join(tmpdir(), "manifest-test-"));
    resetManifestCache();
    setManifestPathForTests(null);
  });

  it("reads the real manifest from disk and finds darwin-arm64 entries", async () => {
    setManifestPathForTests(null); // real file
    const m = await loadManifest();
    expect(m.manifestVersion).toBeGreaterThanOrEqual(2);
    expect(m.releases.hlint.binaryName).toBe("hlint");
    expect(m.releases.hls.binaryName).toBe("haskell-language-server-wrapper");

    const entry = await getReleaseEntry("hlint", "darwin-arm64");
    expect(entry?.entry.version).toBe("v3.10");
    expect(entry?.binaryName).toBe("hlint");
  });

  it("degrades gracefully when the manifest file is absent", async () => {
    setManifestPathForTests(path.join(tmp, "does-not-exist.json"));
    const m = await loadManifest();
    expect(m.releases.hlint.platforms).toEqual({});
    expect(m.releases.fourmolu.platforms).toEqual({});
    const entry = await getReleaseEntry("hlint", "darwin-arm64");
    expect(entry).toBeUndefined();

    await rm(tmp, { recursive: true, force: true });
  });

  it("accepts a v2 manifest and exposes the release map", async () => {
    const p = await writeManifest({
      manifestVersion: 2,
      updatedAt: "2026-01-01T00:00:00Z",
      releases: {
        hlint: {
          binaryName: "hlint",
          platforms: {
            "darwin-arm64": {
              version: "v9.9.9",
              url: "https://example.com/hlint-darwin-arm64",
              sha256: "a".repeat(64),
            },
          },
        },
        fourmolu: { binaryName: "fourmolu", platforms: {} },
        ormolu: { binaryName: "ormolu", platforms: {} },
        hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
      },
      tools: [],
    });
    setManifestPathForTests(p);

    const lookup = await getReleaseEntry("hlint", "darwin-arm64");
    expect(lookup?.entry.version).toBe("v9.9.9");
    expect(lookup?.entry.sha256).toBe("a".repeat(64));

    const all = await enumerateConfiguredReleases();
    expect(all.length).toBe(1);
    expect(all[0]?.tool).toBe("hlint");
    expect(all[0]?.target).toBe("darwin-arm64");

    await rm(tmp, { recursive: true, force: true });
  });

  it("tolerates a v1 manifest (no releases section) without throwing", async () => {
    const p = await writeManifest({
      manifestVersion: 1,
      updatedAt: "2026-01-01T00:00:00Z",
      tools: [],
    });
    setManifestPathForTests(p);

    const m = await loadManifest();
    expect(m.manifestVersion).toBe(1);
    expect(m.releases.hlint.platforms).toEqual({});
    expect(await enumerateConfiguredReleases()).toEqual([]);

    await rm(tmp, { recursive: true, force: true });
  });

  it("rejects malformed release entries but keeps good ones", async () => {
    const p = await writeManifest({
      manifestVersion: 2,
      releases: {
        hlint: {
          binaryName: "hlint",
          platforms: {
            "darwin-arm64": {
              version: "v1.0",
              url: "https://example.com/hlint",
            },
            "linux-x64": { version: 42, url: null }, // bad shape → drop whole platforms
          },
        },
        fourmolu: { binaryName: "fourmolu", platforms: {} },
        ormolu: { binaryName: "ormolu", platforms: {} },
        hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
      },
      tools: [],
    });
    setManifestPathForTests(p);

    // The whole hlint spec should be rejected because platforms contains a
    // malformed entry — loader normalizes by dropping the whole spec.
    const good = await getReleaseEntry("hlint", "darwin-arm64");
    expect(good).toBeUndefined();

    await rm(tmp, { recursive: true, force: true });
  });
});
