import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { getPlaygroundDir, discoverProjects } from "../project-manager.js";
import { mkdtemp, rm, writeFile, mkdir } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("getPlaygroundDir", () => {
  it("appends playground to base dir", () => {
    expect(getPlaygroundDir("/home/user/project")).toBe("/home/user/project/playground");
  });

  it("handles trailing slash", () => {
    const result = getPlaygroundDir("/home/user/project");
    expect(result.endsWith("//")).toBe(false);
  });
});

describe("discoverProjects — multi-package (cabal.project)", () => {
  let playgroundDir: string;

  beforeEach(async () => {
    playgroundDir = await mkdtemp(path.join(os.tmpdir(), "pm-test-"));
  });

  afterEach(async () => {
    await rm(playgroundDir, { recursive: true, force: true });
  });

  it("discovers a single package in playground", async () => {
    const pkgDir = path.join(playgroundDir, "my-pkg");
    await mkdir(pkgDir);
    await writeFile(
      path.join(pkgDir, "my-pkg.cabal"),
      "cabal-version: 2.4\nname: my-pkg\nversion: 0.1.0.0\n",
      "utf-8"
    );
    const projects = await discoverProjects(playgroundDir);
    expect(projects.length).toBe(1);
    expect(projects[0]!.name).toBe("my-pkg");
  });

  it("discovers two packages in playground", async () => {
    for (const name of ["pkg-a", "pkg-b"]) {
      const d = path.join(playgroundDir, name);
      await mkdir(d);
      await writeFile(
        path.join(d, `${name}.cabal`),
        `cabal-version: 2.4\nname: ${name}\nversion: 0.1.0.0\n`,
        "utf-8"
      );
    }
    const projects = await discoverProjects(playgroundDir);
    expect(projects.length).toBe(2);
    const names = projects.map((p) => p.name);
    expect(names).toContain("pkg-a");
    expect(names).toContain("pkg-b");
  });

  it("returns empty array for empty playground", async () => {
    const projects = await discoverProjects(playgroundDir);
    expect(projects).toEqual([]);
  });

  it("ignores non-project directories", async () => {
    await mkdir(path.join(playgroundDir, "not-a-project"));
    const projects = await discoverProjects(playgroundDir);
    expect(projects).toEqual([]);
  });

  it("skips projects with an empty .cabal file (no name: field)", async () => {
    // Bug fix: a directory with an empty .cabal was previously discovered and
    // added to the project list using the directory name as fallback.  When
    // ghci_switch_project tried to start GHCi there it failed, and the server
    // was left pointing at the broken project.
    const brokenDir = path.join(playgroundDir, "broken-pkg");
    await mkdir(brokenDir);
    await writeFile(path.join(brokenDir, "broken-pkg.cabal"), "", "utf-8"); // empty!

    const projects = await discoverProjects(playgroundDir);
    expect(projects.map((p) => p.name)).not.toContain("broken-pkg");
    expect(projects).toHaveLength(0);
  });

  it("skips projects whose .cabal has content but no name: field", async () => {
    const pkgDir = path.join(playgroundDir, "nameless");
    await mkdir(pkgDir);
    // Valid cabal syntax but deliberately missing the name: field
    await writeFile(
      path.join(pkgDir, "nameless.cabal"),
      "cabal-version: 2.4\nversion: 0.1.0.0\n",
      "utf-8"
    );

    const projects = await discoverProjects(playgroundDir);
    expect(projects).toHaveLength(0);
  });

  it("still discovers valid projects alongside a broken one", async () => {
    // Good project
    const goodDir = path.join(playgroundDir, "good-pkg");
    await mkdir(goodDir);
    await writeFile(
      path.join(goodDir, "good-pkg.cabal"),
      "cabal-version: 2.4\nname: good-pkg\nversion: 0.1.0.0\n",
      "utf-8"
    );

    // Broken project (empty cabal)
    const brokenDir = path.join(playgroundDir, "broken-pkg");
    await mkdir(brokenDir);
    await writeFile(path.join(brokenDir, "broken-pkg.cabal"), "", "utf-8");

    const projects = await discoverProjects(playgroundDir);
    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("good-pkg");
  });
});
