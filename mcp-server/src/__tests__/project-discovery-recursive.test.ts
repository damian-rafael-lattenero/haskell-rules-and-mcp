import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { discoverProjects } from "../project-manager.js";
import { mkdir, writeFile, rm } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const testRoot = path.join(__dirname, "fixtures", "recursive-discovery");

describe("discoverProjects - recursive search", () => {
  beforeEach(async () => {
    await rm(testRoot, { recursive: true, force: true });
    await mkdir(testRoot, { recursive: true });
  });

  afterEach(async () => {
    await rm(testRoot, { recursive: true, force: true });
  });

  it("should find projects in subdirectories", async () => {
    // Create structure:
    // testRoot/
    //   playground/
    //     expr-eval/
    //       expr-eval.cabal
    //     parser/
    //       parser.cabal
    //   direct-project/
    //     direct.cabal

    await mkdir(path.join(testRoot, "playground", "expr-eval"), { recursive: true });
    await writeFile(
      path.join(testRoot, "playground", "expr-eval", "expr-eval.cabal"),
      "name: expr-eval\nversion: 0.1.0.0\n"
    );

    await mkdir(path.join(testRoot, "playground", "parser"), { recursive: true });
    await writeFile(
      path.join(testRoot, "playground", "parser", "parser.cabal"),
      "name: parser\nversion: 0.1.0.0\n"
    );

    await mkdir(path.join(testRoot, "direct-project"), { recursive: true });
    await writeFile(
      path.join(testRoot, "direct-project", "direct.cabal"),
      "name: direct\nversion: 0.1.0.0\n"
    );

    const projects = await discoverProjects(testRoot, 3);

    expect(projects).toHaveLength(3);
    expect(projects.map(p => p.name).sort()).toEqual(["direct", "expr-eval", "parser"]);
    
    const exprEval = projects.find(p => p.name === "expr-eval");
    expect(exprEval).toBeDefined();
    expect(exprEval!.path).toContain("playground");
  });

  it("should respect maxDepth parameter", async () => {
    // Create deep structure:
    // testRoot/
    //   level1/
    //     level2/
    //       level3/
    //         level4/
    //           deep.cabal

    await mkdir(path.join(testRoot, "level1", "level2", "level3", "level4"), { recursive: true });
    await writeFile(
      path.join(testRoot, "level1", "level2", "level3", "level4", "deep.cabal"),
      "name: deep\nversion: 0.1.0.0\n"
    );

    // With maxDepth=2, should NOT find the project
    const projectsShallow = await discoverProjects(testRoot, 2);
    expect(projectsShallow).toHaveLength(0);

    // With maxDepth=5, should find it
    const projectsDeep = await discoverProjects(testRoot, 5);
    expect(projectsDeep).toHaveLength(1);
    expect(projectsDeep[0]!.name).toBe("deep");
  });

  it("should skip hidden directories", async () => {
    // Create structure with hidden directory:
    // testRoot/
    //   .hidden/
    //     hidden-project/
    //       hidden.cabal
    //   visible/
    //     visible.cabal

    await mkdir(path.join(testRoot, ".hidden", "hidden-project"), { recursive: true });
    await writeFile(
      path.join(testRoot, ".hidden", "hidden-project", "hidden.cabal"),
      "name: hidden\nversion: 0.1.0.0\n"
    );

    await mkdir(path.join(testRoot, "visible"), { recursive: true });
    await writeFile(
      path.join(testRoot, "visible", "visible.cabal"),
      "name: visible\nversion: 0.1.0.0\n"
    );

    const projects = await discoverProjects(testRoot, 3);

    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("visible");
  });

  it("should skip node_modules and dist-newstyle", async () => {
    // Create structure:
    // testRoot/
    //   node_modules/
    //     some-package/
    //       package.cabal
    //   dist-newstyle/
    //     build/
    //       temp.cabal
    //   real-project/
    //     real.cabal

    await mkdir(path.join(testRoot, "node_modules", "some-package"), { recursive: true });
    await writeFile(
      path.join(testRoot, "node_modules", "some-package", "package.cabal"),
      "name: package\nversion: 0.1.0.0\n"
    );

    await mkdir(path.join(testRoot, "dist-newstyle", "build"), { recursive: true });
    await writeFile(
      path.join(testRoot, "dist-newstyle", "build", "temp.cabal"),
      "name: temp\nversion: 0.1.0.0\n"
    );

    await mkdir(path.join(testRoot, "real-project"), { recursive: true });
    await writeFile(
      path.join(testRoot, "real-project", "real.cabal"),
      "name: real\nversion: 0.1.0.0\n"
    );

    const projects = await discoverProjects(testRoot, 3);

    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("real");
  });

  it("should skip projects with empty cabal files", async () => {
    // Create structure:
    // testRoot/
    //   empty-cabal/
    //     empty.cabal (no name field)
    //   valid-project/
    //     valid.cabal

    await mkdir(path.join(testRoot, "empty-cabal"), { recursive: true });
    await writeFile(
      path.join(testRoot, "empty-cabal", "empty.cabal"),
      "# Empty cabal file\n"
    );

    await mkdir(path.join(testRoot, "valid-project"), { recursive: true });
    await writeFile(
      path.join(testRoot, "valid-project", "valid.cabal"),
      "name: valid\nversion: 0.1.0.0\n"
    );

    const projects = await discoverProjects(testRoot, 3);

    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("valid");
  });

  it("should handle multiple projects in same parent directory", async () => {
    // Create structure:
    // testRoot/
    //   playground/
    //     project-a/
    //       a.cabal
    //     project-b/
    //       b.cabal
    //     project-c/
    //       c.cabal

    for (const name of ["project-a", "project-b", "project-c"]) {
      await mkdir(path.join(testRoot, "playground", name), { recursive: true });
      await writeFile(
        path.join(testRoot, "playground", name, `${name}.cabal`),
        `name: ${name}\nversion: 0.1.0.0\n`
      );
    }

    const projects = await discoverProjects(testRoot, 3);

    expect(projects).toHaveLength(3);
    expect(projects.map(p => p.name).sort()).toEqual(["project-a", "project-b", "project-c"]);
  });
});
