import { describe, it, expect } from "vitest";
import { RULES_REGISTRY, loadRule } from "../resources/rules.js";

describe("RULES_REGISTRY", () => {
  it("has 3 rules", () => {
    expect(RULES_REGISTRY).toHaveLength(3);
  });

  it("each rule has required fields", () => {
    for (const rule of RULES_REGISTRY) {
      expect(rule.name).toBeTruthy();
      expect(rule.uri).toMatch(/^rules:\/\//);
      expect(rule.title).toBeTruthy();
      expect(rule.description).toBeTruthy();
      expect(rule.fileName).toMatch(/\.md$/);
      expect(rule.embeddedContent).toBeTruthy();
    }
  });

  it("automation rule contains warning action table", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-automation")!;
    expect(rule.embeddedContent).toContain("Warning Action Table");
  });

  it("development rule contains typed holes info", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-development")!;
    expect(rule.embeddedContent).toContain("Typed Holes");
  });

  it("conventions rule contains import style", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-project-conventions")!;
    expect(rule.embeddedContent).toContain("Import Style");
  });
});

describe("loadRule", () => {
  it("returns embedded content as fallback when file not found", async () => {
    const fakeRule = {
      name: "test",
      uri: "rules://test",
      title: "Test",
      description: "Test",
      fileName: "nonexistent-file-xyz.md",
      embeddedContent: "fallback content here",
    };
    const content = await loadRule(fakeRule);
    expect(content).toBe("fallback content here");
  });

  it("loads real rule files when they exist", async () => {
    const automationRule = RULES_REGISTRY.find(r => r.name === "haskell-automation")!;
    const content = await loadRule(automationRule);
    expect(content).toBeTruthy();
    expect(content.length).toBeGreaterThan(100);
    // Should contain the full version from disk, which has more content than embedded
    expect(content).toContain("Warning Action Table");
  });
});
