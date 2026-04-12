import { describe, it, expect } from "vitest";
import { getPlaygroundDir } from "../project-manager.js";

describe("getPlaygroundDir", () => {
  it("appends playground to base dir", () => {
    expect(getPlaygroundDir("/home/user/project")).toBe("/home/user/project/playground");
  });

  it("handles trailing slash", () => {
    const result = getPlaygroundDir("/home/user/project");
    expect(result.endsWith("//")).toBe(false);
  });
});
