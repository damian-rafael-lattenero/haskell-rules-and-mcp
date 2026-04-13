import { describe, it, expect } from "vitest";
import { handleDoc } from "../tools/doc.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleDoc", () => {
  it("returns documentation when available", async () => {
    const session = createMockSession({
      docOf: {
        output: " Map each element of a structure to a monadic action.",
        success: true,
      },
    });
    const result = JSON.parse(await handleDoc(session, { name: "mapM" }));
    expect(result.success).toBe(true);
    expect(result.name).toBe("mapM");
    expect(result.documentation).toContain("monadic action");
  });

  it("returns null documentation when not available", async () => {
    const session = createMockSession({
      docOf: { output: "No documentation found for 'myFunc'", success: true },
    });
    const result = JSON.parse(await handleDoc(session, { name: "myFunc" }));
    expect(result.success).toBe(true);
    expect(result.documentation).toBeNull();
    expect(result.message).toContain("No documentation");
  });

  it("handles empty output", async () => {
    const session = createMockSession({
      docOf: { output: "", success: true },
    });
    const result = JSON.parse(await handleDoc(session, { name: "x" }));
    expect(result.success).toBe(true);
    expect(result.documentation).toBeNull();
  });

  it("handles session error", async () => {
    const session = createMockSession({
      docOf: { output: "Error: not found", success: false },
    });
    const result = JSON.parse(await handleDoc(session, { name: "x" }));
    expect(result.success).toBe(false);
  });
});
