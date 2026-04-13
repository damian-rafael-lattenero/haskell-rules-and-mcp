import { describe, it, expect } from "vitest";
import { handleImports } from "../tools/imports.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleImports", () => {
  it("returns parsed imports", async () => {
    const session = createMockSession({
      showImports: {
        output:
          "import Prelude -- implicit\n" +
          "import Data.Map.Strict qualified as Map\n" +
          "import TestLib",
        success: true,
      },
    });
    const result = JSON.parse(await handleImports(session));
    expect(result.success).toBe(true);
    expect(result.count).toBe(3);
    expect(result.imports[0].module).toBe("Prelude");
    expect(result.imports[0].implicit).toBe(true);
    expect(result.imports[1].module).toBe("Data.Map.Strict");
    expect(result.imports[1].qualified).toBe(true);
    expect(result.imports[1].alias).toBe("Map");
  });

  it("handles empty imports", async () => {
    const session = createMockSession({
      showImports: { output: "", success: true },
    });
    const result = JSON.parse(await handleImports(session));
    expect(result.success).toBe(true);
    expect(result.count).toBe(0);
  });

  it("handles session error", async () => {
    const session = createMockSession({
      showImports: { output: "Error", success: false },
    });
    const result = JSON.parse(await handleImports(session));
    expect(result.success).toBe(false);
  });
});
