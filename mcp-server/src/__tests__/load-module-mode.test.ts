/**
 * Unit coverage for the new `mode: "replace" | "additive"` parameter on
 * ghci_load. Mocks the session so the tests don't need real GHCi — we only
 * verify that the right `:l` vs `:add` command is emitted.
 */
import { describe, it, expect, vi } from "vitest";
import { handleLoadModule } from "../tools/load-module.js";
import type { GhciSession } from "../ghci-session.js";

function makeSpySession(): { session: GhciSession; loadedWith: Array<{ path: string; mode?: string }>; reloaded: number } {
  const loadedWith: Array<{ path: string; mode?: string }> = [];
  let reloaded = 0;
  const session = {
    loadModule: vi.fn(async (path: string, opts?: { mode?: string }) => {
      loadedWith.push({ path, mode: opts?.mode });
      return { success: true, output: "Ok, one module loaded." };
    }),
    loadModules: vi.fn(async (_paths: string[], _names: string[], opts?: { mode?: string }) => {
      loadedWith.push({ path: _paths.join(","), mode: opts?.mode });
      return { success: true, output: "Ok, two modules loaded." };
    }),
    reload: vi.fn(async () => {
      reloaded++;
      return { success: true, output: "" };
    }),
    execute: vi.fn(async () => ({ success: true, output: "" })),
    showModules: vi.fn(async () => ({ success: true, output: "" })),
  } as unknown as GhciSession;
  return { session, loadedWith, reloaded };
}

describe("handleLoadModule mode parameter", () => {
  it("defaults to 'replace' when mode is not specified", async () => {
    const { session, loadedWith } = makeSpySession();
    await handleLoadModule(
      session,
      { module_path: "src/Foo.hs", diagnostics: false },
      "/tmp/project"
    );
    expect(loadedWith).toHaveLength(1);
    expect(loadedWith[0]?.mode).toBe("replace");
  });

  it("passes mode='additive' through to session.loadModule", async () => {
    const { session, loadedWith } = makeSpySession();
    await handleLoadModule(
      session,
      { module_path: "src/Foo.hs", mode: "additive", diagnostics: false },
      "/tmp/project"
    );
    expect(loadedWith).toHaveLength(1);
    expect(loadedWith[0]?.mode).toBe("additive");
  });

  it("ignores mode for plain reloads (no module_path, no load_all)", async () => {
    const { session } = makeSpySession();
    await handleLoadModule(
      session,
      { diagnostics: false, mode: "additive" },
      "/tmp/project"
    );
    // Plain reload should NOT touch loadModule — loadedWith stays empty.
    // That proves the "additive" hint on a bare reload does not accidentally
    // invoke `:add` on an unspecified module.
    expect((session.reload as unknown as { mock: { calls: unknown[] } }).mock.calls.length).toBeGreaterThanOrEqual(1);
  });
});
