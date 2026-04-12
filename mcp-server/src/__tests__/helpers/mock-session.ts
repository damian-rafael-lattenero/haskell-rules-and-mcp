import { vi } from "vitest";
import type { GhciSession, GhciResult } from "../../ghci-session.js";

/**
 * Create a mock GhciSession for unit testing tool handlers.
 * Pass overrides to control the return values of specific methods.
 */
export function createMockSession(
  overrides: Record<string, GhciResult | ((...args: unknown[]) => Promise<GhciResult>)> = {}
): GhciSession {
  const defaultResult: GhciResult = { output: "", success: true };

  const makeHandler = (key: string) => {
    const override = overrides[key];
    if (typeof override === "function") return vi.fn(override);
    if (override) return vi.fn().mockResolvedValue(override);
    return vi.fn().mockResolvedValue(defaultResult);
  };

  return {
    execute: makeHandler("execute"),
    typeOf: makeHandler("typeOf"),
    infoOf: makeHandler("infoOf"),
    kindOf: makeHandler("kindOf"),
    loadModule: makeHandler("loadModule"),
    loadModules: makeHandler("loadModules"),
    reload: makeHandler("reload"),
    executeBatch: vi.fn().mockResolvedValue({ results: [], allSuccess: true }),
    isAlive: vi.fn().mockReturnValue(true),
    kill: vi.fn().mockResolvedValue(undefined),
    restart: vi.fn().mockResolvedValue(undefined),
    start: vi.fn().mockResolvedValue(undefined),
    on: vi.fn(),
    emit: vi.fn(),
  } as unknown as GhciSession;
}
