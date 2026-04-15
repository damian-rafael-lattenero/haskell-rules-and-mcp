import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { GhciSession } from "../ghci-session.js";
import path from "node:path";
import { rm } from "node:fs/promises";

const TEST_PROJECT = path.resolve(import.meta.dirname, "fixtures", "test-project");

describe.sequential("GhciSession Health Monitoring", () => {
  let session: GhciSession;

  beforeEach(async () => {
    session = new GhciSession(TEST_PROJECT);
    await session.start();
  });

  afterEach(async () => {
    if (session.isAlive()) {
      await session.kill();
    }
  });

  it("starts with healthy status", () => {
    const health = session.getHealth();
    expect(health.status).toBe("healthy");
    expect(health.lastCommand).toBeUndefined();
    expect(health.bufferSize).toBeGreaterThanOrEqual(0);
  });

  it("tracks last executed command", async () => {
    await session.execute(":t map");
    const health = session.getHealth();
    expect(health.lastCommand).toBe(":t map");
  });

  it("marks session as corrupted after timeout", async () => {
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    const health = session.getHealth();
    expect(health.status).toBe("corrupted");
  });

  it("prevents execution on corrupted session", async () => {
    // Force timeout to corrupt session
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    // Wait for process to be killed
    await new Promise(r => setTimeout(r, 500));
    
    // Try to execute another command - should fail because session is not running
    await expect(
      session.execute(":t map")
    ).rejects.toThrow(/corrupted|not running/);
  });

  it("auto-recovers via restart", async () => {
    // Corrupt the session
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    expect(session.getHealth().status).toBe("corrupted");
    
    // Wait for process to be killed
    await new Promise(r => setTimeout(r, 500));
    
    // Restart should reset health
    await session.restart();
    
    const health = session.getHealth();
    expect(health.status).toBe("healthy");
    expect(health.lastCommand).toBeUndefined();
    
    // Should be able to execute commands again
    const result = await session.execute(":t map");
    expect(result.success).toBe(true);
  }, 60000); // Increase timeout to 60s for restart test

  it("rejects dangerous commands in batch", async () => {
    await expect(
      session.executeBatch([":t map", ":set +m", ":t foldr"])
    ).rejects.toThrow("Dangerous GHCi command");
    
    await expect(
      session.executeBatch([":t map", ":set prompt \"test>\""])
    ).rejects.toThrow("Dangerous GHCi command");
  });

  it("allows safe commands in batch", async () => {
    const result = await session.executeBatch([
      ":t map",
      ":t foldr",
      "1 + 1"
    ]);
    
    expect(result.allSuccess).toBe(true);
    expect(result.results).toHaveLength(3);
  });
});
