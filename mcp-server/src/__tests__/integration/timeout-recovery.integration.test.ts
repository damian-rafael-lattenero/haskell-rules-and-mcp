import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { GhciSession } from "../../ghci-session.js";
import path from "node:path";

const TEST_PROJECT = path.resolve(import.meta.dirname, "..", "fixtures", "test-project");

describe("Timeout Recovery Integration", () => {
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

  it("recovers from timeout and continues working", async () => {
    // Execute a normal command
    const result1 = await session.execute(":t map");
    expect(result1.success).toBe(true);
    
    // Cause a timeout
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    expect(session.getHealth().status).toBe("corrupted");
    
    // Wait for process to be killed
    await new Promise(r => setTimeout(r, 300));
    
    // Session should not be alive after timeout + kill
    expect(session.isAlive()).toBe(false);
    
    // Restart and verify recovery
    await session.restart();
    expect(session.getHealth().status).toBe("healthy");
    
    // Should work normally again
    const result2 = await session.execute(":t foldr");
    expect(result2.success).toBe(true);
  });

  it("handles multiple timeouts gracefully", async () => {
    // First timeout
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    await session.restart();
    
    // Second timeout
    await expect(
      session.execute("let loop = loop in loop", 100)
    ).rejects.toThrow("timed out");
    
    await session.restart();
    
    // Should still work
    const result = await session.execute(":t map");
    expect(result.success).toBe(true);
  });

  it("batch execution stops on dangerous commands", async () => {
    const commands = [
      ":t map",
      ":set +m",  // This should be rejected
      ":t foldr"
    ];
    
    await expect(
      session.executeBatch(commands)
    ).rejects.toThrow("Dangerous GHCi command");
    
    // Session should still be healthy (command was rejected before execution)
    expect(session.getHealth().status).toBe("healthy");
    
    // Should still be able to execute commands
    const result = await session.execute(":t map");
    expect(result.success).toBe(true);
  });

  it("recovers from timeout in batch execution", async () => {
    // Execute a batch with a timeout-causing command
    await expect(
      session.executeBatch(["let loop = loop in loop"], { stopOnError: true })
    ).rejects.toThrow("timed out");
    
    expect(session.getHealth().status).toBe("corrupted");
    
    // Restart
    await session.restart();
    
    // Normal batch should work
    const result = await session.executeBatch([":t map", ":t foldr"]);
    expect(result.allSuccess).toBe(true);
  });
});
