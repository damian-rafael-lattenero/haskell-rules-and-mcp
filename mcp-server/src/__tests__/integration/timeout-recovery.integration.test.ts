import { describe, it, expect, beforeAll, beforeEach, afterAll, afterEach } from "vitest";
import { GhciSession } from "../../ghci-session.js";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

describe("Timeout Recovery Integration", () => {
  let session: GhciSession;
  let fixture: IsolatedFixture;

  beforeAll(async () => {
    fixture = await setupIsolatedFixture("test-project", "timeout-recovery");
  });

  afterAll(async () => {
    await fixture.cleanup();
  });

  beforeEach(async () => {
    session = new GhciSession(fixture.dir);
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
    
    // Session may still be alive but must be marked corrupted until restart.
    expect(session.getHealth().status).toBe("corrupted");
    
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
      session.executeBatch(["let loop = loop in loop"], { stopOnError: true, timeoutMs: 100 })
    ).rejects.toThrow("timed out");
    
    expect(session.getHealth().status).toBe("corrupted");
    
    // Restart
    await session.restart();
    
    // Normal batch should work
    const result = await session.executeBatch([":t map", ":t foldr"]);
    expect(result.allSuccess).toBe(true);
  });
});
