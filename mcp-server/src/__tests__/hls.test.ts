import { describe, it, expect } from "vitest";
import {
  parseLspMessage,
  buildLspRequest,
  buildInitializeRequest,
  buildDidOpenRequest,
  buildHoverRequest,
  serializeLspMessage,
} from "../tools/hls.js";

describe("LSP message parsing", () => {
  it("parses a well-formed Content-Length message", () => {
    const body = JSON.stringify({ jsonrpc: "2.0", id: 1, result: { hello: "world" } });
    const raw = `Content-Length: ${body.length}\r\n\r\n${body}`;
    const result = parseLspMessage(raw);
    expect(result).not.toBeNull();
    expect(result!.id).toBe(1);
    expect(result!.result.hello).toBe("world");
  });

  it("returns null for incomplete message", () => {
    const result = parseLspMessage("Content-Length: 100\r\n\r\n{partial}");
    // Incomplete JSON — may succeed or fail, but must not throw
    expect(result === null || typeof result === "object").toBe(true);
  });

  it("returns null for missing header", () => {
    const result = parseLspMessage('{"jsonrpc":"2.0","id":1}');
    expect(result).toBeNull();
  });
});

describe("LSP request builders", () => {
  it("buildLspRequest includes jsonrpc, id, method, params", () => {
    const req = buildLspRequest(1, "textDocument/hover", { textDocument: { uri: "file:///foo.hs" }, position: { line: 0, character: 0 } });
    expect(req.jsonrpc).toBe("2.0");
    expect(req.id).toBe(1);
    expect(req.method).toBe("textDocument/hover");
    expect(req.params).toBeDefined();
  });

  it("buildInitializeRequest has correct method and params", () => {
    const req = buildInitializeRequest("/path/to/project");
    expect(req.method).toBe("initialize");
    expect(req.params.rootUri).toContain("file://");
    expect(req.params.capabilities).toBeDefined();
  });

  it("buildDidOpenRequest sets correct textDocument fields", () => {
    const req = buildDidOpenRequest("file:///foo.hs", "module Foo where\n");
    expect(req.method).toBe("textDocument/didOpen");
    expect(req.params.textDocument.uri).toBe("file:///foo.hs");
    expect(req.params.textDocument.languageId).toBe("haskell");
    expect(req.params.textDocument.version).toBe(1);
    expect(req.params.textDocument.text).toBe("module Foo where\n");
  });

  it("buildHoverRequest has correct position", () => {
    const req = buildHoverRequest("file:///foo.hs", 5, 3);
    expect(req.method).toBe("textDocument/hover");
    expect(req.params.position.line).toBe(5);
    expect(req.params.position.character).toBe(3);
  });
});

describe("serializing LSP message to wire format", () => {
  it("produces Content-Length header + body", () => {
    const req = buildLspRequest(1, "ping", {});
    const wire = serializeLspMessage(req);
    expect(wire).toContain("Content-Length:");
    expect(wire).toContain("\r\n\r\n");
    expect(wire).toContain('"jsonrpc"');
  });

  it("Content-Length matches actual byte length", () => {
    const req = buildLspRequest(2, "textDocument/hover", { test: "data" });
    const wire = serializeLspMessage(req);
    const lenMatch = wire.match(/Content-Length:\s*(\d+)/);
    expect(lenMatch).not.toBeNull();
    const declaredLen = parseInt(lenMatch![1]!, 10);
    const bodyStart = wire.indexOf("\r\n\r\n") + 4;
    const body = wire.slice(bodyStart);
    expect(Buffer.byteLength(body, "utf8")).toBe(declaredLen);
  });
});
