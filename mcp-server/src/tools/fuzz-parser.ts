import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";

const DEFAULT_FUZZ_CASES = [
  "",
  " ",
  "(",
  ")",
  "()",
  "((",
  "))",
  "[]",
  "{}",
  "\"",
  "\"unterminated",
  "'",
  "\\",
  "+++",
  "abc",
  "123",
  "-123",
  "1 + 2",
  "(1 + 2",
  "1 + 2)",
  "\n",
  "\t",
  "\0",
];

export function escapeHaskellString(value: string): string {
  return JSON.stringify(value).slice(1, -1)
    .replace(/\0/g, "\\0")
    .replace(/\\u0000/g, "\\0");
}

export function buildFuzzCorpus(userInputs: string[] = [], generatedCases = 8): string[] {
  const corpus = new Set<string>([...DEFAULT_FUZZ_CASES, ...userInputs]);
  const seeds = ["(", ")", "[", "]", "{", "}", "\"", "'", "+", "-", "*", "/", ","];
  for (let i = 0; i < generatedCases; i++) {
    const token = seeds[i % seeds.length]!;
    corpus.add(token.repeat((i % 5) + 1));
    corpus.add(`${token}${i}${token}`);
    corpus.add(`prefix${token.repeat((i % 3) + 1)}suffix`);
  }
  return [...corpus];
}

async function evaluateParserNoCrash(
  session: GhciSession,
  parserExpr: string,
  input: string
): Promise<{ crashed: boolean; raw: string }> {
  await session.execute("import qualified Control.Exception as E");
  const escaped = escapeHaskellString(input);
  const expr =
    `(E.try (E.evaluate (((${parserExpr}) "${escaped}") \`seq\` ())) ` +
    ":: IO (Either E.SomeException ()))";
  const result = await session.execute(expr);
  const output = result.output.trim();
  return {
    crashed: output.includes("Left ") || output.includes("*** Exception:"),
    raw: output,
  };
}

export async function handleFuzzParser(
  session: GhciSession,
  args: { parser: string; inputs?: string[]; generated_cases?: number }
): Promise<string> {
  const corpus = buildFuzzCorpus(args.inputs, args.generated_cases ?? 8);
  const crashes: Array<{ input: string; output: string }> = [];

  for (const input of corpus) {
    const result = await evaluateParserNoCrash(session, args.parser, input);
    if (result.crashed) {
      crashes.push({ input, output: result.raw });
    }
  }

  return JSON.stringify({
    success: crashes.length === 0,
    parser: args.parser,
    totalCases: corpus.length,
    crashes,
    summary:
      crashes.length === 0
        ? `No crashes across ${corpus.length} malformed input(s)`
        : `${crashes.length} crash(es) detected across ${corpus.length} malformed input(s)`,
    _nextStep:
      crashes.length === 0
        ? "Parser survived malformed inputs at WHNF. Add targeted roundtrip/properties if you need deeper validation."
        : "Parser crashed on malformed input. Reproduce one failing case with ghci_eval or ghci_trace and harden the parser.",
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_fuzz_parser",
    "Smoke-test a parser against malformed inputs and report whether evaluation crashes. " +
      "Useful for parser robustness checks and no-crash guarantees.",
    {
      parser: z.string().describe(
        'Parser expression to test. Examples: "parseExpr", "(\\\\s -> read s :: Int)"'
      ),
      inputs: z.array(z.string()).optional().describe(
        "Optional additional malformed inputs to add to the built-in corpus."
      ),
      generated_cases: z.number().int().positive().optional().describe(
        "How many extra deterministic malformed cases to synthesize. Default: 8."
      ),
    },
    async ({ parser, inputs, generated_cases }) => {
      const session = await ctx.getSession();
      const result = await handleFuzzParser(session, { parser, inputs, generated_cases });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
