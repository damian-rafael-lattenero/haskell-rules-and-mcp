/**
 * Validate that every release URL in the manifest resolves, the bundled files
 * land the way the host expects, and the two views agree.
 *
 * Modes:
 *   (default)       — legacy bundled-tools local check (unchanged behavior)
 *   --check-urls    — HEAD each URL in manifest.releases
 *                     - status 200
 *                     - content-length > 0
 *                     - content-type not text/html (catches 404 pages served 200)
 *   --include-hls   — include hls in legacy bundled check
 *
 * Combined:        --check-urls --include-hls
 */
import {
  enumerateConfiguredReleases,
  resetManifestCache,
  type PlatformTarget,
  type SupportedTool,
} from "../vendor-tools/manifest.js";
import { getBundledToolStatus, resetBundledManifestCache } from "../tools/tool-installer.js";

type ValidatedTool = "hlint" | "fourmolu" | "ormolu" | "hls";

function getValidatedTools(): ValidatedTool[] {
  return process.argv.includes("--include-hls")
    ? ["hlint", "fourmolu", "ormolu", "hls"]
    : ["hlint", "fourmolu", "ormolu"];
}

interface UrlCheckResult {
  tool: SupportedTool;
  target: PlatformTarget;
  url: string;
  ok: boolean;
  status?: number;
  contentLength?: number;
  contentType?: string;
  error?: string;
}

async function headCheck(url: string): Promise<Omit<UrlCheckResult, "tool" | "target" | "url">> {
  try {
    const res = await fetch(url, { method: "HEAD", redirect: "follow" });
    const contentLength = Number(res.headers.get("content-length") ?? "0");
    const contentType = res.headers.get("content-type") ?? "";
    const looksLikeHtml404 =
      res.status === 200 && /text\/html/i.test(contentType);
    const ok =
      res.status === 200 && contentLength > 0 && !looksLikeHtml404;
    return { ok, status: res.status, contentLength, contentType };
  } catch (err) {
    return { ok: false, error: err instanceof Error ? err.message : String(err) };
  }
}

async function checkUrls(): Promise<number> {
  resetManifestCache();
  const entries = await enumerateConfiguredReleases();
  const results: UrlCheckResult[] = [];

  for (const { tool, target, entry } of entries) {
    const check = await headCheck(entry.url);
    results.push({ tool, target, url: entry.url, ...check });
  }

  const failures = results.filter((r) => !r.ok);
  const passes = results.filter((r) => r.ok);

  for (const r of passes) {
    process.stdout.write(
      `OK  ${r.tool} ${r.target} status=${r.status} size=${r.contentLength}\n`
    );
  }
  for (const r of failures) {
    process.stderr.write(
      `FAIL ${r.tool} ${r.target} status=${r.status ?? "?"} size=${r.contentLength ?? "?"} ct=${r.contentType ?? "?"} error=${r.error ?? ""} url=${r.url}\n`
    );
  }

  process.stderr.write(
    `\nSummary: ${passes.length} ok, ${failures.length} failed (of ${results.length})\n`
  );

  return failures.length === 0 ? 0 : 1;
}

async function checkBundledLegacy(): Promise<number> {
  resetBundledManifestCache();
  const failures: string[] = [];

  for (const tool of getValidatedTools()) {
    const status = await getBundledToolStatus(tool);
    if (!status.available) {
      failures.push(`${tool}: ${status.message}`);
      continue;
    }
    process.stdout.write(
      `${tool}: available source=${status.source} version=${status.version ?? "unknown"} binary=${status.binaryPath ?? "n/a"}\n`
    );
  }

  if (failures.length > 0) {
    process.stderr.write(`Bundled tool validation failed:\n- ${failures.join("\n- ")}\n`);
    return 1;
  }
  return 0;
}

async function main(): Promise<void> {
  const mode = process.argv.includes("--check-urls") ? "urls" : "bundled";
  const code =
    mode === "urls" ? await checkUrls() : await checkBundledLegacy();
  process.exit(code);
}

main().catch((err) => {
  process.stderr.write(`validate-bundled-tools failed: ${String(err)}\n`);
  process.exit(1);
});
