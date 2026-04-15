/**
 * Auto-download system for bundled tools from GitHub Releases.
 * Downloads tools on-demand the first time they're needed.
 * Subsequent calls use the cached binary in vendor-tools/.
 */
import { mkdir, writeFile, chmod, access, readFile } from "node:fs/promises";
import { createWriteStream } from "node:fs";
import { createHash } from "node:crypto";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
const VENDOR_TOOLS_DIR = path.join(ROOT_DIR, "vendor-tools");

type SupportedTool = "hlint" | "fourmolu" | "ormolu" | "hls";
type SupportedPlatform = "darwin" | "linux" | "win32";
type SupportedArch = "x64" | "arm64";

interface ToolRelease {
  version: string;
  url: string;
  sha256: string;
  binaryName: string;
}

/**
 * GitHub Release URLs for bundled tools.
 * These point to releases in the same repository.
 * Format: https://github.com/OWNER/REPO/releases/download/TAG/FILE
 */
const GITHUB_RELEASES: Record<
  SupportedTool,
  Partial<Record<`${SupportedPlatform}-${SupportedArch}`, ToolRelease>>
> = {
  hlint: {
    "darwin-arm64": {
      version: "v3.10",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/hlint-darwin-arm64",
      sha256: "660d5288ca1a2c6220f9549a64f190f3df749cf01ea8349f8e8ef35ceb169d63",
      binaryName: "hlint",
    },
    "darwin-x64": {
      version: "v3.10",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/hlint-darwin-x64",
      sha256: "PENDING_CHECKSUM_DARWIN_X64",
      binaryName: "hlint",
    },
    "linux-x64": {
      version: "v3.10",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/hlint-linux-x64",
      sha256: "PENDING_CHECKSUM_LINUX_X64",
      binaryName: "hlint",
    },
    "linux-arm64": {
      version: "v3.10",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/hlint-linux-arm64",
      sha256: "PENDING_CHECKSUM_LINUX_ARM64",
      binaryName: "hlint",
    },
  },
  fourmolu: {
    "darwin-arm64": {
      version: "v0.19.0.1",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/fourmolu-darwin-arm64",
      sha256: "e8e793f2c361ad6e506fce46f4b89d46fce2af6647e753dc088fb005b650bb8c",
      binaryName: "fourmolu",
    },
    "darwin-x64": {
      version: "v0.19.0.1",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/fourmolu-darwin-x64",
      sha256: "PENDING_CHECKSUM_DARWIN_X64",
      binaryName: "fourmolu",
    },
    "linux-x64": {
      version: "v0.19.0.1",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/fourmolu-linux-x64",
      sha256: "PENDING_CHECKSUM_LINUX_X64",
      binaryName: "fourmolu",
    },
    "linux-arm64": {
      version: "v0.19.0.1",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/fourmolu-linux-arm64",
      sha256: "PENDING_CHECKSUM_LINUX_ARM64",
      binaryName: "fourmolu",
    },
  },
  ormolu: {
    "darwin-arm64": {
      version: "v0.7.7.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/ormolu-darwin-arm64",
      sha256: "d073199f566100cf57893d08b3df4f02c70ff7e650bf38d601e4fe9b3935b218",
      binaryName: "ormolu",
    },
    "darwin-x64": {
      version: "v0.7.7.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/ormolu-darwin-x64",
      sha256: "PENDING_CHECKSUM_DARWIN_X64",
      binaryName: "ormolu",
    },
    "linux-x64": {
      version: "v0.7.7.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/ormolu-linux-x64",
      sha256: "PENDING_CHECKSUM_LINUX_X64",
      binaryName: "ormolu",
    },
    "linux-arm64": {
      version: "v0.7.7.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/ormolu-linux-arm64",
      sha256: "PENDING_CHECKSUM_LINUX_ARM64",
      binaryName: "ormolu",
    },
  },
  hls: {
    "darwin-arm64": {
      version: "v2.13.0.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/haskell-language-server-wrapper-darwin-arm64",
      sha256: "949cb139b269a4487c82adfb17ff326f3defa9fa9fee6342297895e9ef2647c8",
      binaryName: "haskell-language-server-wrapper",
    },
    "darwin-x64": {
      version: "v2.13.0.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/haskell-language-server-wrapper-darwin-x64",
      sha256: "PENDING_CHECKSUM_DARWIN_X64",
      binaryName: "haskell-language-server-wrapper",
    },
    "linux-x64": {
      version: "v2.13.0.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/haskell-language-server-wrapper-linux-x64",
      sha256: "PENDING_CHECKSUM_LINUX_X64",
      binaryName: "haskell-language-server-wrapper",
    },
    "linux-arm64": {
      version: "v2.13.0.0",
      url: "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/tools-v1.0/haskell-language-server-wrapper-linux-arm64",
      sha256: "PENDING_CHECKSUM_LINUX_ARM64",
      binaryName: "haskell-language-server-wrapper",
    },
  },
};

async function computeSHA256(filePath: string): Promise<string> {
  const content = await readFile(filePath);
  return createHash("sha256").update(content).digest("hex");
}

async function downloadFile(url: string, destPath: string): Promise<void> {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Download failed: ${response.status} ${response.statusText}`);
  }
  if (!response.body) {
    throw new Error("Response body is null");
  }

  await mkdir(path.dirname(destPath), { recursive: true });
  const fileStream = createWriteStream(destPath);
  
  // Write response body to file
  const reader = response.body.getReader();
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    fileStream.write(value);
  }
  
  return new Promise<void>((resolve, reject) => {
    fileStream.end(() => resolve());
    fileStream.on('error', reject);
  });
}

export interface AutoDownloadResult {
  success: boolean;
  binaryPath?: string;
  version?: string;
  downloaded?: boolean;
  cached?: boolean;
  error?: string;
  message: string;
}

/**
 * Auto-download a tool if not already present.
 * Returns the path to the binary (either cached or freshly downloaded).
 */
export async function autoDownloadTool(tool: SupportedTool): Promise<AutoDownloadResult> {
  const platform = process.platform as SupportedPlatform;
  const arch = process.arch as SupportedArch;
  const target = `${platform}-${arch}` as const;

  // Check if we have a release for this platform
  const release = GITHUB_RELEASES[tool]?.[target];
  if (!release) {
    return {
      success: false,
      error: `No release available for ${tool} on ${platform}-${arch}`,
      message: `${tool} is not available for your platform (${platform}-${arch}). Please install manually.`,
    };
  }

  // Check if already downloaded
  const toolDir = path.join(VENDOR_TOOLS_DIR, tool, target);
  const binaryPath = path.join(toolDir, release.binaryName);

  try {
    await access(binaryPath);
    // Binary exists - verify it's executable
    try {
      await chmod(binaryPath, 0o755);
    } catch {
      // Already executable or can't change permissions
    }
    
    return {
      success: true,
      binaryPath,
      version: release.version,
      cached: true,
      message: `Using cached ${tool} ${release.version}`,
    };
  } catch {
    // Binary doesn't exist - download it
  }

  // Download the binary
  try {
    await mkdir(toolDir, { recursive: true });
    
    const tempPath = `${binaryPath}.download`;
    await downloadFile(release.url, tempPath);
    
    // Make executable
    await chmod(tempPath, 0o755);
    
    // Verify checksum if provided
    if (release.sha256) {
      const actualSHA = await computeSHA256(tempPath);
      if (actualSHA !== release.sha256) {
        throw new Error(`Checksum mismatch: expected ${release.sha256}, got ${actualSHA}`);
      }
    }
    
    // Move to final location
    await writeFile(binaryPath, await readFile(tempPath));
    await chmod(binaryPath, 0o755);
    
    return {
      success: true,
      binaryPath,
      version: release.version,
      downloaded: true,
      message: `Downloaded ${tool} ${release.version} (first time setup)`,
    };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
      message: `Failed to download ${tool}: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
}

/**
 * Check if a tool can be auto-downloaded for the current platform.
 */
export function canAutoDownload(tool: SupportedTool): boolean {
  const platform = process.platform as SupportedPlatform;
  const arch = process.arch as SupportedArch;
  const target = `${platform}-${arch}` as const;
  return GITHUB_RELEASES[tool]?.[target] !== undefined;
}
