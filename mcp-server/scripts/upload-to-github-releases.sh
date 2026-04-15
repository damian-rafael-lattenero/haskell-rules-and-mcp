#!/usr/bin/env bash
set -euo pipefail

# Script to upload bundled tools to GitHub Releases
# Usage: ./upload-to-github-releases.sh <tag>
# Example: ./upload-to-github-releases.sh tools-v1.0

TAG="${1:-tools-v1.0}"
REPO="damian-rafael-lattenero/haskell-rules-and-mcp"

echo "📦 Uploading bundled tools to GitHub Release: $TAG"
echo "Repository: $REPO"
echo ""

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
    echo "❌ GitHub CLI (gh) is not installed"
    echo "Install with: brew install gh"
    exit 1
fi

# Check if authenticated
if ! gh auth status &> /dev/null; then
    echo "❌ Not authenticated with GitHub"
    echo "Run: gh auth login"
    exit 1
fi

# Create release if it doesn't exist
echo "Creating release $TAG (if it doesn't exist)..."
gh release create "$TAG" \
    --repo "$REPO" \
    --title "Bundled Tools $TAG" \
    --notes "Pre-compiled binaries for hlint, fourmolu, ormolu, and hls. Downloaded automatically on first use." \
    || echo "Release already exists, continuing..."

echo ""
echo "Uploading binaries..."

# Function to upload a binary
upload_binary() {
    local tool=$1
    local platform=$2
    local arch=$3
    local binary_name=$4
    
    local source_path="vendor-tools/$tool/$platform-$arch/$binary_name"
    local upload_name="${tool}-${platform}-${arch}"
    
    if [ ! -f "$source_path" ]; then
        echo "⚠️  Skipping $upload_name (not found at $source_path)"
        return
    fi
    
    # Check if it's a real binary (not a shim script)
    if file "$source_path" | grep -q "shell script"; then
        echo "⚠️  Skipping $upload_name (is a shim script, not a real binary)"
        return
    fi
    
    local size=$(du -h "$source_path" | cut -f1)
    echo "📤 Uploading $upload_name ($size)..."
    
    gh release upload "$TAG" \
        "$source_path#$upload_name" \
        --repo "$REPO" \
        --clobber
    
    echo "✅ Uploaded $upload_name"
}

# Upload hlint
upload_binary "hlint" "darwin" "arm64" "hlint"
upload_binary "hlint" "darwin" "x64" "hlint"
upload_binary "hlint" "linux" "arm64" "hlint"
upload_binary "hlint" "linux" "x64" "hlint"

# Upload fourmolu
upload_binary "fourmolu" "darwin" "arm64" "fourmolu"
upload_binary "fourmolu" "darwin" "x64" "fourmolu"
upload_binary "fourmolu" "linux" "arm64" "fourmolu"
upload_binary "fourmolu" "linux" "x64" "fourmolu"

# Upload ormolu
upload_binary "ormolu" "darwin" "arm64" "ormolu"
upload_binary "ormolu" "darwin" "x64" "ormolu"
upload_binary "ormolu" "linux" "arm64" "ormolu"
upload_binary "ormolu" "linux" "x64" "ormolu"

# Upload hls
upload_binary "hls" "darwin" "arm64" "haskell-language-server-wrapper"
upload_binary "hls" "darwin" "x64" "haskell-language-server-wrapper"
upload_binary "hls" "linux" "arm64" "haskell-language-server-wrapper"
upload_binary "hls" "linux" "x64" "haskell-language-server-wrapper"

echo ""
echo "✅ All binaries uploaded successfully!"
echo "View release: https://github.com/$REPO/releases/tag/$TAG"
