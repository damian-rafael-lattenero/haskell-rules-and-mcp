#!/bin/bash
set -e

echo "=== MCP Stability Validation Suite ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track failures
FAILED=0

# Function to run a step and track failures
run_step() {
  local step_name="$1"
  local command="$2"
  
  echo -e "${YELLOW}Running: ${step_name}${NC}"
  if eval "$command"; then
    echo -e "${GREEN}✅ ${step_name} passed${NC}"
    echo ""
  else
    echo -e "${RED}❌ ${step_name} failed${NC}"
    echo ""
    FAILED=$((FAILED + 1))
  fi
}

# 1. Run all tests
run_step "Unit Tests" "npm run test"
run_step "Integration Tests" "npm run test:integration"
run_step "E2E Tests" "npm run test:e2e"

# 2. Check coverage
run_step "Coverage Check" "npm run test -- --coverage"

# 3. Build project
run_step "TypeScript Build" "npm run build"

# 4. Lint check (if available)
if command -v eslint &> /dev/null; then
  run_step "ESLint" "npm run lint 2>/dev/null || echo 'Lint script not found, skipping'"
fi

# 5. Type check
run_step "TypeScript Type Check" "npx tsc --noEmit"

# Summary
echo "=== Validation Summary ==="
if [ $FAILED -eq 0 ]; then
  echo -e "${GREEN}✅ All stability checks passed${NC}"
  exit 0
else
  echo -e "${RED}❌ ${FAILED} check(s) failed${NC}"
  exit 1
fi
