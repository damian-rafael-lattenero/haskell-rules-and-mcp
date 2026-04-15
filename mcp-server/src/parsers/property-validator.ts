export interface PropertyValidationIssue {
  code:
    | "empty"
    | "ghci-command"
    | "too-long"
    | "unused-binder"
    | "invalid-lambda";
  message: string;
}

export interface PropertyValidationResult {
  ok: boolean;
  issues: PropertyValidationIssue[];
}

const MAX_PROPERTY_LENGTH = 2000;

function readTopLevelArrowIndex(source: string): number {
  let depth = 0;
  for (let i = 0; i < source.length - 1; i++) {
    const c = source[i];
    if (c === "(" || c === "[" || c === "{") depth++;
    else if (c === ")" || c === "]" || c === "}") depth = Math.max(0, depth - 1);
    else if (c === "-" && source[i + 1] === ">" && depth === 0) return i;
  }
  return -1;
}

function parseLambdaBinders(lambdaExpr: string): { binders: string[]; body: string } | null {
  const trimmed = lambdaExpr.trim();
  if (!trimmed.startsWith("\\")) return null;
  const arrowIndex = readTopLevelArrowIndex(trimmed);
  if (arrowIndex === -1) return null;
  const binderChunk = trimmed.slice(1, arrowIndex).trim();
  const body = trimmed.slice(arrowIndex + 2).trim();
  if (!binderChunk || !body) return null;

  const binders: string[] = [];
  const tokens = binderChunk.match(/\([^)]*\)|[^\s]+/g) ?? [];
  for (const token of tokens) {
    const t = token.trim();
    if (!t) continue;
    if (t === "_") continue;
    if (t.startsWith("(") && t.endsWith(")")) {
      const inner = t.slice(1, -1).trim();
      const m = inner.match(/^([A-Za-z_][A-Za-z0-9_']*)\s*::/);
      if (m?.[1]) {
        binders.push(m[1]);
      }
      continue;
    }
    if (/^[A-Za-z_][A-Za-z0-9_']*$/.test(t)) {
      binders.push(t);
    }
  }
  return { binders, body };
}

function hasWord(body: string, name: string): boolean {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(`\\b${escaped}\\b`);
  return re.test(body);
}

export function validatePropertyText(property: string): PropertyValidationResult {
  const issues: PropertyValidationIssue[] = [];
  const trimmed = property.trim();

  if (!trimmed) {
    issues.push({ code: "empty", message: "Property cannot be empty." });
    return { ok: false, issues };
  }
  if (trimmed.startsWith(":")) {
    issues.push({
      code: "ghci-command",
      message: "Property cannot start with ':' (looks like a GHCi command).",
    });
  }
  if (trimmed.length > MAX_PROPERTY_LENGTH) {
    issues.push({
      code: "too-long",
      message: `Property too long (max ${MAX_PROPERTY_LENGTH} characters).`,
    });
  }

  if (trimmed.startsWith("\\")) {
    const parsed = parseLambdaBinders(trimmed);
    if (!parsed) {
      issues.push({
        code: "invalid-lambda",
        message: "Lambda property has invalid syntax (expected '\\\\args -> body').",
      });
    } else {
      for (const binder of parsed.binders) {
        if (binder.startsWith("_")) continue;
        if (!hasWord(parsed.body, binder)) {
          issues.push({
            code: "unused-binder",
            message: `Lambda binder '${binder}' is unused. Use '_' or remove it to avoid ambiguous exported tests.`,
          });
        }
      }
    }
  }

  return { ok: issues.length === 0, issues };
}
