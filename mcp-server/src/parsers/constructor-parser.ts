/**
 * Parse data type constructors from GHCi :i output.
 */

export interface Constructor {
  name: string;
  fields: string[]; // field type names
}

/**
 * Parse constructors from a data type definition string.
 *
 * Input examples:
 *   "data Lit = LInt Integer | LBool Bool"
 *   "data Expr = Var Name | App Expr Expr | Lam Name Expr | Lit Lit | If Expr Expr Expr"
 *   "type Expr :: *\ndata Expr = Var Name | App Expr Expr ..."
 *   "newtype Identity a = Identity a"
 *   "data Rec = Rec { field1 :: Int, field2 :: String }"
 */
export function parseConstructors(definition: string): Constructor[] {
  // Join multiline into a single string
  const joined = definition.replace(/\n/g, " ").replace(/\s+/g, " ").trim();

  // Strip "-- Defined at ..." or "-- Defined in ..." suffixes
  const stripped = joined.replace(/\s*--\s+Defined\s+(at|in)\s+.*/g, "");

  // Strip instance lines that may have been joined in
  const withoutInstances = stripped.replace(/\s*instance\s+.*/g, "");

  // Find the "data Foo ... =" or "newtype Foo ... =" part
  // We look for the last occurrence of "data" or "newtype" keyword followed by "="
  const dataMatch = withoutInstances.match(
    /(?:data|newtype)\s+(?:[^=]*?)\s*=\s*(.*)/
  );
  if (!dataMatch) return [];

  const rhsSide = dataMatch[1]!.trim();

  // Bail out on GADTs / existential types (they use "where" or "forall" in constructors)
  if (/\bwhere\b/.test(rhsSide) || /\bforall\b/.test(rhsSide)) return [];

  // Split by top-level "|" — but not inside parentheses or braces
  const alternatives = splitByPipe(rhsSide);

  const constructors: Constructor[] = [];

  for (const alt of alternatives) {
    const trimmed = alt.trim();
    if (!trimmed) continue;

    // Check for record syntax: "Foo { bar :: Int, baz :: String }"
    const recordMatch = trimmed.match(/^(\S+)\s*\{(.*)\}\s*$/);
    if (recordMatch) {
      const name = recordMatch[1]!;
      const fieldsStr = recordMatch[2]!;
      const fields = parseRecordFields(fieldsStr);
      constructors.push({ name, fields });
      continue;
    }

    // Normal constructor: first token is name, rest are field types
    const tokens = tokenizeConstructor(trimmed);
    if (tokens.length === 0) continue;

    const name = tokens[0]!;
    const fields = tokens.slice(1);
    constructors.push({ name, fields });
  }

  return constructors;
}

/**
 * Split a string by top-level "|" characters, respecting parentheses, brackets, and braces.
 */
function splitByPipe(s: string): string[] {
  const parts: string[] = [];
  let current = "";
  let depth = 0;

  for (const ch of s) {
    if (ch === "(" || ch === "[" || ch === "{") {
      depth++;
      current += ch;
    } else if (ch === ")" || ch === "]" || ch === "}") {
      depth = Math.max(0, depth - 1);
      current += ch;
    } else if (ch === "|" && depth === 0) {
      parts.push(current);
      current = "";
    } else {
      current += ch;
    }
  }

  if (current.trim()) parts.push(current);
  return parts;
}

/**
 * Tokenize a constructor into its name and field types.
 * Handles parenthesized types like "(a -> b)" and bracketed types like "[a]".
 */
function tokenizeConstructor(s: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  const str = s.trim();

  while (i < str.length) {
    // Skip whitespace
    while (i < str.length && /\s/.test(str[i]!)) i++;
    if (i >= str.length) break;

    const ch = str[i]!;

    if (ch === "(" || ch === "[") {
      // Consume matching balanced expression
      const close = ch === "(" ? ")" : "]";
      let depth = 1;
      let token = ch;
      i++;
      while (i < str.length && depth > 0) {
        if (str[i] === ch) depth++;
        else if (str[i] === close) depth--;
        token += str[i];
        i++;
      }
      tokens.push(token);
    } else if (ch === "!") {
      // Strict annotation — skip it and attach to next token
      i++;
    } else {
      // Regular token (type name or constructor name)
      let token = "";
      while (i < str.length && !/\s/.test(str[i]!)) {
        token += str[i];
        i++;
      }
      tokens.push(token);
    }
  }

  return tokens;
}

/**
 * Parse record fields from inside braces.
 * Input: "bar :: Int, baz :: String"
 * Output: ["Int", "String"]
 */
function parseRecordFields(fieldsStr: string): string[] {
  const fields: string[] = [];
  // Split by comma at top level
  const parts = fieldsStr.split(",");

  for (const part of parts) {
    const trimmed = part.trim();
    // Match "fieldName :: Type"
    const match = trimmed.match(/::\s*(.+)/);
    if (match) {
      // The type might be complex; take the whole thing but trim
      const typeStr = match[1]!.trim();
      fields.push(typeStr);
    }
  }

  return fields;
}
