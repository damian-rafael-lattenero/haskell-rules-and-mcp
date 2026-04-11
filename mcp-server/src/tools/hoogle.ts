export const hoogleSearchTool = {
  name: "hoogle_search",
  description:
    "Search Hoogle for Haskell functions by name or type signature. " +
    "Hoogle is Haskell's type-aware search engine. " +
    'You can search by type like "(a -> b) -> [a] -> [b]" to find "map", ' +
    'or by name like "mapM" to find its type and module.',
  inputSchema: {
    type: "object" as const,
    properties: {
      query: {
        type: "string",
        description:
          'Search query. Can be a function name ("mapM"), a type signature ("(a -> b) -> [a] -> [b]"), or a module-qualified name ("Data.Map.lookup").',
      },
      count: {
        type: "number",
        description: "Number of results to return. Default: 10, max: 30.",
      },
    },
    required: ["query"],
  },
};

interface HoogleResult {
  url: string;
  module: { name: string; url: string };
  package: { name: string; url: string };
  item: string;
  type: string;
  docs: string;
}

export async function handleHoogleSearch(args: {
  query: string;
  count?: number;
}): Promise<string> {
  const count = Math.min(args.count ?? 10, 30);
  const url = `https://hoogle.haskell.org/?mode=json&hoogle=${encodeURIComponent(args.query)}&start=1&count=${count}`;

  try {
    const response = await fetch(url);
    if (!response.ok) {
      return JSON.stringify({
        success: false,
        error: `Hoogle returned HTTP ${response.status}`,
      });
    }

    const results: HoogleResult[] = (await response.json()) as HoogleResult[];

    const formatted = results.map((r) => ({
      name: stripHtml(r.item),
      module: r.module?.name,
      package: r.package?.name,
      docs: stripHtml(r.docs).substring(0, 200),
      url: r.url,
    }));

    return JSON.stringify({
      success: true,
      query: args.query,
      count: formatted.length,
      results: formatted,
    });
  } catch (err) {
    return JSON.stringify({
      success: false,
      error: `Hoogle request failed: ${err instanceof Error ? err.message : String(err)}`,
    });
  }
}

function stripHtml(html: string): string {
  return html
    .replace(/<[^>]*>/g, "")
    .replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .trim();
}
