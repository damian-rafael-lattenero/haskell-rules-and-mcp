/**
 * ghci_property_lifecycle — Manage QuickCheck property lifecycle.
 * Provides tools to list, remove, deprecate, and replace properties in the store.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { type ToolContext, registerStrictTool } from "./registry.js";
import {
  getAllProperties,
  getModuleProperties,
  removeProperty,
  deprecateProperty,
  type PropertyRecord,
} from "../property-store.js";
import { validatePropertyText } from "../parsers/property-validator.js";

export async function handlePropertyLifecycle(
  projectDir: string,
  args: {
    action: "list" | "audit" | "remove" | "deprecate" | "replace";
    property?: string;
    module?: string;
    replaced_by?: string;
    reason?: string;
  }
): Promise<string> {
  if (args.action === "audit") {
    const properties = args.module
      ? await getModuleProperties(projectDir, args.module)
      : await getAllProperties(projectDir);
    const invalid = properties
      .map((prop) => ({
        property: prop.property,
        module: prop.tests_module ?? prop.module,
        issues: validatePropertyText(prop.property).issues,
      }))
      .filter((entry) => entry.issues.length > 0);
    return JSON.stringify({
      success: true,
      action: "audit",
      count: properties.length,
      invalidCount: invalid.length,
      invalid,
      _nextStep:
        invalid.length > 0
          ? "Deprecate/remove invalid properties with ghci_property_lifecycle and re-run ghci_quickcheck to store fixed versions."
          : "No invalid properties detected in the store.",
    });
  }

  if (args.action === "list") {
    const properties = args.module
      ? await getModuleProperties(projectDir, args.module)
      : await getAllProperties(projectDir);

    if (properties.length === 0) {
      return JSON.stringify({
        success: true,
        action: "list",
        count: 0,
        properties: [],
        message: args.module
          ? `No properties found for module ${args.module}`
          : "No properties found in the store",
      });
    }

    // Group by semantic module (tests_module or module)
    const grouped = new Map<string, PropertyRecord[]>();
    for (const prop of properties) {
      const key = prop.tests_module ?? prop.module;
      if (!grouped.has(key)) grouped.set(key, []);
      grouped.get(key)!.push(prop);
    }

    const summary = Array.from(grouped.entries()).map(([mod, props]) => ({
      module: mod,
      count: props.length,
      active: props.filter((p) => !p.deprecated).length,
      deprecated: props.filter((p) => p.deprecated).length,
      properties: props.map((p) => ({
        property: p.property,
        passCount: p.passCount,
        lastPassed: p.lastPassed,
        deprecated: p.deprecated ?? false,
        ...(p.deprecated ? { deprecation_reason: p.deprecation_reason } : {}),
        ...(p.replaced_by ? { replaced_by: p.replaced_by } : {}),
      })),
    }));

    return JSON.stringify({
      success: true,
      action: "list",
      count: properties.length,
      active: properties.filter((p) => !p.deprecated).length,
      deprecated: properties.filter((p) => p.deprecated).length,
      modules: summary,
    });
  }

  if (args.action === "remove") {
    if (!args.property) {
      return JSON.stringify({
        success: false,
        action: "remove",
        error: "property parameter is required for remove action",
      });
    }

    const removed = await removeProperty(projectDir, args.property);
    return JSON.stringify({
      success: removed,
      action: "remove",
      property: args.property,
      message: removed
        ? "Property removed from store"
        : "Property not found in store",
    });
  }

  if (args.action === "deprecate") {
    if (!args.property) {
      return JSON.stringify({
        success: false,
        action: "deprecate",
        error: "property parameter is required for deprecate action",
      });
    }

    const deprecated = await deprecateProperty(projectDir, args.property, {
      replaced_by: args.replaced_by,
      reason: args.reason,
    });

    return JSON.stringify({
      success: deprecated,
      action: "deprecate",
      property: args.property,
      ...(args.replaced_by ? { replaced_by: args.replaced_by } : {}),
      ...(args.reason ? { reason: args.reason } : {}),
      message: deprecated
        ? "Property marked as deprecated (will be filtered from exports)"
        : "Property not found in store",
    });
  }

  if (args.action === "replace") {
    if (!args.property || !args.replaced_by) {
      return JSON.stringify({
        success: false,
        action: "replace",
        error: "Both property and replaced_by parameters are required for replace action",
      });
    }

    // Deprecate the old property and link to the new one
    const deprecated = await deprecateProperty(projectDir, args.property, {
      replaced_by: args.replaced_by,
      reason: args.reason ?? "Replaced with improved version",
    });

    return JSON.stringify({
      success: deprecated,
      action: "replace",
      old_property: args.property,
      new_property: args.replaced_by,
      message: deprecated
        ? "Old property deprecated and linked to replacement"
        : "Old property not found in store",
      _nextStep: deprecated
        ? "The old property will be filtered from exports. The new property will be used in future runs."
        : "No action taken — property not found",
    });
  }

  return JSON.stringify({
    success: false,
    error: `Unknown action: ${args.action}`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_property_lifecycle",
    "Manage QuickCheck property lifecycle: list, remove, deprecate, or replace properties. " +
      "Use this to clean up obsolete properties, mark properties as deprecated, or link old properties to replacements.",
    {
      action: z.enum(["list", "audit", "remove", "deprecate", "replace"]).describe(
        "Action to perform: " +
          "'list' shows all properties (optionally filtered by module), " +
          "'audit' validates persisted properties and reports unsafe ones, " +
          "'remove' permanently deletes a property, " +
          "'deprecate' marks a property as deprecated (filters from exports), " +
          "'replace' deprecates old property and links to new one"
      ),
      property: z.string().optional().describe(
        "Property text to remove/deprecate/replace (exact match required)"
      ),
      module: z.string().optional().describe(
        "Filter by module path when action='list' (e.g. 'src/Expr/Eval.hs')"
      ),
      replaced_by: z.string().optional().describe(
        "New property text that replaces the old one (used with action='replace' or 'deprecate')"
      ),
      reason: z.string().optional().describe(
        "Reason for deprecation/replacement (optional, for documentation)"
      ),
    },
    async ({ action, property, module: mod, replaced_by, reason }) => {
      const result = await handlePropertyLifecycle(ctx.getProjectDir(), {
        action,
        property,
        module: mod,
        replaced_by,
        reason,
      });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
