import { GhciSession } from "../ghci-session.js";
import { handleLoadModule } from "./load-module.js";

/**
 * @deprecated Use ghci_load with diagnostics=true instead.
 * This tool now delegates to the enhanced ghci_load.
 */
export async function handleDiagnostics(
  session: GhciSession,
  args: { module_path: string },
  projectDir?: string
): Promise<string> {
  return handleLoadModule(
    session,
    { module_path: args.module_path, diagnostics: true },
    projectDir
  );
}
