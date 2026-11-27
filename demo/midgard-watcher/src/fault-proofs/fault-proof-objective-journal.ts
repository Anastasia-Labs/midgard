import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

/** A proof objective: one installed category's proof against one queue
 * header. */
export type WatcherProofObjective = Readonly<{
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
}>;
