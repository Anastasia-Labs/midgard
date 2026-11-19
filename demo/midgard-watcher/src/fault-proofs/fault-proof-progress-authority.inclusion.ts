import { isCollateralForfeitureEvent } from "@al-ft/midgard-fault-proofs/workflow/collateral-forfeiture";
import type { FraudProofWorkflowJournalEntry } from "@al-ft/midgard-fault-proofs/workflow/journal";
import { isFinal } from "@al-ft/midgard-l1-follower";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";

/**
 * Whether an objective's last outcome is an included correction whose block
 * still stands on the follower's chain short of completion depth. Its
 * workflow records `completed` only beyond k + 1 (automaticRecoveryMaxDepth
 * + 1). Only a rollback that takes that block off the chain, or the block
 * reaching completion depth, changes what a run would find, so until then
 * no run is queued for it.
 */
export const inclusionStands = async (
  retention: WatcherProofRetention | undefined,
  entries: readonly FraudProofWorkflowJournalEntry[],
): Promise<boolean> => {
  // Collateral bookkeeping for an attempt read after the inclusion moves
  // no step.
  const last = [...entries]
    .reverse()
    .find(({ event }) => !isCollateralForfeitureEvent(event))?.event;
  if (retention === undefined || last?.kind !== "terminal_included")
    return false;
  const depth = await retention.blockDepth(last.terminal.observedAt);
  return (
    depth !== null &&
    !isFinal(depth, { securityParameter: retention.securityParameter + 1 })
  );
};
