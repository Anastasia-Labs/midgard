import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type PhaseAValidatedTx,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";

/** Classify normal block candidates. A failure stays Phase B's own, so the
 * commit can tell a transaction's failure from the node's. A transaction
 * whose scripts pass `scriptWallCapMs` fails the evaluation, so the commit
 * evicts it like any other transaction it cannot process. */
export const evaluateNormalBlockCandidates = (input: {
  readonly candidates: readonly PhaseAValidatedTx[];
  readonly state: Map<string, Buffer>;
  readonly blockSlot: bigint;
  readonly l1SlotConfig: L1SlotConfig;
  readonly bucketConcurrency: number;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly scriptWallCapMs?: number;
}) =>
  runPhaseBValidationWithPatch(input.candidates, input.state, {
    nowCardanoSlotNo: input.blockSlot,
    l1SlotConfig: input.l1SlotConfig,
    bucketConcurrency: input.bucketConcurrency,
    enforceScriptBudget: true,
    maxScriptExecutionSteps:
      input.consensusProfile.limits.maxValidationMachineStepCount,
    scriptWallCapMs: input.scriptWallCapMs,
  });
