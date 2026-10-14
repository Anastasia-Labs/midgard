import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type PhaseAValidatedTx,
  RejectCodes,
  runPhaseBValidationWithPatch,
  ScriptWallCapExceededError,
} from "@al-ft/midgard-validation";
import { Effect, Metric } from "effect";

import { COMMIT_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET } from "./commit-rejection.js";

export const validationCommitWallCapDeferralCounter = Metric.counter(
  "validation_commit_wall_cap_deferral_count",
  {
    description:
      "Total number of txs left pending at commit because their script runs passed the commit wall cap",
    bigint: true,
    incremental: true,
  },
);

/** Classify normal block candidates. A failure stays Phase B's own, so the
 * commit can tell a transaction's failure from the node's.
 *
 * A transaction whose scripts pass `scriptWallCapMs` is left out of this
 * block, with every candidate that spends or references its outputs, and is
 * neither accepted nor rejected: the cap measures this node's load, not the
 * transaction, so it stays pending for a later block. One whose scripts
 * reach an arm outside the gated set is rejected, and its dependents with
 * it: that refusal does not depend on load. Either way the rest is decided
 * in the same Phase B run. */
export const evaluateNormalBlockCandidates = (input: {
  readonly candidates: readonly PhaseAValidatedTx[];
  readonly state: Map<string, Buffer>;
  readonly blockSlot: bigint;
  readonly l1SlotConfig: L1SlotConfig;
  readonly bucketConcurrency: number;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly scriptWallCapMs?: number;
}) =>
  Effect.gen(function* () {
    const result = yield* runPhaseBValidationWithPatch(
      input.candidates,
      input.state,
      {
        nowCardanoSlotNo: input.blockSlot,
        l1SlotConfig: input.l1SlotConfig,
        bucketConcurrency: input.bucketConcurrency,
        enforceScriptBudget: true,
        maxScriptExecutionSteps:
          input.consensusProfile.limits.maxValidationMachineStepCount,
        scriptWallCapMs: input.scriptWallCapMs,
        reportLocalRefusals: true,
      },
    );
    const rejected: {
      readonly txId: Buffer;
      readonly code: string;
      readonly detail: string | null;
    }[] = [...result.rejected];
    for (const { refusal, candidate, dependents } of result.localRefusals ??
      []) {
      const txIdHex = candidate.ledgerTx.txId.toString("hex");
      if (refusal instanceof ScriptWallCapExceededError) {
        yield* Metric.increment(validationCommitWallCapDeferralCounter);
        yield* Effect.logWarning(
          `Leaving ${(dependents.length + 1).toString()} transaction(s) from tx ${txIdHex} pending for a later block: ${refusal.message}`,
        );
        continue;
      }
      rejected.push({
        txId: candidate.ledgerTx.txId,
        code: COMMIT_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET,
        detail: refusal.message,
      });
      for (const dependent of dependents)
        rejected.push({
          txId: dependent.ledgerTx.txId,
          code: RejectCodes.DependsOnRejectedTx,
          detail: `depends on rejected tx ${txIdHex}`,
        });
    }
    return { ...result, rejected };
  });
