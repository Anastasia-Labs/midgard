import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type LocalScriptEvaluation,
  type PhaseAValidatedTx,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as MempoolDB from "../database/mempool.js";
import { DatabaseError } from "../database/utils/common.js";

/** Classify normal block candidates and retain the executions for their traces. */
export const evaluateNormalBlockCandidates = (input: {
  readonly candidates: readonly PhaseAValidatedTx[];
  readonly state: Map<string, Buffer>;
  readonly blockSlot: bigint;
  readonly l1SlotConfig: L1SlotConfig;
  readonly bucketConcurrency: number;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly scriptEvaluationsByTxId: Map<string, LocalScriptEvaluation[]>;
}) =>
  runPhaseBValidationWithPatch(input.candidates, input.state, {
    nowCardanoSlotNo: input.blockSlot,
    l1SlotConfig: input.l1SlotConfig,
    bucketConcurrency: input.bucketConcurrency,
    enforceScriptBudget: true,
    // The validation trace replays every retained CEK step.
    cekExecutionMode: "proof",
    maxScriptExecutionSteps:
      input.consensusProfile.limits.maxValidationMachineStepCount,
    onScriptEvaluated: (txId, evaluation) => {
      const key = txId.toString("hex");
      const captures = input.scriptEvaluationsByTxId.get(key) ?? [];
      captures.push(evaluation);
      input.scriptEvaluationsByTxId.set(key, captures);
    },
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DatabaseError({
          table: MempoolDB.tableName,
          message: "V1 normal transaction Phase B failed",
          cause,
        }),
    ),
  );
