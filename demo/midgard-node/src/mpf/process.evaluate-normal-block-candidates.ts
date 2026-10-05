import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
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
  readonly bucketConcurrency: number;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly scriptEvaluationsByTxId: Map<string, LocalScriptEvaluation[]>;
}) =>
  runPhaseBValidationWithPatch(input.candidates, input.state, {
    nowCardanoSlotNo: input.blockSlot,
    bucketConcurrency: input.bucketConcurrency,
    enforceScriptBudget: true,
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
