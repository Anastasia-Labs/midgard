/** A stage of our case that does not build, for any reason other than our
 * wallet's funding or the chain having moved on: the operator's alarm under
 * the move-unbuilt state, naming the stage and the deadline it must land
 * by. Init, Open and VerifySource feed the case open guard, so each must
 * land by the end of the case open window, `block_end + M/2`
 * (validation-dispute-v1 `case_can_open`). The case's close must land
 * before the operator's bond unlocks at `started_at + D_case + P`
 * (validation-case-v1 `defense_bond_unlock_time`). A thread move names its
 * thread and deadline once it reaches its own build (move-dispatch.ts); one
 * that fails before it names its stage only. The run resumes on the next
 * observation and builds the stage again. */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { isValidationTraceMove, type ValidationTraceStage } from "./actions.js";
import { VALIDATION_TRACE_MOVE_UNBUILT } from "./threads.js";
import {
  challengedBlockUtxo,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";

/** P: what is left of maturity after the open window and the longest case. */
const POST_CASE_ALLOWANCE_MS =
  MIDGARD_CONSENSUS_LIMITS.blockMaturityMs -
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS -
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;

/** When the operator's bond on our case unlocks: `started_at + D_case + P`. */
const validationTraceBondUnlockMs = (caseDatum: SDK.Case): number =>
  Number(caseDatum.started_at) +
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS +
  POST_CASE_ALLOWANCE_MS;

/** The end of the case open window on the block `headerHash`, by its
 * state-queue `block_time`; `undefined` when the block cannot be read. */
const openWindowEndMs = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
  headerHash: string,
): Promise<number | undefined> => {
  try {
    const { blockTime } = await Effect.runPromise(
      SDK.validationDisputeChallengedBlock(
        await challengedBlockUtxo(lucid, deployment, headerHash),
        deployment.stateQueue.policyId,
      ),
    );
    return Number(blockTime) + MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS;
  } catch {
    return undefined;
  }
};

class ValidationTraceStageUnbuiltError extends WorkflowActionChangedError {
  constructor(
    input: Readonly<{
      stage: ValidationTraceStage;
      headerHash: string;
      deadline: string;
      deadlineMs: number | undefined;
      cause: unknown;
    }>,
  ) {
    const by =
      input.deadlineMs === undefined
        ? input.deadline
        : `${input.deadlineMs.toString()}, ${input.deadline}`;
    const cause =
      input.cause instanceof Error ? input.cause.message : String(input.cause);
    super(
      `${VALIDATION_TRACE_MOVE_UNBUILT}: the ${input.stage} move of our case on block ${input.headerHash} does not build, and must land by ${by}: ${cause}`,
    );
    this.name = "ValidationTraceStageUnbuiltError";
  }
}

/**
 * The alarm for the `stage` move of our claim on the block `headerHash`
 * that failed with `cause`: `found` is our case when the stage read it.
 */
export const validationTraceStageUnbuilt = async ({
  stage,
  lucid,
  deployment,
  headerHash,
  found,
  cause,
}: {
  readonly stage: ValidationTraceStage;
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly headerHash: string;
  readonly found: Readonly<{ caseDatum: SDK.Case }> | undefined;
  readonly cause: unknown;
}): Promise<ValidationTraceStageUnbuiltError> => {
  const named = { stage, headerHash, cause };
  if (stage === "close")
    return new ValidationTraceStageUnbuiltError({
      ...named,
      deadline: "the operator's bond unlock",
      deadlineMs:
        found === undefined
          ? undefined
          : validationTraceBondUnlockMs(found.caseDatum),
    });
  if (!isValidationTraceMove(stage))
    return new ValidationTraceStageUnbuiltError({
      ...named,
      deadline: "the end of the case open window",
      deadlineMs: await openWindowEndMs(lucid, deployment, headerHash),
    });
  return new ValidationTraceStageUnbuiltError({
    ...named,
    deadline: "its deadline",
    deadlineMs: undefined,
  });
};
