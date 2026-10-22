/** A stage of our case that does not build, for any reason other than our
 * wallet's funding or the chain having moved on: the operator's alarm under
 * the move-unbuilt state, naming the stage and the deadline it must land
 * by. Init, Open and VerifySource feed the case open guard, so each must
 * land by the end of the case open window, `block_end + M/2`
 * (validation-dispute-v1 `case_can_open`). The case's close must land
 * before the operator's bond unlocks at `started_at + D_case + P`
 * (validation-case-v1 `defense_bond_unlock_time`), and the slash of a
 * defender who lost a thread before that defender's bond unlocks
 * (lost-defense-slash.ts). A thread move names its
 * thread and deadline once it reaches its own build (move-dispatch.ts); one
 * that fails before it names its stage only. The run resumes on the next
 * observation and builds the stage again. A claim that does not build
 * before Init (claim-material.ts) is held without an alarm until the case
 * open window it must open our case in is at risk. */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { WorkflowActionUnbuiltError } from "../workflow/orchestrator.unbuilt-actions.js";
import {
  isWorkflowArtifactPendingError,
  WorkflowArtifactPendingError,
} from "../workflow/workflow-artifact-pending.js";
import {
  isValidationTraceMove,
  type ParsedValidationTraceAction,
  validationTraceActionId,
} from "./actions.js";
import { VALIDATION_TRACE_CLAIM_BUILD_HELD } from "./claim-material.js";
import { validationTraceLostDefenseSlashDeadlineMs } from "./lost-defense-slash-due.js";
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

/** The end of the case open window on a block whose state-queue node has
 * the `block_time` `blockTime`: `block_time + M/2`. */
export const validationTraceCaseOpenWindowEndMs = (blockTime: bigint): number =>
  Number(blockTime) + MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS;

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
    return validationTraceCaseOpenWindowEndMs(blockTime);
  } catch {
    return undefined;
  }
};

/** How near the end of the case open window a claim that does not build
 * becomes an alarm: once half the window or less is left, while a retry on
 * the next observation can still open the case in time. */
export const VALIDATION_TRACE_CLAIM_BUILD_AT_RISK_MS = Math.floor(
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS / 2,
);

/**
 * The deadline an alarm on a hold of a block names, at `nowMs`, when the
 * block's case open window ends at `endMs`: `undefined` while more than
 * `VALIDATION_TRACE_CLAIM_BUILD_AT_RISK_MS` of the window is left; once that
 * or less is left, or the block cannot be read to tell (`endMs` undefined),
 * the end of the window, by which our case must open.
 */
export const validationTraceCaseOpenAtRisk = (
  endMs: number | undefined,
  nowMs: number,
): string | undefined => {
  if (
    endMs !== undefined &&
    endMs - nowMs > VALIDATION_TRACE_CLAIM_BUILD_AT_RISK_MS
  )
    return undefined;
  return endMs === undefined
    ? "the end of the case open window"
    : `${endMs.toString()}, the end of the case open window`;
};

/**
 * Rethrows a held claim build of the block `headerHash` (claim-material.ts)
 * as an alarm naming the end of the case open window, once
 * `VALIDATION_TRACE_CLAIM_BUILD_AT_RISK_MS` or less of the window is left,
 * or the block cannot be read to tell; earlier, and any other error, as it
 * came. The window bounds Init, Open and VerifySource, so a claim still
 * held near its end can lose the case before it opens.
 */
export const heldClaimBuildAlarm =
  ({
    lucid,
    deployment,
    now,
  }: {
    readonly lucid: LucidEvolution;
    readonly deployment: ValidationTraceDisputeDeployment;
    readonly now: () => number;
  }) =>
  (headerHash: string) =>
  async (cause: unknown): Promise<never> => {
    // The claim build's is the only artifact hold this family raises.
    if (!isWorkflowArtifactPendingError(cause)) throw cause;
    const by = validationTraceCaseOpenAtRisk(
      await openWindowEndMs(lucid, deployment, headerHash),
      now(),
    );
    if (by === undefined) throw cause;
    throw new WorkflowArtifactPendingError(
      `${VALIDATION_TRACE_CLAIM_BUILD_HELD}: the claim on block ${headerHash} does not build, and our case must open by ${by}: ${cause.message.slice(VALIDATION_TRACE_CLAIM_BUILD_HELD.length + 2)}`,
      Object.freeze({ name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: true }),
      { cause: cause.cause },
    );
  };

class ValidationTraceStageUnbuiltError extends WorkflowActionUnbuiltError {
  constructor(
    input: Readonly<{
      action: ParsedValidationTraceAction;
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
      validationTraceActionId(input.action),
      `${VALIDATION_TRACE_MOVE_UNBUILT}: the ${input.action.stage} move of our case on block ${input.headerHash} does not build, and must land by ${by}: ${cause}`,
    );
    this.name = "ValidationTraceStageUnbuiltError";
  }
}

/**
 * The alarm for the `stage` move of our claim on the block `headerHash`
 * that failed with `cause`: `found` is our case when the stage read it.
 */
export const validationTraceStageUnbuilt = async ({
  action,
  lucid,
  deployment,
  headerHash,
  found,
  cause,
}: {
  readonly action: ParsedValidationTraceAction;
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly headerHash: string;
  readonly found: Readonly<{ caseDatum: SDK.Case }> | undefined;
  readonly cause: unknown;
}): Promise<ValidationTraceStageUnbuiltError> => {
  const { stage } = action;
  const named = { action, headerHash, cause };
  if (stage === "close")
    return new ValidationTraceStageUnbuiltError({
      ...named,
      deadline: "the operator's bond unlock",
      deadlineMs:
        found === undefined
          ? undefined
          : validationTraceBondUnlockMs(found.caseDatum),
    });
  if (stage === "slash_lost_defense")
    return new ValidationTraceStageUnbuiltError({
      ...named,
      deadline: "the defender's bond unlock",
      deadlineMs: await validationTraceLostDefenseSlashDeadlineMs(
        lucid,
        deployment,
        action.consumes!,
      ),
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
