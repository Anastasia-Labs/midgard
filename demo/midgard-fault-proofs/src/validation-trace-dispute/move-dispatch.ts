/** The challenger's move of stage `stage` on a thread of our defended case:
 * the game's moves and the timeout's close (moves.ts), and the one-step
 * resolution's (one-step-moves.ts), which read the case's recorded threads
 * for the defender a thread past the game no longer names. A move our wallet
 * cannot fund raises the operator's alarm, naming the case, the thread and
 * the deadline the move must land by; so does a move that does not build,
 * under the one-step-required state for a move of the one-step resolution
 * and the move-unbuilt state for any other. */
import type * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { ValidationDisputeWalletMoveUnfundedError } from "../workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import { WorkflowActionUnbuiltError } from "../workflow/orchestrator.unbuilt-actions.js";
import {
  validationTraceActionId,
  type ValidationTraceStage,
} from "./actions.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  captureValidationTraceReveal,
  captureValidationTraceThreadClose,
  captureValidationTraceTimeout,
  type ValidationTraceMoveInput,
} from "./moves.js";
import {
  captureValidationTraceAwardedClose,
  captureValidationTraceEnterResolution,
  captureValidationTraceResolve,
} from "./one-step-moves.js";
import { validationTraceRecordedThreads } from "./thread-observation.js";
import {
  VALIDATION_TRACE_MOVE_UNBUILT,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  validationTraceCaseExpiryMs,
} from "./threads.js";
import { type CapturedValidationTraceStage, ours } from "./transactions.js";
import { validationTraceMoveDeadlineMs } from "./wallet-move-unfunded.js";

type ThreadMove = Readonly<{
  stage: ValidationTraceStage;
  move: ValidationTraceMoveInput & {
    readonly material: ValidationTraceClaimMaterial | undefined;
    /** Our published copy of the field an observe move reads. */
    readonly observeCopy?: () => Promise<UTxO | undefined>;
  };
  /** Our case as it stands now. */
  caseOutRef: string;
  history: Readonly<{
    caseHistory: ValidationTraceCaseHistory;
    headerHash: string;
    caseUnit: string;
  }>;
}>;

const capture = async ({
  stage,
  move,
  caseOutRef,
  history,
}: ThreadMove): Promise<CapturedValidationTraceStage> => {
  switch (stage) {
    case "reveal":
      return await captureValidationTraceReveal(move);
    case "timeout":
      return await captureValidationTraceTimeout(move);
    case "close_thread":
      return await captureValidationTraceThreadClose({ ...move, caseOutRef });
    case "enter_resolution":
      return await captureValidationTraceEnterResolution(move);
    case "resolve":
      return await captureValidationTraceResolve({
        ...move,
        defenders: await validationTraceRecordedThreads(history),
        history,
      });
    case "close_awarded":
      return await captureValidationTraceAwardedClose({
        ...move,
        caseOutRef,
        defenders: await validationTraceRecordedThreads(history),
      });
    case "init":
    case "open":
    case "verify_source":
    case "close":
      throw new Error(`validation-trace ${stage} is not a thread move`);
  }
};

/** The moves of a thread's one-step resolution. */
const ONE_STEP_STAGES: ReadonlySet<ValidationTraceStage> = new Set([
  "enter_resolution",
  "resolve",
  "close_awarded",
]);

/**
 * A thread move that does not build: the operator's alarm, under the
 * one-step-required state for a move of the one-step resolution and the
 * move-unbuilt state for any other, naming the case, the thread and the
 * deadline the move must land by. The run resumes on the next observation
 * and builds the move again.
 */
export class ValidationTraceMoveUnbuiltError extends WorkflowActionUnbuiltError {
  constructor(
    input: Readonly<{
      stage: ValidationTraceStage;
      caseUnit: string;
      threadOutRef: string;
      deadlineMs: number | undefined;
      cause: unknown;
    }>,
  ) {
    const deadline =
      input.deadlineMs === undefined
        ? "its deadline"
        : input.deadlineMs.toString();
    const cause =
      input.cause instanceof Error ? input.cause.message : String(input.cause);
    const state = ONE_STEP_STAGES.has(input.stage)
      ? VALIDATION_TRACE_ONE_STEP_REQUIRED
      : VALIDATION_TRACE_MOVE_UNBUILT;
    super(
      validationTraceActionId({
        stage: input.stage,
        consumes: input.threadOutRef,
      }),
      `${state}: the ${input.stage} move on thread ${input.threadOutRef} of case ${input.caseUnit} does not build, and must land by ${deadline}: ${cause}`,
    );
    this.name = "ValidationTraceMoveUnbuiltError";
  }
}

/**
 * The move on a thread of our case, whose datum is `caseDatum`. A build our
 * wallet is short of ada for is the operator's alarm, and so is a move that
 * does not build for any other reason than the thread having moved.
 */
export const captureValidationTraceThreadMove = async ({
  caseDatum,
  ...input
}: ThreadMove &
  Readonly<{ caseDatum: SDK.Case }>): Promise<CapturedValidationTraceStage> => {
  const { move } = input;
  try {
    return await capture(input);
  } catch (cause) {
    const unfunded = isWorkflowFundingShortfall(cause);
    if (!unfunded && cause instanceof WorkflowActionChangedError) throw cause;
    const thread = await ours(move.lucid, move.threadOutRef, "thread").catch(
      () => undefined,
    );
    const named = {
      caseUnit: move.caseUnit,
      threadOutRef: move.threadOutRef,
      deadlineMs: validationTraceMoveDeadlineMs({
        stage: input.stage,
        caseExpiresAtMs: validationTraceCaseExpiryMs(caseDatum),
        thread,
      }),
      cause,
    };
    if (!unfunded)
      throw new ValidationTraceMoveUnbuiltError({
        stage: input.stage,
        ...named,
      });
    throw new ValidationDisputeWalletMoveUnfundedError({
      actionId: validationTraceActionId({
        stage: input.stage,
        consumes: move.threadOutRef,
      }),
      ...named,
    });
  }
};
