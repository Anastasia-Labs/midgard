/** The challenger's move of stage `stage` on a thread of our defended case:
 * the game's moves and the timeout's close (moves.ts), and the one-step
 * resolution's (one-step-moves.ts), which read the case's recorded threads
 * for the defender a thread past the game no longer names. A move our wallet
 * cannot fund raises the operator's alarm, naming the case, the thread and
 * the deadline the move must land by. */
import type * as SDK from "@al-ft/midgard-sdk";

import { ValidationDisputeWalletMoveUnfundedError } from "../workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
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
import { type CapturedValidationTraceStage, ours } from "./transactions.js";
import {
  validationTraceCaseExpiryMs,
  validationTraceMoveDeadlineMs,
} from "./wallet-move-unfunded.js";

type ThreadMove = Readonly<{
  stage: ValidationTraceStage;
  move: ValidationTraceMoveInput & {
    readonly material: ValidationTraceClaimMaterial | undefined;
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

/**
 * The move on a thread of our case, whose datum is `caseDatum`. A build our
 * wallet is short of ada for is the operator's alarm.
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
    if (!isWorkflowFundingShortfall(cause)) throw cause;
    const thread = await ours(move.lucid, move.threadOutRef, "thread").catch(
      () => undefined,
    );
    throw new ValidationDisputeWalletMoveUnfundedError({
      actionId: validationTraceActionId({
        stage: input.stage,
        consumes: move.threadOutRef,
      }),
      caseUnit: move.caseUnit,
      threadOutRef: move.threadOutRef,
      deadlineMs: validationTraceMoveDeadlineMs({
        stage: input.stage,
        caseExpiresAtMs: validationTraceCaseExpiryMs(caseDatum),
        thread,
      }),
      cause,
    });
  }
};
