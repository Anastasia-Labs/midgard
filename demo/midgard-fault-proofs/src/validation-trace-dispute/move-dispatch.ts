/** The challenger's move of stage `stage` on a thread of our defended case:
 * the game's moves and the timeout's close (moves.ts), and the one-step
 * resolution's (one-step-moves.ts), which read the case's recorded threads
 * for the defender a thread past the game no longer names. */
import type { ValidationTraceStage } from "./actions.js";
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
import type { CapturedValidationTraceStage } from "./transactions.js";

export const captureValidationTraceThreadMove = async ({
  stage,
  move,
  caseOutRef,
  history,
}: {
  readonly stage: ValidationTraceStage;
  readonly move: ValidationTraceMoveInput & {
    readonly material: ValidationTraceClaimMaterial | undefined;
  };
  /** Our case as it stands now. */
  readonly caseOutRef: string;
  readonly history: Readonly<{
    caseHistory: ValidationTraceCaseHistory;
    headerHash: string;
    caseUnit: string;
  }>;
}): Promise<CapturedValidationTraceStage> => {
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
