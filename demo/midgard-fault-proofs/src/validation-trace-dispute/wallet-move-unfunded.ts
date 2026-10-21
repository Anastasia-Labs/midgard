/** The deadline a thread move of our case must land by, which the operator's
 * alarm names when our wallet cannot fund the move: the case's expiry, and
 * for a move on our own clock (a reveal, or the entry into the one-step
 * resolution) the end of that clock if it comes first. */
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceStage } from "./actions.js";
import { validationTraceChallengerDeadlineMs } from "./threads.js";

/** The case's expiry: `started_at + D_case`, the validators' own bound. */
export const validationTraceCaseExpiryMs = (caseDatum: SDK.Case): number =>
  Number(caseDatum.started_at) + MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;

/** Whether a move of `stage` is lost once our clock on its thread runs out. */
const onOurClock = (stage: ValidationTraceStage): boolean =>
  stage === "reveal" || stage === "enter_resolution";

/**
 * The deadline the `stage` move spending `thread` must land by, given the
 * case's expiry `caseExpiresAtMs`; `undefined` when it is not known.
 */
export const validationTraceMoveDeadlineMs = ({
  stage,
  caseExpiresAtMs,
  thread,
}: {
  readonly stage: ValidationTraceStage;
  readonly caseExpiresAtMs: number | undefined;
  readonly thread: UTxO | undefined;
}): number | undefined => {
  if (caseExpiresAtMs === undefined || !onOurClock(stage))
    return caseExpiresAtMs;
  if (typeof thread?.datum !== "string") return undefined;
  try {
    const { data } = Data.from(thread.datum, SDK.CaseThreadDatum);
    return data === null
      ? undefined
      : Math.min(caseExpiresAtMs, validationTraceChallengerDeadlineMs(data));
  } catch {
    return undefined;
  }
};
