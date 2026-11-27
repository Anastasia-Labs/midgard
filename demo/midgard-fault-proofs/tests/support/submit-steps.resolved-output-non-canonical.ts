/**
 * The resolved-output-non-canonical builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal, beside the
 * generic helpers of ./submit-steps.ts, so a test takes both from one
 * module.
 */
import { buildResolvedOutputNonCanonicalCancel } from "../../src/resolved-output-non-canonical/submit-cancel.js";
import { buildResolvedOutputNonCanonicalStep01Accepted } from "../../src/resolved-output-non-canonical/submit-step-01-accepted.js";
import { buildResolvedOutputNonCanonicalStep01Forced } from "../../src/resolved-output-non-canonical/submit-step-01-forced.js";
import { buildResolvedOutputNonCanonicalStep02 } from "../../src/resolved-output-non-canonical/submit-step-02.js";
import { buildResolvedOutputNonCanonicalStep03 } from "../../src/resolved-output-non-canonical/submit-step-03.js";
import { buildResolvedOutputNonCanonicalStep04 } from "../../src/resolved-output-non-canonical/submit-step-04.js";
import { buildResolvedOutputNonCanonicalStep04Datum } from "../../src/resolved-output-non-canonical/submit-step-04-datum.js";
import { buildResolvedOutputNonCanonicalStep05 } from "../../src/resolved-output-non-canonical/submit-step-05.js";
import { stepSubmitter, turnsSubmitter } from "./submit-steps.js";

export const submitResolvedOutputNonCanonicalCancel = stepSubmitter(
  buildResolvedOutputNonCanonicalCancel,
);
export const submitResolvedOutputNonCanonicalStep01Accepted = stepSubmitter(
  buildResolvedOutputNonCanonicalStep01Accepted,
);
export const submitResolvedOutputNonCanonicalStep01Forced = stepSubmitter(
  buildResolvedOutputNonCanonicalStep01Forced,
);
export const submitResolvedOutputNonCanonicalStep02 = turnsSubmitter(
  buildResolvedOutputNonCanonicalStep02,
);
export const submitResolvedOutputNonCanonicalStep03 = stepSubmitter(
  buildResolvedOutputNonCanonicalStep03,
);
export const submitResolvedOutputNonCanonicalStep04 = stepSubmitter(
  buildResolvedOutputNonCanonicalStep04,
);
export const submitResolvedOutputNonCanonicalStep04Datum = stepSubmitter(
  buildResolvedOutputNonCanonicalStep04Datum,
);
export const submitResolvedOutputNonCanonicalStep05 = stepSubmitter(
  buildResolvedOutputNonCanonicalStep05,
);

export * from "./submit-steps.js";
