/**
 * The resolved-output-non-canonical builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal, beside the
 * generic helpers of ./submit-unjournaled.ts, so a test takes both from one
 * module.
 */
import {
  buildResolvedOutputNonCanonicalCancel,
  buildResolvedOutputNonCanonicalStep01Accepted,
  buildResolvedOutputNonCanonicalStep01Forced,
  buildResolvedOutputNonCanonicalStep02,
  buildResolvedOutputNonCanonicalStep03,
  buildResolvedOutputNonCanonicalStep04,
  buildResolvedOutputNonCanonicalStep04Datum,
  buildResolvedOutputNonCanonicalStep05,
} from "../../src/resolved-output-non-canonical/index.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitResolvedOutputNonCanonicalCancel = unjournaled(
  buildResolvedOutputNonCanonicalCancel,
);
export const submitResolvedOutputNonCanonicalStep01Accepted = unjournaled(
  buildResolvedOutputNonCanonicalStep01Accepted,
);
export const submitResolvedOutputNonCanonicalStep01Forced = unjournaled(
  buildResolvedOutputNonCanonicalStep01Forced,
);
export const submitResolvedOutputNonCanonicalStep02 = unjournaledTurns(
  buildResolvedOutputNonCanonicalStep02,
);
export const submitResolvedOutputNonCanonicalStep03 = unjournaled(
  buildResolvedOutputNonCanonicalStep03,
);
export const submitResolvedOutputNonCanonicalStep04 = unjournaled(
  buildResolvedOutputNonCanonicalStep04,
);
export const submitResolvedOutputNonCanonicalStep04Datum = unjournaled(
  buildResolvedOutputNonCanonicalStep04Datum,
);
export const submitResolvedOutputNonCanonicalStep05 = unjournaled(
  buildResolvedOutputNonCanonicalStep05,
);

export * from "./submit-unjournaled.js";
