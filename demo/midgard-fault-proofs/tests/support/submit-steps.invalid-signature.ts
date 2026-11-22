/**
 * The invalid-signature builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal, beside the generic
 * helpers of ./submit-steps.ts, so a test takes both from one module.
 */
import { buildInvalidSignatureStep01Forced } from "../../src/invalid-signature/submit.js";
import { buildInvalidSignatureStep02 } from "../../src/submit-invalid-signature-step-02.submit-invalid-signature-step02.js";
import { stepSubmitter, turnsSubmitter } from "./submit-steps.js";

export const submitInvalidSignatureStep01Forced = stepSubmitter(
  buildInvalidSignatureStep01Forced,
);
export const submitInvalidSignatureStep02 = turnsSubmitter(
  buildInvalidSignatureStep02,
);

export * from "./submit-steps.js";
