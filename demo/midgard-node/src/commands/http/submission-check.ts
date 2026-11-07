/**
 * The check of one `/submit` body: the V1 proof-submission envelope, then
 * `checkSubmittedTransaction` on the transaction it carries. A pure function
 * of the body bytes and `SubmissionCheckConfig` (plain data), so the submit
 * check workers (`workers/submit-check.ts`) import and run this module as
 * it is.
 */
import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";

import {
  checkSubmittedTransaction,
  type SubmittedTxCheck,
} from "./transaction-admission.js";

export type SubmissionRefusalReason =
  | "malformed_envelope"
  | "invalid_transaction";

export type SubmissionCheck =
  | Extract<SubmittedTxCheck, { readonly ok: true }>
  | (Extract<SubmittedTxCheck, { readonly ok: false }> & {
      readonly reason: SubmissionRefusalReason;
    });

/**
 * What the check reads besides the body. Plain data: it is handed to every
 * submit check worker when the worker starts.
 */
export type SubmissionCheckConfig = {
  readonly maxSubmitTxCborBytes: number;
};

/** The check configuration of a node configuration. */
export const submissionCheckConfig = (config: {
  readonly MAX_SUBMIT_TX_CBOR_BYTES: number;
}): SubmissionCheckConfig => ({
  maxSubmitTxCborBytes: config.MAX_SUBMIT_TX_CBOR_BYTES,
});

export const checkSubmissionBody = (
  body: Uint8Array,
  config: SubmissionCheckConfig,
): SubmissionCheck => {
  let transactionCbor: Uint8Array;
  try {
    transactionCbor = decodeMidgardProofSubmission(body).transactionCbor;
  } catch {
    return {
      ok: false,
      reason: "malformed_envelope",
      status: 400,
      body: { error: "Invalid canonical V1 submission envelope" },
      log: ["malformed V1 submission envelope"],
    };
  }
  const checked = checkSubmittedTransaction(
    transactionCbor,
    config.maxSubmitTxCborBytes,
  );
  return checked.ok ? checked : { ...checked, reason: "invalid_transaction" };
};
