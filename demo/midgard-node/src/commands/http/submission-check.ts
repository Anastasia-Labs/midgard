/**
 * The check of one `/submit` body: the V1 proof-submission envelope, then
 * `checkSubmittedTransaction` on the transaction it carries; and the same
 * check of a bare transaction, as a peer sends it (`/peer/txs`). Pure
 * functions of the bytes, `SubmissionCheckConfig` (plain data) and the
 * clock, so the submit check workers (`workers/submit-check.ts`) import and
 * run this module as it is, each reading its own clock.
 */
import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";

import {
  checkSubmittedTransaction,
  type SubmittedTxCheck,
  type SubmittedTxRules,
} from "./transaction-admission.js";

export type SubmissionRefusalReason =
  | "malformed_envelope"
  | "invalid_transaction"
  | "signature_verifier_unavailable";

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
  /** Network, fee rule and L1 slot mapping (`submittedTxRules`), read on
   * the main thread: a worker holds no Lucid. */
  readonly rules: SubmittedTxRules;
};

/** The check configuration of a node configuration and its rules. */
export const submissionCheckConfig = (
  config: { readonly MAX_SUBMIT_TX_CBOR_BYTES: number },
  rules: SubmittedTxRules,
): SubmissionCheckConfig => ({
  maxSubmitTxCborBytes: config.MAX_SUBMIT_TX_CBOR_BYTES,
  rules,
});

/** `checkSubmittedTransaction` on bare transaction bytes, with its reason. */
export const checkTransactionBytes = (
  transactionCbor: Uint8Array,
  config: SubmissionCheckConfig,
  nowMs: number,
): SubmissionCheck => {
  const checked = checkSubmittedTransaction(
    transactionCbor,
    config.maxSubmitTxCborBytes,
    config.rules,
    nowMs,
  );
  if (checked.ok) return checked;
  // A 503 is the only verdict that is not about the tx: the signature
  // verifier could not answer, and the sender retries.
  return {
    ...checked,
    reason:
      checked.status === 503
        ? "signature_verifier_unavailable"
        : "invalid_transaction",
  };
};

export const checkSubmissionBody = (
  body: Uint8Array,
  config: SubmissionCheckConfig,
  nowMs: number,
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
  return checkTransactionBytes(transactionCbor, config, nowMs);
};
