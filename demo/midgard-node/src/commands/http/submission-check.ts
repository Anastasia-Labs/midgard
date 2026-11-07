/**
 * The check of one `/submit` body: the V1 proof-submission envelope, then
 * `checkSubmittedTransaction` on the transaction it carries. A pure function
 * of the body bytes and the configured bound, so it runs wherever the body
 * is.
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

export const checkSubmissionBody = (
  body: Uint8Array,
  maxSubmitTxCborBytes: number,
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
    maxSubmitTxCborBytes,
  );
  return checked.ok ? checked : { ...checked, reason: "invalid_transaction" };
};
