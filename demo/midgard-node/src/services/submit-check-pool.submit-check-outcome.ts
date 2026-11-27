/** What a submit check worker's answer for one body means to the pool. */
import type { SubmissionCheck } from "../commands/http/submission-check.js";
import type { CheckedSubmittedTx } from "../commands/http/transaction-admission.js";
import {
  SUBMIT_CHECK_FULL_HASH_END,
  SUBMIT_CHECK_TX_ID_BYTES,
  type SubmitCheckWireResult,
} from "../workers/utils/submit-check-protocol.js";

/** What became of one posted body. */
export type SubmitCheckOutcome =
  | { readonly kind: "checked"; readonly check: SubmissionCheck }
  /** Its worker stopped, or the pool closed, before a verdict. */
  | { readonly kind: "unavailable"; readonly detail: string }
  /** The check threw on it. */
  | { readonly kind: "failed"; readonly detail: string };

/** The canonical bytes of a result, viewed without a copy. */
const checkedTx = (
  result: Extract<SubmitCheckWireResult, { readonly ok: true }>,
): CheckedSubmittedTx => {
  const bytes = Buffer.from(result.bytes);
  return {
    txId: bytes.subarray(0, SUBMIT_CHECK_TX_ID_BYTES),
    txIdHex: result.txIdHex,
    txFullHash: bytes.subarray(
      SUBMIT_CHECK_TX_ID_BYTES,
      SUBMIT_CHECK_FULL_HASH_END,
    ),
    txCanonicalCbor: bytes.subarray(SUBMIT_CHECK_FULL_HASH_END),
    spentOutRefs: result.spentOutRefs,
    producedOutRefs: result.producedOutRefs,
    fee: result.fee,
    byteLength: result.byteLength,
    source: "native",
  };
};

export const outcomeOf = (
  result: SubmitCheckWireResult,
): SubmitCheckOutcome => {
  if (result.ok === "failed") return { kind: "failed", detail: result.detail };
  if (result.ok)
    return {
      kind: "checked",
      check: { ok: true, normalized: checkedTx(result) },
    };
  return { kind: "checked", check: result };
};
