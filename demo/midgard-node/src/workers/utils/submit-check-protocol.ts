/**
 * Messages between the submit check pool (`services/submit-check-pool.ts`)
 * and its workers (`workers/submit-check.ts`). Every field survives a
 * structured clone; byte payloads travel as transferred `ArrayBuffer`s.
 */
import type {
  SubmissionCheckConfig,
  SubmissionRefusalReason,
} from "../../commands/http/submission-check.js";

/** The worker's `workerData`. */
export type SubmitCheckWorkerInit = {
  readonly config: SubmissionCheckConfig;
};

/** One message to a worker: bodies posted together. */
export type SubmitCheckBatch = {
  readonly batch: number;
  /**
   * Whether each body is a bare transaction (a peer's), checked by
   * `checkTransactionBytes`, rather than a `/submit` body, checked by
   * `checkSubmissionBody`.
   */
  readonly raw: boolean;
  /** One body each, transferred. */
  readonly bodies: readonly ArrayBuffer[];
};

/** Bytes 0–31 of an accepted result's `bytes`: the tx id. */
export const SUBMIT_CHECK_TX_ID_BYTES = 32;
/** Bytes 32–63: the tx's full hash; the canonical tx bytes follow. */
export const SUBMIT_CHECK_FULL_HASH_END = 64;

/** The check's verdict on one body, in body order. */
export type SubmitCheckWireResult =
  | {
      readonly ok: true;
      /** Tx id, full hash, then the canonical tx bytes; transferred. */
      readonly bytes: ArrayBuffer;
      readonly txIdHex: string;
      readonly spentOutRefs: readonly string[];
      readonly producedOutRefs: readonly string[];
      readonly fee: bigint;
      readonly byteLength: number;
    }
  | {
      readonly ok: false;
      readonly reason: SubmissionRefusalReason;
      /** 503 only when the signature verifier could not answer. */
      readonly status: 400 | 413 | 503;
      readonly body: Readonly<Record<string, unknown>>;
      readonly log: readonly string[];
    }
  | {
      /** The check threw; the request gets an internal error. */
      readonly ok: "failed";
      readonly detail: string;
    };

export type SubmitCheckBatchResult = {
  readonly batch: number;
  readonly results: readonly SubmitCheckWireResult[];
};

/** Sent once by a worker when it has loaded the check and takes batches. */
export type SubmitCheckWorkerReady = { readonly ready: true };

/** Every message a worker sends. */
export type SubmitCheckWorkerMessage =
  | SubmitCheckWorkerReady
  | SubmitCheckBatchResult;
