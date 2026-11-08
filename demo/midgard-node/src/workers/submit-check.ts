/**
 * A submit check worker: runs `checkSubmissionBody` (a `/submit` body) or
 * `checkTransactionBytes` (a peer's bare transaction), from the module the
 * HTTP handlers import, on each body of a batch and answers the verdicts in
 * body order. An accepted tx comes back as one transferred buffer holding
 * its tx id, full hash and canonical bytes, so the main thread neither
 * decodes nor hashes it.
 */
import { parentPort, workerData } from "node:worker_threads";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";

import {
  checkSubmissionBody,
  checkTransactionBytes,
} from "../commands/http/submission-check.js";
import {
  SUBMIT_CHECK_FULL_HASH_END,
  SUBMIT_CHECK_TX_ID_BYTES,
  type SubmitCheckBatch,
  type SubmitCheckBatchResult,
  type SubmitCheckWireResult,
  type SubmitCheckWorkerInit,
  type SubmitCheckWorkerReady,
} from "./utils/submit-check-protocol.js";

const port = parentPort;
if (port === null) {
  throw new Error("submit check worker requires a parent port");
}

const { config } = workerData as SubmitCheckWorkerInit;

const checkOne = (
  check: typeof checkSubmissionBody,
  body: ArrayBuffer,
  nowMs: number,
  transfer: ArrayBuffer[],
): SubmitCheckWireResult => {
  try {
    const checked = check(new Uint8Array(body), config, nowMs);
    if (!checked.ok)
      return {
        ok: false,
        reason: checked.reason,
        status: checked.status,
        body: checked.body,
        log: checked.log,
      };
    const tx = checked.normalized;
    const bytes = new ArrayBuffer(
      SUBMIT_CHECK_FULL_HASH_END + tx.txCanonicalCbor.length,
    );
    const view = new Uint8Array(bytes);
    view.set(tx.txId, 0);
    view.set(tx.txFullHash, SUBMIT_CHECK_TX_ID_BYTES);
    view.set(tx.txCanonicalCbor, SUBMIT_CHECK_FULL_HASH_END);
    transfer.push(bytes);
    return {
      ok: true,
      bytes,
      txIdHex: tx.txIdHex,
      spentOutRefs: tx.spentOutRefs,
      producedOutRefs: tx.producedOutRefs,
      fee: tx.fee,
      byteLength: tx.byteLength,
    };
  } catch (error) {
    return {
      ok: "failed",
      detail: error instanceof Error ? error.message : String(error),
    };
  }
};

port.on("message", ({ batch, raw, bodies }: SubmitCheckBatch) => {
  const transfer: ArrayBuffer[] = [];
  // This worker's own clock, read once per batch, for the expiry check.
  const nowMs = Date.now();
  const check = raw ? checkTransactionBytes : checkSubmissionBody;
  const results = bodies.map((body) => checkOne(check, body, nowMs, transfer));
  const reply: SubmitCheckBatchResult = { batch, results };
  port.postMessage(reply, transfer);
});

// The imports above have loaded. Witness signatures are verified by
// libsodium, which initialises asynchronously within a bound
// (`LEDGER_ED25519_READY_TIMEOUT_MS`, under the pool's start timeout): the
// pool may send batches once it can answer. A verifier that cannot
// initialise stops this worker on an uncaught error, so the pool answers its
// waiting bodies `unavailable` (503, retry) and starts a fresh worker, which
// loads libsodium again.
const ready: SubmitCheckWorkerReady = { ready: true };
void ledgerEd25519SignatureReady().then(
  () => port.postMessage(ready),
  (error: unknown) => {
    setImmediate(() => {
      throw error;
    });
  },
);
