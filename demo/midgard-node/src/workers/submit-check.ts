/**
 * A submit check worker: runs `checkSubmissionBody`, the same module the
 * HTTP handler imports, on each body of a batch and answers the verdicts in
 * body order. An accepted tx comes back as one transferred buffer holding
 * its tx id, full hash and canonical bytes, so the main thread neither
 * decodes nor hashes it.
 */
import { parentPort, workerData } from "node:worker_threads";

import { checkSubmissionBody } from "../commands/http/submission-check.js";
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
  body: ArrayBuffer,
  transfer: ArrayBuffer[],
): SubmitCheckWireResult => {
  try {
    const checked = checkSubmissionBody(new Uint8Array(body), config);
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

port.on("message", ({ batch, bodies }: SubmitCheckBatch) => {
  const transfer: ArrayBuffer[] = [];
  const results = bodies.map((body) => checkOne(body, transfer));
  const reply: SubmitCheckBatchResult = { batch, results };
  port.postMessage(reply, transfer);
});

// The imports above have loaded: the pool may send batches.
const ready: SubmitCheckWorkerReady = { ready: true };
port.postMessage(ready);
