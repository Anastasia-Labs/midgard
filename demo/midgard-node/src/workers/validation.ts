import { parentPort, threadId, workerData } from "node:worker_threads";

import {
  findLedgerSignatureVerifierUnavailable,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import {
  evaluateMidgardCekVerdict,
  phaseAAddressCacheStats,
  serializePhaseACandidate,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";

import type {
  PhaseAJobRequest,
  ValidationJobRequest,
  ValidationWorkerInit,
  ValidationWorkerResponse,
} from "./utils/validation-pool.js";

const port = parentPort;
if (port === null) {
  throw new Error("validation worker requires a parent port");
}

const init = workerData as ValidationWorkerInit;

const runPhaseA = (request: PhaseAJobRequest): ValidationWorkerResponse => {
  const results = request.txs.map((descriptor) => {
    const queued = {
      txId: Buffer.from(request.arena, descriptor.txIdOffset, 32),
      txCbor: Buffer.from(
        request.arena,
        descriptor.cborOffset,
        descriptor.cborLength,
      ),
      programMaterialSidecarCbor:
        descriptor.programMaterialLength === 0
          ? null
          : Buffer.from(
              request.arena,
              descriptor.programMaterialOffset,
              descriptor.programMaterialLength,
            ),
      arrivalSeq: descriptor.arrivalSeq,
      createdAt: new Date(descriptor.createdAtMs),
    };
    const result = validatePhaseASingle(queued, {
      ...init.config,
      concurrency: 1,
    });
    return "ledgerTx" in result
      ? { ok: true as const, candidate: serializePhaseACandidate(result) }
      : {
          ok: false as const,
          txId: new Uint8Array(
            result.txId.buffer,
            result.txId.byteOffset,
            result.txId.byteLength,
          ),
          code: result.code,
          detail: result.detail,
        };
  });
  return {
    kind: "phase_a",
    jobId: request.jobId,
    workerThreadId: threadId,
    addressCache: phaseAAddressCacheStats(),
    results,
  };
};

const failed = (jobId: number, error: unknown): ValidationWorkerResponse => ({
  kind: "job_failed",
  jobId,
  error:
    error instanceof Error ? (error.stack ?? error.message) : String(error),
  ...(findLedgerSignatureVerifierUnavailable(error) === undefined
    ? {}
    : { localFault: "verifier_unavailable" as const }),
});

const respond = (request: ValidationJobRequest): ValidationWorkerResponse => {
  try {
    return request.kind === "cek_verdict"
      ? {
          kind: "cek_verdict",
          jobId: request.jobId,
          result: evaluateMidgardCekVerdict(request.request),
        }
      : runPhaseA(request);
  } catch (error) {
    return failed(request.jobId, error);
  }
};

// Witness signatures are verified by libsodium, which initialises
// asynchronously: every job waits for it, in arrival order, and a failed
// initialisation fails the job instead of answering it.
port.on("message", (request: ValidationJobRequest) => {
  void ledgerEd25519SignatureReady().then(
    () => port.postMessage(respond(request)),
    (error: unknown) => port.postMessage(failed(request.jobId, error)),
  );
});
