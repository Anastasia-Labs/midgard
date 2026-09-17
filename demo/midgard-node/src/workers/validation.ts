import { parentPort, threadId, workerData } from "node:worker_threads";

import {
  evaluateUplcWithContextCbor,
  phaseAAddressCacheStats,
  phaseAPublicKeyCacheStats,
  serializePhaseACandidate,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";

import { NodeEd25519Verifier } from "./utils/ed25519-verifier.js";
import type {
  PhaseAJobRequest,
  UplcJobRequest,
  ValidationJobRequest,
  ValidationWorkerInit,
  ValidationWorkerResponse,
} from "./utils/validation-pool.js";

const port = parentPort;
if (port === null) {
  throw new Error("validation worker requires a parent port");
}

const init = workerData as ValidationWorkerInit;
if (
  init.signatureVerifier !== undefined &&
  init.signatureVerifier !== "node" &&
  init.signatureVerifier !== "cml"
) {
  throw new Error(
    `unsupported validation signature verifier ${String(init.signatureVerifier)}`,
  );
}
const nodeEd25519Verifier =
  init.signatureVerifier === "node" ? new NodeEd25519Verifier() : undefined;
nodeEd25519Verifier?.assertReady();

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
    const result = validatePhaseASingle(
      queued,
      {
        ...init.config,
        concurrency: 1,
      },
      nodeEd25519Verifier === undefined
        ? undefined
        : {
            verifyVKeyWitnessSignature: nodeEd25519Verifier.verify,
          },
    );
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
    publicKeyCache: nodeEd25519Verifier?.stats() ?? phaseAPublicKeyCacheStats(),
    addressCache: phaseAAddressCacheStats(),
    results,
  };
};

const runUplc = (request: UplcJobRequest): ValidationWorkerResponse => {
  const result = evaluateUplcWithContextCbor(
    new Uint8Array(request.scriptBytes),
    new Uint8Array(request.contextCbor),
  );
  return {
    kind: "uplc",
    jobId: request.jobId,
    result:
      result.kind === "accepted"
        ? { ok: true, cpu: result.budget.cpu, memory: result.budget.memory }
        : { ok: false, detail: result.detail },
  };
};

port.on("message", (request: ValidationJobRequest) => {
  try {
    port.postMessage(
      request.kind === "phase_a" ? runPhaseA(request) : runUplc(request),
    );
  } catch (error) {
    port.postMessage({
      kind: "job_failed",
      jobId: request.jobId,
      error:
        error instanceof Error ? (error.stack ?? error.message) : String(error),
    } satisfies ValidationWorkerResponse);
  }
});
