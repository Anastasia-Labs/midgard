import type {
  PhaseAConfig,
  RejectCode,
  WirePhaseACandidateV1,
} from "@al-ft/midgard-validation";

export type ValidationWorkerInit = {
  readonly config: {
    readonly expectedNetworkId: bigint;
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
    readonly strictnessProfile: string;
    readonly consensusProfile: NonNullable<PhaseAConfig["consensusProfile"]>;
  };
  /** Worker-local runtime choice; never enters serializable PhaseAConfig. */
  readonly signatureVerifier?: "node" | "cml";
};

export type PhaseAJobRequest = {
  readonly kind: "phase_a";
  readonly jobId: number;
  readonly arena: ArrayBuffer;
  readonly txs: ReadonlyArray<{
    readonly txIdOffset: number;
    readonly cborOffset: number;
    readonly cborLength: number;
    readonly programMaterialOffset: number;
    readonly programMaterialLength: number;
    readonly arrivalSeq: bigint;
    readonly createdAtMs: number;
  }>;
};

export type UplcJobRequest = {
  readonly kind: "uplc";
  readonly jobId: number;
  readonly scriptBytes: ArrayBuffer;
  readonly contextCbor: ArrayBuffer;
};

export type ValidationJobRequest = PhaseAJobRequest | UplcJobRequest;

export type ValidationCacheStats = {
  readonly size: number;
  readonly maxEntries: number;
  readonly hits: number;
  readonly misses: number;
  readonly evictions: number;
};

export type PhaseAJobResponse = {
  readonly kind: "phase_a";
  readonly jobId: number;
  readonly workerThreadId: number;
  readonly publicKeyCache: ValidationCacheStats;
  readonly addressCache: ValidationCacheStats;
  readonly results: ReadonlyArray<
    | { readonly ok: true; readonly candidate: WirePhaseACandidateV1 }
    | {
        readonly ok: false;
        readonly txId: Uint8Array;
        readonly code: RejectCode;
        readonly detail: string | null;
      }
  >;
};

export type UplcJobResponse = {
  readonly kind: "uplc";
  readonly jobId: number;
  readonly result:
    | { readonly ok: true; readonly cpu: bigint; readonly memory: bigint }
    | { readonly ok: false; readonly detail: string };
};

export type WorkerFailure = {
  readonly kind: "job_failed";
  readonly jobId: number;
  readonly error: string;
};

export type ValidationWorkerResponse =
  | PhaseAJobResponse
  | UplcJobResponse
  | WorkerFailure;

export type PhaseAWorkerInput = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly programMaterialSidecarCbor?: Buffer | null;
  readonly arrivalSeq: bigint;
  readonly createdAt: Date;
};

export const packPhaseAJob = (
  jobId: number,
  txs: readonly PhaseAWorkerInput[],
): PhaseAJobRequest => {
  for (const tx of txs) {
    if (tx.txId.byteLength !== 32) {
      throw new Error(
        `validation Phase A tx id must be 32 bytes, got ${tx.txId.byteLength}`,
      );
    }
  }
  const totalBytes = txs.reduce(
    (total, tx) =>
      total +
      tx.txId.byteLength +
      tx.txCbor.byteLength +
      (tx.programMaterialSidecarCbor?.byteLength ?? 0),
    0,
  );
  const arena = new ArrayBuffer(totalBytes);
  const bytes = new Uint8Array(arena);
  let offset = 0;
  const descriptors = txs.map((tx) => {
    const txIdOffset = offset;
    bytes.set(tx.txId, offset);
    offset += tx.txId.byteLength;
    const cborOffset = offset;
    bytes.set(tx.txCbor, offset);
    offset += tx.txCbor.byteLength;
    const programMaterialOffset = offset;
    const programMaterialLength =
      tx.programMaterialSidecarCbor?.byteLength ?? 0;
    if (programMaterialLength > 0) {
      bytes.set(tx.programMaterialSidecarCbor!, offset);
      offset += programMaterialLength;
    }
    return {
      txIdOffset,
      cborOffset,
      cborLength: tx.txCbor.byteLength,
      programMaterialOffset,
      programMaterialLength,
      arrivalSeq: tx.arrivalSeq,
      createdAtMs: tx.createdAt.getTime(),
    };
  });
  return { kind: "phase_a", jobId, arena, txs: descriptors };
};

export const copyToTransferable = (bytes: Uint8Array): ArrayBuffer => {
  const copy = new ArrayBuffer(bytes.byteLength);
  new Uint8Array(copy).set(bytes);
  return copy;
};
