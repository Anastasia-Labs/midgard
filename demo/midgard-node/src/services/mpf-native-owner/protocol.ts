import type { MessagePort } from "node:worker_threads";

export const NATIVE_MPF_RPC_MAGIC = "MGRP" as const;
export const NATIVE_MPF_RPC_SCHEMA = 1 as const;
export const NATIVE_MPF_RPC_DIGEST_DOMAIN = "MIDGARD-MPF-OWNER-RPC-V1" as const;

export const NATIVE_MPF_OWNER_DEFAULT_CAPS = {
  maxFrameBytes: 64 * 1024 * 1024,
  maxChunkBytes: 16 * 1024 * 1024,
  maxResidentNodes: 2_000_000,
  maxResidentBytes: 2 * 1024 * 1024 * 1024,
  maxGeneratedNodes: 1_000_000,
  maxGeneratedBytes: 1024 * 1024 * 1024,
  maxEvents: 100_000,
  maxOps: 400_000,
  maxActiveGenerations: 2,
  handshakeTimeoutMs: 5_000,
  loadTimeoutMs: 120_000,
  applyTimeoutMs: 30_000,
  promotionTimeoutMs: 120_000,
  shutdownTimeoutMs: 10_000,
} as const;

export type NativeMpfOwnerCaps = {
  readonly [K in keyof typeof NATIVE_MPF_OWNER_DEFAULT_CAPS]: number;
};

export const enum NativeMpfRpcKind {
  Hello = 1,
  HelloAck = 2,
  LoadBegin = 3,
  LoadChunk = 4,
  LoadEnd = 5,
  Ready = 6,
  Fork = 7,
  Forked = 8,
  ApplyEvents = 9,
  Applied = 10,
  Discard = 11,
  Discarded = 12,
  PreparePromotion = 13,
  PromotionChunk = 14,
  PromotionEnd = 15,
  PromotionCommitted = 16,
  Diagnostics = 17,
  DiagnosticsResult = 18,
  Ping = 19,
  Pong = 20,
  Shutdown = 21,
  ShutdownAck = 22,
  Error = 23,
}

export type NativeMpfRpcFrame = {
  readonly schema: typeof NATIVE_MPF_RPC_SCHEMA;
  readonly kind: NativeMpfRpcKind;
  readonly requestId: bigint;
  readonly ownerEpoch: Uint8Array;
  readonly payload: Uint8Array;
};

export type NativeMpfGenerationHandle = {
  readonly ownerEpoch: Uint8Array;
  readonly generationId: Uint8Array;
  readonly baseRoot: string;
};

export type NativeMpfApplyResult = {
  readonly handle: NativeMpfGenerationHandle;
  readonly candidateRoot: string;
  readonly eventRoots: readonly string[];
  readonly eventLogDigest: string;
  readonly proofArenaDurationNs: number;
  readonly mutationDurationNs: number;
};

export type PersistedNativeMpfReplay = {
  readonly schema: typeof NATIVE_MPF_RPC_SCHEMA;
  readonly ownerBinarySha256: string;
  readonly baseRoot: string;
  readonly candidateRoot: string;
  readonly eventLog: Uint8Array;
  readonly eventLogDigest: string;
  readonly eventRoots: Uint8Array;
  readonly eventCount: number;
};

export type NativeMpfOwnerDiagnostics = {
  readonly ownerEpoch: Uint8Array;
  readonly durableRoot: string;
  readonly residentNodes: number;
  readonly residentEdges: number;
  readonly residentBytes: number;
  readonly activeGenerations: number;
  readonly generatedNodes: number;
  readonly generatedBytes: number;
  readonly rssBytes: number;
  readonly peakRssBytes: number;
  readonly childRestarts: number;
};

export interface NativeMpfOwnerClient {
  fork(baseRoot: string): Promise<NativeMpfGenerationHandle>;
  applyEvents(
    handle: NativeMpfGenerationHandle,
    eventLog: Uint8Array,
  ): Promise<NativeMpfApplyResult>;
  discard(handle: NativeMpfGenerationHandle): Promise<void>;
}

/** A durable, source-authorized recovery plan supplied by the parent owner.
 * These hashes identify the plan and native states; they do not authenticate L1.
 */
export type NativeMpfCanonicalRootRecovery = Readonly<{
  recoveryId: string;
  expectedRoot: string;
  targetRoot: string;
}>;

/** `restoreCanonicalRoot` refused a target whose node closure is not in the
 * native MPF store: a record the target root reaches is absent, or is not a
 * well-formed node. Raised for nothing else; the restore changed nothing. */
export class NativeMpfRootNotRetained extends Error {
  readonly _tag = "NativeMpfRootNotRetained";
  constructor(
    readonly targetRoot: string,
    options?: ErrorOptions,
  ) {
    super(
      `Native MPF canonical recovery target root ${targetRoot} is not retained in full; refusing to restore`,
      options,
    );
  }
}

/** The full-index caps the native MPF owner loads a root under: TypeScript
 * `FULL_INDEX_MAX_RECORDS` and `FULL_INDEX_MAX_BYTES`, which the native child
 * mirrors with its own constants of the same names. */
export type NativeMpfFullIndexCap =
  | "FULL_INDEX_MAX_RECORDS"
  | "FULL_INDEX_MAX_BYTES";

/** Names `cap`, its configured `limit` and what `root`'s closure reached. */
export const fullIndexCapMessage = (
  root: string,
  cap: NativeMpfFullIndexCap,
  limit: number,
  observed: number,
) =>
  cap === "FULL_INDEX_MAX_RECORDS"
    ? `Native MPF root ${root} reaches ${observed.toString()} records, over the full-index record cap FULL_INDEX_MAX_RECORDS = ${limit.toString()}`
    : `Native MPF root ${root} has a full index of at least ${observed.toString()} bytes, over the full-index byte cap FULL_INDEX_MAX_BYTES = ${limit.toString()}`;

/** `restoreCanonicalRoot` refused a target whose node closure is in the
 * native MPF store but whose full index exceeds `cap`, whose configured value
 * is `limit`, so the owner cannot load it. `observed` is the closure's record
 * count, or the index bytes counted when the byte cap was crossed. The
 * restore changed nothing. */
export class NativeMpfFullIndexCapExceeded extends Error {
  readonly _tag = "NativeMpfFullIndexCapExceeded";
  constructor(
    readonly targetRoot: string,
    readonly cap: NativeMpfFullIndexCap,
    readonly limit: number,
    readonly observed: number,
    options?: ErrorOptions,
  ) {
    super(fullIndexCapMessage(targetRoot, cap, limit, observed), options);
  }
}

/** `promote` refused a candidate root whose full index would exceed `cap`,
 * whose configured value is `limit`, so the owner could not load it at its
 * next start. `observed` is the candidate's full-index record count or byte
 * size. The promotion changed nothing: its generation is discarded, and the
 * durable root marker and the store stay at the last promoted root. */
export class NativeMpfPromotionIndexCapExceeded extends Error {
  readonly _tag = "NativeMpfPromotionIndexCapExceeded";
  constructor(
    readonly candidateRoot: string,
    readonly cap: NativeMpfFullIndexCap,
    readonly limit: number,
    readonly observed: number,
  ) {
    super(
      `Native MPF promotion refused: ${fullIndexCapMessage(candidateRoot, cap, limit, observed)}, so the owner could not load it at its next start`,
    );
  }
}

/** The live durable root's full-index size, which every promotion accounts
 * for, and the promotion the owner last refused over a full-index cap, until
 * a later promotion or a canonical restore succeeds. */
export type NativeMpfFullIndexHealth = {
  readonly bytes: number;
  readonly records: number;
  readonly promotionRefusal: NativeMpfPromotionIndexCapExceeded | undefined;
};

/** `restoreCanonicalRoot` could not read the target root's node closure from
 * the native MPF store because a store read failed. Transient: the restore
 * changed nothing, and a later restore reads the closure again. */
export class NativeMpfRestoreReadFailed extends Error {
  readonly _tag = "NativeMpfRestoreReadFailed";
  constructor(
    readonly targetRoot: string,
    options: ErrorOptions & { readonly cause: unknown },
  ) {
    super(
      `Native MPF canonical recovery could not read target root ${targetRoot}'s node closure from the native MPF store: ${options.cause instanceof Error ? options.cause.message : String(options.cause)}`,
      options,
    );
  }
}

export interface NativeMpfOwnerService extends NativeMpfOwnerClient {
  createWorkerPort(): MessagePort;
  promote(handle: NativeMpfGenerationHandle): Promise<void>;
  recover(replay: PersistedNativeMpfReplay): Promise<void>;
  restoreCanonicalRoot(plan: NativeMpfCanonicalRootRecovery): Promise<void>;
  diagnostics(): Promise<NativeMpfOwnerDiagnostics>;
  /** Why the owner refuses work, while it does: failed restarts exhaust the
   * restart window, or a committed recovery is not installed yet. Each call
   * also starts a due restart from the durable root, so the refusal clears
   * once the oldest failure leaves the window and the restart succeeds. */
  terminalFailure(): Error | undefined;
  close(): Promise<void>;
}

export const assertNativeMpfHashHex = (value: string, field: string): void => {
  if (!/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${field} must be canonical 32-byte lowercase hex`);
  }
};

export const assertNativeMpfOpaqueId = (
  value: Uint8Array,
  field: string,
): void => {
  if (value.byteLength !== 16) {
    throw new Error(`${field} must contain exactly 16 bytes`);
  }
};

export const assertNativeMpfGenerationHandle = (
  handle: NativeMpfGenerationHandle,
): void => {
  assertNativeMpfOpaqueId(handle.ownerEpoch, "ownerEpoch");
  assertNativeMpfOpaqueId(handle.generationId, "generationId");
  assertNativeMpfHashHex(handle.baseRoot, "baseRoot");
};
