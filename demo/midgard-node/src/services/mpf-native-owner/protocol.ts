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

export interface NativeMpfOwnerService extends NativeMpfOwnerClient {
  createWorkerPort(): MessagePort;
  promote(handle: NativeMpfGenerationHandle): Promise<void>;
  recover(replay: PersistedNativeMpfReplay): Promise<void>;
  diagnostics(): Promise<NativeMpfOwnerDiagnostics>;
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
