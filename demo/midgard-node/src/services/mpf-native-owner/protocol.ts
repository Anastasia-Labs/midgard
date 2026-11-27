import type { MessagePort } from "node:worker_threads";

import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";

export const NATIVE_MPF_RPC_MAGIC = "MGRP" as const;
export const NATIVE_MPF_RPC_SCHEMA = 2 as const;
export const NATIVE_MPF_RPC_DIGEST_DOMAIN = "MIDGARD-MPF-OWNER-RPC-V2" as const;
/** The schema of a replay the block journal keeps (its event log and
 * roots), which the RPC schema does not change. */
export const NATIVE_MPF_REPLAY_SCHEMA = 1 as const;

export const NATIVE_MPF_OWNER_DEFAULT_CAPS = {
  maxFrameBytes: 64 * 1024 * 1024,
  maxChunkBytes: 16 * 1024 * 1024,
  maxGeneratedNodes: 1_000_000,
  maxGeneratedBytes: 1024 * 1024 * 1024,
  maxEvents: 100_000,
  maxOps: 400_000,
  maxActiveGenerations: 2,
  handshakeTimeoutMs: 5_000,
  requestTimeoutMs: 120_000,
  applyTimeoutMs: 30_000,
  promotionTimeoutMs: 120_000,
  shutdownTimeoutMs: 10_000,
} as const;

/** The byte budget of the child's trie node cache, sent in Hello. At about
 * 470 bytes a node it holds the whole trie up to roughly a million UTxOs;
 * above that the colder nodes are read from the store again. */
export const NATIVE_MPF_OWNER_NODE_CACHE_BYTES = 1024 * 1024 * 1024;

/** The most rounds one apply may take. Each NeedNodes round reaches at
 * least one node further down every path it stops on, and a path has at
 * most 65 nodes (a branch per key nibble, then the leaf); one more round
 * fetches the siblings a delete collapsing at the bottom needs. */
export const NATIVE_MPF_APPLY_MAX_NODE_ROUNDS = 64 + 1 + 1;

export const enum NativeMpfRpcKind {
  Hello = 1,
  HelloAck = 2,
  SetRoot = 3,
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
  NeedNodes = 24,
  ProvideNodes = 25,
  NodesProvided = 26,
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
  readonly schema: typeof NATIVE_MPF_REPLAY_SCHEMA;
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
  readonly cachedNodes: number;
  readonly cachedBytes: number;
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

/** `restoreCanonicalRoot` refused a target the native MPF store does not
 * retain. Raised for nothing else; the restore changed nothing. */
export class NativeMpfRootNotRetained extends Error {
  readonly _tag = "NativeMpfRootNotRetained";
  constructor(
    readonly targetRoot: string,
    options?: ErrorOptions,
  ) {
    super(
      `Native MPF canonical recovery target root ${targetRoot} is not retained; refusing to restore`,
      options,
    );
  }
}

/** `restoreCanonicalRoot` could not read from the native MPF store whether
 * it retains the target root. Transient: the restore changed nothing, and a
 * later restore reads it again. */
export class NativeMpfRestoreReadFailed extends Error {
  readonly _tag = "NativeMpfRestoreReadFailed";
  constructor(
    readonly targetRoot: string,
    options: ErrorOptions & { readonly cause: unknown },
  ) {
    super(
      `Native MPF canonical recovery could not read whether the native MPF store retains target root ${targetRoot}: ${options.cause instanceof Error ? options.cause.message : String(options.cause)}`,
      options,
    );
  }
}

/** An apply from `root` needed trie nodes the native MPF store does not
 * hold: `missing` are the hashes the child asked for that are absent. The apply
 * changed nothing, and an apply that needs none of them still runs. */
export class NativeMpfClosureIncomplete extends Error {
  readonly _tag = "NativeMpfClosureIncomplete";
  constructor(
    readonly root: string,
    readonly missing: readonly string[],
  ) {
    super(
      `Native MPF store is missing ${missing.length.toString()} trie node(s) under root ${root} that the apply needs, first ${missing[0] ?? ""}`,
    );
  }
}

export interface NativeMpfOwnerService extends NativeMpfOwnerClient {
  createWorkerPort(): MessagePort;
  promote(handle: NativeMpfGenerationHandle): Promise<void>;
  recover(replay: PersistedNativeMpfReplay): Promise<void>;
  restoreCanonicalRoot(plan: NativeMpfCanonicalRootRecovery): Promise<void>;
  /**
   * Keeps the ledgers of `roots` and of the roots the owner needs itself
   * (its durable root, its `__root__` marker and every leased fork base),
   * and drops every other one. Resolves once the store's deletion has
   * ended, to the roots of `roots` the store does not retain.
   */
  retain(
    roots: ReadonlySet<string>,
  ): Promise<{ readonly notRetained: ReadonlySet<string> }>;
  /**
   * The ledger with `root` from the owner's ledger store (`LedgerStore.at`):
   * refused with `LedgerNotRetained` when the store does not retain `root`.
   * The caller may apply to it and keep the result with `persistLedger`.
   */
  ledger(root: string): Promise<Ledger>;
  /**
   * Keeps `ledger`, one `ledger` returned and applied to, in the owner's
   * ledger store and retains its root (`LedgerStore.persist`); the durable
   * root does not move. Keeping a retained root changes nothing.
   */
  persistLedger(ledger: Ledger): Promise<void>;
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
