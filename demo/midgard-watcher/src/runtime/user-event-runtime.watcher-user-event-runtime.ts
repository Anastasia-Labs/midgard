import { type FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";

import { type WatcherStateQueueHeaderObservation } from "../indexers/authenticated-state-queue-observation.js";
import { createWatcherLocalUserEventPublisher } from "../indexers/user-event-history.js";
import {
  type WatcherLocalUserEventAuthority,
  type WatcherUserEventKind,
} from "../indexers/user-event-indexer.js";
import { type WatcherLocalBackfillFinalityReceipt } from "../l1/finality-engine.js";
import {
  openWatcherLocalHistoricalCapture,
  readWatcherLocalHistoricalCapture,
} from "../l1/local-historical-capture.js";
import { type WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import { type WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import {
  type WatcherBlockRelevance,
  type WatcherBlockRelevancePolicy,
} from "./block-relevance.js";
import { WATCHER_PACKAGE_NAME } from "./scaffold.js";

export const MAX_BATCH = 64;

export const MAX_EVIDENCE_BYTES = 128 * 1024 * 1024;

export const ACQUISITION_TIMEOUT_MS = 120_000;

/** The monitor wakes a pending read early; periodic queries prevent stale hints. */
export const NO_GROWTH_PROBE_INTERVAL_MS = 10_000;

/** Quiet headers admitted per enumeration round before the round settles. */
export const MAX_ROUND_ITEMS = 4_096;

/** Recently covered chain points kept for hash lookups without a request. */
export const COVERED_RING_CAPACITY = 4_096;

/** One node lookup resolving whether a point is on the canonical chain. */
export const LOOKUP_TIMEOUT_MS = 20_000;

export type Capture = Awaited<
  ReturnType<typeof openWatcherLocalHistoricalCapture>
>;

export type Publisher = Awaited<
  ReturnType<typeof createWatcherLocalUserEventPublisher>
>;

export type Pair = Parameters<Publisher["publish"]>[0] &
  Readonly<{ close(): Promise<void> }>;

export type PlannedBlock = Readonly<{
  point: FraudProofRawL1Point;
  rawBlockCbor: string;
}>;

export type QuietHeader = Readonly<{
  blockHash: string;
  parentBlockHash: string;
  blockNo: string;
  slot: string;
}>;

export type RoundItem =
  | Readonly<{ kind: "quiet"; header: QuietHeader }>
  | Readonly<{ kind: "touched"; plan: PlannedBlock }>;

export type FirstObservation = Readonly<{
  plan: Readonly<{ point: FraudProofRawL1Point; rawBlockCbor?: string }>;
  finality: WatcherLocalBackfillFinalityReceipt;
  tip: ReturnType<
    typeof readWatcherLocalHistoricalCapture
  >["observedNativeTip"];
  raw: string;
  evidenceBytes: number;
}>;

/**
 * Recently covered chain points by height, event and quiet alike, so that a
 * point inside the covered stretch resolves without any request. Nothing here
 * is authority: the publisher's coverage checkpoint and the durable row are,
 * and a miss falls back to one node lookup.
 */
export const createWatcherUserEventCoveredRing = () => {
  const coveredRing = new Map<
    string,
    Readonly<{ blockHash: string; slot: string }>
  >();
  const remember = (point: Readonly<QuietHeader | FraudProofRawL1Point>) => {
    coveredRing.delete(point.blockNo);
    coveredRing.set(
      point.blockNo,
      Object.freeze({ blockHash: point.blockHash, slot: point.slot }),
    );
    while (coveredRing.size > COVERED_RING_CAPACITY) {
      const oldest = coveredRing.keys().next();
      if (oldest.done) break;
      coveredRing.delete(oldest.value);
    }
  };
  const forgetAbove = (blockNo: bigint) => {
    for (const height of coveredRing.keys())
      if (BigInt(height) > blockNo) coveredRing.delete(height);
  };
  const rememberedHeight = (
    point: Readonly<{ blockHash: string; slot: string }>,
  ): string | null => {
    for (const [height, known] of coveredRing)
      if (known.blockHash === point.blockHash && known.slot === point.slot)
        return height;
    return null;
  };
  return { coveredRing, remember, forgetAbove, rememberedHeight };
};

/** One warning per L1 outage seen by a user-event operation. */
export type WatcherUserEventL1Wait = Readonly<{
  event: "user_event_l1_wait";
  error: string;
  retryAfterMs: number;
}>;

const writeL1Wait = (warning: WatcherUserEventL1Wait): void => {
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", ...warning })}\n`,
  );
};

/** Retry options for one operation: warns on its first wait only. */
export const watcherUserEventL1Wait =
  (warn: ((warning: WatcherUserEventL1Wait) => void) | undefined) =>
  (signal: AbortSignal) =>
    Object.freeze({
      signal,
      onRetry: (error: Error, retry: number, retryAfterMs: number) => {
        if (retry === 1)
          (warn ?? writeL1Wait)({
            event: "user_event_l1_wait",
            error: error.message,
            retryAfterMs,
          });
      },
    });

export const runtimeBrand = Symbol("watcher-user-event-runtime");

/** An operation cancelled by an authenticated-source generation change. */
export class WatcherUserEventOperationRetired extends Error {}

export type WatcherUserEventRuntime = Readonly<{
  [runtimeBrand]: true;
  deploymentFingerprint: string;
  blueprintHash: string;
  read(): Readonly<{
    status: "ready" | "suspended" | "closed" | "failed";
    /** The coverage checkpoint: every block through it is covered. */
    currentPoint: FraudProofRawL1Point;
    /** The last event observation the coverage checkpoint sits on. */
    headCursor: FraudProofRawL1Point;
    generation: number;
  }>;
  /** The deployment's relevance predicate, shared with the coordinator. */
  relevancePolicy: WatcherBlockRelevancePolicy;
  /**
   * Classifies a native block from its bytes with the deployment predicate
   * plus the active event outrefs of the published fold and any extra
   * tracked outrefs the caller follows. Never asks a provider.
   */
  classify(
    block: WatcherNativeBlockAdmission,
    extraTrackedOutRefs?: Iterable<string>,
  ): WatcherBlockRelevance;
  /**
   * Covers one quiet native block. The direct child of the coverage
   * checkpoint costs a link check and one in-place row write; a block already
   * covered is verified against the covered chain, and a gap is enumerated
   * natively through the block.
   */
  coverQuiet(block: WatcherNativeBlockAdmission): Promise<void>;
  /**
   * Covers every block through `point`, capturing and publishing only the
   * blocks the relevance predicate marks touched. A point already covered is
   * verified against the covered chain instead.
   */
  advanceThrough(point: FraudProofRawL1Point): Promise<void>;
  eventAuthority(
    input: Readonly<{
      kind: WatcherUserEventKind;
      eventId: string;
      throughHeader: WatcherStateQueueHeaderObservation;
    }>,
  ): Promise<WatcherLocalUserEventAuthority>;
  handleRollback(point: WatcherNativeChainSyncPoint): Promise<void>;
  close(): Promise<void>;
  done: Promise<void>;
}>;

export const runtimes = new WeakMap<object, () => void>();

export const assertWatcherUserEventRuntime = (
  value: WatcherUserEventRuntime,
): void => {
  const assertLive = runtimes.get(value);
  if (assertLive === undefined)
    throw new Error("User-event runtime is not privately admitted");
  assertLive();
};
