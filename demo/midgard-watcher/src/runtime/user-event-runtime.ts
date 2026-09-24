/** Owns native acquisition, bounded semantic publication and rollback fencing. */
import {
  admitFraudProofRawL1Point,
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  createWatcherLocalUserEventPublisher,
  replaceWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../indexers/user-event-history.js";
import {
  isWatcherLocalUserEventAuthorityUnavailable,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  type WatcherLocalUserEventAuthority,
  type WatcherUserEventKind,
} from "../indexers/user-event-indexer.js";
import { admitWatcherUserEventOrigin } from "../indexers/user-event-origin.js";
import {
  createWatcherLocalBackfillUserEventReferenceAuthority,
  readWatcherUserEventReferenceEvidence,
} from "../indexers/user-event-reference-authority.js";
import {
  admitWatcherLocalBackfillFinality,
  makeWatcherFinalityPolicy,
  readWatcherLocalBackfillFinality,
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import {
  admitWatcherLocalBackfillObservation,
  readWatcherLocalBackfillObservation,
} from "../l1/l1-adapter.js";
import {
  openWatcherLocalHistoricalCapture,
  readWatcherLocalHistoricalCapture,
} from "../l1/local-historical-capture.js";
import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import {
  admitWatcherNativeRollForwardBlock,
  type WatcherNativeBlockAdmission,
} from "../l1/native-block-admission.js";
import {
  startWatcherNativeChainSync,
  startWatcherNativeChainSyncWithRetry,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
} from "../l1/native-chain-sync.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
} from "../storage/durable-runtime.js";
import { watcherSameCanonicalJson as same } from "../storage/durable-store.js";
import type { WatcherUserEventArchive } from "../storage/user-event-checkpoint.js";
import type { WatcherUserEventCoverageStore } from "../storage/user-event-coverage-store.js";
import {
  classifyWatcherNativeBlock,
  makeWatcherDeploymentBlockRelevancePolicy,
  type WatcherBlockRelevance,
  type WatcherBlockRelevancePolicy,
} from "./block-relevance.js";
import { parseWatcherConfig } from "./config.js";
import {
  assertWatcherVerifiedDeploymentAuthority,
  type VerifiedWatcherDeploymentAuthority,
} from "./deployment-authority.js";
import {
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
} from "./deployment-identity.js";

const MAX_BATCH = 64;
const MAX_EVIDENCE_BYTES = 128 * 1024 * 1024;
const ACQUISITION_TIMEOUT_MS = 120_000;
/** The monitor wakes a pending read early; periodic queries prevent stale hints. */
const NO_GROWTH_PROBE_INTERVAL_MS = 10_000;
/** Quiet headers admitted per enumeration round before the round settles. */
const MAX_ROUND_ITEMS = 4_096;
/** Recently covered chain points kept for hash lookups without a request. */
const COVERED_RING_CAPACITY = 4_096;
/** One node lookup resolving whether a point is on the canonical chain. */
const LOOKUP_TIMEOUT_MS = 20_000;
type Capture = Awaited<ReturnType<typeof openWatcherLocalHistoricalCapture>>;
type Publisher = Awaited<
  ReturnType<typeof createWatcherLocalUserEventPublisher>
>;
type Pair = Parameters<Publisher["publish"]>[0] &
  Readonly<{ close(): Promise<void> }>;
type PlannedBlock = Readonly<{
  point: FraudProofRawL1Point;
  rawBlockCbor: string;
}>;
type QuietHeader = Readonly<{
  blockHash: string;
  parentBlockHash: string;
  blockNo: string;
  slot: string;
}>;
type RoundItem =
  | Readonly<{ kind: "quiet"; header: QuietHeader }>
  | Readonly<{ kind: "touched"; plan: PlannedBlock }>;
type FirstObservation = Readonly<{
  plan: Readonly<{ point: FraudProofRawL1Point; rawBlockCbor?: string }>;
  finality: WatcherLocalBackfillFinalityReceipt;
  tip: ReturnType<
    typeof readWatcherLocalHistoricalCapture
  >["observedNativeTip"];
  raw: string;
  evidenceBytes: number;
}>;
const runtimeBrand = Symbol("watcher-user-event-runtime");
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
const runtimes = new WeakMap<object, () => void>();
export const assertWatcherUserEventRuntime = (
  value: WatcherUserEventRuntime,
): void => {
  const assertLive = runtimes.get(value);
  if (assertLive === undefined)
    throw new Error("User-event runtime is not privately admitted");
  assertLive();
};

/** No caller-supplied history, origin, pair factory or publication callback can
 * enter this owner. Discovery data is corroborated by actual native W12 capture. */
export const createWatcherUserEventRuntime = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentAuthority: VerifiedWatcherDeploymentAuthority;
    blueprintBytes: Uint8Array;
    nativeChainSyncBinaryPath: string;
    runtime: WatcherDurableRuntime;
    archive: WatcherUserEventArchive;
    /** The single mutable coverage record, authenticated by the store. */
    coverage: WatcherUserEventCoverageStore;
    signal?: AbortSignal;
  }>,
): Promise<WatcherUserEventRuntime> => {
  const {
    runtime: durableRuntime,
    archive,
    coverage: coverageStore,
    nativeChainSyncBinaryPath,
    signal: requestSignal,
  } = input;
  assertWatcherVerifiedDeploymentAuthority(input.deploymentAuthority);
  const deploymentIdentity = input.deploymentAuthority.deploymentIdentity;
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  const scriptBinding = verifyWatcherUserEventScriptBinding({
    deploymentIdentity,
    blueprintBytes: input.blueprintBytes,
  });
  const scripts = readWatcherUserEventScriptBinding({
    binding: scriptBinding,
    deploymentIdentity,
  });
  const relevancePolicy = makeWatcherDeploymentBlockRelevancePolicy({
    deploymentIdentity,
    scripts,
  });
  const finalityPolicy = makeWatcherFinalityPolicy(
    watcherConfig,
    deploymentIdentity,
  );
  if (finalityPolicy === null)
    throw new Error("User-event runtime finality policy is unavailable");
  const confirmationDepth = BigInt(finalityPolicy.confirmationDepth);
  const shutdown = new AbortController();
  const signal =
    requestSignal === undefined
      ? shutdown.signal
      : AbortSignal.any([shutdown.signal, requestSignal]);
  signal.throwIfAborted();
  let status: "ready" | "suspended" | "closed" | "failed" = "ready";
  let generation = 0;
  let publisher: Publisher | null = null;
  let nativeMonitor: WatcherNativeChainSyncRuntime | null = null;
  /** The node tip the monitor stream last reported; bounds release finality. */
  let monitorTip: Readonly<{ blockNo: string }> | null = null;
  let operationAbort = new AbortController();
  let tail: Promise<unknown> = Promise.resolve();
  let recovery: Promise<void> | null = null;
  let recoveryPoint: WatcherNativeChainSyncPoint | null = null;
  let headLease: Pair | null = null;
  // Closed first captures are private W12 facts, never published authority.
  // Publication still follows each caller's exact requested prefix.
  const prefetchedFirst = new Map<string, FirstObservation>();
  // Recently covered chain points by height, event and quiet alike, so that
  // a point inside the covered stretch resolves without any request. Nothing
  // here is authority: the publisher's coverage checkpoint and the durable
  // row are, and a miss falls back to one node lookup.
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
  const captures = new Set<Capture>();
  const streams = new Set<WatcherNativeChainSyncRuntime>();
  let resolveDone!: () => void;
  let rejectDone!: (error: unknown) => void;
  const done = new Promise<void>((resolve, reject) => {
    resolveDone = resolve;
    rejectDone = reject;
  });
  void done.catch(() => undefined);
  const releaseCapture = async (capture: Capture) => {
    captures.delete(capture);
    await capture.close();
  };
  const releaseLease = () => {
    const lease = headLease;
    headLease = null;
    return lease?.close() ?? Promise.resolve();
  };
  const cleanup = async () => {
    await Promise.allSettled([...captures].map(releaseCapture));
    await Promise.allSettled(
      [...streams].map(async (stream) => {
        streams.delete(stream);
        await stream.close();
      }),
    );
  };
  const fail = (error: unknown) => {
    if (status === "closed" || status === "failed") return;
    status = "failed";
    generation += 1;
    prefetchedFirst.clear();
    publisher?.close();
    shutdown.abort(error);
    rejectDone(error);
    void cleanup();
  };
  const assertReady = () => {
    signal.throwIfAborted();
    if (status !== "ready") throw new Error(`User-event runtime is ${status}`);
    if (
      nativeMonitor !== null &&
      watcherNativeChainSyncAuthorityDetails(nativeMonitor.authority) === null
    )
      throw new Error("User-event native monitor is no longer live");
    const finality = durableRuntime.readFinality();
    if (finality.phase === "quarantined" || finality.incident !== null)
      throw new Error("User-event durable runtime is quarantined");
  };
  const captureAt = async (
    point: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ) => {
    const capture = await openWatcherLocalHistoricalCapture({
      watcherConfig,
      deploymentIdentity,
      nativeChainSyncBinaryPath: nativeChainSyncBinaryPath,
      point,
      signal: operationSignal,
      limits: {
        timeoutMs: ACQUISITION_TIMEOUT_MS,
        maxRawResponseBytes: 8 * 1024 * 1024,
      },
    });
    captures.add(capture);
    try {
      operationSignal.throwIfAborted();
      return capture;
    } catch (error) {
      await releaseCapture(capture);
      throw error;
    }
  };
  const pause = async (operationSignal: AbortSignal) => {
    await new Promise<void>((resolve, reject) => {
      operationSignal.throwIfAborted();
      const finish = () => {
        operationSignal.removeEventListener("abort", abort);
        resolve();
      };
      const timer = setTimeout(finish, 1000);
      const abort = () => {
        clearTimeout(timer);
        reject(
          new Error("User-event acquisition aborted", {
            cause: operationSignal.reason,
          }),
        );
      };
      operationSignal.addEventListener("abort", abort, { once: true });
    });
  };
  const evidenceSize = (value: unknown) =>
    Buffer.byteLength(JSON.stringify(value));
  /** Closed captures retain W12's private first-observation facts only. Each
   * block keeps its own actual observed tip for the later growth check. */
  const acquireFirst = async (
    plans: readonly FirstObservation["plan"][],
    operationSignal: AbortSignal,
  ): Promise<
    Readonly<{ observations: readonly FirstObservation[]; bytes: number }>
  > => {
    if (plans.length === 0 || plans.length > MAX_BATCH)
      throw new Error("Invalid user-event acquisition batch");
    const first: FirstObservation[] = [];
    let bytes = plans.reduce(
      (total, plan) => total + Buffer.byteLength(plan.rawBlockCbor ?? ""),
      0,
    );
    for (const plan of plans) {
      const capture = await captureAt(plan.point, operationSignal);
      try {
        const read = readWatcherLocalHistoricalCapture(capture.receipt);
        if (
          plan.rawBlockCbor !== undefined &&
          read.nativeBlock.rawBlockCbor !== plan.rawBlockCbor
        )
          throw new Error(
            "Native enumeration and W12 whole-block bytes differ",
          );
        const observation = admitWatcherLocalBackfillObservation(
          capture.receipt,
        );
        const step = admitWatcherLocalBackfillFinality({
          watcherConfig,
          deploymentIdentity,
          observation,
          previous: null,
        });
        if (step.admitted === null)
          throw new Error("User-event first observation was not admitted");
        const size = evidenceSize({
          observation: readWatcherLocalBackfillObservation(observation),
          finality: readWatcherLocalBackfillFinality(step.admitted),
        });
        if (first.length > 0 && bytes + size > MAX_EVIDENCE_BYTES / 3) break;
        if (bytes + size > MAX_EVIDENCE_BYTES / 3)
          throw new Error(
            "User-event first observation exceeds batch evidence bound",
          );
        first.push(
          Object.freeze({
            plan,
            finality: step.admitted,
            tip: read.observedNativeTip,
            raw: read.nativeBlock.rawBlockCbor,
            evidenceBytes: size + Buffer.byteLength(plan.rawBlockCbor ?? ""),
          }),
        );
        bytes += size;
      } finally {
        await releaseCapture(capture);
      }
    }
    return Object.freeze({ observations: Object.freeze(first), bytes });
  };
  // The caller owns this array: expired paired predecessors are single-use,
  // so renewal replaces their entries before unused first facts are restored.
  const completeFirst = async (
    first: FirstObservation[],
    initialBytes: number,
    operationSignal: AbortSignal,
  ): Promise<Pair[]> => {
    for (;;) {
      const pairs: Pair[] = [];
      let bytes = initialBytes;
      let pairedDeadline = Infinity;
      const freshPrefix = () => {
        for (const pair of pairs)
          readWatcherLocalBackfillObservation(pair.observation);
        return pairs;
      };
      let restart = false;
      try {
        for (const step of first) {
          let nextProbeAt = 0;
          for (;;) {
            operationSignal.throwIfAborted();
            // Block production has no wall-clock bound. Keep first facts while
            // no tip grows, with no live second-capture authority held waiting.
            if (performance.now() >= pairedDeadline) {
              restart = true;
              break;
            }
            if (
              (monitorTip === null ||
                BigInt(monitorTip.blockNo) <= BigInt(step.tip.blockNo)) &&
              performance.now() < nextProbeAt
            ) {
              await pause(operationSignal);
              continue;
            }
            const capture = await captureAt(step.plan.point, operationSignal);
            let retained = false;
            try {
              const read = readWatcherLocalHistoricalCapture(capture.receipt);
              if (read.nativeBlock.rawBlockCbor !== step.raw)
                throw new Error(
                  "User-event native block changed between observations",
                );
              // A later capture may consume the lifetime of an earlier pair.
              // Retire that partial batch explicitly, never mask admission errors.
              if (performance.now() >= pairedDeadline) {
                restart = true;
                break;
              }
              if (
                BigInt(read.observedNativeTip.blockNo) <=
                BigInt(step.tip.blockNo)
              ) {
                await releaseCapture(capture);
                if (pairs.length > 0) {
                  if (performance.now() >= pairedDeadline) {
                    restart = true;
                    break;
                  }
                  return freshPrefix();
                }
                nextProbeAt = performance.now() + NO_GROWTH_PROBE_INTERVAL_MS;
                await pause(operationSignal);
                continue;
              }
              const observation = admitWatcherLocalBackfillObservation(
                capture.receipt,
              );
              const admitted = admitWatcherLocalBackfillFinality({
                watcherConfig,
                deploymentIdentity,
                observation,
                previous: step.finality,
              }).admitted;
              if (admitted === null)
                throw new Error(
                  "User-event second observation was not finalized",
                );
              const referenceAuthority =
                createWatcherLocalBackfillUserEventReferenceAuthority({
                  deploymentIdentity,
                  finality: admitted,
                  observation,
                });
              const size = evidenceSize({
                witness: readWatcherLocalBackfillFinalityOriginalWitness({
                  finality: admitted,
                  observation,
                }),
                referenceEvidence:
                  readWatcherUserEventReferenceEvidence(referenceAuthority),
              });
              if (bytes + size > MAX_EVIDENCE_BYTES)
                throw new Error(
                  "User-event paired evidence exceeds batch evidence bound",
                );
              pairs.push(
                Object.freeze({
                  finality: admitted,
                  observation,
                  referenceAuthority,
                  close: async () => {
                    await releaseCapture(capture);
                  },
                }),
              );
              bytes += size;
              pairedDeadline = Math.min(
                pairedDeadline,
                read.startedAtMonotonicMs + ACQUISITION_TIMEOUT_MS,
                performance.now() +
                  Math.max(0, Date.parse(read.expiresAt) - Date.now()),
              );
              retained = true;
              break;
            } finally {
              if (!retained && captures.has(capture))
                await releaseCapture(capture);
            }
          }
          if (restart) break;
        }
        if (!restart && performance.now() < pairedDeadline)
          return freshPrefix();
        await Promise.all(pairs.map((pair) => pair.close()));
        let consumedBytes = 0;
        const renewalPlans = first.splice(0, pairs.length).map((step) => {
          consumedBytes += step.evidenceBytes;
          return { ...step.plan, rawBlockCbor: step.raw };
        });
        pairs.length = 0;
        const renewed = await acquireFirst(renewalPlans, operationSignal);
        if (renewed.observations.length !== renewalPlans.length)
          throw new Error(
            "User-event expired prefix renewal exceeds batch bound",
          );
        initialBytes += renewed.bytes - consumedBytes;
        if (initialBytes > MAX_EVIDENCE_BYTES / 3)
          throw new Error(
            "User-event renewed first evidence exceeds batch bound",
          );
        first.unshift(...renewed.observations);
      } catch (error) {
        await Promise.allSettled(pairs.map((pair) => pair.close()));
        throw error;
      }
    }
  };
  const acquireBatch = async (
    plans: readonly FirstObservation["plan"][],
    operationSignal: AbortSignal,
  ): Promise<Pair[]> => {
    return await acquireFirst(plans, operationSignal).then((first) =>
      completeFirst([...first.observations], first.bytes, operationSignal),
    );
  };
  const onePair = async (
    point: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ) => (await acquireBatch([{ point }], operationSignal))[0]!;
  const rawPoint = (
    point: Readonly<{ blockHash: string; blockNo: string; slot: string }>,
  ): FraudProofRawL1Point =>
    admitFraudProofRawL1Point({
      blockHash: point.blockHash,
      blockNo: point.blockNo,
      slot: point.slot,
      pointId: computeFraudProofRawL1PointId({
        blockHash: point.blockHash,
        blockNo: point.blockNo,
        slot: point.slot,
      }),
    });
  const headCursor = (): FraudProofRawL1Point => {
    const cursor = publisher!.read().cursor;
    if (cursor === null)
      throw new Error("User-event history has no published head");
    return rawPoint(cursor);
  };
  const coveragePoint = (): FraudProofRawL1Point => {
    const coverage = publisher!.readCoverage();
    if (coverage === null)
      throw new Error("User-event history has no coverage checkpoint");
    return rawPoint(coverage.point);
  };
  // A suspended, closed or failed history refuses reads; `read()` then
  // reports the last points it observed while the history was live.
  let lastKnownPoints: Readonly<{
    currentPoint: FraudProofRawL1Point;
    headCursor: FraudProofRawL1Point;
  }> | null = null;
  const readPoints = () => {
    try {
      const current = Object.freeze({
        currentPoint: coveragePoint(),
        headCursor: headCursor(),
      });
      lastKnownPoints = current;
      return current;
    } catch (error) {
      if (lastKnownPoints === null || status === "ready") throw error;
      return lastKnownPoints;
    }
  };
  /** Writes the single coverage row in place. It references the head entry
   * and the checkpoint that published it, so a torn write between a head
   * publication and this update is detected and discarded on restore. */
  const persistCoverage = () => {
    const read = publisher!.read();
    const coverage = publisher!.readCoverage();
    if (coverage === null || read.checkpoint === null)
      throw new Error("User-event coverage cannot be persisted before a head");
    coverageStore.write({
      blockHash: coverage.point.blockHash,
      blockNo: coverage.point.blockNo,
      slot: coverage.point.slot,
      headEntryDigest: coverage.headEntryDigest,
      checkpointDigest: read.checkpoint.checkpointDigest,
    });
  };
  const trackedOutRefs = (): readonly string[] =>
    publisher === null
      ? []
      : publisher.read().snapshot.activeEvents.map((event) => event.outRef);
  const classify = (
    block: WatcherNativeBlockAdmission,
    extraTrackedOutRefs?: Iterable<string>,
  ): WatcherBlockRelevance =>
    classifyWatcherNativeBlock({
      block,
      policy: relevancePolicy,
      trackedOutRefs: [...trackedOutRefs(), ...(extraTrackedOutRefs ?? [])],
    });
  /** Admits one quiet block as the direct child of the coverage checkpoint. */
  const coverQuietHeader = (header: QuietHeader) => {
    publisher!.advanceCoverage(header);
    persistCoverage();
    remember(header);
  };
  /**
   * One node lookup: is the block with this hash on the canonical chain, and
   * at which height? Chain-sync intersects at the point; the node refuses an
   * unknown point, and the first roll forward (or the reported tip when the
   * point is the tip) names its height.
   */
  const lookupOnChain = async (
    point: Readonly<{ blockHash: string; slot: string }>,
    operationSignal: AbortSignal,
  ): Promise<string | null> => {
    let stream: WatcherNativeChainSyncRuntime | null = null;
    let settled = false;
    let resolveHeight!: (value: string | null) => void;
    let rejectHeight!: (error: unknown) => void;
    const height = new Promise<string | null>((resolve, reject) => {
      resolveHeight = resolve;
      rejectHeight = reject;
    });
    void height.catch(() => undefined);
    const settle = (value: string | null) => {
      if (settled) return;
      settled = true;
      resolveHeight(value);
    };
    const abort = () => {
      if (settled) return;
      settled = true;
      rejectHeight(
        new Error("User-event lookup aborted", {
          cause: operationSignal.reason,
        }),
      );
    };
    operationSignal.throwIfAborted();
    operationSignal.addEventListener("abort", abort, { once: true });
    const timer = setTimeout(() => {
      if (settled) return;
      settled = true;
      rejectHeight(
        new Error(
          "User-event point is not the exact accepted block: the node did not confirm it on its chain",
        ),
      );
    }, LOOKUP_TIMEOUT_MS);
    try {
      try {
        stream = await startWatcherNativeChainSync({
          binaryPath: nativeChainSyncBinaryPath,
          watcherConfig,
          intersection: {
            kind: "point",
            blockHash: point.blockHash,
            slot: point.slot,
          },
          startupTimeoutMs: LOOKUP_TIMEOUT_MS,
          onEvent: async (event) => {
            if (settled) return;
            if (event.kind === "roll_backward") {
              if (
                event.point.kind === "point" &&
                event.point.blockHash === point.blockHash &&
                event.point.slot === point.slot
              )
                return;
              settle(null);
              return;
            }
            settle(
              event.prevHash === point.blockHash
                ? (BigInt(event.blockNo) - 1n).toString()
                : null,
            );
          },
        });
      } catch (error) {
        if (
          error instanceof Error &&
          error.name === "NativeChainSyncStartupFailure" &&
          (error as { code?: unknown }).code === "intersection_failed"
        )
          return null;
        throw error;
      }
      streams.add(stream);
      const details = watcherNativeChainSyncAuthorityDetails(stream.authority);
      if (
        details !== null &&
        details.currentTip.kind === "point" &&
        details.currentTip.blockHash === point.blockHash &&
        details.currentTip.slot === point.slot
      )
        settle(details.currentTip.blockNo);
      void stream.done.then(() => settle(null), rejectHeight);
      return await height;
    } finally {
      settled = true;
      clearTimeout(timer);
      operationSignal.removeEventListener("abort", abort);
      if (stream !== null) {
        streams.delete(stream);
        await stream.close();
      }
    }
  };
  /**
   * Point coverage is a lookup, not a walk. An event block must be the exact
   * accepted block. A quiet block at or below the release-final boundary is
   * covered by height; above it, its hash must be on the covered chain: the
   * recent ring answers without a request, and a miss costs one node lookup.
   */
  const assertCovered = async (
    point: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ) => {
    const head = headCursor();
    if (BigInt(point.blockNo) <= BigInt(head.blockNo)) {
      const kind = await publisher!.assertPointCovered(point);
      if (kind === "event") return;
    }
    const known = coveredRing.get(point.blockNo);
    if (known !== undefined) {
      if (known.blockHash === point.blockHash && known.slot === point.slot)
        return;
      throw new Error(
        "User-event point is not the exact accepted block at its covered height",
      );
    }
    if (
      monitorTip !== null &&
      BigInt(monitorTip.blockNo) - BigInt(point.blockNo) >= confirmationDepth
    )
      return;
    const height = await lookupOnChain(point, operationSignal);
    if (height !== point.blockNo)
      throw new Error(
        "User-event point is not the exact accepted block on the canonical chain",
      );
    remember(point);
  };
  /**
   * Streams the native chain above the coverage checkpoint and classifies
   * every block locally. Quiet headers are admitted to coverage; touched
   * blocks are planned for capture. A round ends at `through`, at the touched
   * bound, or at the header bound, so a long quiet stretch settles in bounded
   * rounds that publish nothing.
   */
  const enumerateRound = async (
    from: FraudProofRawL1Point,
    through: FraudProofRawL1Point | Readonly<{ blockNo: string }>,
    maximumTouched: number,
    operationSignal: AbortSignal,
    replayAll = false,
  ): Promise<readonly RoundItem[]> => {
    if (maximumTouched < 1 || maximumTouched > MAX_BATCH)
      throw new Error("Invalid user-event enumeration bound");
    const items: RoundItem[] = [];
    let previous = from;
    let sawEnumerationEvent = false;
    let touched = 0;
    let plannedBytes = 0;
    let finished = false;
    let resolveBatch!: () => void;
    let rejectBatch!: (error: unknown) => void;
    const collected = new Promise<void>((resolve, reject) => {
      resolveBatch = resolve;
      rejectBatch = reject;
    });
    void collected.catch(() => undefined);
    const abort = () => {
      finished = true;
      rejectBatch(operationSignal.reason);
    };
    operationSignal.throwIfAborted();
    operationSignal.addEventListener("abort", abort, { once: true });
    const timer = setTimeout(() => {
      finished = true;
      rejectBatch(new Error("User-event native enumeration timed out"));
    }, ACQUISITION_TIMEOUT_MS);
    let stream: WatcherNativeChainSyncRuntime | null = null;
    try {
      stream = await startWatcherNativeChainSync({
        binaryPath: nativeChainSyncBinaryPath,
        watcherConfig,
        intersection: {
          kind: "point",
          blockHash: from.blockHash,
          slot: from.slot,
        },
        startupTimeoutMs: ACQUISITION_TIMEOUT_MS,
        onEvent: async (event) => {
          if (finished) return;
          try {
            operationSignal.throwIfAborted();
            const firstEvent = !sawEnumerationEvent;
            sawEnumerationEvent = true;
            if (event.kind === "roll_backward") {
              if (
                firstEvent &&
                event.point.kind === "point" &&
                event.point.blockHash === from.blockHash &&
                event.point.slot === from.slot
              )
                return;
              throw new Error("User-event native enumeration rolled back");
            }
            const point = rawPoint(event);
            if (
              event.prevHash !== previous.blockHash ||
              BigInt(point.blockNo) !== BigInt(previous.blockNo) + 1n ||
              BigInt(point.slot) <= BigInt(previous.slot) ||
              BigInt(point.blockNo) > BigInt(through.blockNo)
            )
              throw new Error(
                "User-event native enumeration is not a strict contiguous prefix",
              );
            if (
              point.blockNo === through.blockNo &&
              "blockHash" in through &&
              !same(point, through)
            )
              throw new Error(
                "User-event native target is on a different fork",
              );
            const block = admitWatcherNativeRollForwardBlock(event);
            if (!replayAll && classify(block) === "quiet") {
              items.push({
                kind: "quiet",
                header: Object.freeze({
                  blockHash: point.blockHash,
                  parentBlockHash: event.prevHash,
                  blockNo: point.blockNo,
                  slot: point.slot,
                }),
              });
            } else {
              plannedBytes += Buffer.byteLength(event.rawBlockCbor);
              touched += 1;
              items.push({
                kind: "touched",
                plan: { point, rawBlockCbor: event.rawBlockCbor },
              });
            }
            previous = point;
            if (
              point.blockNo === through.blockNo ||
              touched === maximumTouched ||
              items.length >= MAX_ROUND_ITEMS ||
              plannedBytes >= 24 * 1024 * 1024
            ) {
              finished = true;
              resolveBatch();
            }
          } catch (error) {
            finished = true;
            rejectBatch(error);
          }
        },
      });
      streams.add(stream);
      void stream.done.then(() => {
        if (!finished)
          rejectBatch(new Error("User-event native enumeration ended early"));
      }, rejectBatch);
      await collected;
      operationSignal.throwIfAborted();
      return items;
    } finally {
      finished = true;
      clearTimeout(timer);
      operationSignal.removeEventListener("abort", abort);
      if (stream !== null) {
        streams.delete(stream);
        await stream.close();
      }
    }
  };
  const serialize = <T>(
    work: (operationSignal: AbortSignal) => Promise<T>,
  ): Promise<T> => {
    const expected = generation;
    const operationSignal = AbortSignal.any([signal, operationAbort.signal]);
    const task = tail.then(async () => {
      if (expected !== generation)
        throw new WatcherUserEventOperationRetired(
          "User-event operation was retired",
        );
      assertReady();
      operationSignal.throwIfAborted();
      const value = await work(operationSignal);
      if (expected !== generation)
        throw new WatcherUserEventOperationRetired(
          "User-event operation changed generation",
        );
      assertReady();
      operationSignal.throwIfAborted();
      return value;
    });
    tail = task.catch((error: unknown) => {
      if (
        status === "ready" &&
        !(error instanceof WatcherUserEventOperationRetired)
      )
        fail(error);
    });
    return task;
  };
  /** A saved head already carries its height. FindIntersect checks its hash and
   * slot without waiting for a future child; the mandatory exact-head capture
   * that follows independently binds height, raw bytes and release finality. */
  const intersectsCanonicalChain = async (
    point: FraudProofRawL1Point,
  ): Promise<boolean> => {
    signal.throwIfAborted();
    let stream: WatcherNativeChainSyncRuntime | null = null;
    let contradicted = false;
    try {
      try {
        stream = await startWatcherNativeChainSync({
          binaryPath: nativeChainSyncBinaryPath,
          watcherConfig,
          signal,
          intersection: {
            kind: "point",
            blockHash: point.blockHash,
            slot: point.slot,
          },
          startupTimeoutMs: LOOKUP_TIMEOUT_MS,
          onEvent: async (event) => {
            if (
              event.kind === "roll_backward" &&
              (event.point.kind !== "point" ||
                event.point.blockHash !== point.blockHash ||
                event.point.slot !== point.slot)
            )
              contradicted = true;
          },
        });
      } catch (error) {
        if (
          error instanceof Error &&
          error.name === "NativeChainSyncStartupFailure" &&
          (error as { code?: unknown }).code === "intersection_failed"
        )
          return false;
        throw error;
      }
      streams.add(stream);
      signal.throwIfAborted();
      const details = watcherNativeChainSyncAuthorityDetails(stream.authority);
      if (details === null)
        throw new Error("Canonical intersection has no native authority");
      return (
        !contradicted &&
        details.selectedIntersection.kind === "point" &&
        details.selectedIntersection.blockHash === point.blockHash &&
        details.selectedIntersection.slot === point.slot
      );
    } finally {
      if (stream !== null) {
        streams.delete(stream);
        await stream.close();
      }
    }
  };
  let activationPointForReplay: FraudProofRawL1Point | null = null;
  const replaceCanonical = async () => {
    if (activationPointForReplay === null)
      throw new Error("Canonical replay activation is unavailable");
    const activation = await onePair(activationPointForReplay, signal);
    try {
      const origin = admitWatcherUserEventOrigin({
        deploymentIdentity,
        scriptBinding,
        ...activation,
      });
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durableRuntime),
      );
      if (protectedHead.payload !== null) {
        const payload = JSON.parse(
          new TextDecoder("utf-8", { fatal: true }).decode(
            protectedHead.payload,
          ),
        ) as { head: { cursor: unknown } };
        const target = admitFraudProofRawL1Point(payload.head.cursor);
        if (await intersectsCanonicalChain(target))
          return await resumeWatcherLocalUserEventPublisher({
            origin,
            deploymentIdentity,
            scriptBinding,
            ...activation,
            runtime: durableRuntime,
            archive,
            readHead: (point) => onePair(point, signal),
          });
      }
      return await replaceWatcherLocalUserEventPublisher({
        origin,
        deploymentIdentity,
        scriptBinding,
        ...activation,
        runtime: durableRuntime,
        archive,
        replayCanonical: async function* (savedHead) {
          let cursor = activationPointForReplay!;
          while (BigInt(cursor.blockNo) < BigInt(savedHead.blockNo)) {
            const items = await enumerateRound(
              cursor,
              { blockNo: savedHead.blockNo },
              MAX_BATCH,
              signal,
              true,
            );
            if (items.length === 0)
              throw new Error("Canonical replay made no progress");
            for (const item of items) {
              if (item.kind !== "touched")
                throw new Error(
                  "Canonical replay skipped native block evidence",
                );
              const pair = await onePair(item.plan.point, signal);
              yield pair;
              cursor = item.plan.point;
            }
          }
        },
      });
    } finally {
      await activation.close();
    }
  };
  /**
   * The fork is the node's rollback point. A fresh capture at the head entry
   * corroborates that the last observation survived. Then the coverage
   * checkpoint moves: a fork inside the quiet stretch rewinds it in place,
   * with the height resolved from the covered ring or one node lookup; a fork
   * below the head requires a fresh canonical fold and one protected CAS.
   * Failed acquisition leaves the runtime suspended for a later native retry.
   */
  const handleRollback = (
    point: WatcherNativeChainSyncPoint,
  ): Promise<void> => {
    if (recovery !== null) {
      if (same(point, recoveryPoint)) return recovery;
      return recovery.then(
        () => handleRollback(point),
        () => handleRollback(point),
      );
    }
    if ((status !== "ready" && status !== "suspended") || publisher === null) {
      const error = new Error(
        "User-event rollback cannot recover this runtime state",
      );
      fail(error);
      return Promise.reject(error);
    }
    const head =
      status === "ready"
        ? readPoints().headCursor
        : lastKnownPoints!.headCursor;
    if (status === "ready") {
      status = "suspended";
      generation += 1;
      publisher.suspend();
    }
    prefetchedFirst.clear();
    recoveryPoint = Object.freeze({ ...point });
    operationAbort.abort(
      new WatcherUserEventOperationRetired("User-event source rolled back"),
    );
    const settled = tail;
    const releasing = releaseLease();
    recovery = (async () => {
      await settled;
      await releasing;
      signal.throwIfAborted();
      if (
        point.kind === "origin" ||
        BigInt(point.slot) < BigInt(head.slot) ||
        (point.slot === head.slot && point.blockHash !== head.blockHash)
      ) {
        const replacement = await replaceCanonical();
        if (status !== "suspended") {
          replacement.close();
          throw new Error("Canonical replay was retired");
        }
        publisher!.close();
        publisher = replacement;
        coverageStore.clear();
        coveredRing.clear();
        persistCoverage();
        remember(headCursor());
        lastKnownPoints = {
          currentPoint: coveragePoint(),
          headCursor: headCursor(),
        };
        operationAbort = new AbortController();
        status = "ready";
        recoveryPoint = null;
        return;
      }
      const pair = await onePair(head, signal);
      try {
        if (status !== "suspended")
          throw new Error("User-event recovery was retired");
        await publisher!.resume(pair);
        signal.throwIfAborted();
        if (status !== "suspended")
          throw new Error("User-event recovery changed during protected read");
        const coverage = coveragePoint();
        if (point.blockHash !== coverage.blockHash) {
          let target: FraudProofRawL1Point;
          if (point.blockHash === head.blockHash && point.slot === head.slot)
            target = head;
          else {
            const height =
              rememberedHeight(point) ?? (await lookupOnChain(point, signal));
            if (height === null)
              throw new Error(
                "User-event rollback point is not on the canonical chain",
              );
            target = rawPoint({
              blockHash: point.blockHash,
              blockNo: height,
              slot: point.slot,
            });
          }
          publisher!.rewindCoverage(target);
          persistCoverage();
          forgetAbove(BigInt(target.blockNo));
          remember(target);
        }
        operationAbort = new AbortController();
        status = "ready";
      } finally {
        await pair.close();
      }
    })().finally(() => {
      recovery = null;
      if (status === "ready") recoveryPoint = null;
    });
    return recovery;
  };
  /** Amortizes first captures without publishing beyond the requested prefix. */
  const acquirePlannedBatch = async (
    plans: readonly PlannedBlock[],
    operationSignal: AbortSignal,
  ): Promise<Pair[]> => {
    const covered = BigInt(coveragePoint().blockNo);
    for (const [key, first] of prefetchedFirst)
      if (BigInt(first.plan.point.blockNo) <= covered)
        prefetchedFirst.delete(key);
    if (prefetchedFirst.size === 0) {
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig,
        deploymentIdentity,
        captureBounds: {
          signal: operationSignal,
          timeoutMs: watcherConfig.l1.requestTimeoutMs,
        },
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({ source });
      operationSignal.throwIfAborted();
      if (BigInt(boundary.kupoCheckpoint.blockNo) > covered) {
        const ahead = await enumerateRound(
          coveragePoint(),
          boundary.kupoCheckpoint,
          MAX_BATCH,
          operationSignal,
        );
        const touched = ahead.flatMap((item) =>
          item.kind === "touched" ? [item.plan] : [],
        );
        if (touched.length > 0) {
          const acquired = await acquireFirst(touched, operationSignal);
          operationSignal.throwIfAborted();
          for (const first of acquired.observations)
            prefetchedFirst.set(first.plan.point.pointId, first);
        }
      }
    }
    const first: FirstObservation[] = [];
    let bytes = 0;
    for (const plan of plans) {
      // Re-enumeration uses the current tracked outrefs. A block newly marked
      // touched after an earlier publication simply acquires its first now.
      const candidate =
        prefetchedFirst.get(plan.point.pointId) ??
        (await acquireFirst([plan], operationSignal)).observations[0]!;
      if (
        !same(candidate.plan.point, plan.point) ||
        candidate.raw !== plan.rawBlockCbor
      )
        throw new Error(
          "Prefetched user-event first observation changed its block",
        );
      if (
        first.length > 0 &&
        bytes + candidate.evidenceBytes > MAX_EVIDENCE_BYTES / 3
      )
        break;
      prefetchedFirst.delete(plan.point.pointId);
      first.push(candidate);
      bytes += candidate.evidenceBytes;
    }
    let retainedBytes = [...prefetchedFirst.values()].reduce(
      (total, candidate) => total + candidate.evidenceBytes,
      0,
    );
    if (bytes + retainedBytes > MAX_EVIDENCE_BYTES / 3) {
      // Newly tracked inputs can add misses to a full speculative batch. Drop
      // unused first facts rather than exceed the existing acquisition bound.
      prefetchedFirst.clear();
      retainedBytes = 0;
    }
    const pairs = await completeFirst(
      first,
      bytes + retainedBytes,
      operationSignal,
    );
    try {
      operationSignal.throwIfAborted();
      // A pending later point returns the completed prefix. Keep its unused
      // closed first facts so the next sequential advance does not start over.
      for (const unused of first.slice(pairs.length))
        prefetchedFirst.set(unused.plan.point.pointId, unused);
      return pairs;
    } catch (error) {
      await Promise.allSettled(pairs.map((pair) => pair.close()));
      throw error;
    }
  };
  /** Covers every block above the coverage checkpoint through `point`. */
  const advance = async (
    point: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ) => {
    // Quiet coverage leaves the head, and any authority leased at it, intact;
    // the lease is released only once an event block moves the head.
    while (!same(coveragePoint(), point)) {
      if (publisher!.read().anchorDue) {
        await releaseLease();
        const pair = await onePair(headCursor(), operationSignal);
        try {
          await publisher!.rotate(pair);
        } finally {
          await pair.close();
        }
      }
      const available = Math.min(
        MAX_BATCH,
        WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries -
          publisher!.read().retainedEntries,
      );
      const items = await enumerateRound(
        coveragePoint(),
        point,
        available,
        operationSignal,
      );
      const plans = items.flatMap((item) =>
        item.kind === "touched" ? [item.plan] : [],
      );
      const pairs =
        plans.length === 0
          ? []
          : await acquirePlannedBatch(plans, operationSignal);
      try {
        let pairIndex = 0;
        for (const item of items) {
          operationSignal.throwIfAborted();
          assertReady();
          if (item.kind === "quiet") {
            coverQuietHeader(item.header);
            continue;
          }
          // The evidence bound may have truncated the captured prefix, or the
          // retained closure may have grown due for an anchor mid-round; the
          // remainder is enumerated again from the new coverage checkpoint.
          if (pairIndex >= pairs.length || publisher!.read().anchorDue) break;
          const pair = pairs[pairIndex]!;
          pairIndex += 1;
          await releaseLease();
          await publisher!.publish(pair);
          persistCoverage();
          remember(item.plan.point);
          await pair.close();
        }
      } finally {
        await Promise.allSettled(pairs.map((pair) => pair.close()));
      }
    }
  };
  const abortOwner = () => fail(signal.reason);
  signal.addEventListener("abort", abortOwner, { once: true });
  try {
    const source = createWatcherLocalKupmiosRawSource({
      watcherConfig,
      deploymentIdentity,
      captureBounds: { signal, timeoutMs: watcherConfig.l1.requestTimeoutMs },
    });
    const boundary = await readAdmittedLocalKupmiosBoundary({ source });
    const history = await readAdmittedLocalKupmiosUnitHistoryAtPoint({
      source,
      unit: scripts.hub.policyId + scripts.hub.assetName,
      point: boundary.kupoCheckpoint,
    });
    const activationCandidates: FraudProofRawL1Point[] = [];
    for (const candidate of history.transactions) {
      signal.throwIfAborted();
      const block = await readAdmittedLocalKupmiosRawBlockAtPoint({
        source,
        point: candidate.inclusionPoint,
      });
      const matches = block.transactions.filter(
        (transaction) => transaction.txHash === candidate.txHash,
      );
      if (matches.length !== 1)
        throw new Error(
          "User-event discovery transaction is not uniquely included",
        );
      const body = CML.Transaction.from_cbor_hex(
        matches[0]!.transactionCbor,
      ).body();
      const inputs = body.inputs();
      if (
        Array.from({ length: inputs.len() }, (_, index) =>
          inputs.get(index),
        ).some(
          (spent) =>
            `${spent.transaction_id().to_hex()}#${spent.index()}` ===
            scripts.canonicalOneShotOutRef,
        )
      )
        activationCandidates.push(candidate.inclusionPoint);
    }
    if (activationCandidates.length !== 1)
      throw new Error("User-event activation discovery is absent or ambiguous");
    const activationPoint = activationCandidates[0]!;
    activationPointForReplay = activationPoint;
    const activation = await onePair(activationPoint, signal);
    try {
      const origin = admitWatcherUserEventOrigin({
        deploymentIdentity,
        scriptBinding,
        ...activation,
      });
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durableRuntime),
      );
      const bootstrap = {
        origin,
        deploymentIdentity,
        scriptBinding,
        ...activation,
        runtime: durableRuntime,
        archive: archive,
      };
      if (protectedHead.checkpoint === null) {
        publisher = await createWatcherLocalUserEventPublisher(bootstrap);
        await publisher.publish(activation);
        coverageStore.clear();
      } else {
        if (protectedHead.payload === null)
          throw new Error("User-event restart payload is missing");
        // The authenticated saved result restores state. A fresh exact-head
        // capture corroborates canonicality without replaying the prefix.
        const payload: unknown = JSON.parse(
          new TextDecoder("utf-8", { fatal: true }).decode(
            protectedHead.payload,
          ),
        );
        const target = admitFraudProofRawL1Point(
          (payload as { head?: { cursor?: unknown } })?.head?.cursor,
        );
        // A negative lookup only chooses the replay path. Replacement still
        // requires a positively admitted conflicting block at the saved height.
        const savedCanonical = await intersectsCanonicalChain(target);
        publisher = savedCanonical
          ? await resumeWatcherLocalUserEventPublisher({
              ...bootstrap,
              readHead: (point) => onePair(point, signal),
            })
          : await replaceCanonical();
        if (savedCanonical && !same(publisher.read().cursor, target))
          throw new Error(
            "User-event restart did not restore the exact saved head",
          );
        // Trust the store: the saved coverage row extends the restored head
        // over the quiet stretch it covered before shutdown. A row from an
        // older head is a torn write and coverage restarts at the head.
        publisher.restoreCoverage(coverageStore.read());
      }
      const head = headCursor();
      remember(head);
      // A separate live stream owns rollback/source-loss revocation between
      // historical queries and while issued event capabilities are in use.
      // It intersects at the coverage checkpoint when the node still has it,
      // else at the head entry: the node's answer is the fork, and coverage
      // rewinds onto the surviving head in place.
      const candidates = [coveragePoint(), head].filter(
        (candidate, index, all) =>
          all.findIndex(
            (other) =>
              other.blockHash === candidate.blockHash &&
              other.slot === candidate.slot,
          ) === index,
      );
      let sawMonitorEvent = false;
      let selectedIntersection: WatcherNativeChainSyncPoint | null = null;
      const monitor = await startWatcherNativeChainSyncWithRetry({
        binaryPath: nativeChainSyncBinaryPath,
        watcherConfig,
        intersectionCandidates: candidates.map((candidate) => ({
          kind: "point" as const,
          blockHash: candidate.blockHash,
          slot: candidate.slot,
        })),
        startupTimeoutMs: ACQUISITION_TIMEOUT_MS,
        onEvent: async (event) => {
          if (event.tip.kind === "point")
            monitorTip = Object.freeze({ blockNo: event.tip.blockNo });
          const firstEvent = !sawMonitorEvent;
          sawMonitorEvent = true;
          if (
            firstEvent &&
            event.kind === "roll_backward" &&
            event.point.kind === "point" &&
            candidates.some(
              (candidate) =>
                event.point.kind === "point" &&
                candidate.blockHash === event.point.blockHash &&
                candidate.slot === event.point.slot,
            ) &&
            (selectedIntersection === null ||
              same(event.point, selectedIntersection))
          )
            return;
          if (event.kind === "roll_backward")
            void handleRollback(event.point).catch(() => undefined);
          else if (
            status === "suspended" &&
            recovery === null &&
            recoveryPoint !== null
          )
            void handleRollback(recoveryPoint).catch(() => undefined);
        },
      });
      nativeMonitor = monitor;
      streams.add(monitor);
      const details = watcherNativeChainSyncAuthorityDetails(monitor.authority);
      if (details === null)
        throw new Error("User-event native monitor did not report readiness");
      selectedIntersection = details.selectedIntersection;
      if (details.currentTip.kind === "point")
        monitorTip = Object.freeze({ blockNo: details.currentTip.blockNo });
      const selected = candidates.find(
        (candidate) =>
          details.selectedIntersection.kind === "point" &&
          candidate.blockHash === details.selectedIntersection.blockHash &&
          candidate.slot === details.selectedIntersection.slot,
      );
      if (selected === undefined)
        throw new Error(
          "User-event native monitor intersected outside the covered chain",
        );
      if (!same(selected, coveragePoint())) {
        publisher.rewindCoverage(selected);
        forgetAbove(BigInt(selected.blockNo));
      }
      persistCoverage();
      remember(coveragePoint());
      void monitor.done.then(() => {
        if (streams.has(monitor))
          fail(new Error("User-event native monitor ended"));
      }, fail);
    } finally {
      await activation.close();
    }
    assertReady();
    const runtime: WatcherUserEventRuntime = Object.freeze({
      [runtimeBrand]: true as const,
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintHash: deploymentIdentity.blueprintHash,
      relevancePolicy,
      read: () => Object.freeze({ status, ...readPoints(), generation }),
      done,
      classify,
      coverQuiet: (block: WatcherNativeBlockAdmission) => {
        const point = rawPoint(block);
        return serialize(async (operationSignal) => {
          const coverage = coveragePoint();
          if (BigInt(point.blockNo) <= BigInt(coverage.blockNo)) {
            await assertCovered(point, operationSignal);
            return;
          }
          if (
            BigInt(point.blockNo) !== BigInt(coverage.blockNo) + 1n ||
            block.prevHash !== coverage.blockHash
          ) {
            await advance(point, operationSignal);
            return;
          }
          if (classify(block) !== "quiet")
            throw new Error(
              "User-event quiet coverage was offered a block the fold tracks",
            );
          coverQuietHeader({
            blockHash: point.blockHash,
            parentBlockHash: block.prevHash,
            blockNo: point.blockNo,
            slot: point.slot,
          });
        });
      },
      advanceThrough: (requested: FraudProofRawL1Point) => {
        const point = Object.freeze(admitFraudProofRawL1Point(requested));
        return serialize(async (operationSignal) => {
          if (BigInt(point.blockNo) <= BigInt(coveragePoint().blockNo)) {
            await assertCovered(point, operationSignal);
            return;
          }
          await advance(point, operationSignal);
        });
      },
      eventAuthority: (
        request: Readonly<{
          kind: WatcherUserEventKind;
          eventId: string;
          throughHeader: WatcherStateQueueHeaderObservation;
        }>,
      ) => {
        const selected = Object.freeze({
          kind: request.kind,
          eventId: request.eventId,
          throughHeader: request.throughHeader,
        });
        assertWatcherStateQueueHeaderObservation(selected.throughHeader);
        const outcome = serialize(async (operationSignal) => {
          if (headLease !== null) {
            try {
              readWatcherLocalBackfillObservation(headLease.observation);
            } catch {
              await releaseLease();
            }
          }
          if (headLease === null)
            headLease = await onePair(headCursor(), operationSignal);
          try {
            return {
              status: "admitted" as const,
              authority: await publisher!.eventAuthority({
                ...selected,
                ...headLease,
              }),
            };
          } catch (error) {
            if (!isWatcherLocalUserEventAuthorityUnavailable(error))
              throw error;
            await publisher!.assertHeadCurrent(headLease);
            assertWatcherStateQueueHeaderObservation(selected.throughHeader);
            return { status: "unavailable" as const, unavailable: error };
          }
        });
        return outcome.then((result) => {
          if (result.status === "unavailable") throw result.unavailable;
          return result.authority;
        });
      },
      handleRollback,
      close: async () => {
        if (status === "closed") return;
        const failed = status === "failed";
        status = "closed";
        generation += 1;
        prefetchedFirst.clear();
        publisher?.close();
        signal.removeEventListener("abort", abortOwner);
        shutdown.abort(new Error("User-event runtime closed"));
        await releaseLease();
        await cleanup();
        await tail;
        await recovery?.catch(() => undefined);
        if (!failed) resolveDone();
      },
    });
    runtimes.set(runtime, assertReady);
    return runtime;
  } catch (error) {
    fail(error);
    signal.removeEventListener("abort", abortOwner);
    await cleanup();
    throw error;
  }
};
