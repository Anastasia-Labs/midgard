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
  resumeWatcherLocalUserEventPublisher,
} from "../indexers/user-event-history.js";
import {
  isWatcherLocalUserEventAuthorityUnavailable,
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
  startWatcherNativeChainSync,
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
export type WatcherUserEventRuntime = Readonly<{
  [runtimeBrand]: true;
  deploymentFingerprint: string;
  blueprintHash: string;
  read(): Readonly<{
    status: "ready" | "suspended" | "closed" | "failed";
    currentPoint: FraudProofRawL1Point;
    generation: number;
  }>;
  advanceThrough(
    point: FraudProofRawL1Point,
    options?: Readonly<{ prefetch?: boolean }>,
  ): Promise<void>;
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
    signal?: AbortSignal;
  }>,
): Promise<WatcherUserEventRuntime> => {
  const {
    runtime: durableRuntime,
    archive,
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
  let currentPoint: FraudProofRawL1Point | null = null;
  let operationAbort = new AbortController();
  let tail: Promise<unknown> = Promise.resolve();
  let recovery: Promise<void> | null = null;
  let recoveryPoint: WatcherNativeChainSyncPoint | null = null;
  let headLease: Pair | null = null;
  // Closed first captures retain only opaque finality facts. Future blocks
  // cannot enter the published history before their requested second capture.
  let prefetched: readonly FirstObservation[] = [];
  let prefetchedBytes = 0;
  const clearPrefetched = () => {
    prefetched = [];
    prefetchedBytes = 0;
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
    clearPrefetched();
    generation += 1;
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
  const completeFirst = async (
    first: readonly FirstObservation[],
    initialBytes: number,
    operationSignal: AbortSignal,
  ): Promise<Pair[]> => {
    const pairs: Pair[] = [];
    let bytes = initialBytes;
    try {
      const deadline = performance.now() + ACQUISITION_TIMEOUT_MS;
      for (const step of first) {
        for (;;) {
          operationSignal.throwIfAborted();
          if (performance.now() >= deadline)
            throw new Error(
              "User-event native tip did not grow within acquisition bound",
            );
          const capture = await captureAt(step.plan.point, operationSignal);
          let retained = false;
          try {
            const read = readWatcherLocalHistoricalCapture(capture.receipt);
            if (read.nativeBlock.rawBlockCbor !== step.raw)
              throw new Error(
                "User-event native block changed between observations",
              );
            if (
              BigInt(read.observedNativeTip.blockNo) <= BigInt(step.tip.blockNo)
            ) {
              await releaseCapture(capture);
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
            retained = true;
            break;
          } finally {
            if (!retained && captures.has(capture))
              await releaseCapture(capture);
          }
        }
      }
      return pairs;
    } catch (error) {
      await Promise.allSettled(pairs.map((pair) => pair.close()));
      throw error;
    }
  };
  const acquireBatch = async (
    plans: readonly FirstObservation["plan"][],
    operationSignal: AbortSignal,
  ): Promise<Pair[]> => {
    const first = await acquireFirst(plans, operationSignal);
    return await completeFirst(
      first.observations,
      first.bytes,
      operationSignal,
    );
  };
  const onePair = async (
    point: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ) => (await acquireBatch([{ point }], operationSignal))[0]!;
  const enumerate = async (
    from: FraudProofRawL1Point,
    through: FraudProofRawL1Point,
    operationSignal: AbortSignal,
  ): Promise<PlannedBlock[]> => {
    const result: PlannedBlock[] = [];
    let previous = from;
    let sawEnumerationEvent = false;
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
            const point = admitFraudProofRawL1Point({
              blockHash: event.blockHash,
              blockNo: event.blockNo,
              slot: event.slot,
              pointId: computeFraudProofRawL1PointId({
                blockHash: event.blockHash,
                blockNo: event.blockNo,
                slot: event.slot,
              }),
            });
            if (
              event.prevHash !== previous.blockHash ||
              BigInt(point.blockNo) !== BigInt(previous.blockNo) + 1n ||
              BigInt(point.slot) <= BigInt(previous.slot) ||
              BigInt(point.blockNo) > BigInt(through.blockNo)
            )
              throw new Error(
                "User-event native enumeration is not a strict contiguous prefix",
              );
            if (point.blockNo === through.blockNo && !same(point, through))
              throw new Error(
                "User-event native target is on a different fork",
              );
            plannedBytes += Buffer.byteLength(event.rawBlockCbor);
            result.push({ point, rawBlockCbor: event.rawBlockCbor });
            previous = point;
            if (
              same(point, through) ||
              result.length === MAX_BATCH ||
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
      return result;
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
      assertReady();
      operationSignal.throwIfAborted();
      if (expected !== generation)
        throw new Error("User-event operation was retired");
      const value = await work(operationSignal);
      assertReady();
      operationSignal.throwIfAborted();
      if (expected !== generation)
        throw new Error("User-event operation changed generation");
      return value;
    });
    tail = task.catch((error: unknown) => {
      if (status === "ready") fail(error);
    });
    return task;
  };
  const handleRollback = (
    point: WatcherNativeChainSyncPoint,
  ): Promise<void> => {
    if (recovery !== null && same(point, recoveryPoint)) return recovery;
    if (status !== "ready" || publisher === null || currentPoint === null) {
      const error = new Error(
        "User-event rollback cannot recover this runtime state",
      );
      fail(error);
      return Promise.reject(error);
    }
    status = "suspended";
    clearPrefetched();
    generation += 1;
    recoveryPoint = Object.freeze({ ...point });
    publisher.suspend();
    operationAbort.abort(new Error("User-event source rolled back"));
    const settled = tail;
    const releasing = releaseLease();
    recovery = (async () => {
      await settled;
      await releasing;
      signal.throwIfAborted();
      const pair = await onePair(currentPoint!, signal);
      try {
        if (status !== "suspended")
          throw new Error("User-event recovery was retired");
        await publisher!.resume(pair);
        signal.throwIfAborted();
        if (status !== "suspended")
          throw new Error("User-event recovery changed during protected read");
        operationAbort = new AbortController();
        status = "ready";
      } finally {
        await pair.close();
      }
    })()
      .catch((error: unknown) => {
        fail(error);
        throw error;
      })
      .finally(() => {
        recovery = null;
        recoveryPoint = null;
      });
    return recovery;
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
    currentPoint = activationCandidates[0]!;
    const activation = await onePair(currentPoint, signal);
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
        publisher = await resumeWatcherLocalUserEventPublisher({
          ...bootstrap,
          readHead: (point) => onePair(point, signal),
        });
        if (!same(publisher.read().cursor, target))
          throw new Error(
            "User-event restart did not restore the exact saved head",
          );
        currentPoint = target;
      }
      // A separate live stream owns rollback/source-loss revocation between
      // historical queries and while issued event capabilities are in use.
      const monitorIntersection = currentPoint!;
      let sawMonitorEvent = false;
      const monitor = await startWatcherNativeChainSync({
        binaryPath: nativeChainSyncBinaryPath,
        watcherConfig,
        intersection: {
          kind: "point",
          blockHash: monitorIntersection.blockHash,
          slot: monitorIntersection.slot,
        },
        startupTimeoutMs: ACQUISITION_TIMEOUT_MS,
        onEvent: async (event) => {
          const firstEvent = !sawMonitorEvent;
          sawMonitorEvent = true;
          if (
            firstEvent &&
            event.kind === "roll_backward" &&
            event.point.kind === "point" &&
            event.point.blockHash === monitorIntersection.blockHash &&
            event.point.slot === monitorIntersection.slot
          )
            return;
          if (event.kind === "roll_backward")
            void handleRollback(event.point).catch(fail);
        },
      });
      nativeMonitor = monitor;
      streams.add(monitor);
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
      read: () =>
        Object.freeze({
          status,
          currentPoint: Object.freeze({ ...currentPoint! }),
          generation,
        }),
      done,
      advanceThrough: (
        requested: FraudProofRawL1Point,
        options?: Readonly<{ prefetch?: boolean }>,
      ) => {
        const point = Object.freeze(admitFraudProofRawL1Point(requested));
        return serialize(async (operationSignal) => {
          if (BigInt(point.blockNo) <= BigInt(currentPoint!.blockNo)) {
            await publisher!.assertPointCovered(point);
            return;
          }
          await releaseLease();
          while (!same(currentPoint, point)) {
            if (publisher!.read().retainedEntries >= 128) {
              const pair = await onePair(currentPoint!, operationSignal);
              try {
                await publisher!.rotate(pair);
              } finally {
                await pair.close();
              }
            }
            const available = 128 - publisher!.read().retainedEntries;
            let plans: readonly FirstObservation["plan"][];
            let pairs: Pair[];
            if (options?.prefetch === true) {
              if (prefetched.length === 0) {
                // A fresh admitted boundary limits lookahead to release-final
                // chain points. Native enumeration and both W12 captures still
                // authenticate every block independently.
                const source = createWatcherLocalKupmiosRawSource({
                  watcherConfig,
                  deploymentIdentity,
                  captureBounds: {
                    signal: operationSignal,
                    timeoutMs: watcherConfig.l1.requestTimeoutMs,
                  },
                });
                const boundary = await readAdmittedLocalKupmiosBoundary({
                  source,
                });
                const through =
                  BigInt(boundary.kupoCheckpoint.blockNo) >
                  BigInt(point.blockNo)
                    ? boundary.kupoCheckpoint
                    : point;
                const ahead = await enumerate(
                  currentPoint!,
                  through,
                  operationSignal,
                );
                const first = await acquireFirst(ahead, operationSignal);
                operationSignal.throwIfAborted();
                prefetched = first.observations;
                prefetchedBytes = first.bytes;
              }
              const selected = prefetched
                .filter(
                  ({ plan }) =>
                    BigInt(plan.point.blockNo) <= BigInt(point.blockNo),
                )
                .slice(0, available);
              if (
                selected.length === 0 ||
                BigInt(selected[0]!.plan.point.blockNo) !==
                  BigInt(currentPoint!.blockNo) + 1n ||
                selected.some(
                  ({ plan }) =>
                    plan.point.blockNo === point.blockNo &&
                    !same(plan.point, point),
                )
              )
                throw new Error(
                  "User-event prefetched prefix differs from requested chain point",
                );
              plans = selected.map(({ plan }) => plan);
              pairs = await completeFirst(
                selected,
                prefetchedBytes,
                operationSignal,
              );
            } else {
              clearPrefetched();
              plans = await enumerate(currentPoint!, point, operationSignal);
              pairs = await acquireBatch(
                plans.slice(0, available),
                operationSignal,
              );
            }
            try {
              for (let index = 0; index < pairs.length; index += 1) {
                operationSignal.throwIfAborted();
                assertReady();
                await publisher!.publish(pairs[index]!);
                currentPoint = plans[index]!.point;
                if (options?.prefetch === true) {
                  prefetchedBytes -= prefetched[0]!.evidenceBytes;
                  prefetched = prefetched.slice(1);
                  if (prefetched.length === 0) clearPrefetched();
                }
                await pairs[index]!.close();
              }
            } finally {
              await Promise.allSettled(pairs.map((pair) => pair.close()));
            }
          }
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
            headLease = await onePair(currentPoint!, operationSignal);
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
        clearPrefetched();
        generation += 1;
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
