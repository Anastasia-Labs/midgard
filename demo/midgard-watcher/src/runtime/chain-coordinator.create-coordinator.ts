import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import { type WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type {
  WatcherNativeChainSyncEvent,
  WatcherNativeChainSyncPoint,
} from "../l1/native-chain-sync.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import type { WatcherBlockRelevance } from "./block-relevance.js";
import { advanceWatcherCanonical } from "./chain-coordinator.advance-canonical.js";
import {
  type AdmitRollForward,
  depthAtTip,
  nextBufferedChild,
  pointKey,
  recoverWatcherCoordinatorAfterRestart,
  WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION,
  type WatcherChainCoordinator,
  type WatcherChainCoordinatorDependencies,
  type WatcherChainCoordinatorHooks,
  WatcherConsumerDeliveryHeld,
  type WatcherProcessedHead,
} from "./chain-coordinator.canonical-path-from-history.js";
import {
  coordinatorHoldReason,
  type WatcherCoordinatorHoldReason,
  WatcherCoordinatorIntegrityHeld,
} from "./chain-coordinator.integrity-hold.js";
import {
  type CapturedWatcherObservation,
  observeCapturedBlock,
} from "./chain-coordinator.observe-captured-block.js";
import { reconcileRollbackReplacement } from "./chain-coordinator.reconcile-rollback-replacement.js";
import {
  guardRecoveryEvent,
  headOf,
  retryQuarantinedRecovery,
} from "./chain-coordinator.recovery-evidence.js";

export const createCoordinator = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly hooks: WatcherChainCoordinatorHooks;
  readonly dependencies: Readonly<{
    admitRollForward: AdmitRollForward;
    unsafeAllowUnprovenancedEvents?: true;
  }> &
    WatcherChainCoordinatorDependencies;
}): WatcherChainCoordinator => {
  let epoch = 0;
  let stopped = false;
  const buffered = new Map<string, WatcherNativeBlockAdmission>();
  const releaseFinalizedHooked = new Map<
    string,
    Readonly<{ blockHash: string; blockNo: string; slot: string }>
  >();
  const relevances = new Map<string, WatcherBlockRelevance>();
  const relevanceOf = (
    block: WatcherNativeBlockAdmission,
  ): WatcherBlockRelevance => {
    const key = pointKey(block.blockHash, block.slot);
    const known = relevances.get(key);
    if (known !== undefined) return known;
    const relevance = input.dependencies.relevance?.(block) ?? "touched";
    relevances.set(key, relevance);
    return relevance;
  };
  const forget = (key: string): void => {
    buffered.delete(key);
    captured.delete(key);
    relevances.delete(key);
  };
  const captured = new Map<string, CapturedWatcherObservation>();
  const retainedFinality = input.durable.readFinality();
  // A sparse queue cursor may lag the finality snapshot after a crash. Only
  // this retained prefix can be replayed without advancing durable finality.
  // A pending successor does not itself belong to the finalized prefix.
  let replayBoundary: Readonly<{
    point: WatcherProcessedHead;
    inclusive: boolean;
  }> | null =
    retainedFinality.phase === "finalized" &&
    retainedFinality.finalized !== null
      ? { point: retainedFinality.finalized, inclusive: true }
      : retainedFinality.phase === "pending" &&
          retainedFinality.pending !== null
        ? { point: retainedFinality.pending, inclusive: false }
        : null;
  const retainedHistory =
    retainedFinality.phase === "pending"
      ? (input.durable.read().authenticatedConsistencyHistory ?? [])
      : [];
  const progress = input.dependencies.progress ?? null;
  let progressHead = progress?.readHead() ?? null;
  const authorityFinalizedHead = (): WatcherProcessedHead | null => {
    const state = input.durable.readFinality();
    return state.phase === "finalized" && state.finalized !== null
      ? Object.freeze({
          blockHash: state.finalized.blockHash,
          blockNo: state.finalized.blockNo,
          slot: state.finalized.slot,
        })
      : null;
  };
  // "Processed through": the progress ring first, then the durable finality
  // authority. Everything at or below it is recorded fact until a rollback.
  const recomputeEffectiveHead = (): WatcherProcessedHead | null => {
    if (progressHead !== null) return headOf(progressHead);
    const authority = authorityFinalizedHead();
    if (authority !== null) {
      const key = pointKey(authority.blockHash, authority.slot);
      // Durable finality alone does not finish a delivery still in our buffer.
      if (buffered.has(key) && !releaseFinalizedHooked.has(key)) return null;
    }
    return authority;
  };
  let effectiveHead = recomputeEffectiveHead();
  if (
    replayBoundary !== null &&
    replayBoundary.inclusive &&
    progress !== null &&
    progressHead !== null &&
    input.restartIntersection?.kind === "point" &&
    BigInt(progressHead.blockNo) >= BigInt(replayBoundary.point.blockNo)
  ) {
    // The progress ring attests every block from durable finality to the
    // restart point; there is no retained prefix left to replay.
    const intersection = input.restartIntersection;
    const attested = progress
      .readRange({
        afterBlockNo: (BigInt(replayBoundary.point.blockNo) - 1n).toString(),
        throughBlockNo: progressHead.blockNo,
      })
      .some(
        (row) =>
          row.blockHash === intersection.blockHash &&
          row.slot === intersection.slot,
      );
    if (attested) replayBoundary = null;
  }
  let retainedReplay: readonly WatcherNativeBlockAdmission[] | null = null;
  let retainedReplacement: Readonly<{
    block: WatcherNativeBlockAdmission;
    parent: Extract<WatcherNativeChainSyncPoint, { readonly kind: "point" }>;
  }> | null = null;
  if (
    replayBoundary !== null &&
    input.restartIntersection?.kind === "point" &&
    input.restartIntersection.blockHash === replayBoundary.point.blockHash &&
    input.restartIntersection.slot === replayBoundary.point.slot
  ) {
    replayBoundary = null;
  }
  let rollbackPoint: WatcherNativeChainSyncPoint | null = null;
  let quarantined = input.durable.readFinality().phase === "quarantined";

  let restartRecoveryFailure: Error | null = null;
  const restartRecovery = recoverWatcherCoordinatorAfterRestart({
    ...input,
    assertCurrent: () => {
      if (stopped || epoch !== 0)
        throw new WatcherCoordinatorIntegrityHeld(
          "native_generation_changed",
          "restart intersection generation changed before recovery CAS",
        );
    },
  }).then(
    (pending) => {
      quarantined = pending;
    },
    (error: unknown) => {
      restartRecoveryFailure =
        error instanceof Error
          ? error
          : new Error("watcher restart recovery failed", { cause: error });
    },
  );
  const waitForRestartRecovery = async () => {
    await restartRecovery;
    const failure = restartRecoveryFailure;
    restartRecoveryFailure = null;
    if (failure !== null) throw failure;
  };

  const observe = observeCapturedBlock({
    captured,
    observation: input.observation,
    generation: () => epoch,
    stopped: () => stopped,
  });

  const recordProgress = (
    block: WatcherNativeBlockAdmission,
    relevance: WatcherBlockRelevance,
  ): void => {
    if (progress !== null) {
      if (
        progressHead !== null &&
        BigInt(block.blockNo) <= BigInt(progressHead.blockNo)
      ) {
        if (
          progressHead.blockNo === block.blockNo &&
          progressHead.blockHash !== block.blockHash
        ) {
          throw new Error(
            "watcher block progress head conflicts with the finalized block",
          );
        }
      } else {
        if (
          progressHead !== null &&
          progressHead.blockHash !== block.prevHash
        ) {
          // Only a contiguous ring attests ancestry. After a rewind to a point
          // the ring never recorded, the durable authority alone anchors it.
          progress.rollbackTo({ kind: "origin" });
          progressHead = null;
        }
        const record = Object.freeze({
          blockHash: block.blockHash,
          parentBlockHash: block.prevHash,
          blockNo: block.blockNo,
          slot: block.slot,
          relevance,
        });
        const finalized = authorityFinalizedHead();
        progress.record(
          record,
          finalized === null
            ? undefined
            : { retainFromBlockNo: finalized.blockNo },
        );
        progressHead = record;
      }
    }
    if (
      effectiveHead === null ||
      BigInt(block.blockNo) > BigInt(effectiveHead.blockNo)
    ) {
      effectiveHead = headOf(block);
    }
  };

  const deliverFinalized = async (
    block: WatcherNativeBlockAdmission,
    observation: WatcherLocalKupmiosNativeObservation | null,
    relevance: WatcherBlockRelevance,
  ): Promise<void> => {
    const key = pointKey(block.blockHash, block.slot);
    if (releaseFinalizedHooked.has(key)) return;
    assertCurrentDrain();
    (
      observation as
        | (WatcherLocalKupmiosNativeObservation &
            Readonly<{ assertCurrent?: () => void }>)
        | null
    )?.assertCurrent?.();
    await input.hooks.onFinalized({
      nativeBlock: block,
      localObservation: observation,
      relevance,
    });
    assertCurrentDrain();
    releaseFinalizedHooked.set(
      key,
      Object.freeze({
        blockHash: block.blockHash,
        blockNo: block.blockNo,
        slot: block.slot,
      }),
    );
    // Written after the block's own transactions: a crash between them costs
    // one idempotent replay of this block on restart, never lost work.
    recordProgress(block, relevance);
  };

  const ancestryFromFinalized = (
    finalized: WatcherProcessedHead,
    target: WatcherNativeBlockAdmission,
  ) => {
    if (BigInt(target.blockNo) === BigInt(finalized.blockNo) + 1n) return [];
    if (progress === null) return [];
    return progress
      .readRange({
        afterBlockNo: finalized.blockNo,
        throughBlockNo: (BigInt(target.blockNo) - 1n).toString(),
      })
      .map((row) =>
        Object.freeze({
          blockHash: row.blockHash,
          parentBlockHash: row.parentBlockHash,
          blockNo: row.blockNo,
          slot: row.slot,
        }),
      );
  };

  const replayRetainedPrefix = async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<boolean> => {
    if (replayBoundary === null) return true;
    const boundary = replayBoundary;
    if (retainedReplay === null) {
      const intersection = input.restartIntersection;
      if (intersection?.kind !== "point") {
        throw new Error(
          "retained finality replay requires an exact restart intersection",
        );
      }
      if (BigInt(event.blockNo) < BigInt(boundary.point.blockNo)) return false;
      const candidates = [...buffered.values()]
        .filter(
          (block) => BigInt(block.blockNo) <= BigInt(boundary.point.blockNo),
        )
        .sort((left, right) =>
          BigInt(left.blockNo) < BigInt(right.blockNo) ? -1 : 1,
        );
      const last = candidates.at(-1);
      if (
        last === undefined ||
        last.blockNo !== boundary.point.blockNo ||
        (boundary.inclusive &&
          (last.blockHash !== boundary.point.blockHash ||
            last.slot !== boundary.point.slot)) ||
        (last.blockHash === boundary.point.blockHash &&
          last.slot !== boundary.point.slot)
      ) {
        throw new Error(
          "native replay differs from the retained finality boundary",
        );
      }
      let parentHash = intersection.blockHash;
      let parentSlot = intersection.slot;
      let parentBlockNo: string | null = null;
      for (const block of candidates) {
        if (
          block.prevHash !== parentHash ||
          BigInt(block.slot) <= BigInt(parentSlot) ||
          (parentBlockNo !== null &&
            BigInt(block.blockNo) !== BigInt(parentBlockNo) + 1n)
        ) {
          throw new Error(
            "native replay is not a contiguous retained finality prefix",
          );
        }
        parentHash = block.blockHash;
        parentSlot = block.slot;
        parentBlockNo = block.blockNo;
      }
      const prefix = boundary.inclusive ? candidates : candidates.slice(0, -1);
      if (!boundary.inclusive && prefix.length > 0) {
        // Pending alone is not finality authority. Re-establish its exact
        // predecessor's finalized state from the retained authenticated
        // observations, then bind that point to the native ancestry above.
        const predecessor = prefix.at(-1)!;
        const history = retainedHistory
          .filter(
            ({ agreement }) =>
              agreement?.blockHash === predecessor.blockHash &&
              agreement.blockNo === predecessor.blockNo &&
              agreement.slot === predecessor.slot,
          )
          .sort((left, right) =>
            BigInt(left.agreement!.minimumDepth) <
            BigInt(right.agreement!.minimumDepth)
              ? -1
              : 1,
          );
        let state = makeWatcherFinalityBootstrapState(input.policy);
        for (const consistency of history) {
          if (state === null) break;
          const result = evaluateWatcherFinality(
            input.policy,
            state,
            consistency,
          );
          if (result.action === "reject" || result.state === null) {
            state = null;
            break;
          }
          state = result.state;
          if (state.phase === "finalized") break;
        }
        if (
          state?.phase !== "finalized" ||
          state.finalized?.blockHash !== predecessor.blockHash ||
          state.finalized.blockNo !== predecessor.blockNo ||
          state.finalized.slot !== predecessor.slot
        ) {
          throw new Error(
            "pending replay prefix lacks retained predecessor finality",
          );
        }
      }
      retainedReplay = prefix;
      if (!boundary.inclusive && last.blockHash !== boundary.point.blockHash) {
        const parent = candidates.at(-2);
        retainedReplacement = {
          block: last,
          parent:
            parent === undefined
              ? intersection
              : {
                  kind: "point",
                  blockHash: parent.blockHash,
                  slot: parent.slot,
                },
        };
      }
    }
    while (retainedReplay.length > 0) {
      const block = retainedReplay[0]!;
      if (
        BigInt(depthAtTip(block, event)) <
        BigInt(input.policy.confirmationDepth)
      ) {
        return false;
      }
      await deliverFinalized(
        block,
        await observe(block, event),
        relevanceOf(block),
      );
      forget(pointKey(block.blockHash, block.slot));
      retainedReplay = retainedReplay.slice(1);
    }
    replayBoundary = null;
    retainedReplay = null;
    if (retainedReplacement !== null) {
      // FindIntersect can select an ancestor of an orphaned pending point.
      // Once its replacement and ancestry are authenticated, use the normal
      // pre-finality rewind path. A finalized boundary never takes this path.
      const replacement = retainedReplacement;
      retainedReplacement = null;
      const pendingForward = lastForward;
      fenceRollback(replacement.parent);
      lastForward = pendingForward;
      await input.hooks.onRollback(replacement.parent);
      rollbackPoint = replacement.parent;
      await processRollbackReplacement(replacement.block, event);
      if (quarantined) return false;
    }
    return true;
  };

  const bufferedRollbackReplacement = (): WatcherNativeBlockAdmission => {
    const target = rollbackPoint;
    const replacement =
      target === null
        ? undefined
        : target.kind === "origin"
          ? nextBufferedChild(buffered, null, null)
          : [...buffered.values()].find(
              (candidate) => candidate.prevHash === target.blockHash,
            );
    if (replacement === null || replacement === undefined)
      throw new WatcherCoordinatorIntegrityHeld(
        "rollback_evidence_rejected",
        "native rollback replacement child is not yet buffered",
      );
    return replacement;
  };
  let commonPrefixReplay: WatcherNativeBlockAdmission[] = [];
  const processRollbackReplacement = async (
    block: WatcherNativeBlockAdmission,
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    if (rollbackPoint === null) return;
    const result = await reconcileRollbackReplacement({
      durable: input.durable,
      policy: input.policy,
      block,
      target: rollbackPoint,
      observed: await observe(block, event),
    });
    if (result.kind === "common_prefix") {
      commonPrefixReplay.push(block);
      rollbackPoint = result.atFrontier
        ? null
        : { kind: "point", blockHash: block.blockHash, slot: block.slot };
      if (!result.atFrontier) return;
      if (input.durable.readFinality().phase === "pending")
        commonPrefixReplay.pop();
    } else {
      quarantined = result.quarantined;
      rollbackPoint = null;
    }
    const lastCommon = commonPrefixReplay.at(-1);
    if (lastCommon !== undefined && !quarantined) {
      replayBoundary = { point: lastCommon, inclusive: true };
      retainedReplay = Object.freeze([...commonPrefixReplay]);
    }
    commonPrefixReplay = [];
    progressHead = progress?.readHead() ?? null;
    effectiveHead = recomputeEffectiveHead();
  };

  // A quiet block waiting for its run to grow is already finalizable, so it
  // skips onIncluded as a quiet block finalized on arrival does.
  const waitingInRun = new Set<string>();
  const advanceCanonical = advanceWatcherCanonical({
    policy: input.policy,
    durable: input.durable,
    buffered,
    captured,
    authorityFinalizedHead,
    effectiveHead: () => effectiveHead,
    onQuarantine: () => {
      quarantined = true;
    },
    observe,
    deliverFinalized,
    relevanceOf,
    forget,
    waitingInRun,
    ancestryFromFinalized,
  });

  const includedHooked = new Set<string>();

  type Forward = Extract<
    WatcherNativeChainSyncEvent,
    { readonly kind: "roll_forward" }
  >;
  let lastForward: {
    readonly block: WatcherNativeBlockAdmission;
    readonly event: Forward;
  } | null = null;
  let deliveryHeld = false;
  let integrityHold: WatcherCoordinatorHoldReason | null = null;
  let integrityRetry: ReturnType<typeof setTimeout> | null = null;
  const scheduleIntegrityRetry = () => {
    if (stopped || integrityRetry !== null) return;
    integrityRetry = setTimeout(() => {
      integrityRetry = null;
      void resume().catch((error: unknown) => {
        deliveryFailure = error;
        settleDeliveryWaiters();
      });
    }, 1_000);
    integrityRetry.unref();
  };
  let firstArrival = true;
  let deliveryFailure: unknown = null;
  const deliveryWaiters = new Set<{
    resolve: () => void;
    reject: (error: unknown) => void;
  }>();
  const settleDeliveryWaiters = () => {
    if (
      deliveryFailure === null &&
      !stopped &&
      (deliveryHeld ||
        integrityHold !== null ||
        rollbackPoint !== null ||
        quarantined)
    )
      return;
    for (const waiter of deliveryWaiters) {
      if (deliveryFailure !== null) waiter.reject(deliveryFailure);
      else if (stopped)
        waiter.reject(new Error("Watcher coordinator is stopped"));
      else waiter.resolve();
    }
    deliveryWaiters.clear();
  };
  const fenceRollback = (point: WatcherNativeChainSyncPoint) => {
    epoch += 1;
    captured.clear();
    lastForward = null;
    deliveryHeld = false;
    input.hooks.onRollbackArrived?.(point);
  };
  let drainingEpoch: number | null = null;
  const assertCurrentDrain = () => {
    if (stopped || (drainingEpoch !== null && epoch !== drainingEpoch))
      throw new WatcherConsumerDeliveryHeld();
  };
  let serial: Promise<void> = Promise.resolve();
  const enqueue = (work: () => Promise<void>): Promise<void> => {
    const task = serial.then(async () => {
      if (stopped) throw new Error("Watcher coordinator is stopped");
      try {
        await work();
      } catch (error) {
        const reason = coordinatorHoldReason(error);
        if (reason === null) throw error;
        integrityHold = reason;
        deliveryHeld = true;
        captured.clear();
        scheduleIntegrityRetry();
      }
    });
    serial = task.then(settleDeliveryWaiters, (error: unknown) => {
      deliveryFailure = error;
      settleDeliveryWaiters();
    });
    return task;
  };
  const drain = async ({
    block,
    event,
  }: {
    readonly block: WatcherNativeBlockAdmission;
    readonly event: Forward;
  }): Promise<void> => {
    // Touched blocks are observed at first visibility; quiet blocks cost
    // nothing beyond the local classification until they finalize.
    if (relevanceOf(block) === "touched") await observe(block, event);
    if (!(await replayRetainedPrefix(event))) return;
    await advanceCanonical(event);
    if (input.hooks.onIncluded !== undefined) {
      // Replay the volatile suffix in native order. Finalized processing keeps
      // its own cursor and may still be waiting on its oldest pending block.
      let includedParent = effectiveHead;
      for (const candidate of [...buffered.values()].sort((left, right) =>
        BigInt(left.blockNo) < BigInt(right.blockNo) ? -1 : 1,
      )) {
        if (
          includedParent !== null &&
          (candidate.prevHash !== includedParent.blockHash ||
            BigInt(candidate.blockNo) !== BigInt(includedParent.blockNo) + 1n ||
            BigInt(candidate.slot) <= BigInt(includedParent.slot))
        )
          throw new Error(
            "included native suffix is not contiguous with canonical progress",
          );
        includedParent = headOf(candidate);
        const candidateKey = pointKey(candidate.blockHash, candidate.slot);
        if (includedHooked.has(candidateKey) || waitingInRun.has(candidateKey))
          continue;
        const relevance = relevanceOf(candidate);
        assertCurrentDrain();
        await input.hooks.onIncluded({
          nativeBlock: candidate,
          relevance,
          localObservation:
            relevance === "touched" ? await observe(candidate, event) : null,
        });
        assertCurrentDrain();
        includedHooked.add(candidateKey);
      }
      for (const includedKey of includedHooked)
        if (!buffered.has(includedKey)) includedHooked.delete(includedKey);
    }
    const minimumBlockNo = BigInt(block.blockNo) - 2_160n;
    for (const [bufferedKey, candidate] of buffered) {
      if (BigInt(candidate.blockNo) < minimumBlockNo) forget(bufferedKey);
    }
    for (const [hookedKey, hooked] of releaseFinalizedHooked) {
      if (BigInt(hooked.blockNo) < minimumBlockNo) {
        releaseFinalizedHooked.delete(hookedKey);
      }
    }
  };
  const drainHeld = async (forward: {
    readonly block: WatcherNativeBlockAdmission;
    readonly event: Forward;
  }): Promise<void> => {
    deliveryHeld = false;
    drainingEpoch = epoch;
    try {
      await drain(forward);
    } catch (error) {
      if (!(error instanceof WatcherConsumerDeliveryHeld)) throw error;
      deliveryHeld = true;
      // Retain every undelivered block. A bounded backlog fails explicitly
      // instead of allowing the normal retention sweep to discard it.
      if (buffered.size > 2_160)
        throw new Error(
          "Watcher held consumer backlog exceeds the retained block bound",
        );
    } finally {
      drainingEpoch = null;
    }
  };
  const resume = () => {
    const requestedEpoch = epoch;
    return enqueue(async () => {
      await waitForRestartRecovery();
      if (integrityHold !== null) {
        try {
          if (input.durable.reconcile === undefined)
            throw new Error("durable reconciliation is unavailable");
          await input.durable.reconcile();
          quarantined = input.durable.readFinality().phase === "quarantined";
        } catch (error) {
          throw new WatcherDurableAuthorityConflict(
            "watcher durable reconciliation remains held",
            { cause: error },
          );
        }
        integrityHold = null;
        captured.clear();
        if (lastForward !== null && rollbackPoint !== null) {
          await processRollbackReplacement(
            bufferedRollbackReplacement(),
            lastForward.event,
          );
        }
      }
      if (
        requestedEpoch !== epoch ||
        !deliveryHeld ||
        lastForward === null ||
        rollbackPoint !== null
      )
        return;
      if (quarantined || input.durable.readFinality().phase === "quarantined")
        return;
      await drainHeld(lastForward);
    });
  };

  return Object.freeze({
    schemaVersion: WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION,
    handle: (event) => {
      if (stopped)
        return Promise.reject(new Error("Watcher coordinator is stopped"));
      const selectedPoint = input.restartIntersection;
      const acknowledgement =
        firstArrival &&
        event.kind === "roll_backward" &&
        selectedPoint !== undefined &&
        (selectedPoint.kind === "origin"
          ? event.point.kind === "origin"
          : event.point.kind === "point" &&
            event.point.blockHash === selectedPoint.blockHash &&
            event.point.slot === selectedPoint.slot);
      firstArrival = false;
      const rewindsHead =
        event.kind === "roll_backward" &&
        progress !== null &&
        effectiveHead !== null &&
        (event.point.kind === "origin" ||
          event.point.blockHash !== effectiveHead.blockHash ||
          event.point.slot !== effectiveHead.slot);
      if (
        event.kind === "roll_backward" &&
        (!acknowledgement || quarantined || rewindsHead)
      ) {
        fenceRollback(event.point);
      }
      const arrivalEpoch = epoch;
      return enqueue(async () => {
        if (event.kind === "roll_forward" && arrivalEpoch !== epoch) return;
        if (integrityHold !== null) {
          try {
            if (input.durable.reconcile === undefined)
              throw new Error("durable reconciliation is unavailable");
            await input.durable.reconcile();
            quarantined = input.durable.readFinality().phase === "quarantined";
          } catch (error) {
            throw new WatcherDurableAuthorityConflict(
              "watcher durable reconciliation remains held",
              { cause: error },
            );
          }
          integrityHold = null;
          captured.clear();
        }
        const initialAcknowledgement = acknowledgement;
        if (initialAcknowledgement) {
          // Native FindIntersect acknowledges the selected point with a backward
          // frame. This first exact acknowledgement is not a rewind unless the
          // node intersected below the recorded processed head.
          await waitForRestartRecovery();
          if (!quarantined) {
            // With a progress ring the node was offered the recorded head
            // first, so a lower selection means it lacks that head: a rewind.
            // Without one, a lower selection is the lagging queue cursor and
            // the retained prefix replays below.
            const head = effectiveHead;
            if (
              progress === null ||
              head === null ||
              (event.point.kind === "point" &&
                event.point.blockHash === head.blockHash &&
                event.point.slot === head.slot)
            )
              return;
          }
        }
        if (event.kind === "roll_backward") {
          // The production hook invalidates the in-memory actuation generation
          // before awaiting its durable cache rollback.
          lastForward = null;
          deliveryHeld = false;
          includedHooked.clear();
          await input.hooks.onRollback(event.point);
          replayBoundary = null;
          retainedReplay = null;
          retainedReplacement = null;
          commonPrefixReplay = [];
          for (const [key, hooked] of releaseFinalizedHooked) {
            if (
              event.point.kind === "origin" ||
              BigInt(hooked.slot) > BigInt(event.point.slot) ||
              (hooked.slot === event.point.slot &&
                hooked.blockHash !== event.point.blockHash)
            ) {
              releaseFinalizedHooked.delete(key);
            }
          }
        }
        const heldForRecovery = quarantined;
        await waitForRestartRecovery();
        if (quarantined) {
          quarantined = await retryQuarantinedRecovery(
            input.durable,
            event,
            guardRecoveryEvent(
              event,
              () => !stopped && epoch === arrivalEpoch,
              input.dependencies.unsafeAllowUnprovenancedEvents === true,
            ),
          );
          if (quarantined) return;
        }
        if (event.kind === "roll_backward") {
          const point = event.point;
          for (const [key, block] of buffered) {
            if (
              point.kind === "origin" ||
              BigInt(block.slot) > BigInt(point.slot) ||
              (block.slot === point.slot && block.blockHash !== point.blockHash)
            ) {
              forget(key);
            }
          }
          progress?.rollbackTo(point);
          progressHead = progress?.readHead() ?? null;
          const finality = input.durable.readFinality();
          const frontier =
            finality.phase === "pending"
              ? finality.pending
              : finality.phase === "finalized"
                ? finality.finalized
                : null;
          const retainedAtFork =
            event.point.kind === "point"
              ? [...buffered.values()].find(
                  (candidate) =>
                    event.point.kind === "point" &&
                    candidate.blockHash === event.point.blockHash &&
                    candidate.slot === event.point.slot,
                )
              : undefined;
          if (
            retainedAtFork !== undefined &&
            !releaseFinalizedHooked.has(
              pointKey(retainedAtFork.blockHash, retainedAtFork.slot),
            )
          ) {
            lastForward = {
              block: retainedAtFork,
              event: {
                schemaVersion: "midgard-watcher-native-chain-sync-v1",
                kind: "roll_forward",
                blockHash: retainedAtFork.blockHash,
                blockType: retainedAtFork.blockType,
                prevHash: retainedAtFork.prevHash,
                slot: retainedAtFork.slot,
                blockNo: retainedAtFork.blockNo,
                rawBlockCbor: retainedAtFork.rawBlockCbor,
                tip: event.tip,
              },
            };
            deliveryHeld = true;
          }
          const belowAuthority =
            frontier === null || frontier === undefined
              ? !heldForRecovery
              : point.kind === "origin" ||
                BigInt(point.slot) < BigInt(frontier.slot) ||
                (point.slot === frontier.slot &&
                  point.blockHash !== frontier.blockHash);
          if (belowAuthority) {
            // The durable authority itself is contradicted: its dedicated
            // rewind path evaluates the replacement block.
            rollbackPoint = point;
            return;
          }
          // Only quiet, ring-attested history above the authority is affected.
          effectiveHead = recomputeEffectiveHead();
          return;
        }
        const block = input.dependencies.admitRollForward(event);
        const key = pointKey(block.blockHash, block.slot);
        const existing = buffered.get(key);
        if (
          existing !== undefined &&
          JSON.stringify(existing) !== JSON.stringify(block)
        ) {
          throw new Error("native buffered block identity was substituted");
        }
        buffered.set(key, block);
        lastForward = { block, event };
        if (rollbackPoint !== null) {
          await processRollbackReplacement(
            bufferedRollbackReplacement(),
            event,
          );
          if (quarantined || rollbackPoint !== null) return;
        }
        lastForward = { block, event };
        await drainHeld({ block, event });
      });
    },
    resume,
    waitForDelivery: () =>
      new Promise<void>((resolve, reject) => {
        // Register behind admitted work, but never await delivery inside this queue.
        void enqueue(async () => {
          deliveryWaiters.add({ resolve, reject });
        }).catch(reject);
      }),
    stop: () => {
      stopped = true;
      if (integrityRetry !== null) clearTimeout(integrityRetry);
      epoch += 1;
      settleDeliveryWaiters();
      return serial;
    },
    status: () =>
      Object.freeze({
        rollbackPoint,
        quarantined,
        integrityHold,
        deliveryHeld,
        bufferedBlockCount: buffered.size,
        processedThrough: effectiveHead,
      }),
  });
};
