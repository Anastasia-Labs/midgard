import type { WatcherFinalityPolicy } from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../l1/native-chain-sync.js";
import type { WatcherRollbackCanonicalAncestryLink } from "../l1/rollback-engine.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import type { WatcherBlockRelevance } from "./block-relevance.js";
import {
  depthAtTip,
  nextBufferedChild,
  pointKey,
  WATCHER_AUTHORITY_CHECKPOINT_INTERVAL_BLOCKS,
  type WatcherProcessedHead,
} from "./chain-coordinator.canonical-path-from-history.js";
import { WatcherCoordinatorIntegrityHeld } from "./chain-coordinator.integrity-hold.js";
import type { CapturedWatcherObservation } from "./chain-coordinator.observe-captured-block.js";
import { persistQuietRecoveryEvidence } from "./chain-coordinator.recovery-evidence.js";

type Forward = Extract<
  WatcherNativeChainSyncEvent,
  { readonly kind: "roll_forward" }
>;
export const advanceWatcherCanonical = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly buffered: Map<string, WatcherNativeBlockAdmission>;
  readonly captured: Map<string, CapturedWatcherObservation>;
  readonly authorityFinalizedHead: () => WatcherProcessedHead | null;
  readonly effectiveHead: () => WatcherProcessedHead | null;
  readonly onQuarantine: () => void;
  readonly observe: (
    block: WatcherNativeBlockAdmission,
    event: Forward,
  ) => Promise<
    WatcherLocalKupmiosNativeObservation &
      Readonly<{ assertCurrent?: () => void }>
  >;
  readonly deliverFinalized: (
    block: WatcherNativeBlockAdmission,
    observation: WatcherLocalKupmiosNativeObservation | null,
    relevance: WatcherBlockRelevance,
  ) => Promise<void>;
  readonly relevanceOf: (
    block: WatcherNativeBlockAdmission,
  ) => WatcherBlockRelevance;
  readonly forget: (key: string) => void;
  readonly ancestryFromFinalized: (
    finalized: WatcherProcessedHead,
    target: WatcherNativeBlockAdmission,
  ) => readonly WatcherRollbackCanonicalAncestryLink[];
}) => {
  return async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    const confirmationDepth = BigInt(input.policy.confirmationDepth);
    const maximumIterations = input.buffered.size + 1;
    for (let iteration = 0; iteration < maximumIterations; iteration += 1) {
      const state = input.durable.readFinality();
      if (state.phase === "quarantined") {
        input.onQuarantine();
        return;
      }
      if (state.phase === "pending" && state.pending !== null) {
        const pending = state.pending;
        const target =
          [...input.buffered.values()].find(
            (block) =>
              block.blockHash === pending.blockHash &&
              block.slot === pending.slot &&
              block.blockNo === pending.blockNo,
          ) ?? null;
        if (target === null) return;
        const depth = depthAtTip(target, event);
        // Finality needs a second observation at confirmation depth. Every
        // shallower arrival would only persist another pending snapshot.
        if (BigInt(depth) < confirmationDepth) return;
        const key = pointKey(target.blockHash, target.slot);
        if (input.captured.get(key)?.persistedDepth === depth) return;
        const observed = await input.observe(target, event);
        const progressed =
          await input.durable.persistCanonicalProgress(observed);
        if (progressed.persistence === "conflict") {
          throw new WatcherDurableAuthorityConflict(
            "watcher canonical progress persistence conflicted",
          );
        }
        if (progressed.finalityResult.action === "reject")
          throw new WatcherCoordinatorIntegrityHeld(
            "rollback_evidence_rejected",
            "canonical finality evidence was rejected",
          );
        input.captured.get(key)!.persistedDepth = depth;
        if (progressed.finalityResult.action !== "finalize") return;
        await input.deliverFinalized(
          target,
          observed,
          input.relevanceOf(target),
        );
        input.forget(key);
        continue;
      }
      const finalized = input.authorityFinalizedHead();
      const head = input.effectiveHead();
      const target = nextBufferedChild(
        input.buffered,
        head?.blockHash ?? null,
        head?.blockNo ?? null,
      );
      if (target === null) return;
      const key = pointKey(target.blockHash, target.slot);
      const relevance = input.relevanceOf(target);
      const atFinalized =
        finalized !== null &&
        finalized.blockHash === target.blockHash &&
        finalized.slot === target.slot;
      if (
        finalized !== null &&
        !atFinalized &&
        BigInt(target.blockNo) <= BigInt(finalized.blockNo)
      ) {
        throw new Error(
          "watcher processed head trails durable finality by more than one block",
        );
      }
      if (atFinalized) {
        // The authority committed this block but its progress row is absent:
        // the process stopped between them. Re-run its idempotent hooks.
        const observed =
          relevance === "touched"
            ? (input.captured.get(key)?.latest ??
              (await input.observe(target, event)))
            : null;
        await input.deliverFinalized(target, observed, relevance);
        input.forget(key);
        continue;
      }
      const forcedCheckpoint =
        finalized !== null &&
        BigInt(target.blockNo) - BigInt(finalized.blockNo) >=
          WATCHER_AUTHORITY_CHECKPOINT_INTERVAL_BLOCKS;
      if (relevance === "quiet" && !forcedCheckpoint) {
        if (BigInt(depthAtTip(target, event)) < confirmationDepth) return;
        // Quiet blocks are part of either side of a recovery path. Persist
        // their authenticated identity without advancing the sparse authority.
        await persistQuietRecoveryEvidence(
          input.durable,
          await input.observe(target, event),
        );
        await input.deliverFinalized(target, null, "quiet");
        input.forget(key);
        continue;
      }
      if (!input.captured.has(key)) await input.observe(target, event);
      const arrival = input.captured.get(key)!;
      let observed = arrival.first;
      const ancestry =
        finalized === null
          ? []
          : input.ancestryFromFinalized(finalized, target);
      let progressed = await input.durable.persistCanonicalProgress({
        ...observed,
        ancestry,
      });
      if (progressed.persistence === "conflict") {
        throw new WatcherDurableAuthorityConflict(
          "watcher canonical progress persistence conflicted",
        );
      }
      if (progressed.finalityResult.action === "reject")
        throw new WatcherCoordinatorIntegrityHeld(
          "rollback_evidence_rejected",
          "canonical finality evidence was rejected",
        );
      arrival.persistedDepth = arrival.firstDepth;
      if (
        progressed.finalityResult.action !== "finalize" &&
        BigInt(depthAtTip(target, event)) > BigInt(arrival.firstDepth)
      ) {
        observed = await input.observe(target, event);
        progressed = await input.durable.persistCanonicalProgress(observed);
        if (progressed.persistence === "conflict") {
          throw new WatcherDurableAuthorityConflict(
            "watcher canonical progress persistence conflicted",
          );
        }
        if (progressed.finalityResult.action === "reject")
          throw new WatcherCoordinatorIntegrityHeld(
            "rollback_evidence_rejected",
            "canonical finality evidence was rejected",
          );
        arrival.persistedDepth = depthAtTip(target, event);
      }
      if (progressed.finalityResult.action !== "finalize") return;
      await input.deliverFinalized(target, observed, relevance);
      input.forget(key);
    }
    throw new Error("watcher canonical buffer did not converge");
  };
};
