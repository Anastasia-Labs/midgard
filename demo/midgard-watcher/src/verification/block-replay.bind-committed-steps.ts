import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import {
  buildValidationMachineLedgerInsertOp,
  type CanonicalTransitionEffect,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";

import {
  replayCandidates,
  type ReplayCore,
} from "./block-replay.replay-candidates.js";
import { type EvaluateWatcherBlockReplayCandidatesInput } from "./block-replay.validate-event-authority.js";
import {
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
} from "./block-replay.watcher-block-replay-reason-codes.js";
import {
  errorResult,
  NULL_CONTEXT,
  orderStageMismatches,
  selectRejection,
  type WatcherBlockReplayContext,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  digestResult,
  orderReasonCodes,
  reasonCodeOf,
  type WatcherBlockReplayAction,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayReasonCode,
  type WatcherBlockReplayResult,
  type WatcherBlockReplayStageMismatch,
  type WatcherBlockReplayTransactionRoot,
} from "./block-replay.watcher-block-replay-result.js";
import { WATCHER_RULE_BUNDLE_REJECTION_SELECTION } from "./rule-bundle.js";

/**
 * The W25 core: runs the canonical Phase B pipeline over already-derived
 * candidates and recomputes every intermediate ledger root with the canonical
 * validation-machine mutator.
 *
 * This is the only place a verdict is produced, and it produces none of its
 * own. Everything after the canonical calls is bookkeeping.
 */
export const evaluateWatcherBlockReplayCandidates = async (
  input: EvaluateWatcherBlockReplayCandidatesInput,
): Promise<WatcherBlockReplayResult> => {
  const context = input.context ?? null;
  const transactionCount = input.candidates.length;
  try {
    const core = await replayCandidates(input);
    return finalizeResult({
      core,
      context,
      transactionCount,
      expectedPriorStateRoot: input.expectedPriorStateRoot,
      expectedPostStateRoot: input.expectedPostStateRoot ?? null,
      committedSteps: null,
    });
  } catch (error) {
    return errorResult([reasonCodeOf(error)], context, transactionCount);
  }
};

export type CommittedReplayGroup = Readonly<{
  step: WatcherBlockReplayCommittedStep;
  txIndex: number | null;
  txId: string | null;
  phase: WatcherBlockReplayCommittedStep["phase"];
  eventKeyFingerprint: string;
  operations: readonly ValidationMachineLedgerOp[];
}>;

export const eventLedgerOperations = (
  effect: CanonicalTransitionEffect,
): readonly ValidationMachineLedgerOp[] =>
  effect.operations.map((operation) =>
    operation.type === "delete"
      ? {
          type: "delete",
          key: Buffer.from(operation.outRefCbor),
        }
      : buildValidationMachineLedgerInsertOp({
          key: operation.outRefCbor,
          outputCbor: operation.outputCbor,
        }),
  );

export const applyRawEventOperations = (
  state: Map<string, Buffer>,
  effect: CanonicalTransitionEffect,
): void => {
  for (const operation of effect.operations) {
    if (operation.type === "delete") {
      state.delete(operation.outRefCbor.toString("hex"));
    } else {
      state.set(
        operation.outRefCbor.toString("hex"),
        Buffer.from(operation.outputCbor),
      );
    }
  }
};

// ---------------------------------------------------------------------------
// Events stage: the committed transition trace
// ---------------------------------------------------------------------------

/**
 * Binds the operator's committed transition trace to the replay.
 *
 * The trace is the operator's own claim about the order the block's events were
 * applied in and about every intermediate root along the way. This is where the
 * watcher's independently recomputed roots meet that claim: the L2 steps must
 * be exactly the accepted transactions in the same order, the chain of
 * committed pre/post roots must run unbroken from the committed
 * `prevUtxosRoot`, and each committed `post_utxos_root` must equal the root the
 * watcher recomputed for that transaction.
 *
 * A non-L2 step is bounded by the same chain, using the local user-event
 * authority-derived effect that the replay core passed through the canonical ledger
 * mutator. This binder does not classify whether the event was due or
 * legitimate.
 */
const bindCommittedSteps = (input: {
  readonly core: ReplayCore;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[];
  readonly expectedPriorStateRoot: string;
}): {
  readonly transactionRoots: readonly WatcherBlockReplayTransactionRoot[];
  readonly stageMismatches: readonly WatcherBlockReplayStageMismatch[];
  readonly reasonCodes: readonly WatcherBlockReplayReasonCode[];
} => {
  const { core, committedSteps } = input;
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];
  const reasonCodes: WatcherBlockReplayReasonCode[] = [];
  const steps = [...committedSteps].sort(
    (left, right) => left.stepIndex - right.stepIndex,
  );
  const seenFingerprints = new Set<string>();
  for (const [index, step] of steps.entries()) {
    if (
      step.stepIndex !== index ||
      seenFingerprints.has(step.eventKeyFingerprint)
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `$.transitionTrace[${index.toString()}].step_index`,
        expected: index.toString(),
        actual: step.stepIndex.toString(),
      });
    }
    seenFingerprints.add(step.eventKeyFingerprint);
    if (
      step.eventToStepIndex !== step.stepIndex ||
      step.eventToStepPhase !== step.phase
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `$.transitionTrace[${step.stepIndex.toString()}].event_to_step`,
        expected: `${step.stepIndex.toString()}:${step.phase}`,
        actual: `${(step.eventToStepIndex ?? -1).toString()}:${step.eventToStepPhase ?? ""}`,
      });
    }
  }
  const transactionByStep = new Map(
    core.transactionRoots.map((root) => [root.committedStepIndex, root]),
  );
  const eventByStep = new Map(
    core.eventRoots.map((root) => [root.stepIndex, root]),
  );
  let expectedPreRoot = input.expectedPriorStateRoot;
  for (const step of steps) {
    const field = `$.transitionTrace[${step.stepIndex.toString()}]`;
    const replayed =
      step.phase === "L2Transaction"
        ? transactionByStep.get(step.stepIndex)
        : eventByStep.get(step.stepIndex);
    if (replayed === undefined) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `${field}.replayedBoundary`,
        expected: `${step.phase}:${step.eventKeyFingerprint}`,
        actual: "missing",
      });
      continue;
    }
    if (
      step.preRoot !== expectedPreRoot ||
      replayed.preRoot !== expectedPreRoot
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `${field}.pre_utxos_root`,
        expected: expectedPreRoot,
        actual: `${step.preRoot}:${replayed.preRoot}`,
      });
    }
    if (step.postRoot !== replayed.postRoot) {
      reasonCodes.push("intermediate_root_mismatch");
      stageMismatches.push({
        stage: "post_state",
        reasonCode: "intermediate_root_mismatch",
        field: `${field}.post_utxos_root`,
        expected: replayed.postRoot,
        actual: step.postRoot,
      });
    }
    expectedPreRoot = step.postRoot;
  }

  return {
    transactionRoots: core.transactionRoots,
    stageMismatches,
    reasonCodes,
  };
};

// ---------------------------------------------------------------------------
// Result assembly
// ---------------------------------------------------------------------------

export const finalizeResult = (input: {
  readonly core: ReplayCore;
  readonly context: WatcherBlockReplayContext | null;
  readonly transactionCount: number;
  readonly expectedPriorStateRoot: string;
  readonly expectedPostStateRoot: string | null;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[] | null;
}): WatcherBlockReplayResult => {
  const { core } = input;
  const reasonCodes = new Set(core.reasonCodes);
  const stageMismatches = [...core.stageMismatches];
  let transactionRoots: readonly WatcherBlockReplayTransactionRoot[] =
    core.transactionRoots;

  const priorStateFailed = reasonCodes.has("prior_state_root_mismatch");

  // #517. The two bindings below are the only places a recomputed root is
  // compared against something the operator committed, and both are
  // conditional. Deriving `accept` from an empty `reasonCodes` alone was
  // therefore fail-open: a caller that supplied neither a committed transition
  // trace nor an expected post-state root skipped both comparisons, left
  // `reasonCodes` empty, and was stamped `accept` without one committed value
  // ever being checked. These receipts are set only from inside the branch that
  // actually performs each binding, and `accept` is gated on both, so an unrun
  // binding cannot be an acceptance regardless of what the reason-code set
  // happens to contain.
  let committedTraceBound = false;
  let postStateRootBound = false;

  if (!priorStateFailed && input.committedSteps !== null) {
    const bound = bindCommittedSteps({
      core,
      committedSteps: input.committedSteps,
      expectedPriorStateRoot: input.expectedPriorStateRoot,
    });
    transactionRoots = bound.transactionRoots;
    stageMismatches.push(...bound.stageMismatches);
    for (const code of bound.reasonCodes) {
      reasonCodes.add(code);
    }
    committedTraceBound = true;
  }

  if (!priorStateFailed && input.expectedPostStateRoot !== null) {
    postStateRootBound = true;
    if (core.postStateRoot !== input.expectedPostStateRoot) {
      reasonCodes.add("post_state_root_mismatch");
      stageMismatches.push({
        stage: "post_state",
        reasonCode: "post_state_root_mismatch",
        field: "$.header.utxosRoot",
        expected: input.expectedPostStateRoot,
        actual: core.postStateRoot,
      });
    }
  }

  if (!committedTraceBound) {
    reasonCodes.add("committed_trace_binding_unrun");
  }
  if (!postStateRootBound) {
    reasonCodes.add("post_state_binding_unrun");
  }

  // `accept` means root-exact replay only. W25 does not adjudicate whether
  // an authenticated event was due, omitted, fabricated, or duplicated.
  const action: WatcherBlockReplayAction =
    committedTraceBound && postStateRootBound && reasonCodes.size === 0
      ? "accept"
      : "reject";

  return digestResult({
    schemaVersion: WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
    action,
    reasonCodes: orderReasonCodes(reasonCodes),
    verifiedRequires: WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
    rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    ...(input.context ?? NULL_CONTEXT),
    authorityManifestDigest: core.authorityManifestDigest,
    sourceManifestDigest: core.sourceManifestDigest,
    effectManifestDigest: core.effectManifestDigest,
    priorStateRoot: core.priorStateRoot,
    expectedPriorStateRoot: input.expectedPriorStateRoot,
    postStateRoot: core.postStateRoot,
    expectedPostStateRoot: input.expectedPostStateRoot,
    transactionCount: input.transactionCount,
    acceptedCount: core.acceptedTxIds.length,
    acceptedTxIds: Object.freeze([...core.acceptedTxIds]),
    intermediateRoots: Object.freeze([...core.intermediateRoots]),
    transactionRoots: Object.freeze([...transactionRoots]),
    eventRoots: Object.freeze([...core.eventRoots]),
    forcedValidationFacts: Object.freeze([...core.forcedValidationFacts]),
    stageMismatches: orderStageMismatches(stageMismatches),
    rejections: Object.freeze([...core.rejections]),
    selectedRejection: selectRejection(core.rejections),
  });
};
