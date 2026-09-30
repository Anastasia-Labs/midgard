import { makeReturn } from "@al-ft/midgard-sdk";
import { buildValidationMachineLedgerMutationSteps } from "@al-ft/midgard-validation";
import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation/phase-b";
import type {
  PhaseAConfig,
  PhaseAValidatedTx,
  PhaseBConfig,
} from "@al-ft/midgard-validation/types";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  applyRawEventOperations,
  type CommittedReplayGroup,
  eventLedgerOperations,
} from "./block-replay.bind-committed-steps.js";
import {
  buildLedgerOperations,
  type ReplayCore,
} from "./block-replay.replay-candidates.js";
import {
  applyAcceptedCandidate,
  replayForcedTransitionEffect,
} from "./block-replay.replay-forced-transition-effect.js";
import {
  eventEffectManifest,
  type ReplayDeploymentBinding,
  type ReplayHeaderBinding,
  validateEventAuthority,
} from "./block-replay.validate-event-authority.js";
import {
  eventKeyFingerprint,
  type ValidatedEventAuthority,
  watcherBlockReplayPriorState,
} from "./block-replay.watcher-block-replay-prior-state.js";
import { REACHABLE_SET } from "./block-replay.watcher-block-replay-reason-codes.js";
import {
  normalizeRootHex,
  type WatcherBlockReplayPriorUtxo,
  watcherBlockReplayRejectionProjection,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  fail,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayEventRoot,
  type WatcherBlockReplayForcedValidationFact,
  type WatcherBlockReplayIntermediateRoot,
  type WatcherBlockReplayReasonCode,
  type WatcherBlockReplayRejection,
  type WatcherBlockReplayStageMismatch,
  type WatcherBlockReplayTransactionRoot,
} from "./block-replay.watcher-block-replay-result.js";
import { type WatcherCommittedEventClaim } from "./event-claims.js";

/**
 * Replays the authenticated transition sequence exactly. Non-L2 deltas are
 * local user-event authority effects bound by the committed trace roots; contiguous
 * L2 runs are evaluated by canonical Phase B against the state produced by all
 * preceding events, so event/L2 interleavings cannot observe a stale ledger.
 */
export const replayCommittedBlock = async (input: {
  readonly candidates: readonly PhaseAValidatedTx[];
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly expectedPriorStateRoot: string;
  readonly config: PhaseBConfig;
  readonly phaseAConfig: PhaseAConfig;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[];
  readonly eventAuthorities: readonly WatcherBlockReplayEventAuthority[];
  readonly committedEventClaims: readonly WatcherCommittedEventClaim[];
  readonly deployment: ReplayDeploymentBinding;
  readonly header: ReplayHeaderBinding;
}): Promise<ReplayCore> => {
  const prior = await watcherBlockReplayPriorState(input.priorState);
  const reasonCodes = new Set<WatcherBlockReplayReasonCode>();
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];
  if (prior.root !== input.expectedPriorStateRoot) {
    reasonCodes.add("prior_state_root_mismatch");
    stageMismatches.push({
      stage: "prior_state",
      reasonCode: "prior_state_root_mismatch",
      field: "$.header.prevUtxosRoot",
      expected: input.expectedPriorStateRoot,
      actual: prior.root,
    });
    return {
      reasonCodes,
      stageMismatches,
      rejections: [],
      acceptedTxIds: [],
      intermediateRoots: [],
      transactionRoots: [],
      eventRoots: [],
      forcedValidationFacts: [],
      priorStateRoot: prior.root,
      postStateRoot: prior.root,
      authorityManifestDigest: null,
      sourceManifestDigest: null,
      effectManifestDigest: null,
    };
  }

  const steps = [...input.committedSteps].sort(
    (left, right) => left.stepIndex - right.stepIndex,
  );
  const candidateByTxId = new Map(
    input.candidates.map((candidate) => [
      candidate.ledgerTx.txId.toString("hex"),
      candidate,
    ]),
  );
  const indexByTxId = new Map(
    input.candidates.map((candidate, index) => [
      candidate.ledgerTx.txId.toString("hex"),
      index,
    ]),
  );
  const authorityByFingerprint = new Map<string, ValidatedEventAuthority>();
  for (const [index, authority] of input.eventAuthorities.entries()) {
    const fingerprint = eventKeyFingerprint(authority.eventKey);
    if (authorityByFingerprint.has(fingerprint)) {
      return fail(
        "duplicate_event_authority",
        `$.eventAuthorities[${index.toString()}].eventKey`,
      );
    }
    authorityByFingerprint.set(
      fingerprint,
      await validateEventAuthority(
        authority,
        input.committedEventClaims,
        input.deployment,
        input.header,
      ),
    );
  }

  const state = new Map(
    prior.ledgerEntries.map((entry) => [
      entry.outRef.toString("hex"),
      Buffer.from(entry.output),
    ]),
  );
  const groups: CommittedReplayGroup[] = [];
  const rejections: WatcherBlockReplayRejection[] = [];
  const acceptedTxIds: string[] = [];
  const seenCandidateIds = new Set<string>();
  const seenEventFingerprints = new Set<string>();
  const forcedValidationFacts: WatcherBlockReplayForcedValidationFact[] = [];

  let pendingL2: WatcherBlockReplayCommittedStep[] = [];
  const flushL2 = async (): Promise<void> => {
    if (pendingL2.length === 0) {
      return;
    }
    const segmentSteps = pendingL2;
    pendingL2 = [];
    const segmentCandidates: PhaseAValidatedTx[] = [];
    for (const step of segmentSteps) {
      const txId = step.txId;
      const candidate = txId === null ? undefined : candidateByTxId.get(txId);
      if (
        candidate === undefined ||
        txId === null ||
        seenCandidateIds.has(txId)
      ) {
        reasonCodes.add("transition_trace_mismatch");
        stageMismatches.push({
          stage: "events",
          reasonCode: "transition_trace_mismatch",
          field: `$.transitionTrace[${step.stepIndex.toString()}].event_key`,
          expected: "one previously-unseen Phase-A-accepted transaction",
          actual: txId ?? "",
        });
        continue;
      }
      seenCandidateIds.add(txId);
      segmentCandidates.push(candidate);
    }
    let phaseB;
    try {
      phaseB = await makeReturn(
        runPhaseBValidationWithPatch(segmentCandidates, state, input.config),
      ).unsafeRun();
    } catch {
      return fail("canonical_validation_threw", "$.phaseB");
    }
    const projected = phaseB.rejected
      .map((rejected) =>
        watcherBlockReplayRejectionProjection({ rejected, indexByTxId }),
      )
      .sort((left, right) => left.index - right.index);
    rejections.push(...projected);
    for (const rejection of projected) {
      if (!REACHABLE_SET.has(rejection.code)) {
        reasonCodes.add("undeclared_reachable_code");
      }
      reasonCodes.add("phase_b_rejection");
    }
    const segmentGroupStart = groups.length;
    for (const candidate of phaseB.accepted) {
      const txId = candidate.ledgerTx.txId.toString("hex");
      const step = segmentSteps.find((entry) => entry.txId === txId);
      if (step === undefined) {
        return fail("canonical_replay_threw", `$.accepted[${txId}]`);
      }
      const operations = buildLedgerOperations([candidate])[0]!.operations;
      groups.push({
        step,
        txIndex: indexByTxId.get(txId) ?? null,
        txId,
        phase: "L2Transaction",
        eventKeyFingerprint: step.eventKeyFingerprint,
        operations,
      });
      acceptedTxIds.push(txId);
      applyAcceptedCandidate(state, candidate);
    }
    // A canonical rejection is a real no-op boundary, not a missing event.
    // Insert these boundaries without changing canonical accepted replay order.
    for (const rejection of projected) {
      const step = segmentSteps.find((entry) => entry.txId === rejection.txId);
      if (step === undefined) {
        return fail("canonical_replay_threw", `$.rejected[${rejection.txId}]`);
      }
      const group: CommittedReplayGroup = {
        step,
        txIndex: rejection.index,
        txId: rejection.txId,
        phase: "L2Transaction",
        eventKeyFingerprint: step.eventKeyFingerprint,
        operations: [],
      };
      const nextIndex = groups.findIndex(
        (entry, index) =>
          index >= segmentGroupStart && entry.step.stepIndex > step.stepIndex,
      );
      groups.splice(nextIndex < 0 ? groups.length : nextIndex, 0, group);
    }
  };

  for (const step of steps) {
    if (step.phase === "L2Transaction") {
      pendingL2.push(step);
      continue;
    }
    await flushL2();
    const authority = authorityByFingerprint.get(step.eventKeyFingerprint);
    if (authority === undefined) {
      return fail(
        "missing_event_authority",
        `$.transitionTrace[${step.stepIndex.toString()}].event_key`,
      );
    }
    if (authority.phase !== step.phase) {
      return fail(
        "event_authority_identity_mismatch",
        `$.eventAuthorities[${step.eventKeyFingerprint}].phase`,
      );
    }
    const forcedReplay = await replayForcedTransitionEffect({
      authority,
      state,
      phaseAConfig: input.phaseAConfig,
      phaseBConfig: input.config,
      step,
    });
    if (forcedReplay !== null) {
      const forcedValidationFact = forcedReplay.fact;
      if (
        forcedValidationFacts.some(
          (fact) =>
            fact.eventKeyFingerprint ===
              forcedValidationFact.eventKeyFingerprint ||
            fact.stepIndex === forcedValidationFact.stepIndex,
        )
      ) {
        return fail(
          "duplicate_event_authority",
          `$.forcedValidationFacts[${step.stepIndex.toString()}]`,
        );
      }
      forcedValidationFacts.push(forcedValidationFact);
      if (
        forcedValidationFact.authenticatedOperatorValidity !==
        forcedValidationFact.canonicalOperatorValidity
      ) {
        reasonCodes.add("transition_effect_semantics_mismatch");
        stageMismatches.push({
          stage: "events",
          reasonCode: "transition_effect_semantics_mismatch",
          field: `$.transitionTrace[${step.stepIndex.toString()}].operatorValidity`,
          expected: forcedValidationFact.canonicalOperatorValidity,
          actual: forcedValidationFact.authenticatedOperatorValidity,
        });
      }
    }
    const effect = forcedReplay?.effect ?? authority.effect;
    if (effect === null) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.unresolved",
      );
    }
    authorityByFingerprint.set(
      step.eventKeyFingerprint,
      Object.freeze({
        ...authority,
        effect,
        effectManifest: eventEffectManifest(
          authority.phase,
          authority.eventKeyFingerprint,
          effect,
        ),
      }),
    );
    seenEventFingerprints.add(step.eventKeyFingerprint);
    groups.push({
      step,
      txIndex: null,
      txId: null,
      phase: step.phase,
      eventKeyFingerprint: step.eventKeyFingerprint,
      operations: eventLedgerOperations(effect),
    });
    applyRawEventOperations(state, effect);
  }
  await flushL2();

  for (const txId of candidateByTxId.keys()) {
    if (!seenCandidateIds.has(txId)) {
      reasonCodes.add("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: "$.transitionTrace.l2Transactions",
        expected: txId,
        actual: "omitted",
      });
    }
  }
  for (const fingerprint of authorityByFingerprint.keys()) {
    if (!seenEventFingerprints.has(fingerprint)) {
      return fail(
        "event_authority_identity_mismatch",
        `$.eventAuthorities[${fingerprint}]`,
      );
    }
  }

  let mutationSteps;
  try {
    mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: prior.ledgerEntries,
      operations: groups.flatMap((group) => group.operations),
    });
  } catch {
    return fail("canonical_replay_threw", "$.intermediateRoots");
  }

  const intermediateRoots: WatcherBlockReplayIntermediateRoot[] = [];
  const transactionRoots: WatcherBlockReplayTransactionRoot[] = [];
  const eventRoots: WatcherBlockReplayEventRoot[] = [];
  let cursor = 0;
  let postStateRoot = prior.root;
  for (const group of groups) {
    const first = cursor;
    for (
      let remaining = group.operations.length;
      remaining > 0;
      remaining -= 1
    ) {
      const mutationStep = mutationSteps[cursor]!;
      intermediateRoots.push(
        Object.freeze({
          sequence: cursor,
          txIndex: group.txIndex,
          txId: group.txId,
          stepIndex: group.step.stepIndex,
          phase: group.phase,
          operation: mutationStep.operation.type,
          outRef: mutationStep.operation.key.toString("hex"),
          preRoot: normalizeRootHex(mutationStep.preRoot.toString("hex")),
          postRoot: normalizeRootHex(mutationStep.postRoot.toString("hex")),
        }),
      );
      cursor += 1;
    }
    const preRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[first]!.preRoot.toString("hex"));
    postStateRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[cursor - 1]!.postRoot.toString("hex"));
    if (group.phase === "L2Transaction") {
      transactionRoots.push(
        Object.freeze({
          txIndex: group.txIndex!,
          txId: group.txId!,
          preRoot,
          postRoot: postStateRoot,
          mutationCount: cursor - first,
          committedStepIndex: group.step.stepIndex,
          committedPreRoot: group.step.preRoot,
          committedPostRoot: group.step.postRoot,
        }),
      );
    } else {
      eventRoots.push(
        Object.freeze({
          stepIndex: group.step.stepIndex,
          phase: group.phase,
          eventKeyFingerprint: group.eventKeyFingerprint,
          preRoot,
          postRoot: postStateRoot,
          mutationCount: cursor - first,
        }) as WatcherBlockReplayEventRoot,
      );
    }
  }

  const orderedAuthorities = steps.flatMap((step) => {
    if (step.phase === "L2Transaction") {
      return [];
    }
    const authority = authorityByFingerprint.get(step.eventKeyFingerprint);
    return authority === undefined ? [] : [authority];
  });
  return {
    reasonCodes,
    stageMismatches,
    rejections: rejections.sort((left, right) => left.index - right.index),
    acceptedTxIds,
    intermediateRoots,
    transactionRoots,
    eventRoots,
    forcedValidationFacts,
    priorStateRoot: prior.root,
    postStateRoot,
    eventAuthorityRecords: Object.freeze(
      orderedAuthorities.map((authority) => {
        const effect =
          authority.effect ??
          fail("canonical_replay_threw", "$.eventAuthorityRecords.effect");
        return Object.freeze({
          ...authority.recordSource,
          transitionEffect: Object.freeze({
            canonicalCborHex: effect.canonicalCbor.toString("hex"),
            digest: effect.digest,
            operations: Object.freeze(
              effect.operations.map((operation) =>
                Object.freeze(
                  operation.type === "delete"
                    ? {
                        type: operation.type,
                        outRefCborHex: operation.outRefCbor.toString("hex"),
                      }
                    : {
                        type: operation.type,
                        outRefCborHex: operation.outRefCbor.toString("hex"),
                        outputCborHex: operation.outputCbor.toString("hex"),
                      },
                ),
              ),
            ),
          }),
        });
      }),
    ),
    authorityManifestDigest: watcherSha256CanonicalJson(
      orderedAuthorities.map((authority) => authority.authorityManifest),
    ),
    sourceManifestDigest: watcherSha256CanonicalJson(
      steps.map((step) => ({
        stepIndex: step.stepIndex,
        phase: step.phase,
        eventKeyFingerprint: step.eventKeyFingerprint,
        preRoot: step.preRoot,
        postRoot: step.postRoot,
        eventToStepIndex: step.eventToStepIndex,
        eventToStepPhase: step.eventToStepPhase,
      })),
    ),
    effectManifestDigest: watcherSha256CanonicalJson(
      orderedAuthorities.map((authority) => authority.effectManifest),
    ),
  };
};
