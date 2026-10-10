import type {
  AuthenticatedStateQueueHeaderObservation,
  EventKey,
  EventToStepValue,
  EvidenceProvenance,
  TransitionStep,
} from "@al-ft/midgard-sdk";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import type {
  PhaseAValidatedTx,
  QueuedTx,
} from "@al-ft/midgard-validation/types";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  eventKeyFingerprint,
  eventKeyTxId,
  phaseForEventKey,
} from "./block-replay.watcher-block-replay-prior-state.js";
import { type WatcherBlockReplayPriorUtxo } from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  fail,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayEventAuthority,
} from "./block-replay.watcher-block-replay-result.js";
import type { WatcherHeaderRootReconstructionResult } from "./header-root-reconstruction.js";
import { WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION } from "./header-root-reconstruction.js";
import {
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  type WatcherPhaseAVerificationResult,
} from "./phase-a-verifier.js";
import { type WatcherRuleBundle } from "./rule-bundle.js";

// ---------------------------------------------------------------------------
// Block evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherBlockReplayInput = {
  /** L1-authenticated header observation, as W22 and W24 consumed it. */
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  /** The accepted W22 record for this block. */
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  /** The accepted W24 record for this block. */
  readonly phaseA: WatcherPhaseAVerificationResult;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the header decision's canonical evidence. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenance;
  /** Prior-state ledger entries: the parent block's canonical reconstruction UTxOs. */
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  /** The W23 rule bundle, with its commitment. */
  readonly ruleBundle: WatcherRuleBundle;
  readonly ruleBundleCommitment: string;
  /** Local user-event publication authorities and shared canonical effects. */
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthority[];
  readonly minimumConfirmationDepth?: number;
};

/** Re-checks the caller's W22 record against a fresh canonical recomputation. */
export const bindReconstruction = (input: {
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
}): void => {
  const { reconstruction } = input;
  if (
    reconstruction.schemaVersion !==
    WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION
  ) {
    fail("reconstruction_unsupported_schema", "$.reconstruction.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = reconstruction;
  if (watcherSha256CanonicalJson(withoutDigest) !== resultDigest) {
    fail("reconstruction_digest_mismatch", "$.reconstruction.resultDigest");
  }
  if (
    reconstruction.action !== "accept" ||
    reconstruction.reconstructedRoots === null ||
    reconstruction.reconstructedCounts === null ||
    reconstruction.reasonCodes.length !== 0
  ) {
    fail("reconstruction_not_accepted", "$.reconstruction.action");
  }
  if (reconstruction.headerHash !== input.headerHash) {
    fail("reconstruction_header_mismatch", "$.reconstruction.headerHash");
  }
  if (reconstruction.payloadEnvelopeSha256 !== input.payloadEnvelopeSha256) {
    fail("payload_bytes_mismatch", "$.reconstruction.payloadEnvelopeSha256");
  }
};

/**
 * Re-checks the caller's W24 record the same way, and additionally requires it
 * to describe this exact block, this exact reconstruction, and this exact rule
 * bundle. A Phase A record that rejected anything stops the replay: replaying a
 * block whose transactions Phase A already refused would attribute Phase B
 * faults to a block that never had a valid candidate set.
 */
export const bindPhaseAV1 = (input: {
  readonly phaseA: WatcherPhaseAVerificationResult;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly reconstructionDigest: string;
  readonly ruleBundleCommitment: string;
}): void => {
  const { phaseA } = input;
  if (phaseA.schemaVersion !== WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION) {
    fail("phase_a_unsupported_schema", "$.phaseA.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = phaseA;
  if (watcherSha256CanonicalJson(withoutDigest) !== resultDigest) {
    fail("phase_a_digest_mismatch", "$.phaseA.resultDigest");
  }
  if (
    phaseA.action !== "accept" ||
    phaseA.rejections.length !== 0 ||
    phaseA.reasonCodes.length !== 0
  ) {
    fail("phase_a_not_accepted", "$.phaseA.action");
  }
  if (
    phaseA.headerHash !== input.headerHash ||
    phaseA.payloadEnvelopeSha256 !== input.payloadEnvelopeSha256 ||
    phaseA.reconstructionDigest !== input.reconstructionDigest ||
    phaseA.ruleBundleCommitment !== input.ruleBundleCommitment
  ) {
    fail("phase_a_context_mismatch", "$.phaseA.headerHash");
  }
};

/**
 * Projects the canonical reconstruction's transition trace and event-to-step
 * map into the committed-step shape the events stage binds. Every field is
 * copied from the canonical decode; nothing is re-derived.
 */
export const watcherBlockReplayCommittedSteps = (input: {
  readonly transitionTrace: readonly {
    readonly key: bigint;
    readonly value: TransitionStep;
  }[];
  readonly eventToStep: readonly {
    readonly key: EventKey;
    readonly value: EventToStepValue;
  }[];
}): readonly WatcherBlockReplayCommittedStep[] => {
  const eventToStepByFingerprint = new Map<string, EventToStepValue>();
  for (const [index, entry] of input.eventToStep.entries()) {
    const fingerprint = eventKeyFingerprint(entry.key);
    if (eventToStepByFingerprint.has(fingerprint)) {
      fail(
        "transition_trace_mismatch",
        `$.eventToStep[${index.toString()}].eventKey`,
      );
    }
    eventToStepByFingerprint.set(fingerprint, entry.value);
  }
  const orderedTrace = [...input.transitionTrace].sort((left, right) =>
    left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
  );
  if (eventToStepByFingerprint.size !== orderedTrace.length) {
    fail("transition_trace_mismatch", "$.eventToStep.length");
  }
  const seenTraceFingerprints = new Set<string>();
  return Object.freeze(
    orderedTrace.map((entry, index) => {
      const fingerprint = eventKeyFingerprint(entry.value.event_key);
      const mapped =
        eventToStepByFingerprint.get(fingerprint) ??
        fail(
          "transition_trace_mismatch",
          `$.transitionTrace[${index.toString()}].eventToStep`,
        );
      if (
        entry.key !== BigInt(index) ||
        entry.value.step_index !== BigInt(index) ||
        entry.value.phase !== phaseForEventKey(entry.value.event_key) ||
        seenTraceFingerprints.has(fingerprint) ||
        mapped.step_index !== entry.value.step_index ||
        mapped.phase !== entry.value.phase
      ) {
        fail(
          "transition_trace_mismatch",
          `$.transitionTrace[${index.toString()}]`,
        );
      }
      seenTraceFingerprints.add(fingerprint);
      return Object.freeze({
        stepIndex: Number(entry.value.step_index),
        phase: entry.value.phase,
        txId: eventKeyTxId(entry.value.event_key),
        eventKeyFingerprint: fingerprint,
        preRoot: entry.value.pre_utxos_root,
        postRoot: entry.value.post_utxos_root,
        eventToStepIndex: Number(mapped.step_index),
        eventToStepPhase: mapped.phase,
      });
    }),
  );
};

export const snapshotWatcherBlockReplayEventAuthorities = (
  authorities: readonly WatcherBlockReplayEventAuthority[],
): readonly WatcherBlockReplayEventAuthority[] =>
  Object.freeze(
    authorities.map((authority) => {
      const origin = { userEvent: authority.userEvent };
      const eventKey = structuredClone(authority.eventKey);
      if (authority.phase === "ForcedTransaction") {
        if (
          "transitionEffect" in authority ||
          authority.canonicalNativeTxCbor == null
        ) {
          return fail(
            "transition_effect_semantics_mismatch",
            "$.transitionEffect.forced.callerEffect",
          );
        }
        return Object.freeze({
          ...origin,
          eventKey,
          phase: authority.phase,
          canonicalNativeTxCbor: Buffer.from(authority.canonicalNativeTxCbor),
          programMaterialSidecarCbor:
            authority.programMaterialSidecarCbor == null
              ? null
              : Buffer.from(authority.programMaterialSidecarCbor),
        });
      }
      return Object.freeze({
        ...origin,
        eventKey,
        phase: authority.phase,
        transitionEffect: Object.freeze({
          ...authority.transitionEffect,
          canonicalCbor: Buffer.from(authority.transitionEffect.canonicalCbor),
          operations: Object.freeze(
            authority.transitionEffect.operations.map((operation) =>
              Object.freeze({
                ...operation,
                outRefCbor: Buffer.from(operation.outRefCbor),
                ...(operation.type === "insert"
                  ? { outputCbor: Buffer.from(operation.outputCbor) }
                  : {}),
              }),
            ),
          ),
        }),
      });
    }),
  );

/**
 * Re-derives the canonical Phase A candidates the replay needs.
 *
 * W24's record carries verdicts, not the `PhaseAValidatedTx` values Phase B
 * consumes, so the candidates are produced by the same canonical function W24
 * used (`validatePhaseASingle`) over the same canonical inputs (W24's own
 * `watcherPhaseAQueuedTxs` derivation and `makeWatcherPhaseAConfig`
 * configuration). The result is then required to agree with W24's accepted list
 * exactly, so the two lanes cannot silently disagree about what the block
 * contains.
 */
export const deriveCandidates = (
  queuedTxs: readonly QueuedTx[],
  config: Parameters<typeof validatePhaseASingle>[1],
  phaseA: WatcherPhaseAVerificationResult,
): readonly PhaseAValidatedTx[] => {
  const candidates: PhaseAValidatedTx[] = [];
  for (const [index, queuedTx] of queuedTxs.entries()) {
    let outcome;
    try {
      outcome = validatePhaseASingle(queuedTx, config);
    } catch {
      return fail("canonical_validation_threw", `$.candidates[${index}]`);
    }
    if (!("ledgerTx" in outcome)) {
      return fail("phase_a_candidate_mismatch", `$.candidates[${index}]`);
    }
    candidates.push(outcome);
  }
  const derivedTxIds = candidates.map((candidate) =>
    candidate.ledgerTx.txId.toString("hex"),
  );
  if (
    derivedTxIds.length !== phaseA.acceptedTxIds.length ||
    derivedTxIds.some((txId, index) => txId !== phaseA.acceptedTxIds[index])
  ) {
    fail("phase_a_candidate_mismatch", "$.phaseA.acceptedTxIds");
  }
  return Object.freeze(candidates);
};
