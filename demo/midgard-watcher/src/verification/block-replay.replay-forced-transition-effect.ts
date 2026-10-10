import {
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  type ForcedRejection,
  forcedRejectionReason,
  ForcedRejectionStopped,
} from "@al-ft/midgard-fault-proofs";
import { makeReturn, rejectionReasonArmOf } from "@al-ft/midgard-sdk";
import {
  buildCanonicalTransitionEffect,
  type CanonicalTransitionEffect,
  canonicalTransitionEffectFromStatePatch,
} from "@al-ft/midgard-validation";
import { LedgerColumns } from "@al-ft/midgard-validation/ledger";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation/phase-b";
import type {
  PhaseAConfig,
  PhaseAValidatedTx,
  PhaseBConfig,
  QueuedTx,
  RejectCode,
} from "@al-ft/midgard-validation/types";

import { type ValidatedEventAuthority } from "./block-replay.watcher-block-replay-prior-state.js";
import {
  fail,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayForcedValidationFact,
} from "./block-replay.watcher-block-replay-result.js";
import {
  WATCHER_FORCED_TX_VALID,
  type WatcherForcedOperatorVerdict,
} from "./user-event.js";

/**
 * The arm the node would record for a canonical rejection, when the
 * rejection can be proved exactly. Unsupported or unavailable evaluation stops
 * replay with a typed local failure and produces no forced-validation fact.
 */
export const canonicalRejectionArm = (rejection: ForcedRejection): string => {
  try {
    return rejectionReasonArmOf(forcedRejectionReason(rejection));
  } catch (error) {
    if (error instanceof ForcedRejectionStopped) {
      return fail(
        error.retryable
          ? "forced_evaluation_unavailable"
          : "forced_rejection_unsupported",
        `$.forced.${rejection.consensusPhase ?? "unknown"}.${rejection.code}`,
      );
    }
    throw error;
  }
};

export const applyAcceptedCandidate = (
  state: Map<string, Buffer>,
  candidate: PhaseAValidatedTx,
): void => {
  for (const outRef of candidate.graph.spentOutRefHexes) {
    state.delete(outRef);
  }
  for (const produced of candidate.graph.produced) {
    state.set(
      produced[LedgerColumns.OUTREF].toString("hex"),
      Buffer.from(produced[LedgerColumns.OUTPUT]),
    );
  }
};

export const replayForcedTransitionEffect = async (input: {
  readonly authority: ValidatedEventAuthority;
  readonly state: ReadonlyMap<string, Buffer>;
  readonly phaseAConfig: PhaseAConfig;
  readonly phaseBConfig: PhaseBConfig;
  readonly step: WatcherBlockReplayCommittedStep;
}): Promise<Readonly<{
  fact: WatcherBlockReplayForcedValidationFact;
  effect: CanonicalTransitionEffect;
}> | null> => {
  if (input.authority.phase !== "ForcedTransaction") {
    return null;
  }
  // #517, same class as the acceptance gate above: this used to be a second
  // disjunct of the early `return null`, so a forced step whose canonical
  // native transaction was missing skipped the canonical Phase A/B rerun and
  // the terminal-validity comparison entirely, and the caller applied the
  // authority's effect as if the terminality binding had passed.
  // `validateEventAuthority` already refuses such an authority, so this is
  // unreachable today - which is exactly why it must fail closed rather than
  // silently skip if that invariant is ever relaxed.
  if (input.authority.canonicalNativeTxCbor === null) {
    return fail(
      "transition_effect_semantics_mismatch",
      "$.canonicalNativeTxCbor",
    );
  }
  if (input.authority.committedForcedValidity === null) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.committedEventClaim.verdict",
    );
  }
  const nativeTx = decodeMidgardForcedTxFullFromCanonicalCbor(
    input.authority.canonicalNativeTxCbor,
  );
  const queued: QueuedTx = {
    txId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    txCbor: Buffer.from(input.authority.canonicalNativeTxCbor),
    sourceKind: "forced",
    programMaterialSidecarCbor: input.authority.programMaterialSidecarCbor,
    arrivalSeq: 0n,
    createdAt: new Date(0),
  };
  const phaseA = validatePhaseASingle(queued, input.phaseAConfig);
  let derived = buildCanonicalTransitionEffect([]);
  let phaseAStatus: WatcherBlockReplayForcedValidationFact["phaseAStatus"] =
    "accepted";
  let phaseARejectCode: RejectCode | null = null;
  let phaseBStatus: WatcherBlockReplayForcedValidationFact["phaseBStatus"] =
    "not_run";
  let phaseBRejectCode: RejectCode | null = null;
  let canonicalOperatorValidity: WatcherForcedOperatorVerdict;
  if ("code" in phaseA) {
    phaseAStatus = "rejected";
    phaseARejectCode = phaseA.code;
    const arm = canonicalRejectionArm(phaseA);
    canonicalOperatorValidity = arm;
  } else {
    const phaseB = await makeReturn(
      runPhaseBValidationWithPatch(
        [phaseA],
        new Map(
          [...input.state.entries()].map(([outRef, output]) => [
            outRef,
            Buffer.from(output),
          ]),
        ),
        input.phaseBConfig,
      ),
    ).unsafeRun();
    if (phaseB.rejected.length > 0) {
      if (phaseB.rejected.length !== 1 || phaseB.accepted.length !== 0) {
        return fail("canonical_validation_threw", "$.phaseB.forced");
      }
      phaseBStatus = "rejected";
      phaseBRejectCode = phaseB.rejected[0]!.code;
      const arm = canonicalRejectionArm(phaseB.rejected[0]!);
      canonicalOperatorValidity = arm;
    } else {
      if (phaseB.accepted.length !== 1) {
        return fail("canonical_validation_threw", "$.phaseB.forced");
      }
      phaseBStatus = "accepted";
      canonicalOperatorValidity = WATCHER_FORCED_TX_VALID;
      derived = canonicalTransitionEffectFromStatePatch(phaseB.statePatch);
    }
  }
  return Object.freeze({
    effect: derived,
    fact: Object.freeze({
      eventKeyFingerprint: input.authority.eventKeyFingerprint,
      stepIndex: input.step.stepIndex,
      authenticatedOperatorValidity: input.authority.committedForcedValidity,
      canonicalOperatorValidity,
      phaseAStatus,
      phaseARejectCode,
      phaseBStatus,
      phaseBRejectCode,
      canonicalEffectDigest: derived.digest,
      canonicalEffectMutationCount: derived.operations.length,
    }),
  });
};
