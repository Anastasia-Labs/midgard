import { createHash } from "node:crypto";

import {
  TransitionFaultProof,
  type TransitionFaultProof as TransitionFaultProofData,
  type ValidationClaimWitness,
  ValidationClaimWitness as ValidationClaimWitnessSchema,
  ValidationTraceDescriptor,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  type DeterministicValidationMachineTrace,
  type ValidationMachineReplayInput,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  detectTransitionTraceFaults,
  type TransitionTraceDetectionEvidence,
  transitionTraceFinalIndex,
} from "../transition-trace/index.js";

export const W25_CHALLENGE_COORDINATE =
  "midgard-production-w25-challenge-coordinate-v1" as const;
export const TRANSITION_TRACE_CHALLENGE =
  "midgard-production-transition-trace-challenge-v1" as const;
export const VALIDATION_TRACE_CHALLENGE =
  "midgard-production-validation-trace-challenge-v1" as const;

export type ReplayChallengeCoordinate = Readonly<{
  schemaVersion: typeof W25_CHALLENGE_COORDINATE;
  deploymentFingerprint: string;
  stateQueueObservationDigest: string;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transcriptDigest: string;
  blockReplayResultDigest: string;
  coordinate: Readonly<{
    domain: "block" | "transaction" | "mutation" | "event" | "transition_step";
    index: string;
  }>;
}>;

export type TransitionTraceChallenge = Readonly<{
  schemaVersion: typeof TRANSITION_TRACE_CHALLENGE;
  coordinate: ReplayChallengeCoordinate;
  detectionIndex: number;
  kind: string;
  invariant: string;
  diagnostic: string;
  finalIndex: number;
  proofCbor: string;
  exactL1ReferenceOutRefs: readonly string[];
  challengeDigest: string;
}>;

export type ValidationTraceChallenge = Readonly<{
  schemaVersion: typeof VALIDATION_TRACE_CHALLENGE;
  coordinate: ReplayChallengeCoordinate;
  claimCbor: string;
  challengerDescriptorCbor: string;
  exactL1ReferenceOutRefs: readonly string[];
  challengeDigest: string;
}>;

const admittedTransitionChallenges = new WeakSet<object>();
const transitionProofByChallenge = new WeakMap<
  object,
  TransitionFaultProofData
>();
const admittedValidationChallenges = new WeakSet<object>();
const validationTraceByChallenge = new WeakMap<
  object,
  DeterministicValidationMachineTrace
>();
const validationClaimByChallenge = new WeakMap<
  object,
  ValidationClaimWitness
>();

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

const sha256 = (...parts: readonly string[]): string => {
  const hash = createHash("sha256");
  for (const part of parts) {
    hash.update(part.length.toString()).update(":").update(part);
  }
  return hash.digest("hex");
};

const requireCoordinate = ({
  coordinate,
  evidence,
}: {
  readonly coordinate: ReplayChallengeCoordinate;
  readonly evidence: CanonicalBlockEvidence;
}): ReplayChallengeCoordinate => {
  if (
    coordinate.schemaVersion !== W25_CHALLENGE_COORDINATE ||
    !HEX_32.test(coordinate.deploymentFingerprint) ||
    !HEX_32.test(coordinate.stateQueueObservationDigest) ||
    !HEX_28.test(coordinate.headerHash) ||
    !HEX_32.test(coordinate.payloadEnvelopeSha256) ||
    !HEX_32.test(coordinate.payloadSha256) ||
    !HEX_32.test(coordinate.transcriptDigest) ||
    !HEX_32.test(coordinate.blockReplayResultDigest) ||
    !NATURAL.test(coordinate.coordinate.index) ||
    coordinate.headerHash !== evidence.headerHash ||
    coordinate.payloadEnvelopeSha256 !== evidence.payloadEnvelopeSha256 ||
    coordinate.payloadSha256 !== evidence.payloadSha256
  ) {
    throw new Error(
      "W25 challenge coordinate differs from the authenticated canonical block",
    );
  }
  return Object.freeze({
    ...coordinate,
    coordinate: Object.freeze({ ...coordinate.coordinate }),
  });
};

const exactOutRefs = (values: readonly string[]): readonly string[] => {
  const sorted = [...values].sort();
  if (
    sorted.some((value) => !OUT_REF.test(value)) ||
    sorted.some((value, index) => index > 0 && sorted[index - 1] === value)
  ) {
    throw new Error(
      "W25 challenge L1 reference inputs must be unique canonical out-refs",
    );
  }
  return Object.freeze(sorted);
};

/**
 * Pure family admission called only after the watcher has freshly rebuilt its
 * W21/W15/W16/W22/W24/W25 transcript. The complete evidence is replayed here;
 * a caller cannot supply a prebuilt proof or revive a journal copy.
 */
export const admitTransitionTraceChallenge = async ({
  coordinate,
  evidence,
  completeEvidence,
  detectionIndex,
  exactL1ReferenceOutRefs,
}: {
  readonly coordinate: ReplayChallengeCoordinate;
  readonly evidence: CanonicalBlockEvidence;
  readonly completeEvidence: TransitionTraceDetectionEvidence;
  readonly detectionIndex: number;
  readonly exactL1ReferenceOutRefs: readonly string[];
}): Promise<TransitionTraceChallenge> => {
  const boundCoordinate = requireCoordinate({ coordinate, evidence });
  if (!Number.isSafeInteger(detectionIndex) || detectionIndex < 0) {
    throw new Error("transition-trace W25 detection index is invalid");
  }
  const detections = await detectTransitionTraceFaults(
    evidence.reconstruction,
    completeEvidence,
  );
  if (detections.some((detection) => !detection.buildable)) {
    throw new Error(
      "W25 transition replay contains an unbuildable detection and is not complete",
    );
  }
  const selected = detections[detectionIndex];
  if (selected === undefined || !selected.buildable) {
    throw new Error(
      "W25 transition replay did not reproduce the selected fault",
    );
  }
  const proofCbor = Data.to(selected.proof, TransitionFaultProof);
  const references = exactOutRefs(exactL1ReferenceOutRefs);
  const finalIndex = transitionTraceFinalIndex(selected.proof);
  const challengeInput = Object.freeze({
    schemaVersion: TRANSITION_TRACE_CHALLENGE,
    coordinate: boundCoordinate,
    detectionIndex,
    kind: selected.kind,
    invariant: selected.invariant,
    diagnostic: selected.diagnostic,
    finalIndex,
    proofCbor,
    exactL1ReferenceOutRefs: references,
  });
  const challenge: TransitionTraceChallenge = Object.freeze({
    ...challengeInput,
    challengeDigest: sha256(
      challengeInput.schemaVersion,
      boundCoordinate.transcriptDigest,
      boundCoordinate.blockReplayResultDigest,
      detectionIndex.toString(),
      selected.kind,
      selected.invariant,
      proofCbor,
      ...references,
    ),
  });
  admittedTransitionChallenges.add(challenge);
  transitionProofByChallenge.set(challenge, selected.proof);
  return challenge;
};

export const requireTransitionTraceChallenge = (
  challenge: TransitionTraceChallenge,
): TransitionTraceChallenge => {
  if (
    !admittedTransitionChallenges.has(challenge) ||
    challenge.schemaVersion !== TRANSITION_TRACE_CHALLENGE ||
    transitionProofByChallenge.get(challenge) === undefined
  ) {
    throw new Error("production transition-trace challenge is not admitted");
  }
  return challenge;
};

export const transitionTraceProof = (
  challenge: TransitionTraceChallenge,
): TransitionFaultProofData => {
  requireTransitionTraceChallenge(challenge);
  return transitionProofByChallenge.get(challenge)!;
};

/**
 * Rebuilds the challenger trace from the exact W25 replay input. The operator
 * claim is canonical Data and root-bound by the validation-dispute contract;
 * admission additionally refuses a trace descriptor identical to the claim.
 */
export const admitValidationTraceChallenge = async ({
  coordinate,
  evidence,
  claim,
  challengerReplayInput,
  exactL1ReferenceOutRefs,
}: {
  readonly coordinate: ReplayChallengeCoordinate;
  readonly evidence: CanonicalBlockEvidence;
  readonly claim: ValidationClaimWitness;
  readonly challengerReplayInput: ValidationMachineReplayInput;
  readonly exactL1ReferenceOutRefs: readonly string[];
}): Promise<ValidationTraceChallenge> => {
  const boundCoordinate = requireCoordinate({ coordinate, evidence });
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace(challengerReplayInput),
  );
  const claimCbor = Data.to(claim, ValidationClaimWitnessSchema);
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    trace.tree.descriptor,
  );
  const challengerDescriptorCbor = Data.to(
    challengerDescriptor,
    ValidationTraceDescriptor,
  );
  const operatorDescriptorCbor = Data.to(
    claim.descriptor_membership.value,
    ValidationTraceDescriptor,
  );
  if (operatorDescriptorCbor === challengerDescriptorCbor) {
    throw new Error(
      "validation-trace W25 replay does not disagree with the operator descriptor",
    );
  }
  if (
    claim.descriptor_membership.root !== evidence.header.validationTracesRoot ||
    claim.transition_step_membership.root !==
      evidence.header.transitionTraceRoot ||
    claim.event_to_step_membership.root !== evidence.header.eventToStepRoot
  ) {
    throw new Error(
      "validation-trace claim differs from challenged header roots",
    );
  }
  const references = exactOutRefs(exactL1ReferenceOutRefs);
  const challengeInput = Object.freeze({
    schemaVersion: VALIDATION_TRACE_CHALLENGE,
    coordinate: boundCoordinate,
    claimCbor,
    challengerDescriptorCbor,
    exactL1ReferenceOutRefs: references,
  });
  const challenge: ValidationTraceChallenge = Object.freeze({
    ...challengeInput,
    challengeDigest: sha256(
      challengeInput.schemaVersion,
      boundCoordinate.transcriptDigest,
      boundCoordinate.blockReplayResultDigest,
      claimCbor,
      challengerDescriptorCbor,
      ...references,
    ),
  });
  admittedValidationChallenges.add(challenge);
  validationTraceByChallenge.set(challenge, trace);
  validationClaimByChallenge.set(challenge, claim);
  return challenge;
};

export const requireValidationTraceChallenge = (
  challenge: ValidationTraceChallenge,
): ValidationTraceChallenge => {
  if (
    !admittedValidationChallenges.has(challenge) ||
    challenge.schemaVersion !== VALIDATION_TRACE_CHALLENGE ||
    validationTraceByChallenge.get(challenge) === undefined ||
    validationClaimByChallenge.get(challenge) === undefined
  ) {
    throw new Error("production validation-trace challenge is not admitted");
  }
  return challenge;
};

export const validationTraceMaterial = (
  challenge: ValidationTraceChallenge,
): Readonly<{
  claim: ValidationClaimWitness;
  challengerTrace: DeterministicValidationMachineTrace;
  challengerDescriptor: ValidationTraceDescriptor;
}> => {
  requireValidationTraceChallenge(challenge);
  const challengerTrace = validationTraceByChallenge.get(challenge)!;
  return Object.freeze({
    claim: validationClaimByChallenge.get(challenge)!,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
  });
};
