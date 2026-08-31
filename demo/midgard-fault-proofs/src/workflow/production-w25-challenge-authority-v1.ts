import { createHash } from "node:crypto";

import {
  TransitionFaultProof,
  type TransitionFaultProof as TransitionFaultProofData,
  type ValidationClaimWitnessV1,
  ValidationClaimWitnessV1 as ValidationClaimWitnessV1Schema,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  type DeterministicValidationMachineTrace,
  type ValidationMachineReplayInput,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  detectTransitionTraceFaults,
  type TransitionTraceDetectionEvidence,
  transitionTraceFinalIndex,
} from "../transition-trace/index.js";

export const PRODUCTION_W25_CHALLENGE_COORDINATE_V1 =
  "midgard-production-w25-challenge-coordinate-v1" as const;
export const PRODUCTION_TRANSITION_TRACE_CHALLENGE_V1 =
  "midgard-production-transition-trace-challenge-v1" as const;
export const PRODUCTION_VALIDATION_TRACE_CHALLENGE_V1 =
  "midgard-production-validation-trace-challenge-v1" as const;

export type ProductionW25ChallengeCoordinateV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_W25_CHALLENGE_COORDINATE_V1;
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

export type ProductionTransitionTraceChallengeV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_TRANSITION_TRACE_CHALLENGE_V1;
  coordinate: ProductionW25ChallengeCoordinateV1;
  detectionIndex: number;
  kind: string;
  invariant: string;
  diagnostic: string;
  finalIndex: number;
  proofCbor: string;
  exactL1ReferenceOutRefs: readonly string[];
  challengeDigest: string;
}>;

export type ProductionValidationTraceChallengeV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_VALIDATION_TRACE_CHALLENGE_V1;
  coordinate: ProductionW25ChallengeCoordinateV1;
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
  ValidationClaimWitnessV1
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
  readonly coordinate: ProductionW25ChallengeCoordinateV1;
  readonly evidence: CanonicalBlockEvidenceV1;
}): ProductionW25ChallengeCoordinateV1 => {
  if (
    coordinate.schemaVersion !== PRODUCTION_W25_CHALLENGE_COORDINATE_V1 ||
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
export const admitProductionTransitionTraceChallengeV1 = async ({
  coordinate,
  evidence,
  completeEvidence,
  detectionIndex,
  exactL1ReferenceOutRefs,
}: {
  readonly coordinate: ProductionW25ChallengeCoordinateV1;
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly completeEvidence: TransitionTraceDetectionEvidence;
  readonly detectionIndex: number;
  readonly exactL1ReferenceOutRefs: readonly string[];
}): Promise<ProductionTransitionTraceChallengeV1> => {
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
    schemaVersion: PRODUCTION_TRANSITION_TRACE_CHALLENGE_V1,
    coordinate: boundCoordinate,
    detectionIndex,
    kind: selected.kind,
    invariant: selected.invariant,
    diagnostic: selected.diagnostic,
    finalIndex,
    proofCbor,
    exactL1ReferenceOutRefs: references,
  });
  const challenge: ProductionTransitionTraceChallengeV1 = Object.freeze({
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

export const requireProductionTransitionTraceChallengeV1 = (
  challenge: ProductionTransitionTraceChallengeV1,
): ProductionTransitionTraceChallengeV1 => {
  if (
    !admittedTransitionChallenges.has(challenge) ||
    challenge.schemaVersion !== PRODUCTION_TRANSITION_TRACE_CHALLENGE_V1 ||
    transitionProofByChallenge.get(challenge) === undefined
  ) {
    throw new Error("production transition-trace challenge is not admitted");
  }
  return challenge;
};

export const productionTransitionTraceProofV1 = (
  challenge: ProductionTransitionTraceChallengeV1,
): TransitionFaultProofData => {
  requireProductionTransitionTraceChallengeV1(challenge);
  return transitionProofByChallenge.get(challenge)!;
};

/**
 * Rebuilds the challenger trace from the exact W25 replay input. The operator
 * claim is canonical Data and root-bound by the validation-dispute contract;
 * admission additionally refuses a trace descriptor identical to the claim.
 */
export const admitProductionValidationTraceChallengeV1 = async ({
  coordinate,
  evidence,
  claim,
  challengerReplayInput,
  exactL1ReferenceOutRefs,
}: {
  readonly coordinate: ProductionW25ChallengeCoordinateV1;
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly claim: ValidationClaimWitnessV1;
  readonly challengerReplayInput: ValidationMachineReplayInput;
  readonly exactL1ReferenceOutRefs: readonly string[];
}): Promise<ProductionValidationTraceChallengeV1> => {
  const boundCoordinate = requireCoordinate({ coordinate, evidence });
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace(challengerReplayInput),
  );
  const claimCbor = Data.to(claim, ValidationClaimWitnessV1Schema);
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    trace.tree.descriptor,
  );
  const challengerDescriptorCbor = Data.to(
    challengerDescriptor,
    ValidationTraceDescriptorV1,
  );
  const operatorDescriptorCbor = Data.to(
    claim.descriptor_membership.value,
    ValidationTraceDescriptorV1,
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
    schemaVersion: PRODUCTION_VALIDATION_TRACE_CHALLENGE_V1,
    coordinate: boundCoordinate,
    claimCbor,
    challengerDescriptorCbor,
    exactL1ReferenceOutRefs: references,
  });
  const challenge: ProductionValidationTraceChallengeV1 = Object.freeze({
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

export const requireProductionValidationTraceChallengeV1 = (
  challenge: ProductionValidationTraceChallengeV1,
): ProductionValidationTraceChallengeV1 => {
  if (
    !admittedValidationChallenges.has(challenge) ||
    challenge.schemaVersion !== PRODUCTION_VALIDATION_TRACE_CHALLENGE_V1 ||
    validationTraceByChallenge.get(challenge) === undefined ||
    validationClaimByChallenge.get(challenge) === undefined
  ) {
    throw new Error("production validation-trace challenge is not admitted");
  }
  return challenge;
};

export const productionValidationTraceMaterialV1 = (
  challenge: ProductionValidationTraceChallengeV1,
): Readonly<{
  claim: ValidationClaimWitnessV1;
  challengerTrace: DeterministicValidationMachineTrace;
  challengerDescriptor: ValidationTraceDescriptorV1;
}> => {
  requireProductionValidationTraceChallengeV1(challenge);
  const challengerTrace = validationTraceByChallenge.get(challenge)!;
  return Object.freeze({
    claim: validationClaimByChallenge.get(challenge)!,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
  });
};
