import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  type MidgardValidationDisputeV1,
  type MidgardValidationTraceProofV1,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";

import type { DeterministicValidationMachineTrace } from "./validation-machine.js";
import {
  buildValidationOneStepArgumentV1,
  type ValidationOneStepArgumentV1,
} from "./validation-machine-data.js";
import {
  encodeValidationBoundaryEvidenceCborV1,
  encodeValidationDisputeDataCborV1,
  encodeValidationTraceDescriptorDataCborV1,
  encodeValidationTraceProofDataCborV1,
} from "./validation-one-step-data.js";

export type ValidationDisputeEvidenceMoveV1 = {
  readonly role: "operator" | "challenger";
  readonly disputeBefore: MidgardValidationDisputeV1;
  readonly proof: MidgardValidationTraceProofV1;
  readonly proofCbor: Buffer;
  readonly disputeAfter: MidgardValidationDisputeV1;
  readonly disputeAfterCbor: Buffer;
};

export type ValidationDisputeEvidenceBundleV1 = {
  readonly operatorDescriptorCbor: Buffer;
  readonly challengerDescriptorCbor: Buffer;
  readonly openingDispute: MidgardValidationDisputeV1;
  readonly openingDisputeCbor: Buffer;
  readonly moves: readonly ValidationDisputeEvidenceMoveV1[];
  readonly finalDispute: MidgardValidationDisputeV1;
  readonly finalDisputeCbor: Buffer;
  readonly boundaryEvidenceCbor: Buffer;
  readonly oneStepArgument: ValidationOneStepArgumentV1;
};

const requireProofEnvelope = (bytes: Uint8Array, label: string): void => {
  if (bytes.length >= MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes) {
    throw new Error(
      `${label} exceeds the strict L1 proof envelope: ${bytes.length.toString()} bytes`,
    );
  }
};

/**
 * Constructs every authenticated bisection reveal and the terminal one-step
 * argument from two complete local traces. The routine never guesses missing
 * trace nodes: an absent proof, mismatched endpoint, excessive round count, or
 * oversized independently submitted preimage fails before transaction
 * construction.
 */
export const buildValidationDisputeEvidenceBundleV1 = ({
  operatorTrace,
  challengerTrace,
  currentTime,
}: {
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly currentTime: number;
}): ValidationDisputeEvidenceBundleV1 => {
  const openingDispute = openMidgardValidationDispute({
    operatorDescriptor: operatorTrace.tree.descriptor,
    challengerDescriptor: challengerTrace.tree.descriptor,
    currentTime,
  });
  let dispute = openingDispute;
  const moves: ValidationDisputeEvidenceMoveV1[] = [];
  const maximumMoves =
    2 * MIDGARD_CONSENSUS_LIMITS_V1.maxValidationBisectionRounds;

  while (dispute.turn.type !== "readyForOneStep") {
    if (moves.length >= maximumMoves) {
      throw new Error("validation dispute exceeded its compiled move bound");
    }
    const disputeBefore = dispute;
    const proof =
      dispute.turn.type === "awaitingOperator"
        ? operatorTrace.tree.proofs[dispute.turn.midpoint]
        : challengerTrace.tree.proofs[dispute.turn.midpoint];
    if (proof === undefined) {
      throw new Error(
        `validation trace is missing midpoint ${dispute.turn.midpoint.toString()}`,
      );
    }
    const role =
      dispute.turn.type === "awaitingOperator"
        ? ("operator" as const)
        : ("challenger" as const);
    dispute =
      role === "operator"
        ? revealMidgardValidationOperatorMidpoint({
            dispute,
            proof,
            currentTime,
          })
        : revealMidgardValidationChallengerMidpoint({
            dispute,
            proof,
            currentTime,
          });
    const proofCbor = encodeValidationTraceProofDataCborV1(proof);
    const disputeAfterCbor = encodeValidationDisputeDataCborV1(dispute);
    requireProofEnvelope(proofCbor, `${role} midpoint proof`);
    requireProofEnvelope(disputeAfterCbor, "continued validation dispute");
    moves.push({
      role,
      disputeBefore,
      proof,
      proofCbor,
      disputeAfter: dispute,
      disputeAfterCbor,
    });
  }

  const operatorDescriptorCbor = encodeValidationTraceDescriptorDataCborV1(
    operatorTrace.tree.descriptor,
  );
  const challengerDescriptorCbor = encodeValidationTraceDescriptorDataCborV1(
    challengerTrace.tree.descriptor,
  );
  const openingDisputeCbor = encodeValidationDisputeDataCborV1(openingDispute);
  const finalDisputeCbor = encodeValidationDisputeDataCborV1(dispute);
  const boundaryEvidenceCbor =
    encodeValidationBoundaryEvidenceCborV1({
      dispute,
      operatorTrace,
      challengerTrace,
    });
  const oneStepArgument = buildValidationOneStepArgumentV1({
    trace: challengerTrace,
    stateIndex: dispute.lowIndex,
  });
  requireProofEnvelope(operatorDescriptorCbor, "operator descriptor");
  requireProofEnvelope(challengerDescriptorCbor, "challenger descriptor");
  requireProofEnvelope(openingDisputeCbor, "opening validation dispute");
  requireProofEnvelope(finalDisputeCbor, "final validation dispute");
  requireProofEnvelope(
    boundaryEvidenceCbor,
    "validation one-step boundary evidence",
  );

  return {
    operatorDescriptorCbor,
    challengerDescriptorCbor,
    openingDispute,
    openingDisputeCbor,
    moves,
    finalDispute: dispute,
    finalDisputeCbor,
    boundaryEvidenceCbor,
    oneStepArgument,
  };
};
