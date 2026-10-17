import {
  MIDGARD_CONSENSUS_LIMITS,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";

import {
  requireProofEnvelope,
  type ValidationDisputeEvidenceBundle,
  type ValidationDisputeEvidenceMove,
} from "./validation-dispute-evidence.parse-cek-program-material-necessity-receipt-set.js";
import type { DeterministicValidationMachineTrace } from "./validation-machine/index.js";
import { buildValidationOneStepArgument } from "./validation-machine-data.js";
import {
  encodeValidationBoundaryEvidenceCbor,
  encodeValidationDisputeDataCbor,
  encodeValidationTraceDescriptorDataCbor,
  encodeValidationTraceProofDataCbor,
} from "./validation-one-step-data.js";

/**
 * Constructs every authenticated bisection reveal and the terminal one-step
 * argument from two complete local traces. The routine never guesses missing
 * trace nodes: an absent proof, mismatched endpoint, excessive round count, or
 * oversized independently submitted preimage fails before transaction
 * construction.
 */
export const buildValidationDisputeEvidenceBundle = ({
  operatorTrace,
  challengerTrace,
  currentTime,
  resolveFieldCarriage,
}: {
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly currentTime: number;
  readonly resolveFieldCarriage?: Parameters<
    typeof buildValidationOneStepArgument
  >[0]["resolveFieldCarriage"];
}): ValidationDisputeEvidenceBundle => {
  const openingDispute = openMidgardValidationDispute({
    operatorDescriptor: operatorTrace.tree.descriptor,
    challengerDescriptor: challengerTrace.tree.descriptor,
    currentTime,
  });
  let dispute = openingDispute;
  const moves: ValidationDisputeEvidenceMove[] = [];
  const maximumMoves =
    2 * MIDGARD_CONSENSUS_LIMITS.maxValidationBisectionRounds;

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
    const proofCbor = encodeValidationTraceProofDataCbor(proof);
    const disputeAfterCbor = encodeValidationDisputeDataCbor(dispute);
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

  const operatorDescriptorCbor = encodeValidationTraceDescriptorDataCbor(
    operatorTrace.tree.descriptor,
  );
  const challengerDescriptorCbor = encodeValidationTraceDescriptorDataCbor(
    challengerTrace.tree.descriptor,
  );
  const openingDisputeCbor = encodeValidationDisputeDataCbor(openingDispute);
  const finalDisputeCbor = encodeValidationDisputeDataCbor(dispute);
  const boundaryEvidenceCbor = encodeValidationBoundaryEvidenceCbor({
    dispute,
    operatorTrace,
    challengerTrace,
  });
  const oneStepArgument = buildValidationOneStepArgument({
    trace: challengerTrace,
    stateIndex: dispute.lowIndex,
    ...(resolveFieldCarriage === undefined ? {} : { resolveFieldCarriage }),
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
