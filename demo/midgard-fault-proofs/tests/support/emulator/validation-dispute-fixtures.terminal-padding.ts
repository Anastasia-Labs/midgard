import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import {
  type DeterministicValidationMachineTrace,
  encodeValidationBoundaryEvidenceCbor,
  encodeValidationDisputeDataCbor,
  encodeValidationTraceDescriptorDataCbor,
  encodeValidationTraceProofDataCbor,
  type ValidationDisputeEvidenceMove,
} from "@al-ft/midgard-validation";

import { buildForcedValidationDisputeCommitments } from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildHonestAcceptedValidationDisputeFixture } from "./validation-dispute-fixtures.build-honest-accepted-validation-dispute-fixture.js";
import { buildNativeTransactionTrace } from "./validation-dispute-fixtures.build-native-transaction-trace.js";

/** A real accepted source and honest trace, followed by one forbidden step. */
export const buildTerminalPaddingFixture = async (input: {
  readonly operatorVkey: string;
  readonly now: number;
}) => {
  const base = await buildHonestAcceptedValidationDisputeFixture(input);
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildNativeTransactionTrace({ now: input.now, txOrderSeed: "e9" });
  const terminal = honestTrace.states.at(-1)!;
  const paddedEnd = {
    ...terminal,
    programCounter: terminal.programCounter + 1,
  };
  const operatorStates = [...honestTrace.states, paddedEnd];
  const challengerStates = [
    ...honestTrace.states,
    { ...paddedEnd, workRoot: Buffer.alloc(32, 0x7d) },
  ];
  const trace = (
    states: DeterministicValidationMachineTrace["states"],
  ): DeterministicValidationMachineTrace => ({
    ...honestTrace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      "accepted",
      terminal.rejectionCodeHash,
    ),
  });
  const operatorTrace = trace(operatorStates);
  const challengerTrace = trace(challengerStates);
  const openingDispute = openMidgardValidationDispute({
    operatorDescriptor: operatorTrace.tree.descriptor,
    challengerDescriptor: challengerTrace.tree.descriptor,
    currentTime: input.now + 2_000,
  });
  let dispute = openingDispute;
  const moves: ValidationDisputeEvidenceMove[] = [];
  while (dispute.turn.type !== "readyForOneStep") {
    const disputeBefore = dispute;
    const role =
      dispute.turn.type === "awaitingOperator"
        ? ("operator" as const)
        : ("challenger" as const);
    const proof = (role === "operator" ? operatorTrace : challengerTrace).tree
      .proofs[dispute.turn.midpoint]!;
    dispute =
      role === "operator"
        ? revealMidgardValidationOperatorMidpoint({
            dispute,
            proof,
            currentTime: input.now + 2_000,
          })
        : revealMidgardValidationChallengerMidpoint({
            dispute,
            proof,
            currentTime: input.now + 2_000,
          });
    moves.push({
      role,
      disputeBefore,
      proof,
      proofCbor: encodeValidationTraceProofDataCbor(proof),
      disputeAfter: dispute,
      disputeAfterCbor: encodeValidationDisputeDataCbor(dispute),
    });
  }
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    ...input,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    ...base,
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence: {
      ...base.evidence,
      operatorDescriptorCbor: encodeValidationTraceDescriptorDataCbor(
        operatorTrace.tree.descriptor,
      ),
      challengerDescriptorCbor: encodeValidationTraceDescriptorDataCbor(
        challengerTrace.tree.descriptor,
      ),
      openingDispute,
      openingDisputeCbor: encodeValidationDisputeDataCbor(openingDispute),
      moves,
      finalDispute: dispute,
      finalDisputeCbor: encodeValidationDisputeDataCbor(dispute),
      boundaryEvidenceCbor: encodeValidationBoundaryEvidenceCbor({
        dispute,
        operatorTrace,
        challengerTrace,
      }),
      // The shared staging helper publishes a prepare resolver before opening.
      // Its ordinary one-step argument is not used: this scenario stops at
      // boundary and takes AwardTerminalPadding instead of PrepareResolution.
      oneStepArgument: base.evidence.oneStepArgument,
    },
  };
};
