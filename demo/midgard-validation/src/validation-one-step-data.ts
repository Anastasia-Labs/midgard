import {
  hashMidgardValidationMachineState,
  type MidgardValidationDispute,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  MidgardValidationVerdict,
} from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";

import type { DeterministicValidationMachineTrace } from "./validation-machine/index.js";
import { validationMachineStateData } from "./validation-machine-data.js";

type PlutusData = unknown;

const bytes = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const int = (value: number | bigint): bigint => BigInt(value);

export const validationTraceDescriptorData = (
  descriptor: MidgardValidationTraceDescriptor,
): Constr<PlutusData> =>
  new Constr(0, [
    int(descriptor.schemaVersion),
    int(descriptor.machineVersion),
    bytes(descriptor.traceRoot),
    int(descriptor.stepCount),
    bytes(descriptor.initialStateHash),
    bytes(descriptor.terminalStateHash),
    new Constr(MidgardValidationVerdict[descriptor.verdict], []),
    bytes(descriptor.rejectionCodeHash),
  ]);

export const validationTraceProofData = (
  proof: MidgardValidationTraceProof,
): Constr<PlutusData> =>
  new Constr(0, [
    int(proof.stateIndex),
    bytes(proof.stateHash),
    proof.siblings.map(bytes),
  ]);

export const validationDisputeData = (
  dispute: MidgardValidationDispute,
): Constr<PlutusData> =>
  new Constr(0, [
    int(dispute.version),
    validationTraceDescriptorData(dispute.operatorDescriptor),
    validationTraceDescriptorData(dispute.challengerDescriptor),
    int(dispute.lowIndex),
    int(dispute.highIndex),
    bytes(dispute.agreedLowHash),
    bytes(dispute.operatorHighHash),
    bytes(dispute.challengerHighHash),
    int(dispute.round),
    int(dispute.responseDeadline),
    dispute.turn.type === "awaitingOperator"
      ? new Constr(0, [int(dispute.turn.midpoint)])
      : dispute.turn.type === "awaitingChallenger"
        ? new Constr(1, [
            int(dispute.turn.midpoint),
            bytes(dispute.turn.operatorMidpointHash),
          ])
        : new Constr(2, []),
  ]);

export const buildValidationBoundaryEvidenceData = ({
  dispute,
  operatorTrace,
  challengerTrace,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
}): Constr<PlutusData> => {
  if (
    dispute.turn.type !== "readyForOneStep" ||
    dispute.highIndex !== dispute.lowIndex + 1
  ) {
    throw new Error("validation dispute is not ready for one-step resolution");
  }
  const pre = operatorTrace.states[dispute.lowIndex];
  const challengerPre = challengerTrace.states[dispute.lowIndex];
  const operatorPost = operatorTrace.tree.proofs[dispute.highIndex];
  const challengerPost = challengerTrace.tree.proofs[dispute.highIndex];
  if (
    pre === undefined ||
    challengerPre === undefined ||
    operatorPost === undefined ||
    challengerPost === undefined ||
    !hashMidgardValidationMachineState(pre).equals(dispute.agreedLowHash) ||
    !hashMidgardValidationMachineState(challengerPre).equals(
      dispute.agreedLowHash,
    ) ||
    !operatorPost.stateHash.equals(dispute.operatorHighHash) ||
    !challengerPost.stateHash.equals(dispute.challengerHighHash)
  ) {
    throw new Error(
      "validation traces do not open the authenticated one-step boundary",
    );
  }
  return new Constr(0, [
    validationMachineStateData(pre),
    validationTraceProofData(operatorPost),
    validationTraceProofData(challengerPost),
  ]);
};

export const encodeValidationTraceDescriptorDataCbor = (
  descriptor: MidgardValidationTraceDescriptor,
): Buffer =>
  Buffer.from(
    Data.to(validationTraceDescriptorData(descriptor) as never),
    "hex",
  );

export const encodeValidationTraceProofDataCbor = (
  proof: MidgardValidationTraceProof,
): Buffer =>
  Buffer.from(Data.to(validationTraceProofData(proof) as never), "hex");

export const encodeValidationDisputeDataCbor = (
  dispute: MidgardValidationDispute,
): Buffer =>
  Buffer.from(Data.to(validationDisputeData(dispute) as never), "hex");

export const encodeValidationBoundaryEvidenceCbor = (input: {
  readonly dispute: MidgardValidationDispute;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
}): Buffer =>
  Buffer.from(
    Data.to(buildValidationBoundaryEvidenceData(input) as never),
    "hex",
  );
