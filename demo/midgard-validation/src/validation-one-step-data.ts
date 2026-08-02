import {
  hashMidgardValidationMachineStateV1,
  type MidgardValidationDisputeV1,
  type MidgardValidationTraceDescriptorV1,
  type MidgardValidationTraceProofV1,
  MidgardValidationVerdict,
} from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";

import type { DeterministicValidationMachineTrace } from "./validation-machine.js";
import { validationMachineStateDataV1 } from "./validation-machine-data.js";

type PlutusData = unknown;

const bytes = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const int = (value: number | bigint): bigint => BigInt(value);

export const validationTraceDescriptorDataV1 = (
  descriptor: MidgardValidationTraceDescriptorV1,
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

export const validationTraceProofDataV1 = (
  proof: MidgardValidationTraceProofV1,
): Constr<PlutusData> =>
  new Constr(0, [
    int(proof.stateIndex),
    bytes(proof.stateHash),
    proof.siblings.map(bytes),
  ]);

export const validationDisputeDataV1 = (
  dispute: MidgardValidationDisputeV1,
): Constr<PlutusData> =>
  new Constr(0, [
    int(dispute.version),
    validationTraceDescriptorDataV1(dispute.operatorDescriptor),
    validationTraceDescriptorDataV1(dispute.challengerDescriptor),
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

export const buildValidationBoundaryEvidenceDataV1 = ({
  dispute,
  operatorTrace,
  challengerTrace,
}: {
  readonly dispute: MidgardValidationDisputeV1;
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
    !hashMidgardValidationMachineStateV1(pre).equals(dispute.agreedLowHash) ||
    !hashMidgardValidationMachineStateV1(challengerPre).equals(
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
    validationMachineStateDataV1(pre),
    validationTraceProofDataV1(operatorPost),
    validationTraceProofDataV1(challengerPost),
  ]);
};

export const encodeValidationTraceDescriptorDataCborV1 = (
  descriptor: MidgardValidationTraceDescriptorV1,
): Buffer =>
  Buffer.from(
    Data.to(validationTraceDescriptorDataV1(descriptor) as never),
    "hex",
  );

export const encodeValidationTraceProofDataCborV1 = (
  proof: MidgardValidationTraceProofV1,
): Buffer =>
  Buffer.from(Data.to(validationTraceProofDataV1(proof) as never), "hex");

export const encodeValidationDisputeDataCborV1 = (
  dispute: MidgardValidationDisputeV1,
): Buffer =>
  Buffer.from(Data.to(validationDisputeDataV1(dispute) as never), "hex");

export const encodeValidationBoundaryEvidenceCborV1 = (input: {
  readonly dispute: MidgardValidationDisputeV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
}): Buffer =>
  Buffer.from(
    Data.to(buildValidationBoundaryEvidenceDataV1(input) as never),
    "hex",
  );
