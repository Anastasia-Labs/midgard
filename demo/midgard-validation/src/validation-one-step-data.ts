import {
  hashMidgardValidationMachineState,
  type MidgardValidationDispute,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  MidgardValidationVerdict,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { Constr } from "@lucid-evolution/lucid";

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
    int(dispute.turnStartedAt),
    int(dispute.operatorClockMs),
    int(dispute.challengerClockMs),
    dispute.turn.type === "awaitingOperator"
      ? new Constr(0, [int(dispute.turn.midpoint)])
      : dispute.turn.type === "awaitingChallenger"
        ? new Constr(1, [
            int(dispute.turn.midpoint),
            bytes(dispute.turn.operatorMidpointHash),
          ])
        : new Constr(2, []),
  ]);

/**
 * `ValidationBoundaryEvidenceV1 { pre_state }`: the agreed state at the
 * one-step boundary. The successor hashes are the dispute datum's own
 * `operator_high_hash` and `challenger_high_hash`, so no post proof is carried.
 */
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
  if (
    pre === undefined ||
    challengerPre === undefined ||
    !hashMidgardValidationMachineState(pre).equals(dispute.agreedLowHash) ||
    !hashMidgardValidationMachineState(challengerPre).equals(
      dispute.agreedLowHash,
    )
  ) {
    throw new Error(
      "validation traces do not open the authenticated one-step boundary",
    );
  }
  return new Constr(0, [validationMachineStateData(pre)]);
};

export const encodeValidationTraceDescriptorDataCbor = (
  descriptor: MidgardValidationTraceDescriptor,
): Buffer =>
  lucidDataToCborIterative(validationTraceDescriptorData(descriptor) as never);

export const encodeValidationTraceProofDataCbor = (
  proof: MidgardValidationTraceProof,
): Buffer => lucidDataToCborIterative(validationTraceProofData(proof) as never);

export const encodeValidationDisputeDataCbor = (
  dispute: MidgardValidationDispute,
): Buffer => lucidDataToCborIterative(validationDisputeData(dispute) as never);

export const encodeValidationBoundaryEvidenceCbor = (input: {
  readonly dispute: MidgardValidationDispute;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
}): Buffer =>
  lucidDataToCborIterative(buildValidationBoundaryEvidenceData(input) as never);
