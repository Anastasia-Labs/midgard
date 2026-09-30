import type { MidgardValidationMachineState } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

import {
  type DeterministicValidationMachineTrace,
  type ValidationMachineWorkWitness,
} from "./validation-machine/index.js";
import { resolverPhaseIndex } from "./validation-machine-data.redeemer-item-control-data.js";
import {
  buildCekRouteMaterial,
  validationOneStepWitnessData,
} from "./validation-machine-data.validate-cek-route-material.js";
import { validationAuxiliaryWitnessData } from "./validation-machine-data.validation-auxiliary-witness-data.js";
import {
  record,
  type ValidationMachineFieldCarriageResolver,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";
import {
  type ValidationOneStepArgument,
  validationSemanticResolverIndex,
} from "./validation-machine-data.validation-semantic-resolver-index.js";

export const encodeValidationOneStepWitnessCbor = (input: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineState;
}): Buffer =>
  Buffer.from(Data.to(validationOneStepWitnessData(input) as never), "hex");

export const encodeValidationAuxiliaryWitnessCbor = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
  resolveFieldCarriage?: ValidationMachineFieldCarriageResolver,
): Buffer =>
  Buffer.from(
    Data.to(
      validationAuxiliaryWitnessData(auxiliary, resolveFieldCarriage) as never,
    ),
    "hex",
  );

/**
 * `resolveFieldCarriage` is #600's seam: the tier every field-reading step's
 * evidence names is chosen **here**, because this is where the auxiliary first
 * becomes committed evidence and the earliest point at which a transaction — and
 * therefore a reference-input set — exists. Omit it inside §8.3's tier-1 domain;
 * above the cap it is required, and its absence is a refusal rather than a
 * fabricated index.
 */
export const buildValidationOneStepArgument = ({
  trace,
  stateIndex,
  resolveFieldCarriage,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly stateIndex: number;
  readonly resolveFieldCarriage?: ValidationMachineFieldCarriageResolver;
}): ValidationOneStepArgument => {
  if (!Number.isSafeInteger(stateIndex) || stateIndex < 0) {
    throw new Error(
      "validation one-step state index must be a non-negative safe integer",
    );
  }
  const pre = trace.states[stateIndex];
  const claimedSuccessor = trace.states[stateIndex + 1];
  const witness = trace.witnesses[stateIndex];
  if (
    pre === undefined ||
    claimedSuccessor === undefined ||
    witness === undefined
  ) {
    throw new Error(
      `validation trace does not contain transition ${stateIndex.toString()}`,
    );
  }
  if (
    witness.phase !== pre.phase ||
    witness.programCounter !== pre.programCounter ||
    claimedSuccessor.programCounter !== pre.programCounter + 1
  ) {
    throw new Error(
      "validation one-step witness is not aligned with its trace states",
    );
  }
  const transitionData = validationOneStepWitnessData({
    witness,
    claimedSuccessor,
  });
  const auxiliaryData = validationAuxiliaryWitnessData(
    witness.auxiliary,
    resolveFieldCarriage,
  );
  const transitionCbor = Buffer.from(Data.to(transitionData as never), "hex");
  const auxiliaryCbor = Buffer.from(Data.to(auxiliaryData as never), "hex");
  const evidenceCbor = Buffer.from(
    Data.to(record([transitionData, auxiliaryData]) as never),
    "hex",
  );
  const maximum = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;
  if (
    transitionCbor.length >= maximum ||
    auxiliaryCbor.length >= maximum ||
    evidenceCbor.length >= maximum
  ) {
    throw new Error(
      `validation transition ${stateIndex.toString()} exceeds the strict L1 preimage envelope`,
    );
  }
  const cekRouteMaterial = buildCekRouteMaterial({ trace, witness });
  const resolverIndex = resolverPhaseIndex(pre.phase);
  const semanticResolverIndex = validationSemanticResolverIndex(witness);
  let cekContextSuccessorWorkWitnessCbor: Buffer | undefined;
  if (resolverIndex === 11 && semanticResolverIndex === 2) {
    const adjacent = trace.witnesses[stateIndex + 1];
    if (
      adjacent === undefined ||
      adjacent.phase !== claimedSuccessor.phase ||
      adjacent.programCounter !== claimedSuccessor.programCounter ||
      !hashMidgardValidationWorkWitness({
        phase: adjacent.phase,
        programCounter: adjacent.programCounter,
        witnessCbor: adjacent.cbor,
      }).equals(claimedSuccessor.workRoot)
    ) {
      throw new Error(
        "CEK context requires the exact adjacent successor work witness",
      );
    }
    cekContextSuccessorWorkWitnessCbor = Buffer.from(adjacent.cbor);
  }
  let ledgerOutputProofSuccessorWorkWitnessCbor: Buffer | undefined;
  if (
    ((resolverIndex === 7 && semanticResolverIndex === 3) ||
      (resolverIndex === 8 && semanticResolverIndex === 2)) &&
    claimedSuccessor.phase !== "terminal"
  ) {
    const adjacent = trace.witnesses[stateIndex + 1];
    if (
      adjacent === undefined ||
      adjacent.phase !== claimedSuccessor.phase ||
      adjacent.programCounter !== claimedSuccessor.programCounter ||
      !hashMidgardValidationWorkWitness({
        phase: adjacent.phase,
        programCounter: adjacent.programCounter,
        witnessCbor: adjacent.cbor,
      }).equals(claimedSuccessor.workRoot)
    ) {
      throw new Error(
        "Ledger output proof requires the exact adjacent successor work witness",
      );
    }
    ledgerOutputProofSuccessorWorkWitnessCbor = Buffer.from(adjacent.cbor);
  }
  return {
    resolverIndex,
    semanticResolverIndex,
    transitionCbor,
    auxiliaryCbor,
    evidenceCbor,
    ...(cekRouteMaterial === undefined ? {} : { cekRouteMaterial }),
    ...(cekContextSuccessorWorkWitnessCbor === undefined
      ? {}
      : { cekContextSuccessorWorkWitnessCbor }),
    ...(ledgerOutputProofSuccessorWorkWitnessCbor === undefined
      ? {}
      : { ledgerOutputProofSuccessorWorkWitnessCbor }),
  };
};
