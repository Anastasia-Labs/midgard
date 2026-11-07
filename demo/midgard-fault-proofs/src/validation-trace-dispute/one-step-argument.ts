/** What every step's one-step resolution carries, from its one-step
 * argument: the transition, its auxiliary evidence and the evidence hash
 * the resolver commits; and, for a step whose evidence reads a field by
 * reference, that field and the resolution again with the evidence naming
 * our publication of it (one-step-carriage.ts). */
import type { MidgardValidationMachineState } from "@al-ft/midgard-core";
import type { MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import type { buildValidationOneStepArgument } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import type { ValidationTraceCarriedField } from "./one-step-carriage.js";
import type { ValidationTraceOneStepHolder } from "./one-step-holders.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";

/** `hash_one_step_evidence`'s domain (validation-resolution-v1). */
const ONE_STEP_EVIDENCE_DOMAIN =
  "4d69646761726456616c69646174696f6e4f6e655374657045766964656e63655631";

const blake2b256 = (hex: string): string =>
  Effect.runSync(SDK.hashHexWithBlake2b(hex, 32));

/** `blake2b_256(cbor.serialise(data))` of the Data whose CBOR is `cbor`. */
export const hashSerialisedData = (cbor: string): string =>
  blake2b256(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));

/** `hash_one_step_evidence(transition, auxiliary)`. */
export const hashOneStepEvidence = (
  transitionCbor: string,
  auxiliaryCbor: string,
): string =>
  blake2b256(
    ONE_STEP_EVIDENCE_DOMAIN +
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        `9f${transitionCbor}${auxiliaryCbor}ff`,
      ),
  );

/** A field a step's evidence reads by reference: the field, the holder
 * whose move opens it from the move's reference inputs, and the step's
 * resolution again with its evidence naming `carriage`, our publications
 * of the field among that move's reference inputs. Until then the evidence
 * names placeholder positions, which no move commits. */
export type ValidationTraceCarriedStep = Readonly<{
  field: ValidationTraceCarriedField;
  reader: ValidationTraceOneStepHolder;
  rebind: (carriage: MidgardFieldCarriage) => ValidationTraceOneStepPlan;
}>;

/** What every step's one-step resolution carries. */
export type ValidationTraceOneStepArgument = Readonly<{
  /** The agreed state's index: the step is `stateIndex` to `stateIndex + 1`. */
  stateIndex: number;
  preState: MidgardValidationMachineState;
  resolverIndex: number;
  /** The semantic resolver of the CEK group the step selects. */
  semanticResolverIndex: number;
  /** The transition and its auxiliary evidence, as their Data's CBOR. */
  transitionCbor: string;
  auxiliaryCbor: string;
  /** `hash_one_step_evidence(transition, auxiliary)`. */
  evidenceHash: string;
  /** The field the evidence reads by reference, if any. */
  carried?: ValidationTraceCarriedStep;
}>;

/** What every step's resolution carries, from its one-step argument. */
export const commonOf = (
  stateIndex: number,
  preState: MidgardValidationMachineState,
  argument: ReturnType<typeof buildValidationOneStepArgument>,
): ValidationTraceOneStepArgument => {
  const transitionCbor = argument.transitionCbor.toString("hex");
  const auxiliaryCbor = argument.auxiliaryCbor.toString("hex");
  return {
    stateIndex,
    preState,
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex: argument.semanticResolverIndex,
    transitionCbor,
    auxiliaryCbor,
    evidenceHash: hashOneStepEvidence(transitionCbor, auxiliaryCbor),
  };
};
