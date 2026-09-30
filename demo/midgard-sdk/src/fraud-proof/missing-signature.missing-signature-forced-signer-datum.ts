import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  MissingSignatureForcedSignerDatumSchema,
  MissingSignatureForcedSignerSpendRedeemerSchema,
  MissingSignatureForcedSignerStateSchema,
  MissingSignatureForcedWitnessArgsSchema,
  MissingSignatureForcedWitnessStateSchema,
} from "./missing-signature.missing-signature-field-walk-checkpoint.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export type MissingSignatureForcedSignerDatum = Data.Static<
  typeof MissingSignatureForcedSignerDatumSchema
>;

export const MissingSignatureForcedSignerDatum =
  asDataType<MissingSignatureForcedSignerDatum>(
    MissingSignatureForcedSignerDatumSchema,
  );

export type MissingSignatureForcedSignerSpendRedeemer = Data.Static<
  typeof MissingSignatureForcedSignerSpendRedeemerSchema
>;

export const MissingSignatureForcedSignerSpendRedeemer =
  asDataType<MissingSignatureForcedSignerSpendRedeemer>(
    MissingSignatureForcedSignerSpendRedeemerSchema,
  );

export type MissingSignatureForcedSignerState = Data.Static<
  typeof MissingSignatureForcedSignerStateSchema
>;

export const MissingSignatureForcedSignerState =
  asDataType<MissingSignatureForcedSignerState>(
    MissingSignatureForcedSignerStateSchema,
  );

export const MissingSignatureForcedWitnessDatumSchema =
  faultProofStepDatumSchema(MissingSignatureForcedWitnessStateSchema);

export const MissingSignatureForcedWitnessSpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureForcedWitnessArgsSchema);

export type MissingSignatureForcedWitnessArgs = Data.Static<
  typeof MissingSignatureForcedWitnessArgsSchema
>;

export const MissingSignatureForcedWitnessArgs =
  asDataType<MissingSignatureForcedWitnessArgs>(
    MissingSignatureForcedWitnessArgsSchema,
  );

export type MissingSignatureForcedWitnessDatum = Data.Static<
  typeof MissingSignatureForcedWitnessDatumSchema
>;

export const MissingSignatureForcedWitnessDatum =
  asDataType<MissingSignatureForcedWitnessDatum>(
    MissingSignatureForcedWitnessDatumSchema,
  );

export type MissingSignatureForcedWitnessSpendRedeemer = Data.Static<
  typeof MissingSignatureForcedWitnessSpendRedeemerSchema
>;

export const MissingSignatureForcedWitnessSpendRedeemer =
  asDataType<MissingSignatureForcedWitnessSpendRedeemer>(
    MissingSignatureForcedWitnessSpendRedeemerSchema,
  );

export type MissingSignatureForcedWitnessState = Data.Static<
  typeof MissingSignatureForcedWitnessStateSchema
>;

export const MissingSignatureForcedWitnessState =
  asDataType<MissingSignatureForcedWitnessState>(
    MissingSignatureForcedWitnessStateSchema,
  );
