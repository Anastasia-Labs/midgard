import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { HeaderSchema } from "../ledger-state.js";
import {
  EventToStepCarriedMembershipSchema,
  TransitionCarriedMembershipSchema,
} from "./counted-membership-carriage.js";
import { DirectScriptBoundSchema } from "./direct-script-derivation.js";
import { DirectScriptSourceBoundSchema } from "./direct-script-derivation-source.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import { VerdictSubjectSchema } from "./proof-thread-substrate.js";

export const DirectEventSelectedSchema = Data.Object({
  subject: VerdictSubjectSchema,
  header: HeaderSchema,
  compact_cbor: Data.Bytes(),
  step_index: Data.Integer(),
});
export type DirectEventSelected = Data.Static<typeof DirectEventSelectedSchema>;
export const DirectEventSelected = asDataType<DirectEventSelected>(
  DirectEventSelectedSchema,
);

export const DirectBindEventArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  event_opening: EventToStepCarriedMembershipSchema,
});
export type DirectBindEventArgs = Data.Static<typeof DirectBindEventArgsSchema>;
export const DirectBindEventArgs = asDataType<DirectBindEventArgs>(
  DirectBindEventArgsSchema,
);
export const DirectBindEventDatumSchema = faultProofStepDatumSchema(
  DirectScriptSourceBoundSchema,
);
export type DirectBindEventDatum = Data.Static<
  typeof DirectBindEventDatumSchema
>;
export const DirectBindEventDatum = asDataType<DirectBindEventDatum>(
  DirectBindEventDatumSchema,
);
export const DirectBindEventRedeemerSchema = faultProofStepRedeemerSchema(
  DirectBindEventArgsSchema,
);
export type DirectBindEventRedeemer = Data.Static<
  typeof DirectBindEventRedeemerSchema
>;
export const DirectBindEventRedeemer = asDataType<DirectBindEventRedeemer>(
  DirectBindEventRedeemerSchema,
);

export const DirectBindPreStateArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  transition_opening: TransitionCarriedMembershipSchema,
});
export type DirectBindPreStateArgs = Data.Static<
  typeof DirectBindPreStateArgsSchema
>;
export const DirectBindPreStateArgs = asDataType<DirectBindPreStateArgs>(
  DirectBindPreStateArgsSchema,
);
export const DirectBindPreStateDatumSchema = faultProofStepDatumSchema(
  DirectEventSelectedSchema,
);
export type DirectBindPreStateDatum = Data.Static<
  typeof DirectBindPreStateDatumSchema
>;
export const DirectBindPreStateDatum = asDataType<DirectBindPreStateDatum>(
  DirectBindPreStateDatumSchema,
);
export const DirectBindPreStateRedeemerSchema = faultProofStepRedeemerSchema(
  DirectBindPreStateArgsSchema,
);
export type DirectBindPreStateRedeemer = Data.Static<
  typeof DirectBindPreStateRedeemerSchema
>;
export const DirectBindPreStateRedeemer =
  asDataType<DirectBindPreStateRedeemer>(DirectBindPreStateRedeemerSchema);
export const DirectScriptBoundDatumSchema = faultProofStepDatumSchema(
  DirectScriptBoundSchema,
);
export type DirectScriptBoundDatum = Data.Static<
  typeof DirectScriptBoundDatumSchema
>;
export const DirectScriptBoundDatum = asDataType<DirectScriptBoundDatum>(
  DirectScriptBoundDatumSchema,
);
