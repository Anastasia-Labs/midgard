import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  EventToStepMembershipProofSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedTransactionSourceMembershipProofSchema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  ProofSchema,
  TransitionTraceMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  ConservationAssetActionSchema,
  ConservationDeltaWitnessSchema,
} from "./union-schemas.conservation-asset-action-schema.js";

export const ConservationChunkCarriageSchema = Data.Enum([
  Data.Object({
    InlineChunks: Data.Object({ chunks: Data.Array(Data.Bytes()) }),
  }),
  Data.Object({
    ReferencedChunks: Data.Object({
      reference_indices: Data.Array(Data.Integer()),
    }),
  }),
]);

export type ConservationChunkCarriage = Data.Static<
  typeof ConservationChunkCarriageSchema
>;

export const ConservationChunkCarriage = asDataType<ConservationChunkCarriage>(
  ConservationChunkCarriageSchema,
);

export const ConservationForcedSourceArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  header: HeaderSchema,
  membership: ForcedTransactionSourceMembershipProofSchema,
});

export type ConservationForcedSourceArgs = Data.Static<
  typeof ConservationForcedSourceArgsSchema
>;

export const ConservationForcedSourceArgs =
  asDataType<ConservationForcedSourceArgs>(ConservationForcedSourceArgsSchema);

export const ConservationForcedSourceRedeemerSchema =
  faultProofStepRedeemerSchema(ConservationForcedSourceArgsSchema);

export type ConservationForcedSourceRedeemer = Data.Static<
  typeof ConservationForcedSourceRedeemerSchema
>;

export const ConservationForcedSourceRedeemer =
  asDataType<ConservationForcedSourceRedeemer>(
    ConservationForcedSourceRedeemerSchema,
  );

export const ConservationEventArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  membership: EventToStepMembershipProofSchema,
});

export type ConservationEventArgs = Data.Static<
  typeof ConservationEventArgsSchema
>;

export const ConservationEventArgs = asDataType<ConservationEventArgs>(
  ConservationEventArgsSchema,
);

export const ConservationEventRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationEventArgsSchema,
);

export type ConservationEventRedeemer = Data.Static<
  typeof ConservationEventRedeemerSchema
>;

export const ConservationEventRedeemer = asDataType<ConservationEventRedeemer>(
  ConservationEventRedeemerSchema,
);

export const ConservationPreStateArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  membership: TransitionTraceMembershipProofSchema,
});

export type ConservationPreStateArgs = Data.Static<
  typeof ConservationPreStateArgsSchema
>;

export const ConservationPreStateArgs = asDataType<ConservationPreStateArgs>(
  ConservationPreStateArgsSchema,
);

export const ConservationPreStateRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationPreStateArgsSchema,
);

export type ConservationPreStateRedeemer = Data.Static<
  typeof ConservationPreStateRedeemerSchema
>;

export const ConservationPreStateRedeemer =
  asDataType<ConservationPreStateRedeemer>(ConservationPreStateRedeemerSchema);

export const ConservationInputsArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});

export type ConservationInputsArgs = Data.Static<
  typeof ConservationInputsArgsSchema
>;

export const ConservationInputsArgs = asDataType<ConservationInputsArgs>(
  ConservationInputsArgsSchema,
);

export const ConservationInputsRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationInputsArgsSchema,
);

export type ConservationInputsRedeemer = Data.Static<
  typeof ConservationInputsRedeemerSchema
>;

export const ConservationInputsRedeemer =
  asDataType<ConservationInputsRedeemer>(ConservationInputsRedeemerSchema);

export const ConservationInputValueArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  descriptor_cbor: Data.Bytes(),
  proof: ProofSchema,
});

export type ConservationInputValueArgs = Data.Static<
  typeof ConservationInputValueArgsSchema
>;

export const ConservationInputValueArgs =
  asDataType<ConservationInputValueArgs>(ConservationInputValueArgsSchema);

export const ConservationInputValueRedeemerSchema =
  faultProofStepRedeemerSchema(ConservationInputValueArgsSchema);

export type ConservationInputValueRedeemer = Data.Static<
  typeof ConservationInputValueRedeemerSchema
>;

export const ConservationInputValueRedeemer =
  asDataType<ConservationInputValueRedeemer>(
    ConservationInputValueRedeemerSchema,
  );

export const ConservationAssetsArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  action: ConservationAssetActionSchema,
});

export type ConservationAssetsArgs = Data.Static<
  typeof ConservationAssetsArgsSchema
>;

export const ConservationAssetsArgs = asDataType<ConservationAssetsArgs>(
  ConservationAssetsArgsSchema,
);

export const ConservationAssetsRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationAssetsArgsSchema,
);

export type ConservationAssetsRedeemer = Data.Static<
  typeof ConservationAssetsRedeemerSchema
>;

export const ConservationAssetsRedeemer =
  asDataType<ConservationAssetsRedeemer>(ConservationAssetsRedeemerSchema);

export const ConservationFieldGrammarArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
});

export type ConservationFieldGrammarArgs = Data.Static<
  typeof ConservationFieldGrammarArgsSchema
>;

export const ConservationFieldGrammarArgs =
  asDataType<ConservationFieldGrammarArgs>(ConservationFieldGrammarArgsSchema);

export const ConservationFieldGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(ConservationFieldGrammarArgsSchema);

export type ConservationFieldGrammarRedeemer = Data.Static<
  typeof ConservationFieldGrammarRedeemerSchema
>;

export const ConservationFieldGrammarRedeemer =
  asDataType<ConservationFieldGrammarRedeemer>(
    ConservationFieldGrammarRedeemerSchema,
  );

export const ConservationOutputsArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
});

export type ConservationOutputsArgs = Data.Static<
  typeof ConservationOutputsArgsSchema
>;

export const ConservationOutputsArgs = asDataType<ConservationOutputsArgs>(
  ConservationOutputsArgsSchema,
);

export const ConservationOutputsRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationOutputsArgsSchema,
);

export type ConservationOutputsRedeemer = Data.Static<
  typeof ConservationOutputsRedeemerSchema
>;

export const ConservationOutputsRedeemer =
  asDataType<ConservationOutputsRedeemer>(ConservationOutputsRedeemerSchema);

export const ConservationOutputScanArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  carriage: ConservationChunkCarriageSchema,
  budget: Data.Integer(),
});

export type ConservationOutputScanArgs = Data.Static<
  typeof ConservationOutputScanArgsSchema
>;

export const ConservationOutputScanArgs =
  asDataType<ConservationOutputScanArgs>(ConservationOutputScanArgsSchema);

export const ConservationOutputScanRedeemerSchema =
  faultProofStepRedeemerSchema(ConservationOutputScanArgsSchema);

export type ConservationOutputScanRedeemer = Data.Static<
  typeof ConservationOutputScanRedeemerSchema
>;

export const ConservationOutputScanRedeemer =
  asDataType<ConservationOutputScanRedeemer>(
    ConservationOutputScanRedeemerSchema,
  );

export const ConservationMintArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
});

export type ConservationMintArgs = Data.Static<
  typeof ConservationMintArgsSchema
>;

export const ConservationMintArgs = asDataType<ConservationMintArgs>(
  ConservationMintArgsSchema,
);

export const ConservationMintRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationMintArgsSchema,
);

export type ConservationMintRedeemer = Data.Static<
  typeof ConservationMintRedeemerSchema
>;

export const ConservationMintRedeemer = asDataType<ConservationMintRedeemer>(
  ConservationMintRedeemerSchema,
);

export const ConservationUpdateArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  old_delta: Data.Integer(),
  proof: ProofSchema,
  opening: Data.Bytes(),
});

export type ConservationUpdateArgs = Data.Static<
  typeof ConservationUpdateArgsSchema
>;

export const ConservationUpdateArgs = asDataType<ConservationUpdateArgs>(
  ConservationUpdateArgsSchema,
);

export const ConservationUpdateRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationUpdateArgsSchema,
);

export type ConservationUpdateRedeemer = Data.Static<
  typeof ConservationUpdateRedeemerSchema
>;

export const ConservationUpdateRedeemer =
  asDataType<ConservationUpdateRedeemer>(ConservationUpdateRedeemerSchema);

export const ConservationTerminalArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  witness: Data.Nullable(ConservationDeltaWitnessSchema),
});

export type ConservationTerminalArgs = Data.Static<
  typeof ConservationTerminalArgsSchema
>;

export const ConservationTerminalArgs = asDataType<ConservationTerminalArgs>(
  ConservationTerminalArgsSchema,
);

export const ConservationTerminalRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationTerminalArgsSchema,
);

export type ConservationTerminalRedeemer = Data.Static<
  typeof ConservationTerminalRedeemerSchema
>;

export const ConservationTerminalRedeemer =
  asDataType<ConservationTerminalRedeemer>(ConservationTerminalRedeemerSchema);

export const ConservationAcceptedSourceRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema);

export type ConservationAcceptedSourceRedeemer = Data.Static<
  typeof ConservationAcceptedSourceRedeemerSchema
>;

export const ConservationAcceptedSourceRedeemer =
  asDataType<ConservationAcceptedSourceRedeemer>(
    ConservationAcceptedSourceRedeemerSchema,
  );
