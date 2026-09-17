import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  EventKeySchema,
  EventToStepMembershipProofSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedTransactionSourceMembershipProofSchema,
  HeaderSchema,
  MidgardTxInputSchema,
  NativeTxInclusionCarriageSchema,
  ProofSchema,
  TransitionTraceMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { TransactionOutputScanControlSchema } from "../transaction-output-non-canonical/schemas.js";
import {
  ClaimedAssetSchema,
  ClaimedImbalanceDirectionSchema,
  FrontierPeakSchema,
  ValueNotPreservedStep01ArgsSchema,
} from "./schemas.js";

export const ConservationClaimSchema = Data.Enum([
  Data.Object({
    AcceptedImbalance: Data.Object({
      asset: ClaimedAssetSchema,
      direction: ClaimedImbalanceDirectionSchema,
    }),
  }),
  Data.Literal("ForcedConservation"),
]);
export type ConservationClaim = Data.Static<typeof ConservationClaimSchema>;
export const ConservationClaim = asDataType<ConservationClaim>(
  ConservationClaimSchema,
);

export const ConservationBalanceSchema = Data.Object({
  transaction_id: Data.Bytes(),
  claim: ConservationClaimSchema,
  pre_utxos_root: Data.Bytes(),
  lovelace_delta: Data.Integer(),
  asset_delta_root: Data.Bytes(),
});
export type ConservationBalance = Data.Static<typeof ConservationBalanceSchema>;
export const ConservationBalance = asDataType<ConservationBalance>(
  ConservationBalanceSchema,
);

export const ConservationFoldSchema = Data.Object({
  balance: ConservationBalanceSchema,
  continuation: Data.Any(),
});
export type ConservationFold = Data.Static<typeof ConservationFoldSchema>;
export const ConservationFold = asDataType<ConservationFold>(
  ConservationFoldSchema,
);

export const ConservationSourceSchema = Data.Object({
  transaction_id: Data.Bytes(),
  claim: ConservationClaimSchema,
  fee: Data.Integer(),
  event_key: EventKeySchema,
  event_root: Data.Bytes(),
  event_count: Data.Integer(),
  trace_root: Data.Bytes(),
  trace_count: Data.Integer(),
});
export type ConservationSource = Data.Static<typeof ConservationSourceSchema>;
export const ConservationSource = asDataType<ConservationSource>(
  ConservationSourceSchema,
);

export const ConservationEventSchema = Data.Object({
  transaction_id: Data.Bytes(),
  claim: ConservationClaimSchema,
  fee: Data.Integer(),
  event_key: EventKeySchema,
  trace_root: Data.Bytes(),
  trace_count: Data.Integer(),
  step_index: Data.Integer(),
});
export type ConservationEvent = Data.Static<typeof ConservationEventSchema>;
export const ConservationEvent = asDataType<ConservationEvent>(
  ConservationEventSchema,
);

export const ConservationInputsSchema = Data.Object({ cursor: Data.Integer() });
export type ConservationInputs = Data.Static<typeof ConservationInputsSchema>;
export const ConservationInputs = asDataType<ConservationInputs>(
  ConservationInputsSchema,
);

export const ConservationSelectedInputSchema = Data.Object({
  input: MidgardTxInputSchema,
  cursor: Data.Integer(),
  selector_hash: Data.Bytes(),
});
export type ConservationSelectedInput = Data.Static<
  typeof ConservationSelectedInputSchema
>;
export const ConservationSelectedInput = asDataType<ConservationSelectedInput>(
  ConservationSelectedInputSchema,
);

export const ConservationAssetCursorSchema = Data.Object({
  count: Data.Integer(),
  frontier_commitment: Data.Bytes(),
  cursor: Data.Integer(),
  quantity_sign: Data.Integer(),
  next_script_hash: Data.Bytes(),
  next_continuation: Data.Any(),
});
export type ConservationAssetCursor = Data.Static<
  typeof ConservationAssetCursorSchema
>;
export const ConservationAssetCursor = asDataType<ConservationAssetCursor>(
  ConservationAssetCursorSchema,
);

export const ConservationFieldGrammarSchema = Data.Object({
  field_index: Data.Integer(),
  checkpoint_hash: Data.Nullable(Data.Bytes()),
});
export type ConservationFieldGrammar = Data.Static<
  typeof ConservationFieldGrammarSchema
>;
export const ConservationFieldGrammar = asDataType<ConservationFieldGrammar>(
  ConservationFieldGrammarSchema,
);

export const ConservationFieldCursorSchema = Data.Object({
  field_index: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  grammar_script_hash: Data.Bytes(),
});
export type ConservationFieldCursor = Data.Static<
  typeof ConservationFieldCursorSchema
>;
export const ConservationFieldCursor = asDataType<ConservationFieldCursor>(
  ConservationFieldCursorSchema,
);

export const ConservationOutputItemSchema = Data.Object({
  field_total_length: Data.Integer(),
  field_chunk_hashes: Data.Array(Data.Bytes()),
  index: Data.Integer(),
  offset: Data.Integer(),
  length: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  next_checkpoint_hash: Data.Bytes(),
  selector_hash: Data.Bytes(),
  grammar_script_hash: Data.Bytes(),
});
export type ConservationOutputItem = Data.Static<
  typeof ConservationOutputItemSchema
>;
export const ConservationOutputItem = asDataType<ConservationOutputItem>(
  ConservationOutputItemSchema,
);

export const ConservationOutputScanSchema = Data.Object({
  item: ConservationOutputItemSchema,
  control: TransactionOutputScanControlSchema,
});
export type ConservationOutputScan = Data.Static<
  typeof ConservationOutputScanSchema
>;
export const ConservationOutputScan = asDataType<ConservationOutputScan>(
  ConservationOutputScanSchema,
);

export const ConservationMintPolicySchema = Data.Object({
  offset: Data.Integer(),
  length: Data.Integer(),
  next_checkpoint_hash: Data.Bytes(),
  policy_id: Data.Bytes(),
  cursor: Data.Integer(),
  remaining: Data.Integer(),
  previous_asset: Data.Nullable(Data.Bytes()),
});
export type ConservationMintPolicy = Data.Static<
  typeof ConservationMintPolicySchema
>;
export const ConservationMintPolicy = asDataType<ConservationMintPolicy>(
  ConservationMintPolicySchema,
);

export const ConservationMintCursorSchema = Data.Object({
  field: ConservationFieldCursorSchema,
  previous_policy: Data.Nullable(Data.Bytes()),
  active: Data.Nullable(ConservationMintPolicySchema),
});
export type ConservationMintCursor = Data.Static<
  typeof ConservationMintCursorSchema
>;
export const ConservationMintCursor = asDataType<ConservationMintCursor>(
  ConservationMintCursorSchema,
);

export const ConservationPendingSchema = Data.Object({
  unit: Data.Bytes(),
  quantity: Data.Integer(),
  return_script_hash: Data.Bytes(),
  next_continuation: Data.Any(),
});
export type ConservationPending = Data.Static<typeof ConservationPendingSchema>;
export const ConservationPending = asDataType<ConservationPending>(
  ConservationPendingSchema,
);

export const ConservationDeltaWitnessSchema = Data.Object({
  delta: Data.Integer(),
  proof: ProofSchema,
});
export type ConservationDeltaWitness = Data.Static<
  typeof ConservationDeltaWitnessSchema
>;
export const ConservationDeltaWitness = asDataType<ConservationDeltaWitness>(
  ConservationDeltaWitnessSchema,
);

export const ConservationClaimDatumSchema = faultProofStepDatumSchema(
  ConservationClaimSchema,
);
export type ConservationClaimDatum = Data.Static<
  typeof ConservationClaimDatumSchema
>;
export const ConservationClaimDatum = asDataType<ConservationClaimDatum>(
  ConservationClaimDatumSchema,
);

export const ConservationBalanceDatumSchema = faultProofStepDatumSchema(
  ConservationBalanceSchema,
);
export type ConservationBalanceDatum = Data.Static<
  typeof ConservationBalanceDatumSchema
>;
export const ConservationBalanceDatum = asDataType<ConservationBalanceDatum>(
  ConservationBalanceDatumSchema,
);

export const ConservationFoldDatumSchema = faultProofStepDatumSchema(
  ConservationFoldSchema,
);
export type ConservationFoldDatum = Data.Static<
  typeof ConservationFoldDatumSchema
>;
export const ConservationFoldDatum = asDataType<ConservationFoldDatum>(
  ConservationFoldDatumSchema,
);

export const ConservationSourceDatumSchema = faultProofStepDatumSchema(
  ConservationSourceSchema,
);
export type ConservationSourceDatum = Data.Static<
  typeof ConservationSourceDatumSchema
>;
export const ConservationSourceDatum = asDataType<ConservationSourceDatum>(
  ConservationSourceDatumSchema,
);

export const ConservationEventDatumSchema = faultProofStepDatumSchema(
  ConservationEventSchema,
);
export type ConservationEventDatum = Data.Static<
  typeof ConservationEventDatumSchema
>;
export const ConservationEventDatum = asDataType<ConservationEventDatum>(
  ConservationEventDatumSchema,
);

export const ConservationLaunchArgsSchema = Data.Enum([
  Data.Object({ Args: ValueNotPreservedStep01ArgsSchema }),
  Data.Object({
    LaunchUnion: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      claim: ConservationClaimSchema,
    }),
  }),
]);
export type ConservationLaunchArgs = Data.Static<
  typeof ConservationLaunchArgsSchema
>;
export const ConservationLaunchArgs = asDataType<ConservationLaunchArgs>(
  ConservationLaunchArgsSchema,
);

export const ConservationLaunchRedeemerSchema = faultProofStepRedeemerSchema(
  ConservationLaunchArgsSchema,
);
export type ConservationLaunchRedeemer = Data.Static<
  typeof ConservationLaunchRedeemerSchema
>;
export const ConservationLaunchRedeemer =
  asDataType<ConservationLaunchRedeemer>(ConservationLaunchRedeemerSchema);

export const ConservationAssetActionSchema = Data.Enum([
  Data.Object({
    Select: Data.Object({
      policy_id: Data.Bytes(),
      asset_name: Data.Bytes(),
      quantity: Data.Integer(),
      peaks: Data.Array(FrontierPeakSchema),
      siblings: Data.Array(Data.Bytes()),
    }),
  }),
  Data.Literal("Finish"),
]);
export type ConservationAssetAction = Data.Static<
  typeof ConservationAssetActionSchema
>;
export const ConservationAssetAction = asDataType<ConservationAssetAction>(
  ConservationAssetActionSchema,
);

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
