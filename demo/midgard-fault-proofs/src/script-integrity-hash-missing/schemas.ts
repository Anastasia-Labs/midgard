import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldCarriageSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  NativeTxWitnessSetCompactSchema,
  OutputReferenceSchema,
  RejectionReason as RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ScriptIntegrityVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema as never),
});
export const ScriptIntegrityBindStateSchema = Data.Enum([
  Data.Object({
    BoundAccepted: Data.Object({
      subject: ScriptIntegrityVerdictSubjectSchema,
      witness_set_hash: Data.Bytes(),
    }),
  }),
  Data.Object({ PendingForced: Data.Object({ direction: Data.Integer() }) }),
]);
export const ScriptIntegritySubjectStateSchema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectSchema,
  witness_set_hash: Data.Bytes(),
});
export const ScriptIntegrityStagedPhaseSchema = Data.Enum([
  Data.Object({
    ScriptGrammar: Data.Object({ checkpoint_hash: Data.Bytes() }),
  }),
  Data.Object({
    ScriptScan: Data.Object({
      checkpoint_hash: Data.Bytes(),
      contains_non_native_script: Data.Boolean(),
    }),
  }),
  Data.Object({
    ScriptComplete: Data.Object({ contains_non_native_script: Data.Boolean() }),
  }),
  Data.Object({
    RedeemerGrammar: Data.Object({
      checkpoint_hash: Data.Bytes(),
      contains_non_native_script: Data.Boolean(),
    }),
  }),
]);
export const ScriptIntegrityStagedStateSchema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectSchema,
  witness_set_hash: Data.Bytes(),
  script_integrity_hash: Data.Bytes(),
  phase: ScriptIntegrityStagedPhaseSchema,
});
export const ScriptIntegrityDecisionStateSchema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectSchema,
  script_integrity_hash: Data.Bytes(),
  contains_non_native_script: Data.Boolean(),
  has_redeemers: Data.Boolean(),
});

export const ScriptIntegrityStepDatums = [
  faultProofStepDatumSchema(Data.Any()),
  faultProofStepDatumSchema(ScriptIntegrityBindStateSchema),
  faultProofStepDatumSchema(ScriptIntegritySubjectStateSchema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateSchema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateSchema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateSchema),
  faultProofStepDatumSchema(ScriptIntegrityDecisionStateSchema),
] as const;
export const ScriptIntegrityStep02DatumSchema = ScriptIntegrityStepDatums[1];
export const ScriptIntegrityStep03DatumSchema = ScriptIntegrityStepDatums[2];
export const ScriptIntegrityStep04DatumSchema = ScriptIntegrityStepDatums[6];

export const ScriptIntegrityStep01ArgsSchema = Data.Enum([
  Data.Object({
    BindAccepted: Data.Object({ carriage: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    RecordForced: Data.Object({
      direction: Data.Integer(),
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export const ScriptIntegrityStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  header: HeaderSchema,
  forced_membership: Data.Nullable(
    rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxV1Schema),
  ),
});
export const ScriptIntegrityStep03ArgsSchema = Data.Enum([
  Data.Object({
    Direct: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      native_tx_compact_cbor: Data.Bytes(),
      witness_set: NativeTxWitnessSetCompactSchema,
      script_witnesses: FieldCarriageSchema,
      redeemers: FieldCarriageSchema,
    }),
  }),
  Data.Object({
    StartStaged: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_witnesses_opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
]);
export const ScriptIntegrityScriptGrammarArgsSchema = Data.Enum([
  Data.Object({
    Resume: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    StartScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export const ScriptIntegrityScanArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const ScriptIntegrityRedeemerGrammarArgsSchema = Data.Enum([
  Data.Object({
    Start: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    Resume: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    Finish: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
    }),
  }),
]);
export const ScriptIntegrityStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ScriptIntegritySpendRedeemers = [
  faultProofStepRedeemerSchema(ScriptIntegrityStep01ArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep02ArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep03ArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityScriptGrammarArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityScanArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityRedeemerGrammarArgsSchema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep04ArgsSchema),
] as const;
