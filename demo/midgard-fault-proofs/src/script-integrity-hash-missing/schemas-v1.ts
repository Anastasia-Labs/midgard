import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldCarriageV1Schema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  NativeTxWitnessSetCompactSchema,
  OutputReferenceSchema,
  RejectionReasonV1 as RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ScriptIntegrityVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema as never),
});
export const ScriptIntegrityBindStateV1Schema = Data.Enum([
  Data.Object({
    BoundAccepted: Data.Object({
      subject: ScriptIntegrityVerdictSubjectV1Schema,
      witness_set_hash: Data.Bytes(),
    }),
  }),
  Data.Object({ PendingForced: Data.Object({ direction: Data.Integer() }) }),
]);
export const ScriptIntegritySubjectStateV1Schema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectV1Schema,
  witness_set_hash: Data.Bytes(),
});
export const ScriptIntegrityStagedPhaseV1Schema = Data.Enum([
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
export const ScriptIntegrityStagedStateV1Schema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectV1Schema,
  witness_set_hash: Data.Bytes(),
  script_integrity_hash: Data.Bytes(),
  phase: ScriptIntegrityStagedPhaseV1Schema,
});
export const ScriptIntegrityDecisionStateV1Schema = Data.Object({
  subject: ScriptIntegrityVerdictSubjectV1Schema,
  script_integrity_hash: Data.Bytes(),
  contains_non_native_script: Data.Boolean(),
  has_redeemers: Data.Boolean(),
});

export const ScriptIntegrityStepDatumsV1 = [
  faultProofStepDatumSchema(Data.Any()),
  faultProofStepDatumSchema(ScriptIntegrityBindStateV1Schema),
  faultProofStepDatumSchema(ScriptIntegritySubjectStateV1Schema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateV1Schema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateV1Schema),
  faultProofStepDatumSchema(ScriptIntegrityStagedStateV1Schema),
  faultProofStepDatumSchema(ScriptIntegrityDecisionStateV1Schema),
] as const;
export const ScriptIntegrityStep02DatumV1Schema =
  ScriptIntegrityStepDatumsV1[1];
export const ScriptIntegrityStep03DatumV1Schema =
  ScriptIntegrityStepDatumsV1[2];
export const ScriptIntegrityStep04DatumV1Schema =
  ScriptIntegrityStepDatumsV1[6];

export const ScriptIntegrityStep01ArgsV1Schema = Data.Enum([
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
export const ScriptIntegrityStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  header: HeaderV1Schema,
  forced_membership: Data.Nullable(
    rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxV1Schema),
  ),
});
export const ScriptIntegrityStep03ArgsV1Schema = Data.Enum([
  Data.Object({
    Direct: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      native_tx_compact_cbor: Data.Bytes(),
      witness_set: NativeTxWitnessSetCompactSchema,
      script_witnesses: FieldCarriageV1Schema,
      redeemers: FieldCarriageV1Schema,
    }),
  }),
  Data.Object({
    StartStaged: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_witnesses_opening: FieldOpeningV1Schema,
      item_budget: Data.Integer(),
    }),
  }),
]);
export const ScriptIntegrityScriptGrammarArgsV1Schema = Data.Enum([
  Data.Object({
    Resume: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    StartScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export const ScriptIntegrityScanArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
  checkpoint_bytes: Data.Bytes(),
  item_budget: Data.Integer(),
});
export const ScriptIntegrityRedeemerGrammarArgsV1Schema = Data.Enum([
  Data.Object({
    Start: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    Resume: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    Finish: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
    }),
  }),
]);
export const ScriptIntegrityStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const ScriptIntegritySpendRedeemersV1 = [
  faultProofStepRedeemerSchema(ScriptIntegrityStep01ArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep02ArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep03ArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityScriptGrammarArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityScanArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityRedeemerGrammarArgsV1Schema),
  faultProofStepRedeemerSchema(ScriptIntegrityStep04ArgsV1Schema),
] as const;
