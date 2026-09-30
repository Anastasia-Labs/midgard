import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, PubKeyHashSchema } from "../common.js";
import { ValidationTraceDescriptorSchema } from "../ledger-state.js";
import {
  FrontierPeakSchema,
  ValueAssetMutationWitnessSchema,
} from "./validation-auxiliary-witness.js";

export const ValueAccumulatorSchema = Data.Object({
  lovelace_delta: Data.Integer(),
  asset_root: Data.Bytes(),
  seen_asset_count: Data.Integer(),
  nonzero_asset_count: Data.Integer(),
});

export const ValueAccumulatorUpdateSchema = Data.Enum([
  Data.Object({
    ValueAccumulatorUpdated: Data.Tuple([ValueAccumulatorSchema]),
  }),
  Data.Literal("ValueAccumulatorAssetLimitExceeded"),
  Data.Literal("ValueAccumulatorMutationInvalid"),
]);

export const AssetDescriptorClaimSchema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  asset_index: Data.Integer(),
  asset_peaks: Data.Array(FrontierPeakSchema),
  asset_siblings: Data.Array(Data.Bytes()),
  asset_count: Data.Integer(),
});

export const AssetFoldClaimSchema = Data.Object({
  policy_id: Data.Bytes(),
  asset_name: Data.Bytes(),
  quantity: Data.Integer(),
  mutation: ValueAssetMutationWitnessSchema,
  pre_value_accumulator: ValueAccumulatorSchema,
  outcome: ValueAccumulatorUpdateSchema,
  descriptor: Data.Nullable(AssetDescriptorClaimSchema),
});

export type AssetFoldClaim = Data.Static<typeof AssetFoldClaimSchema>;

export const AssetFoldClaim = asDataType<AssetFoldClaim>(AssetFoldClaimSchema);

export const ValidationMachinePhaseSchema = Data.Enum([
  Data.Literal("CanonicalDecode"),
  Data.Literal("CompactBinding"),
  Data.Literal("StaticLedgerRules"),
  Data.Literal("InputSets"),
  Data.Literal("Signatures"),
  Data.Literal("PhaseANativeScripts"),
  Data.Literal("PhaseAScriptPreconditions"),
  Data.Literal("ResolveInputs"),
  Data.Literal("ScriptSources"),
  Data.Literal("NativeScripts"),
  Data.Literal("ScriptIntegrity"),
  Data.Literal("Cek"),
  Data.Literal("ValueAndMint"),
  Data.Literal("LedgerDelta"),
  Data.Literal("Terminal"),
]);

export const ValidationMachineVerdictSchema = Data.Enum([
  Data.Literal("Pending"),
  Data.Literal("Accepted"),
  Data.Literal("Rejected"),
]);

export const ValidationMachineSourceKindSchema = Data.Enum([
  Data.Literal("Normal"),
  Data.Literal("Forced"),
]);

export const ValidationMachineStateSchema = Data.Object({
  machine_version: Data.Integer(),
  event_key_hash: H32Schema,
  transaction_id: H32Schema,
  transaction_commitment: H32Schema,
  validation_context_hash: H32Schema,
  source_kind: ValidationMachineSourceKindSchema,
  prior_ledger_root: H32Schema,
  phase: ValidationMachinePhaseSchema,
  program_counter: Data.Integer(),
  work_root: H32Schema,
  execution_cpu: Data.Integer(),
  execution_memory: Data.Integer(),
  verdict: ValidationMachineVerdictSchema,
  rejection_code_hash: H32Schema,
  ledger_delta_root: H32Schema,
});

export type ValidationMachineState = Data.Static<
  typeof ValidationMachineStateSchema
>;

export const ValidationMachineState = asDataType<ValidationMachineState>(
  ValidationMachineStateSchema,
);

export const ValidationTraceProofSchema = Data.Object({
  state_index: Data.Integer(),
  state_hash: H32Schema,
  siblings: Data.Array(H32Schema),
});

export type ValidationTraceProof = Data.Static<
  typeof ValidationTraceProofSchema
>;

export const ValidationTraceProof = asDataType<ValidationTraceProof>(
  ValidationTraceProofSchema,
);

export const ValidationDisputeTurnSchema = Data.Enum([
  Data.Object({
    AwaitingOperator: Data.Object({ midpoint: Data.Integer() }),
  }),
  Data.Object({
    AwaitingChallenger: Data.Object({
      midpoint: Data.Integer(),
      operator_midpoint_hash: H32Schema,
    }),
  }),
  Data.Literal("ReadyForOneStep"),
]);

export const ValidationDisputeSchema = Data.Object({
  version: Data.Integer(),
  operator_descriptor: ValidationTraceDescriptorSchema,
  challenger_descriptor: ValidationTraceDescriptorSchema,
  low_index: Data.Integer(),
  high_index: Data.Integer(),
  agreed_low_hash: H32Schema,
  operator_high_hash: H32Schema,
  challenger_high_hash: H32Schema,
  round: Data.Integer(),
  response_deadline: Data.Integer(),
  turn: ValidationDisputeTurnSchema,
});

export type ValidationDispute = Data.Static<typeof ValidationDisputeSchema>;

export const ValidationDispute = asDataType<ValidationDispute>(
  ValidationDisputeSchema,
);

export const ValidationDisputeStateSchema = Data.Object({
  challenged_header_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  operator_vkey: PubKeyHashSchema,
  dispute: ValidationDisputeSchema,
});

export type ValidationDisputeState = Data.Static<
  typeof ValidationDisputeStateSchema
>;

export const ValidationDisputeState = asDataType<ValidationDisputeState>(
  ValidationDisputeStateSchema,
);

export const ValidationDisputeDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ValidationDisputeStateSchema),
});

export type ValidationDisputeDatum = Data.Static<
  typeof ValidationDisputeDatumSchema
>;

export const ValidationDisputeDatum = asDataType<ValidationDisputeDatum>(
  ValidationDisputeDatumSchema,
);

export const ValidationResolutionStateSchema = Data.Object({
  version: Data.Integer(),
  pre_state: ValidationMachineStateSchema,
  operator_successor_hash: H32Schema,
  challenger_successor_hash: H32Schema,
});

export type ValidationResolutionState = Data.Static<
  typeof ValidationResolutionStateSchema
>;

export const ValidationResolutionState = asDataType<ValidationResolutionState>(
  ValidationResolutionStateSchema,
);

export const ValidationResolutionDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ValidationResolutionStateSchema),
});

export type ValidationResolutionDatum = Data.Static<
  typeof ValidationResolutionDatumSchema
>;

export const ValidationResolutionDatum = asDataType<ValidationResolutionDatum>(
  ValidationResolutionDatumSchema,
);

export const PreparedValidationResolutionStateSchema = Data.Object({
  version: Data.Integer(),
  resolution: ValidationResolutionStateSchema,
  evidence_hash: H32Schema,
});

export type PreparedValidationResolutionState = Data.Static<
  typeof PreparedValidationResolutionStateSchema
>;

export const PreparedValidationResolutionState =
  asDataType<PreparedValidationResolutionState>(
    PreparedValidationResolutionStateSchema,
  );

export const PreparedValidationResolutionDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PreparedValidationResolutionStateSchema),
});

export type PreparedValidationResolutionDatum = Data.Static<
  typeof PreparedValidationResolutionDatumSchema
>;

export const PreparedValidationResolutionDatum =
  asDataType<PreparedValidationResolutionDatum>(
    PreparedValidationResolutionDatumSchema,
  );

export const CanonicalDecodeItemSourceSchema = Data.Object({
  expected_field_commitment: H32Schema,
  expected_field_length: Data.Integer(),
});

export type CanonicalDecodeItemSource = Data.Static<
  typeof CanonicalDecodeItemSourceSchema
>;

export const CanonicalDecodeItemSource = asDataType<CanonicalDecodeItemSource>(
  CanonicalDecodeItemSourceSchema,
);

/**
 * What opening the item through §8's door established about it: the field's
 * authenticated §5.2 item count and this item's §5.1 payload length.
 *
 * #597, the TypeScript twin of #592's wire change. It used to carry the prover's
 * `collection_proof` beside a re-derived `item_commitment` — both artifacts of
 * the counted opening §4 made unsatisfiable. The door derives the count and the
 * length from the preimage it authenticated, so there is nothing for a prover to
 * claim and nothing to open. Aiken source of truth:
 * `onchain/aiken/lib/midgard/validation-machine/`.
 */
export const CanonicalDecodeItemObservationSchema = Data.Object({
  item_count: Data.Integer(),
  item_length: Data.Integer(),
});

export type CanonicalDecodeItemObservation = Data.Static<
  typeof CanonicalDecodeItemObservationSchema
>;

export const CanonicalDecodeItemObservation =
  asDataType<CanonicalDecodeItemObservation>(
    CanonicalDecodeItemObservationSchema,
  );

export const CanonicalDecodeItemProofSchema = Data.Object({
  active_item_count: Data.Integer(),
  item_encoding_is_valid: Data.Boolean(),
  next_encoded_length: Data.Integer(),
});

export type CanonicalDecodeItemProof = Data.Static<
  typeof CanonicalDecodeItemProofSchema
>;

export const CanonicalDecodeItemProof = asDataType<CanonicalDecodeItemProof>(
  CanonicalDecodeItemProofSchema,
);

export const WinningValidationResolutionStateSchema = Data.Object({
  version: Data.Integer(),
});

export type WinningValidationResolutionState = Data.Static<
  typeof WinningValidationResolutionStateSchema
>;

export const WinningValidationResolutionState =
  asDataType<WinningValidationResolutionState>(
    WinningValidationResolutionStateSchema,
  );

export const WinningValidationResolutionDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(WinningValidationResolutionStateSchema),
});

export type WinningValidationResolutionDatum = Data.Static<
  typeof WinningValidationResolutionDatumSchema
>;

export const WinningValidationResolutionDatum =
  asDataType<WinningValidationResolutionDatum>(
    WinningValidationResolutionDatumSchema,
  );

export const ValidationOneStepWitnessSchema = Data.Object({
  work_witness_cbor: Data.Bytes(),
  claimed_successor: ValidationMachineStateSchema,
});

export type ValidationOneStepWitness = Data.Static<
  typeof ValidationOneStepWitnessSchema
>;

export const ValidationOneStepWitness = asDataType<ValidationOneStepWitness>(
  ValidationOneStepWitnessSchema,
);

export const AuthenticatedCanonicalDecodeItemSchema = Data.Object({
  version: Data.Integer(),
  base: PreparedValidationResolutionStateSchema,
  transition: ValidationOneStepWitnessSchema,
});

export type AuthenticatedCanonicalDecodeItem = Data.Static<
  typeof AuthenticatedCanonicalDecodeItemSchema
>;

export const AuthenticatedCanonicalDecodeItem =
  asDataType<AuthenticatedCanonicalDecodeItem>(
    AuthenticatedCanonicalDecodeItemSchema,
  );

export const PreparedCanonicalDecodeItemSchema = Data.Object({
  version: Data.Integer(),
  authenticated: AuthenticatedCanonicalDecodeItemSchema,
  source: CanonicalDecodeItemSourceSchema,
});
