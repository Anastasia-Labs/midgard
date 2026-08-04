import type { MidgardValidationDisputeV1 } from "@al-ft/midgard-core/validation-dispute";
import type {
  MidgardValidationMachineStateV1,
  MidgardValidationTraceDescriptorV1,
  MidgardValidationTraceProofV1,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

import {
  H32Schema,
  OutputReferenceSchema,
  PubKeyHashSchema,
} from "@/common.js";
import {
  BoundedCollectionItemProofV1Schema,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  L2TransactionSourceV1Schema,
  TransitionStepSchema,
  ValidationTraceDescriptorV1Schema,
} from "@/ledger-state.js";
import { rootMembershipProofSchema } from "@/transition-trace.js";

import { ValidationAuxiliaryWitnessV1Schema } from "./validation-auxiliary-witness-v1.js";

export const ValidationMachinePhaseV1Schema = Data.Enum([
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

export const ValidationMachineVerdictV1Schema = Data.Enum([
  Data.Literal("Pending"),
  Data.Literal("Accepted"),
  Data.Literal("Rejected"),
]);

export const ValidationMachineSourceKindV1Schema = Data.Enum([
  Data.Literal("Normal"),
  Data.Literal("Forced"),
]);

export const ValidationMachineStateV1Schema = Data.Object({
  machine_version: Data.Integer(),
  event_key_hash: H32Schema,
  transaction_id: H32Schema,
  transaction_commitment: H32Schema,
  validation_context_hash: H32Schema,
  source_kind: ValidationMachineSourceKindV1Schema,
  prior_ledger_root: H32Schema,
  phase: ValidationMachinePhaseV1Schema,
  program_counter: Data.Integer(),
  work_root: H32Schema,
  execution_cpu: Data.Integer(),
  execution_memory: Data.Integer(),
  verdict: ValidationMachineVerdictV1Schema,
  rejection_code_hash: H32Schema,
  ledger_delta_root: H32Schema,
});
export type ValidationMachineStateV1 = Data.Static<
  typeof ValidationMachineStateV1Schema
>;
export const ValidationMachineStateV1 =
  ValidationMachineStateV1Schema as unknown as ValidationMachineStateV1;

export const ValidationTraceProofV1Schema = Data.Object({
  state_index: Data.Integer(),
  state_hash: H32Schema,
  siblings: Data.Array(H32Schema),
});
export type ValidationTraceProofV1 = Data.Static<
  typeof ValidationTraceProofV1Schema
>;
export const ValidationTraceProofV1 =
  ValidationTraceProofV1Schema as unknown as ValidationTraceProofV1;

export const ValidationDisputeTurnV1Schema = Data.Enum([
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

export const ValidationDisputeV1Schema = Data.Object({
  version: Data.Integer(),
  operator_descriptor: ValidationTraceDescriptorV1Schema,
  challenger_descriptor: ValidationTraceDescriptorV1Schema,
  low_index: Data.Integer(),
  high_index: Data.Integer(),
  agreed_low_hash: H32Schema,
  operator_high_hash: H32Schema,
  challenger_high_hash: H32Schema,
  round: Data.Integer(),
  response_deadline: Data.Integer(),
  turn: ValidationDisputeTurnV1Schema,
});
export type ValidationDisputeV1 = Data.Static<typeof ValidationDisputeV1Schema>;
export const ValidationDisputeV1 =
  ValidationDisputeV1Schema as unknown as ValidationDisputeV1;

export const ValidationDisputeStateV1Schema = Data.Object({
  challenged_header_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  operator_vkey: PubKeyHashSchema,
  dispute: ValidationDisputeV1Schema,
});
export type ValidationDisputeStateV1 = Data.Static<
  typeof ValidationDisputeStateV1Schema
>;
export const ValidationDisputeStateV1 =
  ValidationDisputeStateV1Schema as unknown as ValidationDisputeStateV1;

export const ValidationDisputeDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ValidationDisputeStateV1Schema),
});
export type ValidationDisputeDatumV1 = Data.Static<
  typeof ValidationDisputeDatumV1Schema
>;
export const ValidationDisputeDatumV1 =
  ValidationDisputeDatumV1Schema as unknown as ValidationDisputeDatumV1;

export const ValidationResolutionStateV1Schema = Data.Object({
  version: Data.Integer(),
  pre_state: ValidationMachineStateV1Schema,
  operator_successor_hash: H32Schema,
  challenger_successor_hash: H32Schema,
});
export type ValidationResolutionStateV1 = Data.Static<
  typeof ValidationResolutionStateV1Schema
>;
export const ValidationResolutionStateV1 =
  ValidationResolutionStateV1Schema as unknown as ValidationResolutionStateV1;

export const ValidationResolutionDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ValidationResolutionStateV1Schema),
});
export type ValidationResolutionDatumV1 = Data.Static<
  typeof ValidationResolutionDatumV1Schema
>;
export const ValidationResolutionDatumV1 =
  ValidationResolutionDatumV1Schema as unknown as ValidationResolutionDatumV1;

export const PreparedValidationResolutionStateV1Schema = Data.Object({
  version: Data.Integer(),
  resolution: ValidationResolutionStateV1Schema,
  evidence_hash: H32Schema,
});
export type PreparedValidationResolutionStateV1 = Data.Static<
  typeof PreparedValidationResolutionStateV1Schema
>;
export const PreparedValidationResolutionStateV1 =
  PreparedValidationResolutionStateV1Schema as unknown as PreparedValidationResolutionStateV1;

export const PreparedValidationResolutionDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PreparedValidationResolutionStateV1Schema),
});
export type PreparedValidationResolutionDatumV1 = Data.Static<
  typeof PreparedValidationResolutionDatumV1Schema
>;
export const PreparedValidationResolutionDatumV1 =
  PreparedValidationResolutionDatumV1Schema as unknown as PreparedValidationResolutionDatumV1;

export const CanonicalDecodeItemSourceV1Schema = Data.Object({
  expected_field_commitment: H32Schema,
  expected_field_length: Data.Integer(),
});
export type CanonicalDecodeItemSourceV1 = Data.Static<
  typeof CanonicalDecodeItemSourceV1Schema
>;
export const CanonicalDecodeItemSourceV1 =
  CanonicalDecodeItemSourceV1Schema as unknown as CanonicalDecodeItemSourceV1;

export const CanonicalDecodeItemObservationV1Schema = Data.Object({
  collection_proof: BoundedCollectionItemProofV1Schema,
  item_length: Data.Integer(),
  item_commitment: H32Schema,
});
export type CanonicalDecodeItemObservationV1 = Data.Static<
  typeof CanonicalDecodeItemObservationV1Schema
>;
export const CanonicalDecodeItemObservationV1 =
  CanonicalDecodeItemObservationV1Schema as unknown as CanonicalDecodeItemObservationV1;

export const CanonicalDecodeItemProofV1Schema = Data.Object({
  active_item_count: Data.Integer(),
  item_encoding_is_valid: Data.Boolean(),
  next_encoded_length: Data.Integer(),
});
export type CanonicalDecodeItemProofV1 = Data.Static<
  typeof CanonicalDecodeItemProofV1Schema
>;
export const CanonicalDecodeItemProofV1 =
  CanonicalDecodeItemProofV1Schema as unknown as CanonicalDecodeItemProofV1;

export const WinningValidationResolutionStateV1Schema = Data.Object({
  version: Data.Integer(),
});
export type WinningValidationResolutionStateV1 = Data.Static<
  typeof WinningValidationResolutionStateV1Schema
>;
export const WinningValidationResolutionStateV1 =
  WinningValidationResolutionStateV1Schema as unknown as WinningValidationResolutionStateV1;

export const WinningValidationResolutionDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(WinningValidationResolutionStateV1Schema),
});
export type WinningValidationResolutionDatumV1 = Data.Static<
  typeof WinningValidationResolutionDatumV1Schema
>;
export const WinningValidationResolutionDatumV1 =
  WinningValidationResolutionDatumV1Schema as unknown as WinningValidationResolutionDatumV1;

export const ValidationOneStepWitnessV1Schema = Data.Object({
  work_witness_cbor: Data.Bytes(),
  claimed_successor: ValidationMachineStateV1Schema,
});
export type ValidationOneStepWitnessV1 = Data.Static<
  typeof ValidationOneStepWitnessV1Schema
>;
export const ValidationOneStepWitnessV1 =
  ValidationOneStepWitnessV1Schema as unknown as ValidationOneStepWitnessV1;

export const AuthenticatedCanonicalDecodeItemV1Schema = Data.Object({
  version: Data.Integer(),
  base: PreparedValidationResolutionStateV1Schema,
  transition: ValidationOneStepWitnessV1Schema,
});
export type AuthenticatedCanonicalDecodeItemV1 = Data.Static<
  typeof AuthenticatedCanonicalDecodeItemV1Schema
>;
export const AuthenticatedCanonicalDecodeItemV1 =
  AuthenticatedCanonicalDecodeItemV1Schema as unknown as AuthenticatedCanonicalDecodeItemV1;

export const PreparedCanonicalDecodeItemV1Schema = Data.Object({
  version: Data.Integer(),
  authenticated: AuthenticatedCanonicalDecodeItemV1Schema,
  source: CanonicalDecodeItemSourceV1Schema,
});
export type PreparedCanonicalDecodeItemV1 = Data.Static<
  typeof PreparedCanonicalDecodeItemV1Schema
>;
export const PreparedCanonicalDecodeItemV1 =
  PreparedCanonicalDecodeItemV1Schema as unknown as PreparedCanonicalDecodeItemV1;

export const ObservedCanonicalDecodeItemV1Schema = Data.Object({
  version: Data.Integer(),
  prepared: PreparedCanonicalDecodeItemV1Schema,
  observation: CanonicalDecodeItemObservationV1Schema,
});
export type ObservedCanonicalDecodeItemV1 = Data.Static<
  typeof ObservedCanonicalDecodeItemV1Schema
>;
export const ObservedCanonicalDecodeItemV1 =
  ObservedCanonicalDecodeItemV1Schema as unknown as ObservedCanonicalDecodeItemV1;

export const VerifiedCanonicalDecodeItemV1Schema = Data.Object({
  version: Data.Integer(),
  observed: ObservedCanonicalDecodeItemV1Schema,
  proof: CanonicalDecodeItemProofV1Schema,
});
export type VerifiedCanonicalDecodeItemV1 = Data.Static<
  typeof VerifiedCanonicalDecodeItemV1Schema
>;
export const VerifiedCanonicalDecodeItemV1 =
  VerifiedCanonicalDecodeItemV1Schema as unknown as VerifiedCanonicalDecodeItemV1;

export const AuthenticatedCanonicalDecodeItemDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(AuthenticatedCanonicalDecodeItemV1Schema),
});
export type AuthenticatedCanonicalDecodeItemDatumV1 = Data.Static<
  typeof AuthenticatedCanonicalDecodeItemDatumV1Schema
>;
export const AuthenticatedCanonicalDecodeItemDatumV1 =
  AuthenticatedCanonicalDecodeItemDatumV1Schema as unknown as AuthenticatedCanonicalDecodeItemDatumV1;

export const PreparedCanonicalDecodeItemDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PreparedCanonicalDecodeItemV1Schema),
});
export type PreparedCanonicalDecodeItemDatumV1 = Data.Static<
  typeof PreparedCanonicalDecodeItemDatumV1Schema
>;
export const PreparedCanonicalDecodeItemDatumV1 =
  PreparedCanonicalDecodeItemDatumV1Schema as unknown as PreparedCanonicalDecodeItemDatumV1;

export const ObservedCanonicalDecodeItemDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ObservedCanonicalDecodeItemV1Schema),
});
export type ObservedCanonicalDecodeItemDatumV1 = Data.Static<
  typeof ObservedCanonicalDecodeItemDatumV1Schema
>;
export const ObservedCanonicalDecodeItemDatumV1 =
  ObservedCanonicalDecodeItemDatumV1Schema as unknown as ObservedCanonicalDecodeItemDatumV1;

export const VerifiedCanonicalDecodeItemDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(VerifiedCanonicalDecodeItemV1Schema),
});
export type VerifiedCanonicalDecodeItemDatumV1 = Data.Static<
  typeof VerifiedCanonicalDecodeItemDatumV1Schema
>;
export const VerifiedCanonicalDecodeItemDatumV1 =
  VerifiedCanonicalDecodeItemDatumV1Schema as unknown as VerifiedCanonicalDecodeItemDatumV1;

export const ValidationOneStepEvidenceV1Schema = Data.Object({
  transition: ValidationOneStepWitnessV1Schema,
  auxiliary: ValidationAuxiliaryWitnessV1Schema,
});
export type ValidationOneStepEvidenceV1 = Data.Static<
  typeof ValidationOneStepEvidenceV1Schema
>;
export const ValidationOneStepEvidenceV1 =
  ValidationOneStepEvidenceV1Schema as unknown as ValidationOneStepEvidenceV1;

const ValidationDescriptorMembershipV1Schema = rootMembershipProofSchema(
  EventKeySchema,
  ValidationTraceDescriptorV1Schema,
);
const ValidationTransitionStepMembershipV1Schema = rootMembershipProofSchema(
  Data.Integer(),
  TransitionStepSchema,
);
const ValidationEventToStepMembershipV1Schema = rootMembershipProofSchema(
  EventKeySchema,
  EventToStepValueSchema,
);
const ForcedValidationSourceMembershipV1Schema = rootMembershipProofSchema(
  OutputReferenceSchema,
  ForcedInclusionTxV1Schema,
);
const NormalValidationSourceMembershipV1Schema = rootMembershipProofSchema(
  H32Schema,
  L2TransactionSourceV1Schema,
);

export const ValidationSourceMembershipV1Schema = Data.Enum([
  Data.Object({
    ForcedValidationSource: Data.Object({
      membership: ForcedValidationSourceMembershipV1Schema,
    }),
  }),
  Data.Object({
    NormalValidationSource: Data.Object({
      membership: NormalValidationSourceMembershipV1Schema,
    }),
  }),
]);

export const ValidationClaimWitnessV1Schema = Data.Object({
  version: Data.Integer(),
  descriptor_membership: ValidationDescriptorMembershipV1Schema,
  transition_step_membership: ValidationTransitionStepMembershipV1Schema,
  event_to_step_membership: ValidationEventToStepMembershipV1Schema,
  source_membership: ValidationSourceMembershipV1Schema,
  validation_context_cbor: Data.Bytes(),
  initial_state: ValidationMachineStateV1Schema,
  terminal_state: ValidationMachineStateV1Schema,
  initial_state_proof: ValidationTraceProofV1Schema,
  terminal_state_proof: ValidationTraceProofV1Schema,
});
export type ValidationClaimWitnessV1 = Data.Static<
  typeof ValidationClaimWitnessV1Schema
>;
export const ValidationClaimWitnessV1 =
  ValidationClaimWitnessV1Schema as unknown as ValidationClaimWitnessV1;

export const PendingValidationClaimV1Schema = Data.Object({
  challenged_header_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  challenged_header: HeaderV1Schema,
  claim: ValidationClaimWitnessV1Schema,
  challenger_descriptor: ValidationTraceDescriptorV1Schema,
  open_time_upper: Data.Integer(),
});
export type PendingValidationClaimV1 = Data.Static<
  typeof PendingValidationClaimV1Schema
>;
export const PendingValidationClaimV1 =
  PendingValidationClaimV1Schema as unknown as PendingValidationClaimV1;

export const PendingValidationClaimDatumV1Schema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PendingValidationClaimV1Schema),
});
export type PendingValidationClaimDatumV1 = Data.Static<
  typeof PendingValidationClaimDatumV1Schema
>;
export const PendingValidationClaimDatumV1 =
  PendingValidationClaimDatumV1Schema as unknown as PendingValidationClaimDatumV1;

const cancelActionSchema = Data.Object({
  Cancel: Data.Object({
    input_index: Data.Integer(),
    computation_thread_mint_redeemer_index: Data.Integer(),
  }),
});

// These action types each have one Aiken constructor. Lucid unwraps a
// one-member Data.Enum, so model the constructor fields as a record; Data.Object
// still emits the required constructor-0 wire shape.
export const ValidationDisputeOpenActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  claim: ValidationClaimWitnessV1Schema,
  challenger_descriptor: ValidationTraceDescriptorV1Schema,
});
export type ValidationDisputeOpenActionV1 = Data.Static<
  typeof ValidationDisputeOpenActionV1Schema
>;
export const ValidationDisputeOpenActionV1 =
  ValidationDisputeOpenActionV1Schema as unknown as ValidationDisputeOpenActionV1;

export const ValidationDisputeOpenSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationDisputeOpenActionV1Schema]),
  }),
]);
export type ValidationDisputeOpenSpendRedeemerV1 = Data.Static<
  typeof ValidationDisputeOpenSpendRedeemerV1Schema
>;
export const ValidationDisputeOpenSpendRedeemerV1 =
  ValidationDisputeOpenSpendRedeemerV1Schema as unknown as ValidationDisputeOpenSpendRedeemerV1;

export const ValidationSourceActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export type ValidationSourceActionV1 = Data.Static<
  typeof ValidationSourceActionV1Schema
>;
export const ValidationSourceActionV1 =
  ValidationSourceActionV1Schema as unknown as ValidationSourceActionV1;

export const ValidationSourceSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationSourceActionV1Schema]) }),
]);
export type ValidationSourceSpendRedeemerV1 = Data.Static<
  typeof ValidationSourceSpendRedeemerV1Schema
>;
export const ValidationSourceSpendRedeemerV1 =
  ValidationSourceSpendRedeemerV1Schema as unknown as ValidationSourceSpendRedeemerV1;

export const ValidationGameActionV1Schema = Data.Enum([
  Data.Object({
    RevealOperator: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      proof: ValidationTraceProofV1Schema,
    }),
  }),
  Data.Object({
    RevealChallenger: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      proof: ValidationTraceProofV1Schema,
    }),
  }),
  Data.Object({
    EnterResolution: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    EnterChallengerTimeout: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export type ValidationGameActionV1 = Data.Static<
  typeof ValidationGameActionV1Schema
>;
export const ValidationGameActionV1 =
  ValidationGameActionV1Schema as unknown as ValidationGameActionV1;

export const ValidationGameSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationGameActionV1Schema]) }),
]);
export type ValidationGameSpendRedeemerV1 = Data.Static<
  typeof ValidationGameSpendRedeemerV1Schema
>;
export const ValidationGameSpendRedeemerV1 =
  ValidationGameSpendRedeemerV1Schema as unknown as ValidationGameSpendRedeemerV1;

export const ValidationBoundaryEvidenceV1Schema = Data.Object({
  pre_state: ValidationMachineStateV1Schema,
  operator_post: ValidationTraceProofV1Schema,
  challenger_post: ValidationTraceProofV1Schema,
});
export type ValidationBoundaryEvidenceV1 = Data.Static<
  typeof ValidationBoundaryEvidenceV1Schema
>;
export const ValidationBoundaryEvidenceV1 =
  ValidationBoundaryEvidenceV1Schema as unknown as ValidationBoundaryEvidenceV1;

export const ValidationBoundaryActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  resolver_index: Data.Integer(),
  evidence: ValidationBoundaryEvidenceV1Schema,
});
export type ValidationBoundaryActionV1 = Data.Static<
  typeof ValidationBoundaryActionV1Schema
>;
export const ValidationBoundaryActionV1 =
  ValidationBoundaryActionV1Schema as unknown as ValidationBoundaryActionV1;

export const ValidationBoundarySpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationBoundaryActionV1Schema]) }),
]);
export type ValidationBoundarySpendRedeemerV1 = Data.Static<
  typeof ValidationBoundarySpendRedeemerV1Schema
>;
export const ValidationBoundarySpendRedeemerV1 =
  ValidationBoundarySpendRedeemerV1Schema as unknown as ValidationBoundarySpendRedeemerV1;

const ValidationPrepareSelectedFieldsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  semantic_resolver_index: Data.Integer(),
  transition: ValidationOneStepWitnessV1Schema,
  auxiliary: ValidationAuxiliaryWitnessV1Schema,
});
const ValidationPrepareSelectedByEvidenceHashFieldsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  semantic_resolver_index: Data.Integer(),
  transition: ValidationOneStepWitnessV1Schema,
  evidence_hash: H32Schema,
});
export const ValidationPrepareSelectedActionV1Schema =
  ValidationPrepareSelectedFieldsV1Schema;
export type ValidationPrepareSelectedActionV1 = Data.Static<
  typeof ValidationPrepareSelectedActionV1Schema
>;
export const ValidationPrepareSelectedActionV1 =
  ValidationPrepareSelectedActionV1Schema as unknown as ValidationPrepareSelectedActionV1;

export const ValidationPrepareSelectedSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationPrepareSelectedActionV1Schema]),
  }),
]);
export type ValidationPrepareSelectedSpendRedeemerV1 = Data.Static<
  typeof ValidationPrepareSelectedSpendRedeemerV1Schema
>;
export const ValidationPrepareSelectedSpendRedeemerV1 =
  ValidationPrepareSelectedSpendRedeemerV1Schema as unknown as ValidationPrepareSelectedSpendRedeemerV1;

export const ValidationCanonicalDecodePrepareSelectedActionV1Schema = Data.Enum(
  [
    Data.Object({
      PrepareSelected: ValidationPrepareSelectedFieldsV1Schema,
    }),
    Data.Object({
      PrepareSelectedByEvidenceHash:
        ValidationPrepareSelectedByEvidenceHashFieldsV1Schema,
    }),
  ],
);
export type ValidationCanonicalDecodePrepareSelectedActionV1 = Data.Static<
  typeof ValidationCanonicalDecodePrepareSelectedActionV1Schema
>;
export const ValidationCanonicalDecodePrepareSelectedActionV1 =
  ValidationCanonicalDecodePrepareSelectedActionV1Schema as unknown as ValidationCanonicalDecodePrepareSelectedActionV1;

export const ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema =
  Data.Enum([
    cancelActionSchema,
    Data.Object({
      Continue: Data.Tuple([
        ValidationCanonicalDecodePrepareSelectedActionV1Schema,
      ]),
    }),
  ]);
export type ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1 =
  Data.Static<
    typeof ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema
  >;
export const ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1 =
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema as unknown as ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1;

export const ValidationDirectResolveActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  challenger_evidence: ValidationOneStepEvidenceV1Schema,
});
export type ValidationDirectResolveActionV1 = Data.Static<
  typeof ValidationDirectResolveActionV1Schema
>;
export const ValidationDirectResolveActionV1 =
  ValidationDirectResolveActionV1Schema as unknown as ValidationDirectResolveActionV1;

export const ValidationDirectResolveSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationDirectResolveActionV1Schema]),
  }),
]);
export type ValidationDirectResolveSpendRedeemerV1 = Data.Static<
  typeof ValidationDirectResolveSpendRedeemerV1Schema
>;
export const ValidationDirectResolveSpendRedeemerV1 =
  ValidationDirectResolveSpendRedeemerV1Schema as unknown as ValidationDirectResolveSpendRedeemerV1;

/**
 * CEK-only complete-material carriage selected by the immutable CEK direct
 * resolver. Keep this schema separate from the generic direct resolver ABI:
 * ValueAndMint continues to use ValidationDirectResolveSpendRedeemerV1.
 */
export const ValidationCekMaterialRouteV1Schema = Data.Enum([
  Data.Literal("NoCekMaterial"),
  Data.Object({
    DirectCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      sidecar_cbor: Data.Bytes(),
    }),
  }),
  Data.Object({
    SinglePublicationCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      reference_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MinimumMultiOutputCekMaterial: Data.Object({
      envelope_cbor: Data.Bytes(),
      reference_input_indices: Data.Array(Data.Integer()),
    }),
  }),
  Data.Object({
    IncrementalCekMaterial: Data.Object({
      program_envelope_hash: H32Schema,
    }),
  }),
]);
export type ValidationCekMaterialRouteV1 = Data.Static<
  typeof ValidationCekMaterialRouteV1Schema
>;
export const ValidationCekMaterialRouteV1 =
  ValidationCekMaterialRouteV1Schema as unknown as ValidationCekMaterialRouteV1;

export const ValidationCekResolveActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  challenger_evidence: ValidationOneStepEvidenceV1Schema,
  material_route: ValidationCekMaterialRouteV1Schema,
});
export type ValidationCekResolveActionV1 = Data.Static<
  typeof ValidationCekResolveActionV1Schema
>;
export const ValidationCekResolveActionV1 =
  ValidationCekResolveActionV1Schema as unknown as ValidationCekResolveActionV1;

export const ValidationCekSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationCekResolveActionV1Schema]),
  }),
]);
export type ValidationCekSpendRedeemerV1 = Data.Static<
  typeof ValidationCekSpendRedeemerV1Schema
>;
export const ValidationCekSpendRedeemerV1 =
  ValidationCekSpendRedeemerV1Schema as unknown as ValidationCekSpendRedeemerV1;

export const ValidationAwardArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValidationAwardArgsV1 = Data.Static<
  typeof ValidationAwardArgsV1Schema
>;
export const ValidationAwardArgsV1 =
  ValidationAwardArgsV1Schema as unknown as ValidationAwardArgsV1;

export const ValidationAwardSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationAwardArgsV1Schema]),
  }),
]);
export type ValidationAwardSpendRedeemerV1 = Data.Static<
  typeof ValidationAwardSpendRedeemerV1Schema
>;
export const ValidationAwardSpendRedeemerV1 =
  ValidationAwardSpendRedeemerV1Schema as unknown as ValidationAwardSpendRedeemerV1;

export const ValidationTimeoutActionV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValidationTimeoutActionV1 = Data.Static<
  typeof ValidationTimeoutActionV1Schema
>;
export const ValidationTimeoutActionV1 =
  ValidationTimeoutActionV1Schema as unknown as ValidationTimeoutActionV1;

export const ValidationTimeoutSpendRedeemerV1Schema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationTimeoutActionV1Schema]) }),
]);
export type ValidationTimeoutSpendRedeemerV1 = Data.Static<
  typeof ValidationTimeoutSpendRedeemerV1Schema
>;
export const ValidationTimeoutSpendRedeemerV1 =
  ValidationTimeoutSpendRedeemerV1Schema as unknown as ValidationTimeoutSpendRedeemerV1;

const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

const safeNumber = (value: bigint, field: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return number;
};

const verdictData = (
  verdict: MidgardValidationTraceDescriptorV1["verdict"],
): "Accepted" | "Rejected" =>
  verdict === "accepted" ? "Accepted" : "Rejected";

const verdictCore = (
  verdict: Data.Static<typeof ValidationTraceDescriptorV1Schema>["verdict"],
): MidgardValidationTraceDescriptorV1["verdict"] => {
  if (verdict === "Accepted") {
    return "accepted";
  }
  if (verdict === "Rejected") {
    return "rejected";
  }
  throw new Error("descriptor.verdict must be Accepted or Rejected");
};

export const validationTraceDescriptorDataFromCore = (
  descriptor: MidgardValidationTraceDescriptorV1,
): Data.Static<typeof ValidationTraceDescriptorV1Schema> => ({
  schema_version: BigInt(descriptor.schemaVersion),
  machine_version: BigInt(descriptor.machineVersion),
  trace_root: bytesHex(descriptor.traceRoot),
  step_count: BigInt(descriptor.stepCount),
  initial_state_hash: bytesHex(descriptor.initialStateHash),
  terminal_state_hash: bytesHex(descriptor.terminalStateHash),
  verdict: verdictData(descriptor.verdict),
  rejection_code_hash: bytesHex(descriptor.rejectionCodeHash),
});

export const validationTraceDescriptorCoreFromData = (
  descriptor: Data.Static<typeof ValidationTraceDescriptorV1Schema>,
): MidgardValidationTraceDescriptorV1 => ({
  schemaVersion: safeNumber(
    descriptor.schema_version,
    "descriptor.schema_version",
  ) as MidgardValidationTraceDescriptorV1["schemaVersion"],
  machineVersion: safeNumber(
    descriptor.machine_version,
    "descriptor.machine_version",
  ) as MidgardValidationTraceDescriptorV1["machineVersion"],
  traceRoot: Buffer.from(descriptor.trace_root, "hex"),
  stepCount: safeNumber(descriptor.step_count, "descriptor.step_count"),
  initialStateHash: Buffer.from(descriptor.initial_state_hash, "hex"),
  terminalStateHash: Buffer.from(descriptor.terminal_state_hash, "hex"),
  verdict: verdictCore(descriptor.verdict),
  rejectionCodeHash: Buffer.from(descriptor.rejection_code_hash, "hex"),
});

export const validationTraceProofDataFromCore = (
  proof: MidgardValidationTraceProofV1,
): ValidationTraceProofV1 => ({
  state_index: BigInt(proof.stateIndex),
  state_hash: bytesHex(proof.stateHash),
  siblings: proof.siblings.map(bytesHex),
});

export const validationTraceProofCoreFromData = (
  proof: ValidationTraceProofV1,
): MidgardValidationTraceProofV1 => ({
  stateIndex: safeNumber(proof.state_index, "proof.state_index"),
  stateHash: Buffer.from(proof.state_hash, "hex"),
  siblings: proof.siblings.map((sibling) => Buffer.from(sibling, "hex")),
});

export const validationMachineStateDataFromCore = (
  state: MidgardValidationMachineStateV1,
): ValidationMachineStateV1 => ({
  machine_version: BigInt(state.machineVersion),
  event_key_hash: bytesHex(state.eventKeyHash),
  transaction_id: bytesHex(state.transactionId),
  transaction_commitment: bytesHex(state.transactionCommitment),
  validation_context_hash: bytesHex(state.validationContextHash),
  source_kind: state.sourceKind === "normal" ? "Normal" : "Forced",
  prior_ledger_root: bytesHex(state.priorLedgerRoot),
  phase:
    state.phase === "canonicalDecode"
      ? "CanonicalDecode"
      : state.phase === "compactBinding"
        ? "CompactBinding"
        : state.phase === "staticLedgerRules"
          ? "StaticLedgerRules"
          : state.phase === "inputSets"
            ? "InputSets"
            : state.phase === "signatures"
              ? "Signatures"
              : state.phase === "phaseANativeScripts"
                ? "PhaseANativeScripts"
                : state.phase === "phaseAScriptPreconditions"
                  ? "PhaseAScriptPreconditions"
                  : state.phase === "resolveInputs"
                    ? "ResolveInputs"
                    : state.phase === "scriptSources"
                      ? "ScriptSources"
                      : state.phase === "nativeScripts"
                        ? "NativeScripts"
                        : state.phase === "scriptIntegrity"
                          ? "ScriptIntegrity"
                          : state.phase === "cek"
                            ? "Cek"
                            : state.phase === "valueAndMint"
                              ? "ValueAndMint"
                              : state.phase === "ledgerDelta"
                                ? "LedgerDelta"
                                : "Terminal",
  program_counter: BigInt(state.programCounter),
  work_root: bytesHex(state.workRoot),
  execution_cpu: state.executionCpu,
  execution_memory: state.executionMemory,
  verdict:
    state.verdict === "pending"
      ? "Pending"
      : state.verdict === "accepted"
        ? "Accepted"
        : "Rejected",
  rejection_code_hash: bytesHex(state.rejectionCodeHash),
  ledger_delta_root: bytesHex(state.ledgerDeltaRoot),
});

export const validationDisputeDataFromCore = (
  dispute: MidgardValidationDisputeV1,
): ValidationDisputeV1 => ({
  version: BigInt(dispute.version),
  operator_descriptor: validationTraceDescriptorDataFromCore(
    dispute.operatorDescriptor,
  ),
  challenger_descriptor: validationTraceDescriptorDataFromCore(
    dispute.challengerDescriptor,
  ),
  low_index: BigInt(dispute.lowIndex),
  high_index: BigInt(dispute.highIndex),
  agreed_low_hash: bytesHex(dispute.agreedLowHash),
  operator_high_hash: bytesHex(dispute.operatorHighHash),
  challenger_high_hash: bytesHex(dispute.challengerHighHash),
  round: BigInt(dispute.round),
  response_deadline: BigInt(dispute.responseDeadline),
  turn:
    dispute.turn.type === "awaitingOperator"
      ? {
          AwaitingOperator: { midpoint: BigInt(dispute.turn.midpoint) },
        }
      : dispute.turn.type === "awaitingChallenger"
        ? {
            AwaitingChallenger: {
              midpoint: BigInt(dispute.turn.midpoint),
              operator_midpoint_hash: bytesHex(
                dispute.turn.operatorMidpointHash,
              ),
            },
          }
        : "ReadyForOneStep",
});

export const validationDisputeCoreFromData = (
  dispute: ValidationDisputeV1,
): MidgardValidationDisputeV1 => ({
  version: safeNumber(
    dispute.version,
    "dispute.version",
  ) as MidgardValidationDisputeV1["version"],
  operatorDescriptor: validationTraceDescriptorCoreFromData(
    dispute.operator_descriptor,
  ),
  challengerDescriptor: validationTraceDescriptorCoreFromData(
    dispute.challenger_descriptor,
  ),
  lowIndex: safeNumber(dispute.low_index, "dispute.low_index"),
  highIndex: safeNumber(dispute.high_index, "dispute.high_index"),
  agreedLowHash: Buffer.from(dispute.agreed_low_hash, "hex"),
  operatorHighHash: Buffer.from(dispute.operator_high_hash, "hex"),
  challengerHighHash: Buffer.from(dispute.challenger_high_hash, "hex"),
  round: safeNumber(dispute.round, "dispute.round"),
  responseDeadline: safeNumber(
    dispute.response_deadline,
    "dispute.response_deadline",
  ),
  turn:
    dispute.turn === "ReadyForOneStep"
      ? { type: "readyForOneStep" }
      : "AwaitingOperator" in dispute.turn
        ? {
            type: "awaitingOperator",
            midpoint: safeNumber(
              dispute.turn.AwaitingOperator.midpoint,
              "dispute.turn.midpoint",
            ),
          }
        : {
            type: "awaitingChallenger",
            midpoint: safeNumber(
              dispute.turn.AwaitingChallenger.midpoint,
              "dispute.turn.midpoint",
            ),
            operatorMidpointHash: Buffer.from(
              dispute.turn.AwaitingChallenger.operator_midpoint_hash,
              "hex",
            ),
          },
});
