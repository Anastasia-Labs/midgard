import { asDataType } from "@al-ft/midgard-core/lucid-data";
import type { MidgardValidationDispute } from "@al-ft/midgard-core/validation-dispute";
import type {
  MidgardValidationMachineState,
  MidgardValidationTraceDescriptor,
  MidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

import {
  H32Schema,
  OutputReferenceSchema,
  PubKeyHashSchema,
} from "../common.js";
import {
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  L2TransactionSourceSchema,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { ValidationAuxiliaryWitnessSchema } from "./validation-auxiliary-witness.js";

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
export type PreparedCanonicalDecodeItem = Data.Static<
  typeof PreparedCanonicalDecodeItemSchema
>;
export const PreparedCanonicalDecodeItem =
  asDataType<PreparedCanonicalDecodeItem>(PreparedCanonicalDecodeItemSchema);

export const ObservedCanonicalDecodeItemSchema = Data.Object({
  version: Data.Integer(),
  prepared: PreparedCanonicalDecodeItemSchema,
  observation: CanonicalDecodeItemObservationSchema,
});
export type ObservedCanonicalDecodeItem = Data.Static<
  typeof ObservedCanonicalDecodeItemSchema
>;
export const ObservedCanonicalDecodeItem =
  asDataType<ObservedCanonicalDecodeItem>(ObservedCanonicalDecodeItemSchema);

export const VerifiedCanonicalDecodeItemSchema = Data.Object({
  version: Data.Integer(),
  observed: ObservedCanonicalDecodeItemSchema,
  proof: CanonicalDecodeItemProofSchema,
});
export type VerifiedCanonicalDecodeItem = Data.Static<
  typeof VerifiedCanonicalDecodeItemSchema
>;
export const VerifiedCanonicalDecodeItem =
  asDataType<VerifiedCanonicalDecodeItem>(VerifiedCanonicalDecodeItemSchema);

export const AuthenticatedCanonicalDecodeItemDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(AuthenticatedCanonicalDecodeItemSchema),
});
export type AuthenticatedCanonicalDecodeItemDatum = Data.Static<
  typeof AuthenticatedCanonicalDecodeItemDatumSchema
>;
export const AuthenticatedCanonicalDecodeItemDatum =
  asDataType<AuthenticatedCanonicalDecodeItemDatum>(
    AuthenticatedCanonicalDecodeItemDatumSchema,
  );

export const PreparedCanonicalDecodeItemDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PreparedCanonicalDecodeItemSchema),
});
export type PreparedCanonicalDecodeItemDatum = Data.Static<
  typeof PreparedCanonicalDecodeItemDatumSchema
>;
export const PreparedCanonicalDecodeItemDatum =
  asDataType<PreparedCanonicalDecodeItemDatum>(
    PreparedCanonicalDecodeItemDatumSchema,
  );

export const ObservedCanonicalDecodeItemDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(ObservedCanonicalDecodeItemSchema),
});
export type ObservedCanonicalDecodeItemDatum = Data.Static<
  typeof ObservedCanonicalDecodeItemDatumSchema
>;
export const ObservedCanonicalDecodeItemDatum =
  asDataType<ObservedCanonicalDecodeItemDatum>(
    ObservedCanonicalDecodeItemDatumSchema,
  );

export const VerifiedCanonicalDecodeItemDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(VerifiedCanonicalDecodeItemSchema),
});
export type VerifiedCanonicalDecodeItemDatum = Data.Static<
  typeof VerifiedCanonicalDecodeItemDatumSchema
>;
export const VerifiedCanonicalDecodeItemDatum =
  asDataType<VerifiedCanonicalDecodeItemDatum>(
    VerifiedCanonicalDecodeItemDatumSchema,
  );

export const ValidationOneStepEvidenceSchema = Data.Object({
  transition: ValidationOneStepWitnessSchema,
  auxiliary: ValidationAuxiliaryWitnessSchema,
});
export type ValidationOneStepEvidence = Data.Static<
  typeof ValidationOneStepEvidenceSchema
>;
export const ValidationOneStepEvidence = asDataType<ValidationOneStepEvidence>(
  ValidationOneStepEvidenceSchema,
);

const ValidationDescriptorMembershipSchema = rootMembershipProofSchema(
  EventKeySchema,
  ValidationTraceDescriptorSchema,
);
const ValidationTransitionStepMembershipSchema = rootMembershipProofSchema(
  Data.Integer(),
  TransitionStepSchema,
);
const ValidationEventToStepMembershipSchema = rootMembershipProofSchema(
  EventKeySchema,
  EventToStepValueSchema,
);
const ForcedValidationSourceMembershipSchema = rootMembershipProofSchema(
  OutputReferenceSchema,
  ForcedInclusionTxV1Schema,
);
const NormalValidationSourceMembershipSchema = rootMembershipProofSchema(
  H32Schema,
  L2TransactionSourceSchema,
);

export const ValidationSourceMembershipSchema = Data.Enum([
  Data.Object({
    ForcedValidationSource: Data.Object({
      membership: ForcedValidationSourceMembershipSchema,
    }),
  }),
  Data.Object({
    NormalValidationSource: Data.Object({
      membership: NormalValidationSourceMembershipSchema,
    }),
  }),
]);

export const ValidationClaimWitnessSchema = Data.Object({
  version: Data.Integer(),
  descriptor_membership: ValidationDescriptorMembershipSchema,
  transition_step_membership: ValidationTransitionStepMembershipSchema,
  event_to_step_membership: ValidationEventToStepMembershipSchema,
  source_membership: ValidationSourceMembershipSchema,
  validation_context_cbor: Data.Bytes(),
  initial_state: ValidationMachineStateSchema,
  terminal_state: ValidationMachineStateSchema,
  initial_state_proof: ValidationTraceProofSchema,
  terminal_state_proof: ValidationTraceProofSchema,
});
export type ValidationClaimWitness = Data.Static<
  typeof ValidationClaimWitnessSchema
>;
export const ValidationClaimWitness = asDataType<ValidationClaimWitness>(
  ValidationClaimWitnessSchema,
);

export const PendingValidationClaimSchema = Data.Object({
  challenged_header_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  challenged_header: HeaderSchema,
  claim: ValidationClaimWitnessSchema,
  challenger_descriptor: ValidationTraceDescriptorSchema,
  open_time_upper: Data.Integer(),
});
export type PendingValidationClaim = Data.Static<
  typeof PendingValidationClaimSchema
>;
export const PendingValidationClaim = asDataType<PendingValidationClaim>(
  PendingValidationClaimSchema,
);

export const PendingValidationClaimDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(PendingValidationClaimSchema),
});
export type PendingValidationClaimDatum = Data.Static<
  typeof PendingValidationClaimDatumSchema
>;
export const PendingValidationClaimDatum =
  asDataType<PendingValidationClaimDatum>(PendingValidationClaimDatumSchema);

const cancelActionSchema = Data.Object({
  Cancel: Data.Object({
    input_index: Data.Integer(),
    computation_thread_mint_redeemer_index: Data.Integer(),
  }),
});

// These action types each have one Aiken constructor. Lucid unwraps a
// one-member Data.Enum, so model the constructor fields as a record; Data.Object
// still emits the required constructor-0 wire shape.
export const ValidationDisputeOpenActionSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  claim: ValidationClaimWitnessSchema,
  challenger_descriptor: ValidationTraceDescriptorSchema,
});
export type ValidationDisputeOpenAction = Data.Static<
  typeof ValidationDisputeOpenActionSchema
>;
export const ValidationDisputeOpenAction =
  asDataType<ValidationDisputeOpenAction>(ValidationDisputeOpenActionSchema);

export const ValidationDisputeOpenSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationDisputeOpenActionSchema]),
  }),
]);
export type ValidationDisputeOpenSpendRedeemer = Data.Static<
  typeof ValidationDisputeOpenSpendRedeemerSchema
>;
export const ValidationDisputeOpenSpendRedeemer =
  asDataType<ValidationDisputeOpenSpendRedeemer>(
    ValidationDisputeOpenSpendRedeemerSchema,
  );

export const ValidationSourceActionSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export type ValidationSourceAction = Data.Static<
  typeof ValidationSourceActionSchema
>;
export const ValidationSourceAction = asDataType<ValidationSourceAction>(
  ValidationSourceActionSchema,
);

export const ValidationSourceSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationSourceActionSchema]) }),
]);
export type ValidationSourceSpendRedeemer = Data.Static<
  typeof ValidationSourceSpendRedeemerSchema
>;
export const ValidationSourceSpendRedeemer =
  asDataType<ValidationSourceSpendRedeemer>(
    ValidationSourceSpendRedeemerSchema,
  );

export const ValidationGameActionSchema = Data.Enum([
  Data.Object({
    RevealOperator: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      proof: ValidationTraceProofSchema,
    }),
  }),
  Data.Object({
    RevealChallenger: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      proof: ValidationTraceProofSchema,
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
export type ValidationGameAction = Data.Static<
  typeof ValidationGameActionSchema
>;
export const ValidationGameAction = asDataType<ValidationGameAction>(
  ValidationGameActionSchema,
);

export const ValidationGameSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationGameActionSchema]) }),
]);
export type ValidationGameSpendRedeemer = Data.Static<
  typeof ValidationGameSpendRedeemerSchema
>;
export const ValidationGameSpendRedeemer =
  asDataType<ValidationGameSpendRedeemer>(ValidationGameSpendRedeemerSchema);

export const ValidationBoundaryEvidenceSchema = Data.Object({
  pre_state: ValidationMachineStateSchema,
  operator_post: ValidationTraceProofSchema,
  challenger_post: ValidationTraceProofSchema,
});
export type ValidationBoundaryEvidence = Data.Static<
  typeof ValidationBoundaryEvidenceSchema
>;
export const ValidationBoundaryEvidence =
  asDataType<ValidationBoundaryEvidence>(ValidationBoundaryEvidenceSchema);

export const ValidationBoundaryActionSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  resolver_index: Data.Integer(),
  evidence: ValidationBoundaryEvidenceSchema,
});
export type ValidationBoundaryAction = Data.Static<
  typeof ValidationBoundaryActionSchema
>;
export const ValidationBoundaryAction = asDataType<ValidationBoundaryAction>(
  ValidationBoundaryActionSchema,
);

export const ValidationBoundarySpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationBoundaryActionSchema]) }),
]);
export type ValidationBoundarySpendRedeemer = Data.Static<
  typeof ValidationBoundarySpendRedeemerSchema
>;
export const ValidationBoundarySpendRedeemer =
  asDataType<ValidationBoundarySpendRedeemer>(
    ValidationBoundarySpendRedeemerSchema,
  );

const ValidationPrepareSelectedFieldsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  semantic_resolver_index: Data.Integer(),
  transition: ValidationOneStepWitnessSchema,
  auxiliary: ValidationAuxiliaryWitnessSchema,
});
export const ValidationPrepareSelectedActionSchema =
  ValidationPrepareSelectedFieldsSchema;
export type ValidationPrepareSelectedAction = Data.Static<
  typeof ValidationPrepareSelectedActionSchema
>;
export const ValidationPrepareSelectedAction =
  asDataType<ValidationPrepareSelectedAction>(
    ValidationPrepareSelectedActionSchema,
  );

export const ValidationPrepareSelectedSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationPrepareSelectedActionSchema]),
  }),
]);
export type ValidationPrepareSelectedSpendRedeemer = Data.Static<
  typeof ValidationPrepareSelectedSpendRedeemerSchema
>;
export const ValidationPrepareSelectedSpendRedeemer =
  asDataType<ValidationPrepareSelectedSpendRedeemer>(
    ValidationPrepareSelectedSpendRedeemerSchema,
  );

// Option B (#620): the canonical-decode preparation commits to the transition
// alone — the validator computes `hash_one_step_evidence(transition,
// NoAuxiliaryWitness)` on-chain, so its `PrepareSelected` carries no auxiliary
// and the retired `PrepareSelectedByEvidenceHash` arm is gone. Single Aiken
// constructor: modeled as Data.Object (Lucid unwraps a one-member Data.Enum),
// which still emits the required constructor-0 wire shape.
export const ValidationCanonicalDecodePrepareSelectedActionSchema = Data.Object(
  {
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    semantic_resolver_index: Data.Integer(),
    transition: ValidationOneStepWitnessSchema,
  },
);
export type ValidationCanonicalDecodePrepareSelectedAction = Data.Static<
  typeof ValidationCanonicalDecodePrepareSelectedActionSchema
>;
export const ValidationCanonicalDecodePrepareSelectedAction =
  asDataType<ValidationCanonicalDecodePrepareSelectedAction>(
    ValidationCanonicalDecodePrepareSelectedActionSchema,
  );

export const ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema =
  Data.Enum([
    cancelActionSchema,
    Data.Object({
      Continue: Data.Tuple([
        ValidationCanonicalDecodePrepareSelectedActionSchema,
      ]),
    }),
  ]);
export type ValidationCanonicalDecodePrepareSelectedSpendRedeemer = Data.Static<
  typeof ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema
>;
export const ValidationCanonicalDecodePrepareSelectedSpendRedeemer =
  asDataType<ValidationCanonicalDecodePrepareSelectedSpendRedeemer>(
    ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema,
  );

/**
 * CEK complete-material carriage named by the `material_route` field of the
 * CEK execution-selection semantic action
 * (`cek_execution_selection_semantic_v1.VerifyExecutionSelection`). The route
 * is resolver evidence, never part of the hashed step witness.
 */
export const ValidationCekMaterialRouteSchema = Data.Enum([
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
export type ValidationCekMaterialRoute = Data.Static<
  typeof ValidationCekMaterialRouteSchema
>;
export const ValidationCekMaterialRoute =
  asDataType<ValidationCekMaterialRoute>(ValidationCekMaterialRouteSchema);

export const ValidationAwardArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValidationAwardArgs = Data.Static<typeof ValidationAwardArgsSchema>;
export const ValidationAwardArgs = asDataType<ValidationAwardArgs>(
  ValidationAwardArgsSchema,
);

export const ValidationAwardSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({
    Continue: Data.Tuple([ValidationAwardArgsSchema]),
  }),
]);
export type ValidationAwardSpendRedeemer = Data.Static<
  typeof ValidationAwardSpendRedeemerSchema
>;
export const ValidationAwardSpendRedeemer =
  asDataType<ValidationAwardSpendRedeemer>(ValidationAwardSpendRedeemerSchema);

export const ValidationTimeoutActionSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValidationTimeoutAction = Data.Static<
  typeof ValidationTimeoutActionSchema
>;
export const ValidationTimeoutAction = asDataType<ValidationTimeoutAction>(
  ValidationTimeoutActionSchema,
);

export const ValidationTimeoutSpendRedeemerSchema = Data.Enum([
  cancelActionSchema,
  Data.Object({ Continue: Data.Tuple([ValidationTimeoutActionSchema]) }),
]);
export type ValidationTimeoutSpendRedeemer = Data.Static<
  typeof ValidationTimeoutSpendRedeemerSchema
>;
export const ValidationTimeoutSpendRedeemer =
  asDataType<ValidationTimeoutSpendRedeemer>(
    ValidationTimeoutSpendRedeemerSchema,
  );

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
  verdict: MidgardValidationTraceDescriptor["verdict"],
): "Accepted" | "Rejected" =>
  verdict === "accepted" ? "Accepted" : "Rejected";

const verdictCore = (
  verdict: Data.Static<typeof ValidationTraceDescriptorSchema>["verdict"],
): MidgardValidationTraceDescriptor["verdict"] => {
  if (verdict === "Accepted") {
    return "accepted";
  }
  if (verdict === "Rejected") {
    return "rejected";
  }
  throw new Error("descriptor.verdict must be Accepted or Rejected");
};

export const validationTraceDescriptorDataFromCore = (
  descriptor: MidgardValidationTraceDescriptor,
): Data.Static<typeof ValidationTraceDescriptorSchema> => ({
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
  descriptor: Data.Static<typeof ValidationTraceDescriptorSchema>,
): MidgardValidationTraceDescriptor => ({
  schemaVersion: safeNumber(
    descriptor.schema_version,
    "descriptor.schema_version",
  ) as MidgardValidationTraceDescriptor["schemaVersion"],
  machineVersion: safeNumber(
    descriptor.machine_version,
    "descriptor.machine_version",
  ) as MidgardValidationTraceDescriptor["machineVersion"],
  traceRoot: Buffer.from(descriptor.trace_root, "hex"),
  stepCount: safeNumber(descriptor.step_count, "descriptor.step_count"),
  initialStateHash: Buffer.from(descriptor.initial_state_hash, "hex"),
  terminalStateHash: Buffer.from(descriptor.terminal_state_hash, "hex"),
  verdict: verdictCore(descriptor.verdict),
  rejectionCodeHash: Buffer.from(descriptor.rejection_code_hash, "hex"),
});

export const validationTraceProofDataFromCore = (
  proof: MidgardValidationTraceProof,
): ValidationTraceProof => ({
  state_index: BigInt(proof.stateIndex),
  state_hash: bytesHex(proof.stateHash),
  siblings: proof.siblings.map(bytesHex),
});

export const validationTraceProofCoreFromData = (
  proof: ValidationTraceProof,
): MidgardValidationTraceProof => ({
  stateIndex: safeNumber(proof.state_index, "proof.state_index"),
  stateHash: Buffer.from(proof.state_hash, "hex"),
  siblings: proof.siblings.map((sibling) => Buffer.from(sibling, "hex")),
});

export const validationMachineStateDataFromCore = (
  state: MidgardValidationMachineState,
): ValidationMachineState => ({
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
  dispute: MidgardValidationDispute,
): ValidationDispute => ({
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
  dispute: ValidationDispute,
): MidgardValidationDispute => ({
  version: safeNumber(
    dispute.version,
    "dispute.version",
  ) as MidgardValidationDispute["version"],
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
