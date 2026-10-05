import { asDataType } from "@al-ft/midgard-core/lucid-data";
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
import {
  AuthenticatedCanonicalDecodeItemSchema,
  CanonicalDecodeItemObservationSchema,
  CanonicalDecodeItemProofSchema,
  PreparedCanonicalDecodeItemSchema,
  ValidationMachineStateSchema,
  ValidationOneStepWitnessSchema,
  ValidationTraceProofSchema,
} from "./validation-dispute.validation-machine-phase-schema.js";

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

export const cancelActionSchema = Data.Object({
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

export const CommittedValidationStepEvidenceSchema = Data.Object({
  pre_state: ValidationMachineStateSchema,
  pre_proof: ValidationTraceProofSchema,
  post_proof: ValidationTraceProofSchema,
  challenger_successor_hash: H32Schema,
});

export type CommittedValidationStepEvidence = Data.Static<
  typeof CommittedValidationStepEvidenceSchema
>;

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
  Data.Object({
    DirectCommittedStep: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      resolver_index: Data.Integer(),
      evidence: CommittedValidationStepEvidenceSchema,
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

export const ValidationBoundaryActionSchema = Data.Enum([
  Data.Object({
    PrepareResolution: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      resolver_index: Data.Integer(),
      evidence: ValidationBoundaryEvidenceSchema,
    }),
  }),
  Data.Object({
    AwardTerminalPadding: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      terminal_state: ValidationMachineStateSchema,
    }),
  }),
]);

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
