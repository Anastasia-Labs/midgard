import { Data } from "@lucid-evolution/lucid";
import { MerkleRootSchema, POSIXTimeSchema, ProofSchema, VerificationKeyHashSchema } from "./common.js";

export const ResolutionClaimSchema = Data.Object({
  resolution_time: POSIXTimeSchema,
  operator: VerificationKeyHashSchema
});
export type ResolutionClaim = Data.Static<typeof ResolutionClaimSchema>;
export const ResolutionClaim =
  ResolutionClaimSchema as unknown as ResolutionClaim;

export const SettlementDatumSchema = Data.Object({
  deposits_root: MerkleRootSchema,
  withdrawals_root: MerkleRootSchema,
  transactions_root: MerkleRootSchema,
  resolution_claim: Data.Nullable(ResolutionClaimSchema),
});
export type SettlementDatum = Data.Static<typeof SettlementDatumSchema>;
export const SettlementDatum =
  SettlementDatumSchema as unknown as SettlementDatum;

export const OperatorStatusSchema = Data.Enum([
  Data.Literal("ActiveOperator"),
  Data.Literal("RetiredOperator"),
]);
export type OperatorStatus = Data.Static<typeof OperatorStatusSchema>;
export const OperatorStatus =
  OperatorStatusSchema as unknown as OperatorStatus;

export const EventTypeSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Literal("Withdrawal"),
  Data.Literal("TxOrder"),
]);
export type EventType = Data.Static<typeof EventTypeSchema>;
export const EventType =
  EventTypeSchema as unknown as EventType;

export const SettlementSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AttachResolutionClaim: Data.Object({ 
       settlement_input_index: Data.Integer(),
       settlement_output_index: Data.Integer(),
       hub_ref_input_index: Data.Integer(),
       active_operators_node_input_index: Data.Integer(),
       active_operators_redeemer_index: Data.Integer(),
       operator: VerificationKeyHashSchema,
       scheduler_ref_input_index: Data.Integer(), 
    }),
  }),
  Data.Object({
    DisproveResolutionClaim: Data.Object({
       settlement_input_index: Data.Integer(),
       settlement_output_index: Data.Integer(),
       hub_ref_input_index: Data.Integer(),
       operators_redeemer_index: Data.Integer(),
       operator: VerificationKeyHashSchema,
       operator_status: OperatorStatusSchema,
       unresolved_event_ref_input_index: Data.Integer(),
       unresolved_event_asset_name: Data.Bytes(),
       event_type: EventTypeSchema,
       membership_proof: ProofSchema,
    }),
  }),
  Data.Object({
    Resolve: Data.Object({
       settlement_id: Data.Bytes(),
    }),
  }),
]);
export type SettlementSpendRedeemer = Data.Static<typeof SettlementSpendRedeemerSchema>;
export const SettlementSpendRedeemer =
  SettlementSpendRedeemerSchema as unknown as SettlementSpendRedeemer;

export const SettlementMintRedeemerSchema = Data.Enum([
  Data.Object({
    Spawn: Data.Object({ 
      settlement_id: Data.Bytes(),
      output_index: Data.Integer(),
      state_queue_merge_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(), 
    }),
  }),
  Data.Object({
    Remove: Data.Object({
      settlement_id: Data.Bytes(),
      input_index: Data.Integer(),
      spend_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type SettlementMintRedeemer = Data.Static<typeof SettlementMintRedeemerSchema>;
export const SettlementMintRedeemer =
  SettlementMintRedeemerSchema as unknown as SettlementMintRedeemer;