import {
  Address,
  Data,
  fromText,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { GenericErrorFields, LucidError, POSIXTimeSchema } from "./common.js";
import {
  authenticateUTxOs,
  AuthenticUTxO,
  fetchSingleAuthenticUTxOProgram,
} from "./internals.js";

export const SCHEDULER_ASSET_NAME = fromText("MIDGARD_SCHEDULER");

export const SchedulerDatumSchema = Data.Enum([
  Data.Literal("NoActiveOperators"),
  Data.Object({
    ActiveOperator: Data.Object({
      operator: Data.Bytes(),
      start_time: POSIXTimeSchema,
    }),
  }),
]);
export type SchedulerDatum = Data.Static<typeof SchedulerDatumSchema>;
export const SchedulerDatum = SchedulerDatumSchema as unknown as SchedulerDatum;

export const SchedulerMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
]);

export type SchedulerMintRedeemer = Data.Static<
  typeof SchedulerMintRedeemerSchema
>;
export const SchedulerMintRedeemer =
  SchedulerMintRedeemerSchema as unknown as SchedulerMintRedeemer;

export const NeglectedUserEventSchema = Data.Enum([
  Data.Literal("NoNeglectedUserEvent"),
  Data.Object({
    NeglectedDeposit: Data.Object({
      deposit_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    NeglectedWithdrawal: Data.Object({
      withdrawal_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    NeglectedTxOrder: Data.Object({
      tx_order_ref_input_index: Data.Integer(),
    }),
  }),
]);

export const OperatorRemovalReasonSchema = Data.Enum([
  Data.Literal("OperatorRetirement"),
  Data.Literal("OperatorSlashing"),
]);

export const AdvancingApproachSchema = Data.Enum([
  Data.Object({
    GoToNextDueToEndOfShift: Data.Object({
      new_shifts_operator_node_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RewindDueToEndOfShift: Data.Object({
      active_operators_root_ref_input_index: Data.Integer(),
      active_operators_last_node_ref_input_index: Data.Integer(),
      registered_element_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    GoToNextDueToSkippedOperator: Data.Object({
      new_shifts_operator_node_ref_input_index: Data.Integer(),
      skipped_operator_node_input_index: Data.Integer(),
      active_operators_spend_redeemer_index: Data.Integer(),
      state_queue_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      neglected_user_event: NeglectedUserEventSchema,
    }),
  }),
  Data.Object({
    RewindDueToSkippedOperator: Data.Object({
      active_operators_root_ref_input_index: Data.Integer(),
      skipped_operator_node_input_index: Data.Integer(),
      active_operators_spend_redeemer_index: Data.Integer(),
      state_queue_ref_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      m_active_operators_last_node_ref_input_index: Data.Nullable(
        Data.Integer(),
      ),
      registered_element_ref_input_index: Data.Integer(),
      neglected_user_event: NeglectedUserEventSchema,
    }),
  }),
  Data.Object({
    GoToNextDueToOperatorRemoval: Data.Object({
      active_operators_mint_redeemer_index: Data.Integer(),
      removal_reason: OperatorRemovalReasonSchema,
    }),
  }),
  Data.Object({
    RewindDueToOperatorRemoval: Data.Object({
      active_operators_mint_redeemer_index: Data.Integer(),
      m_active_operators_last_node_ref_input_index: Data.Nullable(
        Data.Integer(),
      ),
      removal_reason: OperatorRemovalReasonSchema,
      registered_element_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    AppointFirstOperator: Data.Object({
      new_shifts_operator_node_ref_input_index: Data.Integer(),
      registered_element_ref_input_index: Data.Integer(),
    }),
  }),
]);

export type AdvancingApproach = Data.Static<typeof AdvancingApproachSchema>;
export const AdvancingApproach =
  AdvancingApproachSchema as unknown as AdvancingApproach;

export const SchedulerSpendRedeemerSchema = Data.Object({
  scheduler_input_index: Data.Integer(),
  scheduler_output_index: Data.Integer(),
  advancing_approach: AdvancingApproachSchema,
});

export type SchedulerSpendRedeemer = Data.Static<
  typeof SchedulerSpendRedeemerSchema
>;
export const SchedulerSpendRedeemer =
  SchedulerSpendRedeemerSchema as unknown as SchedulerSpendRedeemer;

export const INITIAL_SCHEDULER_DATUM = "NoActiveOperators" as SchedulerDatum;

export type SchedulerUTxO = AuthenticUTxO<SchedulerDatum>;

export const utxosToSchedulerUTxOs = (
  utxos: UTxO[],
  nftPolicy: PolicyId,
): Effect.Effect<SchedulerUTxO[], LucidError> =>
  authenticateUTxOs<SchedulerDatum>(utxos, nftPolicy, SchedulerDatum);

export type SchedulerConfig = {
  schedulerAddress: Address;
  schedulerPolicyId: PolicyId;
};

export class SchedulerError extends EffectData.TaggedError(
  "SchedulerError",
)<GenericErrorFields> {}

export const fetchSchedulerUTxOProgram = (
  lucid: LucidEvolution,
  config: SchedulerConfig,
): Effect.Effect<SchedulerUTxO, SchedulerError | LucidError> =>
  fetchSingleAuthenticUTxOProgram<SchedulerUTxO, LucidError, SchedulerError>(
    lucid,
    {
      address: config.schedulerAddress,
      policyId: config.schedulerPolicyId,
      utxoLabel: "scheduler",
      conversionFunction: utxosToSchedulerUTxOs,
      onUnexpectedAuthenticUTxOCount: () =>
        new SchedulerError({
          message: "Failed to fetch the scheduler UTxO",
          cause:
            "Exactly one scheduler UTxO was expected, but none or more were found",
        }),
    },
  );
