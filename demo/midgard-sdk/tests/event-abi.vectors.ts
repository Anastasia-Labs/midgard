import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  OperatorRemovalSchedulerSync,
  SlashingArguments,
  SlashingReason,
} from "../src/active-operators.js";
import { LinkedListDatum } from "../src/linked-list.js";
import {
  PayoutDatum,
  PayoutMintRedeemer,
  PayoutSpendRedeemer,
} from "../src/payout.js";
import {
  DuplicateOperatorStatus,
  RegisteredOperatorDatum,
  RegisteredOperatorMintRedeemer,
} from "../src/registered-operators.js";
import { ReserveSpendRedeemer } from "../src/reserve.js";
import {
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
} from "../src/retired-operators.js";
import {
  EventType,
  ResolutionClaim,
  SettlementDatum,
  SettlementMintRedeemer,
  SettlementSpendRedeemer,
} from "../src/settlement.js";
import { EventSettlementMembershipProof } from "../src/transition-trace.js";
import {
  DepositDatum,
  DepositSpendRedeemer,
} from "../src/user-events/deposit.js";
import {
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
} from "../src/user-events/internals.js";
import {
  WithdrawalOrderDatum,
  WithdrawalSpendPurpose,
  WithdrawalSpendRedeemer,
} from "../src/user-events/withdrawal.js";
import {
  ACTIVE_OPERATOR_DATUM,
  activeOperatorData,
  ADDRESS,
  DEPOSIT_DATUM,
  EMPTY_VALUE,
  emptyOperatorRootData,
  H28_A,
  H32_A,
  H32_B,
  OUTPUT_REFERENCE,
  RAW_DEPOSIT_PROOF,
  RAW_WITHDRAWAL_PROOF,
  REGISTERED_OPERATOR_DATUM,
  registeredOperatorData,
  RETIRED_OPERATOR_DATUM,
  retiredOperatorData,
  SLASHING_ARGUMENTS,
  type Vector,
  WITHDRAWAL_DATUM,
} from "./event-abi.withdrawal-datum.js";

export const vectors: readonly Vector[] = [
  {
    label: "user event mint authenticate tag0/4",
    value: {
      AuthenticateEvent: {
        nonce_input_index: 1n,
        event_output_index: 2n,
        hub_ref_input_index: 3n,
        witness_registration_redeemer_index: 4n,
      },
    },
    schema: UserEventMintRedeemer,
  },
  {
    label: "user event mint burn tag1/2",
    value: {
      BurnEventNFT: {
        nonce_asset_name: "aa",
        witness_unregistration_redeemer_index: 1n,
      },
    },
    schema: UserEventMintRedeemer,
  },
  {
    label: "user event witness mint or burn tag0/1",
    value: {
      MintOrBurn: {
        targetPolicy: "aa",
      },
    },
    schema: UserEventWitnessPublishRedeemer,
  },
  {
    label: "user event witness register tag1/1",
    value: {
      RegisterToProveNotRegistered: {
        registrationCertificateIndex: 1n,
      },
    },
    schema: UserEventWitnessPublishRedeemer,
  },
  {
    label: "user event witness unregister tag2/1",
    value: {
      UnregisterToProveNotRegistered: {
        registrationCertificateIndex: 1n,
      },
    },
    schema: UserEventWitnessPublishRedeemer,
  },
  {
    label: "deposit datum record/3",
    value: DEPOSIT_DATUM,
    schema: DepositDatum,
  },
  {
    label: "deposit spend record/7",
    value: {
      input_index: 1n,
      output_index: 2n,
      hub_ref_input_index: 3n,
      settlement_ref_input_index: 4n,
      mint_redeemer_index: 5n,
      membership_proof: RAW_DEPOSIT_PROOF,
      inclusion_proof_script_withdraw_redeemer_index: 6n,
    },
    schema: DepositSpendRedeemer,
  },
  {
    label: "withdrawal datum record/5",
    value: WITHDRAWAL_DATUM,
    schema: WithdrawalOrderDatum,
  },
  {
    label: "withdrawal purpose initialize tag0/0",
    value: "InitializePayout",
    schema: WithdrawalSpendPurpose,
  },
  {
    label: "withdrawal purpose refund tag1/1",
    value: {
      Refund: {
        validity_override: "IncorrectWithdrawalSignature",
      },
    },
    schema: WithdrawalSpendPurpose,
  },
  {
    label: "withdrawal spend record/9",
    value: {
      input_index: 1n,
      output_index: 2n,
      hub_ref_input_index: 3n,
      settlement_ref_input_index: 4n,
      burn_redeemer_index: 5n,
      payout_mint_redeemer_index: 6n,
      membership_proof: RAW_WITHDRAWAL_PROOF,
      inclusion_proof_script_withdraw_redeemer_index: 7n,
      purpose: "InitializePayout",
    },
    schema: WithdrawalSpendRedeemer,
  },
  {
    label: "reserve spend sole constructor/4",
    value: {
      reserve_input_index: 1n,
      payout_input_index: 2n,
      payout_spend_redeemer_index: 3n,
      hub_ref_input_index: 4n,
    },
    schema: ReserveSpendRedeemer,
  },
  {
    label: "slashing reason state tag0/1",
    value: {
      SlashOperatorForBadState: {
        state_queue_redeemer_index: 1n,
      },
    },
    schema: SlashingReason,
  },
  {
    label: "slashing reason settlement tag1/2",
    value: {
      SlashOperatorForBadSettlement: {
        settlement_input_index: 1n,
        settlement_redeemer_index: 2n,
      },
    },
    schema: SlashingReason,
  },
  {
    label: "slashing arguments record/5",
    value: SLASHING_ARGUMENTS,
    schema: SlashingArguments,
  },
  {
    label: "operator scheduler sync inactive tag0/1",
    value: {
      ShowOperatorIsInactive: {
        scheduler_ref_input_index: 1n,
      },
    },
    schema: OperatorRemovalSchedulerSync,
  },
  {
    label: "operator scheduler sync advancing tag1/4",
    value: {
      ShowSchedulerIsAdvancing: {
        scheduler_input_index: 1n,
        scheduler_redeemer_index: 2n,
        removing_operators_anchor_element_key: "aa",
        removing_operator_is_the_last_member: true,
      },
    },
    schema: OperatorRemovalSchedulerSync,
  },
  {
    label: "active operator spend list transition tag0/0",
    value: "ListStateTransition",
    schema: ActiveOperatorSpendRedeemer,
  },
  {
    label: "active operator spend update state tag1/5",
    value: {
      UpdateBondHoldNewState: {
        active_operator: H28_A,
        active_node_input_index: 1n,
        active_node_output_index: 2n,
        hub_oracle_ref_input_index: 3n,
        state_queue_redeemer_index: 4n,
      },
    },
    schema: ActiveOperatorSpendRedeemer,
  },
  {
    label: "active operator spend update settlement tag2/7",
    value: {
      UpdateBondHoldNewSettlement: {
        active_operator: H28_A,
        active_node_input_index: 1n,
        active_node_output_index: 2n,
        hub_oracle_ref_input_index: 3n,
        settlement_input_index: 4n,
        settlement_redeemer_index: 5n,
        resolution_time: 6n,
      },
    },
    schema: ActiveOperatorSpendRedeemer,
  },
  {
    label: "active operator spend strike tag3/7",
    value: {
      StrikeForInactivity: {
        active_node_input_index: 1n,
        active_node_output_index: 2n,
        operator: H28_A,
        active_node_link: "aa",
        scheduler_input_index: 3n,
        scheduler_redeemer_index: 4n,
        hub_oracle_ref_input_index: 5n,
      },
    },
    schema: ActiveOperatorSpendRedeemer,
  },
  {
    label: "active operator mint init tag0/1",
    value: { Init: { output_index: 1n } },
    schema: ActiveOperatorMintRedeemer,
  },
  {
    label: "active operator mint deinit tag1/0",
    value: "Deinit",
    schema: ActiveOperatorMintRedeemer,
  },
  {
    label: "active operator mint activate tag2/5",
    value: {
      ActivateOperator: {
        new_active_operator_key: H28_A,
        active_operator_anchor_element_output_index: 1n,
        active_operator_inserted_node_output_index: 2n,
        registered_operators_redeemer_index: 3n,
        active_operators_set_was_empty: false,
      },
    },
    schema: ActiveOperatorMintRedeemer,
  },
  {
    label: "active operator mint retire tag3/7",
    value: {
      RetireOperator: {
        active_operator_key: H28_A,
        hub_oracle_ref_input_index: 1n,
        active_operator_anchor_element_input_outref: OUTPUT_REFERENCE,
        active_operator_anchor_element_output_index: 2n,
        retired_operators_redeemer_index: 3n,
        penalize_for_inactivity: true,
        operator_removal_scheduler_sync: {
          ShowSchedulerIsAdvancing: {
            scheduler_input_index: 4n,
            scheduler_redeemer_index: 5n,
            removing_operators_anchor_element_key: null,
            removing_operator_is_the_last_member: false,
          },
        },
      },
    },
    schema: ActiveOperatorMintRedeemer,
  },
  {
    label: "active operator mint slash tag4/2",
    value: {
      SlashOperator: {
        slashing_arguments: SLASHING_ARGUMENTS,
        operator_removal_scheduler_sync: {
          ShowOperatorIsInactive: {
            scheduler_ref_input_index: 4n,
          },
        },
      },
    },
    schema: ActiveOperatorMintRedeemer,
  },
  {
    label: "active operator payload record/2",
    value: ACTIVE_OPERATOR_DATUM,
    schema: ActiveOperatorDatum,
  },
  {
    label: "operator persisted root envelope",
    value: {
      data: { Root: { data: emptyOperatorRootData } },
      link: null,
    },
    schema: LinkedListDatum,
  },
  {
    label: "active operator persisted node envelope",
    value: {
      data: { Node: { data: activeOperatorData } },
      link: "aa",
    },
    schema: LinkedListDatum,
  },
  {
    label: "registered operator payload record/1",
    value: REGISTERED_OPERATOR_DATUM,
    schema: RegisteredOperatorDatum,
  },
  {
    label: "registered duplicate status registered tag0/0",
    value: "DuplicateIsRegistered",
    schema: DuplicateOperatorStatus,
  },
  {
    label: "registered duplicate status active tag1/1",
    value: {
      DuplicateIsActive: {
        hub_oracle_ref_input_index: 1n,
      },
    },
    schema: DuplicateOperatorStatus,
  },
  {
    label: "registered duplicate status retired tag2/0",
    value: "DuplicateIsRetired",
    schema: DuplicateOperatorStatus,
  },
  {
    label: "registered operator mint init tag0/1",
    value: { Init: { output_index: 1n } },
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator mint deinit tag1/0",
    value: "Deinit",
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator mint register tag2/6",
    value: {
      RegisterOperator: {
        registering_operator: H28_A,
        root_output_index: 1n,
        registered_node_output_index: 2n,
        hub_oracle_ref_input_index: 3n,
        active_operators_element_ref_input_index: 4n,
        retired_operators_element_ref_input_index: 5n,
      },
    },
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator mint activate tag3/6",
    value: {
      ActivateOperator: {
        activating_operator: H28_A,
        anchor_element_input_outref: OUTPUT_REFERENCE,
        anchor_element_output_index: 1n,
        hub_oracle_ref_input_index: 2n,
        retired_operators_element_ref_input_index: 3n,
        active_operators_redeemer_index: 4n,
      },
    },
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator mint deregister tag4/3",
    value: {
      DeregisterOperator: {
        deregistering_operator: H28_A,
        anchor_element_input_outref: OUTPUT_REFERENCE,
        anchor_element_output_index: 1n,
      },
    },
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator mint slash duplicate tag5/5",
    value: {
      SlashDuplicateOperator: {
        duplicate_operator: H28_A,
        anchor_element_input_outref: OUTPUT_REFERENCE,
        anchor_element_output_index: 1n,
        duplicate_node_ref_input_index: 2n,
        duplicate_operator_status: {
          DuplicateIsActive: {
            hub_oracle_ref_input_index: 3n,
          },
        },
      },
    },
    schema: RegisteredOperatorMintRedeemer,
  },
  {
    label: "registered operator persisted node envelope",
    value: {
      data: { Node: { data: registeredOperatorData } },
      link: null,
    },
    schema: LinkedListDatum,
  },
  {
    label: "retired operator payload record/1",
    value: RETIRED_OPERATOR_DATUM,
    schema: RetiredOperatorDatum,
  },
  {
    label: "retired operator mint init tag0/1",
    value: { Init: { output_index: 1n } },
    schema: RetiredOperatorMintRedeemer,
  },
  {
    label: "retired operator mint deinit tag1/0",
    value: "Deinit",
    schema: RetiredOperatorMintRedeemer,
  },
  {
    label: "retired operator mint retire tag2/6",
    value: {
      RetireOperator: {
        new_retired_operator_key: H28_A,
        bond_unlock_time: null,
        hub_oracle_ref_input_index: 1n,
        retired_operator_anchor_element_output_index: 2n,
        retired_operator_inserted_node_output_index: 3n,
        active_operators_redeemer_index: 4n,
      },
    },
    schema: RetiredOperatorMintRedeemer,
  },
  {
    label: "retired operator mint recover tag3/3",
    value: {
      RecoverOperatorBond: {
        retired_operator_key: H28_A,
        retired_operator_anchor_element_input_outref: OUTPUT_REFERENCE,
        retired_operator_anchor_element_output_index: 1n,
      },
    },
    schema: RetiredOperatorMintRedeemer,
  },
  {
    label: "retired operator mint slash tag4/1",
    value: {
      SlashOperator: {
        slashing_arguments: SLASHING_ARGUMENTS,
      },
    },
    schema: RetiredOperatorMintRedeemer,
  },
  {
    label: "retired operator persisted node envelope",
    value: {
      data: { Node: { data: retiredOperatorData } },
      link: null,
    },
    schema: LinkedListDatum,
  },
  {
    label: "payout datum record/3",
    value: {
      l2_value: EMPTY_VALUE,
      l1_address: ADDRESS,
      l1_datum: "NoDatum",
    },
    schema: PayoutDatum,
  },
  {
    label: "payout spend add funds tag0/7",
    value: {
      AddFunds: {
        payout_input_index: 1n,
        payout_output_index: 2n,
        reserve_input_index: 3n,
        reserve_change_output_index: 4n,
        reserve_spend_redeemer_index: 5n,
        payout_spend_redeemer_index: 6n,
        hub_ref_input_index: 7n,
      },
    },
    schema: PayoutSpendRedeemer,
  },
  {
    label: "payout spend conclude tag1/4",
    value: {
      ConcludeWithdrawal: {
        payout_input_index: 1n,
        l1_output_index: 2n,
        burn_redeemer_index: 3n,
        hub_ref_input_index: 4n,
      },
    },
    schema: PayoutSpendRedeemer,
  },
  {
    label: "payout mint payout tag0/4",
    value: {
      MintPayout: {
        withdrawal_utxo_out_ref: OUTPUT_REFERENCE,
        withdrawal_input_index: 1n,
        retirement_withdraw_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
    },
    schema: PayoutMintRedeemer,
  },
  {
    label: "payout mint burn tag1/4",
    value: {
      BurnPayout: {
        payout_input_index: 1n,
        payout_asset_name: "aa",
        payout_spend_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
    },
    schema: PayoutMintRedeemer,
  },
  {
    label: "settlement resolution claim record/2",
    value: {
      resolution_time: 1n,
      operator: H28_A,
    },
    schema: ResolutionClaim,
  },
  {
    label: "settlement datum record/5 none",
    value: {
      deposits_root: H32_A,
      withdrawals_root: H32_B,
      forced_transactions_root: H32_A,
      transactions_root: H32_B,
      resolution_claim: null,
    },
    schema: SettlementDatum,
  },
  {
    label: "settlement datum record/5 some",
    value: {
      deposits_root: H32_A,
      withdrawals_root: H32_B,
      forced_transactions_root: H32_A,
      transactions_root: H32_B,
      resolution_claim: {
        resolution_time: 1n,
        operator: H28_A,
      },
    },
    schema: SettlementDatum,
  },
  {
    label: "settlement event deposit tag0/0",
    value: "Deposit",
    schema: EventType,
  },
  {
    label: "settlement event withdrawal tag1/1",
    value: {
      Withdrawal: {
        validity_override: "IncorrectWithdrawalValue",
      },
    },
    schema: EventType,
  },
  {
    label: "settlement event tx order tag2/1",
    value: {
      TxOrder: {
        validity_override: "ForcedTxValid",
      },
    },
    schema: EventType,
  },
  {
    label: "settlement event tx order tag2/1 rejected",
    value: {
      TxOrder: {
        validity_override: {
          ForcedTxInvalid: { reason: "EmptyInputs" },
        },
      },
    },
    schema: EventType,
  },
  {
    label: "settlement membership deposit tag0/1",
    value: {
      DepositMembership: {
        witness: RAW_DEPOSIT_PROOF,
      },
    },
    schema: EventSettlementMembershipProof,
  },
  {
    label: "settlement membership withdrawal tag1/1",
    value: {
      WithdrawalMembership: {
        witness: RAW_WITHDRAWAL_PROOF,
      },
    },
    schema: EventSettlementMembershipProof,
  },
  {
    label: "settlement membership tx order tag2/1",
    value: {
      TxOrderMembership: {
        witness: {
          ...RAW_DEPOSIT_PROOF,
          domain: "TransactionsV1RootDomain",
        },
      },
    },
    schema: EventSettlementMembershipProof,
  },
  {
    label: "settlement spend attach tag0/7",
    value: {
      AttachResolutionClaim: {
        settlement_input_index: 1n,
        settlement_output_index: 2n,
        hub_ref_input_index: 3n,
        active_operators_node_input_index: 4n,
        active_operators_redeemer_index: 5n,
        operator: H28_A,
        scheduler_ref_input_index: 6n,
      },
    },
    schema: SettlementSpendRedeemer,
  },
  {
    label: "settlement spend disprove tag1/11",
    value: {
      DisproveResolutionClaim: {
        settlement_input_index: 1n,
        settlement_output_index: 2n,
        hub_ref_input_index: 3n,
        operators_redeemer_index: 4n,
        operator: H28_A,
        operator_is_active: true,
        unresolved_event_ref_input_index: 5n,
        unresolved_event_asset_name: "aa",
        event_type: "Deposit",
        membership_proof: {
          DepositMembership: {
            witness: RAW_DEPOSIT_PROOF,
          },
        },
        inclusion_proof_script_withdraw_redeemer_index: 6n,
      },
    },
    schema: SettlementSpendRedeemer,
  },
  {
    label: "settlement spend resolve tag2/1",
    value: {
      Resolve: {
        settlement_id: "aa",
      },
    },
    schema: SettlementSpendRedeemer,
  },
  {
    label: "settlement mint spawn tag0/4",
    value: {
      Spawn: {
        settlement_id: "aa",
        output_index: 1n,
        state_queue_merge_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
    },
    schema: SettlementMintRedeemer,
  },
  {
    label: "settlement mint remove tag1/3",
    value: {
      Remove: {
        settlement_id: "aa",
        input_index: 1n,
        spend_redeemer_index: 2n,
      },
    },
    schema: SettlementMintRedeemer,
  },
];
