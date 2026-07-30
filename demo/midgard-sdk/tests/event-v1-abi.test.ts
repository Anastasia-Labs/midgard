import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  fetchActiveOperatorUTxOs,
  OperatorRemovalSchedulerSync,
  SlashingArguments,
  SlashingReason,
} from "../src/active-operators.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  LinkedListDatum,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
} from "../src/linked-list.js";
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
  fetchRetiredOperatorUTxOs,
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

const H28_A = "11".repeat(28);
const H28_B = "22".repeat(28);
const H32_A = "33".repeat(32);
const H32_B = "44".repeat(32);

const OUTPUT_REFERENCE = {
  transactionId: H32_A,
  outputIndex: 1n,
};

const ADDRESS = {
  paymentCredential: {
    ScriptCredential: [H28_A],
  },
  stakeCredential: null,
};

const EMPTY_VALUE = new Map<string, Map<string, bigint>>();

const RAW_DEPOSIT_PROOF = {
  domain: "DepositsRootDomain",
  root: H32_A,
  phas_root: H32_B,
  count: 1n,
  key: "aa",
  value: "bb",
  proof: [],
};

const RAW_WITHDRAWAL_PROOF = {
  ...RAW_DEPOSIT_PROOF,
  domain: "WithdrawalsRootDomain",
};

const DEPOSIT_DATUM = {
  event: {
    id: OUTPUT_REFERENCE,
    info: {
      l2_address: ADDRESS,
      l2_network_id: 0n,
      l2_datum: null,
    },
  },
  inclusion_time: 2n,
  witness: H28_B,
};

const WITHDRAWAL_DATUM = {
  event: {
    id: OUTPUT_REFERENCE,
    info: {
      body: {
        l2_outref: OUTPUT_REFERENCE,
        l2_owner: H28_A,
        l2_value: EMPTY_VALUE,
        l1_address: ADDRESS,
        l1_datum: "NoDatum",
      },
      signature: ["aa", "bb"],
      validity: "WithdrawalIsValid",
    },
  },
  inclusion_time: 3n,
  witness: H28_B,
  refund_address: ADDRESS,
  refund_datum: "NoDatum",
};

const SLASHING_ARGUMENTS = {
  slashed_operator: H28_A,
  hub_oracle_ref_input_index: 1n,
  slashed_operator_anchor_element_input_outref: OUTPUT_REFERENCE,
  slashed_operator_anchor_element_output_index: 2n,
  slashing_reason: {
    SlashOperatorForBadState: {
      state_queue_redeemer_index: 3n,
    },
  },
};

const ACTIVE_OPERATOR_DATUM = {
  bond_unlock_time: null,
  inactivity_strikes: 1n,
};

const REGISTERED_OPERATOR_DATUM = {
  operator: H28_A,
};

const RETIRED_OPERATOR_DATUM = {
  bond_unlock_time: 9n,
};

const activeOperatorData = Data.castTo(
  ACTIVE_OPERATOR_DATUM,
  ActiveOperatorDatum,
);
const registeredOperatorData = Data.castTo(
  REGISTERED_OPERATOR_DATUM,
  RegisteredOperatorDatum,
);
const retiredOperatorData = Data.castTo(
  RETIRED_OPERATOR_DATUM,
  RetiredOperatorDatum,
);
const emptyOperatorRootData = "";

type Vector = {
  readonly label: string;
  readonly value: unknown;
  readonly schema: unknown;
};

const vectors: readonly Vector[] = [
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
        withdrawal_spend_redeemer_index: 2n,
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
        validity_override: "FeeTooLow",
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

const EXPECTED_CBOR_BY_LABEL = {
  "user event mint authenticate tag0/4": "d8799f01020304ff",
  "user event mint burn tag1/2": "d87a9f41aa01ff",
  "user event witness mint or burn tag0/1": "d8799f41aaff",
  "user event witness register tag1/1": "d87a9f01ff",
  "user event witness unregister tag2/1": "d87b9f01ff",
  "deposit datum record/3":
    "d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ffd8799fd8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ff00d87a80ffff02581c22222222222222222222222222222222222222222222222222222222ff",
  "deposit spend record/7":
    "d8799f0102030405d8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ff06ff",
  "withdrawal datum record/5":
    "d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ffd8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ff581c11111111111111111111111111111111111111111111111111111111a0d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff9f41aa41bbffd87980ffff03581c22222222222222222222222222222222222222222222222222222222d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff",
  "withdrawal purpose initialize tag0/0": "d87980",
  "withdrawal purpose refund tag1/1": "d87a9fd87e80ff",
  "withdrawal spend record/9":
    "d8799f010203040506d8799fd8798058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ff07d87980ff",
  "reserve spend sole constructor/4": "d8799f01020304ff",
  "slashing reason state tag0/1": "d8799f01ff",
  "slashing reason settlement tag1/2": "d87a9f0102ff",
  "slashing arguments record/5":
    "d8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffff",
  "operator scheduler sync inactive tag0/1": "d8799f01ff",
  "operator scheduler sync advancing tag1/4": "d87a9f0102d8799f41aaffd87a80ff",
  "active operator spend list transition tag0/0": "d87980",
  "active operator spend update state tag1/5":
    "d87a9f581c1111111111111111111111111111111111111111111111111111111101020304ff",
  "active operator spend update settlement tag2/7":
    "d87b9f581c11111111111111111111111111111111111111111111111111111111010203040506ff",
  "active operator spend strike tag3/7":
    "d87c9f0102581c11111111111111111111111111111111111111111111111111111111d8799f41aaff030405ff",
  "active operator mint init tag0/1": "d8799f01ff",
  "active operator mint deinit tag1/0": "d87a80",
  "active operator mint activate tag2/5":
    "d87b9f581c11111111111111111111111111111111111111111111111111111111010203d87980ff",
  "active operator mint retire tag3/7":
    "d87c9f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff0203d87a80d87a9f0405d87a80d87980ffff",
  "active operator mint slash tag4/2":
    "d87d9fd8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffffd8799f04ffff",
  "active operator payload record/2": "d8799fd87a8001ff",
  "operator persisted root envelope": "d8799fd8799f40ffd87a80ff",
  "active operator persisted node envelope":
    "d8799fd87a9fd8799fd87a8001ffffd8799f41aaffff",
  "registered operator payload record/1":
    "d8799f581c11111111111111111111111111111111111111111111111111111111ff",
  "registered duplicate status registered tag0/0": "d87980",
  "registered duplicate status active tag1/1": "d87a9f01ff",
  "registered duplicate status retired tag2/0": "d87b80",
  "registered operator mint init tag0/1": "d8799f01ff",
  "registered operator mint deinit tag1/0": "d87a80",
  "registered operator mint register tag2/6":
    "d87b9f581c111111111111111111111111111111111111111111111111111111110102030405ff",
  "registered operator mint activate tag3/6":
    "d87c9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01020304ff",
  "registered operator mint deregister tag4/3":
    "d87d9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01ff",
  "registered operator mint slash duplicate tag5/5":
    "d87e9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff0102d87a9f03ffff",
  "registered operator persisted node envelope":
    "d8799fd87a9fd8799f581c11111111111111111111111111111111111111111111111111111111ffffd87a80ff",
  "retired operator payload record/1": "d8799fd8799f09ffff",
  "retired operator mint init tag0/1": "d8799f01ff",
  "retired operator mint deinit tag1/0": "d87a80",
  "retired operator mint retire tag2/6":
    "d87b9f581c11111111111111111111111111111111111111111111111111111111d87a8001020304ff",
  "retired operator mint recover tag3/3":
    "d87c9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01ff",
  "retired operator mint slash tag4/1":
    "d87d9fd8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffffff",
  "retired operator persisted node envelope":
    "d8799fd87a9fd8799fd8799f09ffffffd87a80ff",
  "payout datum record/3":
    "d8799fa0d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff",
  "payout spend add funds tag0/7": "d8799f010203d8799f04ff050607ff",
  "payout spend conclude tag1/4": "d87a9f01020304ff",
  "payout mint payout tag0/4":
    "d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ff010203ff",
  "payout mint burn tag1/4": "d87a9f0141aa0203ff",
  "settlement resolution claim record/2":
    "d8799f01581c11111111111111111111111111111111111111111111111111111111ff",
  "settlement datum record/5 none":
    "d8799f58203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444445820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444d87a80ff",
  "settlement datum record/5 some":
    "d8799f58203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444445820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444d8799fd8799f01581c11111111111111111111111111111111111111111111111111111111ffffff",
  "settlement event deposit tag0/0": "d87980",
  "settlement event withdrawal tag1/1": "d87a9fd87d80ff",
  "settlement event tx order tag2/1": "d87b9fd87d80ff",
  "settlement membership deposit tag0/1":
    "d8799fd8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
  "settlement membership withdrawal tag1/1":
    "d87a9fd8799fd8798058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
  "settlement membership tx order tag2/1":
    "d87b9fd8799fd87b8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
  "settlement spend attach tag0/7":
    "d8799f0102030405581c1111111111111111111111111111111111111111111111111111111106ff",
  "settlement spend disprove tag1/11":
    "d87a9f01020304581c11111111111111111111111111111111111111111111111111111111d87a800541aad87980d8799fd8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff06ff",
  "settlement spend resolve tag2/1": "d87b9f41aaff",
  "settlement mint spawn tag0/4": "d8799f41aa010203ff",
  "settlement mint remove tag1/3": "d87a9f41aa0102ff",
} as const satisfies Record<string, string>;

describe("canonical event and operator V1 ABI", () => {
  it("matches every TypeScript event/operator tag, arity, field, and nested shape", () => {
    expect(Object.keys(EXPECTED_CBOR_BY_LABEL)).toHaveLength(vectors.length);
    for (const vector of vectors) {
      const expected =
        EXPECTED_CBOR_BY_LABEL[
          vector.label as keyof typeof EXPECTED_CBOR_BY_LABEL
        ];
      expect(expected, vector.label).toBeDefined();
      expect(
        Data.to(vector.value as never, vector.schema as never),
        vector.label,
      ).toBe(expected);
      expect(Data.from(expected, vector.schema as never), vector.label).toEqual(
        vector.value,
      );
    }
  });

  it("rejects adjacent tags, wrong arities, decorative versions, and malformed nested values", () => {
    const activeStrikeWithMalformedLink = EXPECTED_CBOR_BY_LABEL[
      "active operator spend strike tag3/7"
    ].replace("d8799f41aaff", "01");
    const depositDatumWithDecorativeVersion = `${EXPECTED_CBOR_BY_LABEL[
      "deposit datum record/3"
    ].slice(0, -2)}00ff`;
    const shortOperatorKey = `d87a9f581b${"11".repeat(27)}01020304ff`;

    const invalid: readonly (readonly [string, string, unknown])[] = [
      ["user event mint adjacent tag", "d87b80", UserEventMintRedeemer],
      [
        "user event witness adjacent tag",
        "d87c80",
        UserEventWitnessPublishRedeemer,
      ],
      ["deposit datum wrong arity", "d87980", DepositDatum],
      [
        "deposit datum decorative V2 field",
        depositDatumWithDecorativeVersion,
        DepositDatum,
      ],
      [
        "deposit spend wrong arity",
        "d8799f010203040506ff",
        DepositSpendRedeemer,
      ],
      ["withdrawal datum wrong arity", "d87980", WithdrawalOrderDatum],
      ["withdrawal purpose adjacent tag", "d87b80", WithdrawalSpendPurpose],
      ["withdrawal purpose wrong arity", "d87a80", WithdrawalSpendPurpose],
      [
        "withdrawal spend wrong arity",
        "d8799f0102030405060708ff",
        WithdrawalSpendRedeemer,
      ],
      ["reserve adjacent tag", "d87a80", ReserveSpendRedeemer],
      ["reserve wrong arity", "d8799f010203ff", ReserveSpendRedeemer],
      ["slashing reason adjacent tag", "d87b80", SlashingReason],
      ["slashing arguments wrong arity", "d8799f01020304ff", SlashingArguments],
      [
        "operator scheduler sync adjacent tag",
        "d87b80",
        OperatorRemovalSchedulerSync,
      ],
      ["active spend adjacent tag", "d87d80", ActiveOperatorSpendRedeemer],
      [
        "active spend malformed linked-list Link",
        activeStrikeWithMalformedLink,
        ActiveOperatorSpendRedeemer,
      ],
      [
        "active spend short verification-key hash",
        shortOperatorKey,
        ActiveOperatorSpendRedeemer,
      ],
      ["active mint adjacent tag", "d87e80", ActiveOperatorMintRedeemer],
      ["active payload wrong arity", "d8799f01ff", ActiveOperatorDatum],
      ["registered duplicate adjacent tag", "d87c80", DuplicateOperatorStatus],
      [
        "registered mint adjacent tag",
        "d87f80",
        RegisteredOperatorMintRedeemer,
      ],
      ["retired mint adjacent tag", "d87e80", RetiredOperatorMintRedeemer],
      [
        "retired slash arbitrary legacy payload",
        "d87d9f01ff",
        RetiredOperatorMintRedeemer,
      ],
      ["payout datum wrong arity", "d8799f0102ff", PayoutDatum],
      ["payout spend adjacent tag", "d87b80", PayoutSpendRedeemer],
      ["payout mint adjacent tag", "d87b80", PayoutMintRedeemer],
      ["settlement datum wrong arity", "d8799f01020304ff", SettlementDatum],
      ["settlement event adjacent tag", "d87c80", EventType],
      [
        "settlement membership adjacent tag",
        "d87c80",
        EventSettlementMembershipProof,
      ],
      ["settlement spend adjacent tag", "d87c80", SettlementSpendRedeemer],
      ["settlement mint adjacent tag", "d87b80", SettlementMintRedeemer],
    ];

    for (const [label, cbor, schema] of invalid) {
      expect(() => Data.from(cbor, schema as never), label).toThrow();
    }
  });

  it("unwraps persisted active and retired operator nodes and rejects raw payload datums", async () => {
    const policyId = H28_A;
    const makeUtxo = (datum: string, assetName: string) =>
      ({
        txHash: H32_A,
        outputIndex: 0,
        address: "addr_test1_event_v1_abi",
        assets: {
          lovelace: 2_000_000n,
          [`${policyId}${assetName}`]: 1n,
        },
        datum,
      }) as never;
    const lucidWith = (utxos: readonly unknown[]) =>
      ({
        utxosAt: async () => utxos,
      }) as never;

    const activeAssetName = `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_B}`;
    const active = await Effect.runPromise(
      fetchActiveOperatorUTxOs(
        {
          activeOperatorAddress: "addr_test1_event_v1_abi",
          operator: H28_B,
          activeOperatorPolicyId: policyId,
        },
        lucidWith([
          makeUtxo(
            EXPECTED_CBOR_BY_LABEL["active operator persisted node envelope"],
            activeAssetName,
          ),
          makeUtxo(
            Data.to(ACTIVE_OPERATOR_DATUM, ActiveOperatorDatum),
            `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_A}`,
          ),
        ]),
      ),
    );
    expect(active).toHaveLength(1);
    expect(active[0]?.assetName).toBe(activeAssetName);
    expect(active[0]?.datum).toEqual(ACTIVE_OPERATOR_DATUM);

    const retiredAssetName = `${RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_B}`;
    const retired = await Effect.runPromise(
      fetchRetiredOperatorUTxOs(
        {
          retiredOperatorAddress: "addr_test1_event_v1_abi",
          operator: H28_B,
          retiredOperatorPolicyId: policyId,
        },
        lucidWith([
          makeUtxo(
            EXPECTED_CBOR_BY_LABEL["retired operator persisted node envelope"],
            retiredAssetName,
          ),
          makeUtxo(
            Data.to(RETIRED_OPERATOR_DATUM, RetiredOperatorDatum),
            `${RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${H28_A}`,
          ),
        ]),
      ),
    );
    expect(retired).toHaveLength(1);
    expect(retired[0]?.assetName).toBe(retiredAssetName);
    expect(retired[0]?.datum).toEqual(RETIRED_OPERATOR_DATUM);
  });
});
