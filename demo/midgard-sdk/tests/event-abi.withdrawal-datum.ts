import { Data } from "@lucid-evolution/lucid";

import { ActiveOperatorDatum } from "../src/active-operators.js";
import { RegisteredOperatorDatum } from "../src/registered-operators.js";
import { RetiredOperatorDatum } from "../src/retired-operators.js";

export const H28_A = "11".repeat(28);

export const H28_B = "22".repeat(28);

export const H32_A = "33".repeat(32);

export const H32_B = "44".repeat(32);

export const OUTPUT_REFERENCE = {
  transactionId: H32_A,
  outputIndex: 1n,
};

export const ADDRESS = {
  paymentCredential: {
    ScriptCredential: [H28_A],
  },
  stakeCredential: null,
};

export const EMPTY_VALUE = new Map<string, Map<string, bigint>>();

export const RAW_DEPOSIT_PROOF = {
  domain: "DepositsRootDomain",
  root: H32_A,
  phas_root: H32_B,
  count: 1n,
  key: "aa",
  value: "bb",
  proof: [],
};

export const RAW_WITHDRAWAL_PROOF = {
  ...RAW_DEPOSIT_PROOF,
  domain: "WithdrawalsRootDomain",
};

export const DEPOSIT_DATUM = {
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

export const WITHDRAWAL_DATUM = {
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

export const SLASHING_ARGUMENTS = {
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

export const ACTIVE_OPERATOR_DATUM = {
  bond_unlock_time: null,
  inactivity_strikes: 1n,
};

export const REGISTERED_OPERATOR_DATUM = {
  operator: H28_A,
};

export const RETIRED_OPERATOR_DATUM = {
  bond_unlock_time: 9n,
};

export const activeOperatorData = Data.castTo(
  ACTIVE_OPERATOR_DATUM,
  ActiveOperatorDatum,
);

export const registeredOperatorData = Data.castTo(
  REGISTERED_OPERATOR_DATUM,
  RegisteredOperatorDatum,
);

export const retiredOperatorData = Data.castTo(
  RETIRED_OPERATOR_DATUM,
  RetiredOperatorDatum,
);

export const emptyOperatorRootData = "";

export type Vector = {
  readonly label: string;
  readonly value: unknown;
  readonly schema: unknown;
};
