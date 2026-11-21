/** Aiken/TypeScript codec and rule twins for `double-withdraw` (W-C3). */
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { OutputReference } from "../src/common.js";
import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  type DoubleWithdrawSourceProof,
  type DoubleWithdrawStep02Args,
  DoubleWithdrawStep02Args as DoubleWithdrawStep02ArgsSchema,
  type DoubleWithdrawStep02State,
  DoubleWithdrawStep02State as DoubleWithdrawStep02StateSchema,
  doubleWithdrawStep02State,
  doubleWithdrawThreadTokenAssetName,
  isDoubleWithdrawFault,
  isPayableWithdrawalLeaf,
} from "../src/fraud-proof/double-withdraw.js";
import type { WithdrawalInfo } from "../src/ledger-state.js";
import { ROOT_DOMAINS } from "../src/transition-trace.js";

const FIRST_ID: OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};
const SECOND_ID: OutputReference = {
  transactionId: "c4".repeat(32),
  outputIndex: 1n,
};
const SHARED_OUTREF: OutputReference = {
  transactionId: "7e".repeat(32),
  outputIndex: 1n,
};
const OTHER_OUTREF: OutputReference = {
  transactionId: "1f".repeat(32),
  outputIndex: 0n,
};

const PAYABLE_INFO: WithdrawalInfo = {
  body: {
    l2_outref: SHARED_OUTREF,
    l2_owner: "9c".repeat(28),
    l2_value: new Map([["4b".repeat(28), new Map([["6d696467617264", 42n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
};

const proof = (
  key: OutputReference,
  value: WithdrawalInfo,
): DoubleWithdrawSourceProof => ({
  domain: ROOT_DOMAINS.withdrawals,
  root: "11".repeat(32),
  phas_root: "22".repeat(32),
  count: 2n,
  key,
  value,
  proof: [],
});

const HEADER_HASH = "73".repeat(28);
const STATE: DoubleWithdrawStep02State = doubleWithdrawStep02State({
  challengedHeaderHash: HEADER_HASH,
  committedWithdrawal: proof(FIRST_ID, PAYABLE_INFO),
});

describe("double-withdraw v1 codec twin", () => {
  it("pins the canonical terminal argument prefix before family fields", () => {
    const args: DoubleWithdrawStep02Args = {
      input_index: 11n,
      output_index: 12n,
      fraud_proof_mint_redeemer_index: 13n,
      hub_ref_input_index: 14n,
      state_queue_node_ref_input_index: 15n,
      committed_withdrawal: proof(SECOND_ID, PAYABLE_INFO),
    };
    const encoded = Data.to(args, DoubleWithdrawStep02ArgsSchema);

    expect(encoded).toMatch(/^d8799f0b0c0d0e0f/u);
    expect(Data.from(encoded, DoubleWithdrawStep02ArgsSchema)).toEqual(args);
  });

  it("encodes the fixed-size step-02 state with the Aiken constructor layout", () => {
    const expected =
      `d8799f581c${HEADER_HASH}` +
      `d8799f5820${FIRST_ID.transactionId}02ff` +
      `d8799f5820${SHARED_OUTREF.transactionId}01ffff`;
    expect(Data.to(STATE, DoubleWithdrawStep02StateSchema)).toBe(expected);
    expect(Data.from(expected, DoubleWithdrawStep02StateSchema)).toEqual(STATE);
  });

  it("uses the canonical serialiseData withdrawal leaf bytes", () => {
    expect(committedWithdrawalKeyBytes(FIRST_ID)).toBe(
      `d8799f5820${FIRST_ID.transactionId}02ff`,
    );
    const canonical = committedWithdrawalValueBytes(PAYABLE_INFO);
    expect(canonical).toMatch(/^d8799f/u);
    expect(canonical).not.toBe("");
  });

  it("keeps the unregistered category id caller-supplied", () => {
    expect(doubleWithdrawThreadTokenAssetName("a1b2c3d4", HEADER_HASH)).toBe(
      `a1b2c3d4${HEADER_HASH}`,
    );
  });
});

describe("double-withdraw v1 rule twin", () => {
  it("accepts exactly two distinct payable leaves draining one L2 outref", () => {
    expect(isPayableWithdrawalLeaf(PAYABLE_INFO)).toBe(true);
    expect(isDoubleWithdrawFault(STATE, proof(SECOND_ID, PAYABLE_INFO))).toBe(
      true,
    );
  });

  it("refuses the same leaf twice", () => {
    expect(isDoubleWithdrawFault(STATE, proof(FIRST_ID, PAYABLE_INFO))).toBe(
      false,
    );
  });

  it("refuses distinct L2 outrefs", () => {
    expect(
      isDoubleWithdrawFault(
        STATE,
        proof(SECOND_ID, {
          ...PAYABLE_INFO,
          body: { ...PAYABLE_INFO.body, l2_outref: OTHER_OUTREF },
        }),
      ),
    ).toBe(false);
  });

  it("refuses the honest non-payable duplicate", () => {
    const honestDuplicate: WithdrawalInfo = {
      ...PAYABLE_INFO,
      validity: { SpentWithdrawalUtxo: { l2_tx_id: "5a".repeat(32) } },
    };
    expect(isPayableWithdrawalLeaf(honestDuplicate)).toBe(false);
    expect(
      isDoubleWithdrawFault(STATE, proof(SECOND_ID, honestDuplicate)),
    ).toBe(false);
  });
});
