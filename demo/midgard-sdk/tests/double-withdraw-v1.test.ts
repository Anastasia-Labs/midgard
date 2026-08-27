/** Aiken/TypeScript codec and rule twins for `double-withdraw` (W-C3). */
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { OutputReference } from "../src/common.js";
import {
  committedWithdrawalKeyBytesV1,
  committedWithdrawalValueBytesV1,
  type DoubleWithdrawSourceProofV1,
  type DoubleWithdrawStep02State,
  DoubleWithdrawStep02State as DoubleWithdrawStep02StateSchema,
  doubleWithdrawStep02StateV1,
  doubleWithdrawThreadTokenAssetNameV1,
  isDoubleWithdrawFaultV1,
  isPayableWithdrawalLeafV1,
} from "../src/fraud-proof/double-withdraw-v1.js";
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
): DoubleWithdrawSourceProofV1 => ({
  domain: ROOT_DOMAINS.withdrawals,
  root: "11".repeat(32),
  phas_root: "22".repeat(32),
  count: 2n,
  key,
  value,
  proof: [],
});

const HEADER_HASH = "73".repeat(28);
const STATE: DoubleWithdrawStep02State = doubleWithdrawStep02StateV1({
  challengedHeaderHash: HEADER_HASH,
  committedWithdrawal: proof(FIRST_ID, PAYABLE_INFO),
});

describe("double-withdraw v1 codec twin", () => {
  it("encodes the fixed-size step-02 state with the Aiken constructor layout", () => {
    const expected =
      `d8799f581c${HEADER_HASH}` +
      `d8799f5820${FIRST_ID.transactionId}02ff` +
      `d8799f5820${SHARED_OUTREF.transactionId}01ffff`;
    expect(Data.to(STATE, DoubleWithdrawStep02StateSchema)).toBe(expected);
    expect(Data.from(expected, DoubleWithdrawStep02StateSchema)).toEqual(STATE);
  });

  it("uses the canonical serialiseData withdrawal leaf bytes", () => {
    expect(committedWithdrawalKeyBytesV1(FIRST_ID)).toBe(
      `d8799f5820${FIRST_ID.transactionId}02ff`,
    );
    const canonical = committedWithdrawalValueBytesV1(PAYABLE_INFO);
    expect(canonical).toMatch(/^d8799f/u);
    expect(canonical).not.toBe("");
  });

  it("keeps the unregistered category id caller-supplied", () => {
    expect(doubleWithdrawThreadTokenAssetNameV1("a1b2c3d4", HEADER_HASH)).toBe(
      `a1b2c3d4${HEADER_HASH}`,
    );
  });
});

describe("double-withdraw v1 rule twin", () => {
  it("accepts exactly two distinct payable leaves draining one L2 outref", () => {
    expect(isPayableWithdrawalLeafV1(PAYABLE_INFO)).toBe(true);
    expect(isDoubleWithdrawFaultV1(STATE, proof(SECOND_ID, PAYABLE_INFO))).toBe(
      true,
    );
  });

  it("refuses the same leaf twice", () => {
    expect(isDoubleWithdrawFaultV1(STATE, proof(FIRST_ID, PAYABLE_INFO))).toBe(
      false,
    );
  });

  it("refuses distinct L2 outrefs", () => {
    expect(
      isDoubleWithdrawFaultV1(
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
    expect(isPayableWithdrawalLeafV1(honestDuplicate)).toBe(false);
    expect(
      isDoubleWithdrawFaultV1(STATE, proof(SECOND_ID, honestDuplicate)),
    ).toBe(false);
  });
});
