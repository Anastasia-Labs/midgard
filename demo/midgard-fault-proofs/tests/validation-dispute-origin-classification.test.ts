import {
  committedDepositValueBytes,
  committedWithdrawalValueBytes,
  type DepositInfo,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  committedDepositMatchesOrigin,
  committedWithdrawalMatchesOrigin,
} from "../src/validation-dispute/replay.js";

const depositInfo: DepositInfo = {
  l2_address: {
    paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

const withdrawalInfo: WithdrawalInfo = {
  body: {
    l2_outref: { transactionId: "7e".repeat(32), outputIndex: 1n },
    l2_owner: "9c".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
};

/**
 * Decision 0007: the operator owns the committed validity verdict, so the
 * validation replay's origin comparison judges body and signature only.
 */
describe("validation replay origin comparison", () => {
  it("accepts a committed deposit that reproduces its authentic origin", () => {
    expect(
      committedDepositMatchesOrigin({
        originInfo: depositInfo,
        committedValueBytes: committedDepositValueBytes(depositInfo),
      }),
    ).toBe(true);
  });

  it("refuses a committed deposit whose authentic origin differs in content", () => {
    expect(
      committedDepositMatchesOrigin({
        originInfo: depositInfo,
        committedValueBytes: committedDepositValueBytes({
          ...depositInfo,
          l2_network_id: 1n,
        }),
      }),
    ).toBe(false);
  });

  it("ignores a validity-only difference between the committed leaf and the L1 order", () => {
    // The L1 order datum carries a placeholder validity because the order is
    // created before any block adjudicates it. A committed verdict that
    // differs from it is the operator's claim, not fabricated content.
    for (const committedValidity of [
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
      "IncorrectWithdrawalOwner",
    ] as const) {
      expect(
        committedWithdrawalMatchesOrigin({
          originInfo: withdrawalInfo,
          committedValidity,
          committedValueBytes: committedWithdrawalValueBytes({
            ...withdrawalInfo,
            validity: committedValidity,
          }),
        }),
      ).toBe(true);
    }
  });

  it("refuses a committed withdrawal whose body or signature differs from the authentic order", () => {
    const committedValidity = "NonExistentWithdrawalUtxo" as const;
    for (const committed of [
      {
        ...withdrawalInfo,
        body: { ...withdrawalInfo.body, l2_owner: "3d".repeat(28) },
      },
      {
        ...withdrawalInfo,
        signature: ["ad".repeat(32), "cf".repeat(64)] as [string, string],
      },
    ]) {
      expect(
        committedWithdrawalMatchesOrigin({
          originInfo: withdrawalInfo,
          committedValidity,
          committedValueBytes: committedWithdrawalValueBytes({
            ...committed,
            validity: committedValidity,
          }),
        }),
      ).toBe(false);
    }
  });
});
