import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isWithdrawnInputViolation,
  ROOT_DOMAINS,
  type WithdrawalInfo,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep03SpendRedeemer,
} from "../src/index.js";

type Schema = Parameters<typeof Data.to>[1];

const input = { tx_id: "11".repeat(32), output_index: 7n };

const withdrawal = (
  validity: WithdrawalInfo["validity"] = "WithdrawalIsValid",
): WithdrawalInfo => ({
  body: {
    l2_outref: { transactionId: input.tx_id, outputIndex: input.output_index },
    l2_owner: "22".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["33".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["44".repeat(32), "55".repeat(64)],
  validity,
});

const roundTrip = <A>(value: A, schema: Schema): A =>
  Data.from(Data.to(value as never, schema as never), schema as never) as A;

describe("withdrawn-input V1 wire twin", () => {
  it("round-trips the counted withdrawals commitment in step 02", () => {
    const datum = {
      fraud_prover: "66".repeat(28),
      data: {
        bad_tx_id: "77".repeat(32),
        blocks_withdrawals_root: "88".repeat(32),
        blocks_withdrawal_count: 3n,
      },
    };
    expect(roundTrip(datum, WithdrawnInputStep02Datum)).toEqual(datum);
  });

  it("round-trips the terminal withdrawal membership redeemer", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 1n,
          fraud_proof_mint_redeemer_index: 2n,
          withdrawal_membership: {
            domain: ROOT_DOMAINS.withdrawals,
            root: "88".repeat(32),
            phas_root: "99".repeat(32),
            count: 1n,
            key: { transactionId: "aa".repeat(32), outputIndex: 0n },
            value: withdrawal(),
            proof: [],
          },
        },
      ],
    };
    expect(roundTrip(redeemer, WithdrawnInputStep03SpendRedeemer)).toEqual(
      redeemer,
    );
  });

  it("convicts only a valid withdrawal of the selected spend input", () => {
    expect(isWithdrawnInputViolation({ input, withdrawal: withdrawal() })).toBe(
      true,
    );
    expect(
      isWithdrawnInputViolation({
        input,
        withdrawal: withdrawal({ SpentWithdrawalUtxo: { l2_tx_id: "ff" } }),
      }),
    ).toBe(false);
    expect(
      isWithdrawnInputViolation({
        input,
        withdrawal: {
          ...withdrawal(),
          body: {
            ...withdrawal().body,
            l2_outref: { transactionId: "00".repeat(32), outputIndex: 7n },
          },
        },
      }),
    ).toBe(false);
  });
});
