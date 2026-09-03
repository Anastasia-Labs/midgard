import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  WithdrawnReferenceInputStep02State as Step02StateSchema,
  type WithdrawnReferenceInputStep02State,
  WithdrawnReferenceInputStep03Args as Step03ArgsSchema,
  type WithdrawnReferenceInputStep03Args,
  WithdrawnReferenceInputStep03State as Step03StateSchema,
  type WithdrawnReferenceInputStep03State,
  withdrawnReferenceInputStepDatumSchema,
  withdrawnReferenceInputThreadTokenAssetName,
} from "../src/index.js";

const txId = "aa".repeat(32);
const withdrawalsRoot = "bb".repeat(32);
const prover = "cc".repeat(28);

describe("withdrawn-reference-input v1 codecs", () => {
  it("pins the step-02 state field order", () => {
    const state: WithdrawnReferenceInputStep02State = {
      bad_tx_id: txId,
      blocks_withdrawals_root: withdrawalsRoot,
      blocks_withdrawal_count: 2n,
    };
    expect(Data.to(state, Step02StateSchema)).toBe(
      `d8799f5820${txId}5820${withdrawalsRoot}02ff`,
    );
  });

  it("pins the step-03 state field order and nested out-ref", () => {
    const state: WithdrawnReferenceInputStep03State = {
      missing_reference_input: { tx_id: txId, output_index: 3n },
      blocks_withdrawals_root: withdrawalsRoot,
      blocks_withdrawal_count: 2n,
    };
    expect(Data.to(state, Step03StateSchema)).toBe(
      `d8799fd8799f5820${txId}03ff5820${withdrawalsRoot}02ff`,
    );
  });

  it("round-trips the full step-03 counted withdrawal membership wire shape", () => {
    const args: WithdrawnReferenceInputStep03Args = {
      input_index: 0n,
      output_index: 1n,
      fraud_proof_mint_redeemer_index: 2n,
      withdrawal_membership: {
        domain: "WithdrawalsRootDomain",
        root: withdrawalsRoot,
        phas_root: "dd".repeat(32),
        count: 1n,
        key: { transactionId: "ee".repeat(32), outputIndex: 0n },
        value: {
          body: {
            l2_outref: { transactionId: txId, outputIndex: 3n },
            l2_owner: prover,
            l2_value: new Map(),
            l1_address: {
              paymentCredential: { PublicKeyCredential: [prover] },
              stakeCredential: null,
            },
            l1_datum: "NoDatum",
          },
          signature: ["11".repeat(32), "22".repeat(64)],
          validity: "WithdrawalIsValid",
        },
        proof: [],
      },
    };
    const cbor = Data.to(args, Step03ArgsSchema);
    expect(Data.from(cbor, Step03ArgsSchema)).toEqual(args);
    expect(cbor).toMatch(/^d8799f000102d8799f/);
  });

  it("resolves all three datum schemas and validates the thread asset name", () => {
    expect(withdrawnReferenceInputStepDatumSchema("step_01")).toBeDefined();
    expect(withdrawnReferenceInputStepDatumSchema("step_02")).toBeDefined();
    expect(withdrawnReferenceInputStepDatumSchema("step_03")).toBeDefined();
    expect(
      withdrawnReferenceInputThreadTokenAssetName("00000010", prover),
    ).toBe(`00000010${prover}`);
    expect(() =>
      withdrawnReferenceInputThreadTokenAssetName("0000001G", prover),
    ).toThrow(/category id/);
  });
});
