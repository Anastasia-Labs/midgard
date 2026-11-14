import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  WithdrawnReferenceInputStep01Datum,
  WithdrawnReferenceInputStep02Datum,
  WithdrawnReferenceInputStep02State as Step02StateSchema,
  type WithdrawnReferenceInputStep02State,
  WithdrawnReferenceInputStep03Args as Step03ArgsSchema,
  type WithdrawnReferenceInputStep03Args,
  WithdrawnReferenceInputStep03Datum,
  WithdrawnReferenceInputStep03State as Step03StateSchema,
  type WithdrawnReferenceInputStep03State,
  withdrawnReferenceInputStepDatumSchema,
  withdrawnReferenceInputThreadTokenAssetName,
} from "../src/index.js";

const txId = "aa".repeat(32);
const withdrawalsRoot = "bb".repeat(32);
const prover = "cc".repeat(28);
const phasRoot = "dd".repeat(32);
const withdrawalTxId = "ee".repeat(32);
const signatureKey = "11".repeat(32);
const signatureBytes = "22".repeat(64);

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

  it("pins the full step-03 counted withdrawal-membership wire vector", () => {
    const args: WithdrawnReferenceInputStep03Args = {
      input_index: 0n,
      output_index: 1n,
      fraud_proof_mint_redeemer_index: 2n,
      withdrawal_membership: {
        domain: "WithdrawalsRootDomain",
        root: withdrawalsRoot,
        phas_root: phasRoot,
        count: 1n,
        key: { transactionId: withdrawalTxId, outputIndex: 0n },
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
          signature: [signatureKey, signatureBytes],
          validity: "WithdrawalIsValid",
        },
        proof: [],
      },
    };

    // The step-03 validator hashes the serialised key and value, so the exact
    // bytes -- constructor tags, field order, and the encoding of each nested
    // option and collection -- are the contract, not merely the shape. The
    // expectation is written out segment by segment in the schema's declared
    // order so a reviewer can check each one against the ABI.
    const expected = [
      "d8799f", // WithdrawnReferenceInputStep03Args
      "00", //     input_index = 0
      "01", //     output_index = 1
      "02", //     fraud_proof_mint_redeemer_index = 2
      "d8799f", //   RootMembershipProof<WithdrawalId, WithdrawalInfo>
      "d87980", //     domain = WithdrawalsRootDomain (first constructor)
      `5820${withdrawalsRoot}`, // root
      `5820${phasRoot}`, //        phas_root
      "01", //                     count = 1
      "d8799f", //     key: WithdrawalId
      `5820${withdrawalTxId}`, //    transactionId
      "00", //                       outputIndex = 0
      "ff",
      "d8799f", //     value: signed WithdrawalInfo
      "d8799f", //       body
      `d8799f5820${txId}03ff`, // l2_outref = (txId, 3)
      `581c${prover}`, //          l2_owner
      "a0", //                     l2_value = {}
      "d8799f", //                 l1_address
      `d8799f581c${prover}ff`, //    paymentCredential = PublicKeyCredential
      "d87a80", //                   stakeCredential = None
      "ff",
      "d87980", //                 l1_datum = NoDatum
      "ff",
      "9f", //           signature
      `5820${signatureKey}`, //   verification key
      `5840${signatureBytes}`, // 64-byte signature
      "ff",
      "d87980", //       validity = WithdrawalIsValid (first constructor)
      "ff",
      "80", //         proof = [] (the root is the leaf)
      "ff",
      "ff",
    ].join("");

    const cbor = Data.to(args, Step03ArgsSchema);
    expect(cbor).toBe(expected);
    expect(Data.from(cbor, Step03ArgsSchema)).toEqual(args);
  });

  it("resolves each step to its own datum schema", () => {
    // Identity, not presence: a transposed switch arm would still return
    // "a schema", and the encodings below show the returned schema really is
    // the one that step's datum is written with.
    expect(withdrawnReferenceInputStepDatumSchema("step_01")).toBe(
      WithdrawnReferenceInputStep01Datum,
    );
    expect(withdrawnReferenceInputStepDatumSchema("step_02")).toBe(
      WithdrawnReferenceInputStep02Datum,
    );
    expect(withdrawnReferenceInputStepDatumSchema("step_03")).toBe(
      WithdrawnReferenceInputStep03Datum,
    );

    // The resolver's return type is the union of all three step schemas, so
    // the encodings below go through the resolved value narrowed to the
    // step-02 schema — the narrowing is what the identity assertion above
    // licenses.
    const step02Schema = withdrawnReferenceInputStepDatumSchema(
      "step_02",
    ) as unknown as typeof WithdrawnReferenceInputStep02Datum;
    const step02Datum: WithdrawnReferenceInputStep02Datum = {
      fraud_prover: prover,
      data: {
        bad_tx_id: txId,
        blocks_withdrawals_root: withdrawalsRoot,
        blocks_withdrawal_count: 2n,
      },
    };
    expect(Data.to(step02Datum, step02Schema)).toBe(
      `d8799f581c${prover}d8799fd8799f5820${txId}5820${withdrawalsRoot}02ffffff`,
    );
    // Step 03 carries an out-ref where step 02 carries a bare transaction id,
    // so the step-02 schema must refuse a step-03 state rather than silently
    // accepting the neighbouring step's datum.
    expect(() =>
      Data.to(
        {
          fraud_prover: prover,
          data: {
            missing_reference_input: { tx_id: txId, output_index: 3n },
            blocks_withdrawals_root: withdrawalsRoot,
            blocks_withdrawal_count: 2n,
          },
        } as unknown as WithdrawnReferenceInputStep02Datum,
        step02Schema,
      ),
    ).toThrow();
  });

  it("binds the thread token to a well-formed four-byte category id", () => {
    expect(
      withdrawnReferenceInputThreadTokenAssetName("00000010", prover),
    ).toBe(`00000010${prover}`);
    expect(() =>
      withdrawnReferenceInputThreadTokenAssetName("0000001G", prover),
    ).toThrow(/category id/);
  });
});
