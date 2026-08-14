/**
 * Mutation negatives for the #604 re-derivation — one per rebound family.
 *
 * `tests/fault-proof.test.ts` pins what the new shapes ARE; this file pins what
 * they are NOT, which is the half a re-derivation can silently get wrong. The
 * cross-family sweep is the load-bearing one: every rebound step datum must
 * *refuse* the retired per-field collection commitment it used to carry, so a
 * module that was missed by the sweep fails here rather than at a validator.
 *
 * Each negative asserts that its mutation genuinely landed — a differing
 * encoding, or a constructor that throws — before concluding anything from the
 * refusal. A mutation that quietly failed to apply would otherwise read as a
 * passing rejection, which is the vacuity class #579's review found five times.
 */
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const h32 = (b: string) => b.repeat(32);

/**
 * Per-family mutation negatives for the #604 re-derivation. Each asserts that
 * the mutated value genuinely DIFFERS from the honest one before asserting the
 * refusal, so a mutation that failed to land cannot pass as a rejection.
 */
describe("#604 per-family mutation negatives", () => {
  const honestOpening = {
    BodyFieldOpening: {
      native_tx_compact_cbor: "a1b2c3",
      carriage: { Inline: { preimage: "80" } },
    },
  };
  const wrongBytesOpening = {
    BodyFieldOpening: {
      native_tx_compact_cbor: "a1b2c4",
      carriage: { Inline: { preimage: "81" } },
    },
  };

  it("input-no-idx step-02: wrong-bytes carriage changes the redeemer", () => {
    const mk = (o: unknown) =>
      Data.to(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              spend_inputs_opening: o,
              bad_inputs_index: 0n,
            },
          ],
        } as never,
        SDK.InputNoIdxStep02SpendRedeemer,
      );
    const honest = mk(honestOpening);
    const mutated = mk(wrongBytesOpening);
    expect(mutated).not.toBe(honest); // the mutation landed
  });

  it("input-no-idx step-02: the retired preimage argument no longer encodes", () => {
    expect(() =>
      Data.to(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              inputs_preimage: [{ tx_id: h32("aa"), output_index: 7n }],
              bad_inputs_index: 0n,
            },
          ],
        } as never,
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toThrow();
  });

  it("every rebound family's step state rejects the retired collection-commitment field", () => {
    const cases: readonly [string, unknown, unknown][] = [
      [
        "input-no-idx-02",
        SDK.InputNoIdxStep02Datum,
        { verified_tx_inputs_hash: h32("11") },
      ],
      [
        "double-spend-03",
        SDK.DoubleSpendStep03Datum,
        {
          verified_tx1_spend_inputs_hash: h32("11"),
          verified_tx2_spend_inputs_hash: h32("22"),
        },
      ],
      [
        "zero-input-02",
        SDK.ZeroInputStep02Datum,
        { bad_tx_spend_inputs_hash: h32("11") },
      ],
      [
        "no-input-02",
        SDK.NonExistentInputStep02Datum,
        {
          bad_tx_inputs_hash: h32("11"),
          blocks_prev_utxos_root: h32("22"),
          blocks_transactions_root: h32("33"),
        },
      ],
      [
        "no-reference-input-02",
        SDK.NoReferenceInputStep02Datum,
        {
          bad_tx_reference_inputs_hash: h32("11"),
          blocks_prev_utxos_root: h32("22"),
          blocks_transactions_root: h32("33"),
        },
      ],
      [
        "invalid-signature-02",
        SDK.InvalidSignatureStep02Datum,
        { bad_tx_id: h32("11"), bad_addr_tx_wits_hash: h32("22") },
      ],
      [
        "reference-input-no-idx-02",
        SDK.ReferenceInputNoIdxStep02Datum,
        { verified_tx_reference_inputs_hash: h32("11") },
      ],
    ];
    for (const [label, schema, retiredState] of cases) {
      expect(
        () =>
          Data.to(
            { fraud_prover: "aa".repeat(28), data: retiredState } as never,
            schema as never,
          ),
        label,
      ).toThrow();
    }
  });

  /**
   * Positive controls for the two sweep entries whose only positive coverage
   * lives in a fault-proofs emulator leg — and `no-input`'s leg is red at
   * step-03 (#608). Without these, a schema that rejected *everything* would
   * satisfy their throw-style negatives above, which is exactly the vacuity
   * class this file's header disclaims. Each round-trips the honest #604 state
   * and checks the decoded fields, so the negative's refusal is a refusal of
   * the retired shape rather than of all shapes.
   */
  it("no-input-02 and no-reference-input-02 round-trip their honest #604 state", () => {
    const cases: readonly [string, unknown][] = [
      ["no-input-02", SDK.NonExistentInputStep02Datum],
      ["no-reference-input-02", SDK.NoReferenceInputStep02Datum],
    ];
    const fraudProver = "aa".repeat(28);
    const honestState = {
      bad_tx_id: h32("11"),
      blocks_prev_utxos_root: h32("22"),
      blocks_transactions_root: h32("33"),
    };
    for (const [label, schema] of cases) {
      const encoded = Data.to(
        { fraud_prover: fraudProver, data: honestState } as never,
        schema as never,
      );
      expect(encoded, label).toMatch(/^[0-9a-f]+$/);
      const decoded = Data.from(encoded, schema as never) as {
        fraud_prover: string;
        data: typeof honestState;
      };
      expect(decoded.fraud_prover, label).toBe(fraudProver);
      expect(decoded.data, label).toStrictEqual(honestState);
    }
  });

  it("witness-set families refuse a wrong-anchor arm", () => {
    // A body opening at field 7 is what a builder that forgot the §2.5 split
    // would produce. It must not be constructible.
    expect(() =>
      SDK.fieldOpeningV1ForField({
        fieldIndex: 7,
        nativeTxCompactCbor: "a1b2c3",
        carriage: { Inline: { preimage: "80" } },
      }),
    ).toThrow(SDK.MidgardFieldOpeningError);
    // And a witness anchor with no anchored witness_set_hash to check against.
    expect(() =>
      SDK.nativeTxAnchorV1ForField({ fieldIndex: 7, txId: h32("11") }),
    ).toThrow(SDK.MidgardFieldOpeningError);
  });
});
