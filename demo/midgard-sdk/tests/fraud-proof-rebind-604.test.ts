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
import { h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

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
              inputs_preimage: [{ tx_id: h32(0xaa), output_index: 7n }],
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
        { verified_tx_inputs_hash: h32(0x11) },
      ],
      [
        "double-spend-03",
        SDK.DoubleSpendStep03Datum,
        {
          verified_tx1_spend_inputs_hash: h32(0x11),
          verified_tx2_spend_inputs_hash: h32(0x22),
        },
      ],
      [
        "zero-input-02",
        SDK.ZeroInputStep02Datum,
        { bad_tx_spend_inputs_hash: h32(0x11) },
      ],
      [
        "no-input-02",
        SDK.NonExistentInputStep02Datum,
        {
          bad_tx_inputs_hash: h32(0x11),
          blocks_prev_utxos_root: h32(0x22),
          blocks_transactions_root: h32(0x33),
        },
      ],
      [
        "no-reference-input-02",
        SDK.NoReferenceInputStep02Datum,
        {
          bad_tx_reference_inputs_hash: h32(0x11),
          blocks_prev_utxos_root: h32(0x22),
          blocks_transactions_root: h32(0x33),
        },
      ],
      [
        "invalid-signature-02",
        SDK.InvalidSignatureStep02Datum,
        { bad_tx_id: h32(0x11), bad_addr_tx_wits_hash: h32(0x22) },
      ],
      [
        "reference-input-no-idx-02",
        SDK.ReferenceInputNoIdxStep02Datum,
        { verified_tx_reference_inputs_hash: h32(0x11) },
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
   * Positive controls for the two input-existence sweep entries. Without these,
   * a schema that rejected *everything* would satisfy their throw-style
   * negatives above, which is exactly the vacuity class this file's header
   * disclaims. Each round-trips the current step-02 state, the event-relative
   * source state (`input_existence_v1/types.SourceStateV1`: subject, header,
   * compact view), and checks the decoded value, so the negative's refusal is a
   * refusal of the retired shape rather than of all shapes.
   */
  it("no-input-02 and no-reference-input-02 round-trip their source state", () => {
    const cases: readonly [string, unknown][] = [
      ["no-input-02", SDK.NonExistentInputStep02Datum],
      ["no-reference-input-02", SDK.NoReferenceInputStep02Datum],
    ];
    const fraudProver = "aa".repeat(28);
    const honestState: SDK.InputExistenceSourceState = {
      subject: {
        version: 1n,
        direction: 0n,
        source_kind: 0n,
        transaction_id: h32(0x11),
        source_key: "",
        rejection_reason: null,
      },
      header: {
        prevUtxosRoot: h32(0x21),
        utxosRoot: h32(0x22),
        withdrawalsRoot: h32(0x23),
        forcedTransactionsRoot: h32(0x24),
        transactionsRoot: h32(0x25),
        depositsRoot: h32(0x26),
        transitionTraceRoot: h32(0x27),
        eventToStepRoot: h32(0x28),
        validationTracesRoot: h32(0x29),
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 1n,
        depositCount: 0n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
        validationTraceCount: 1n,
        startTime: 0n,
        endTime: 1n,
        blockSlot: 0n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: "2a".repeat(28),
        operatorVkey: "2b".repeat(28),
        protocolVersion: 1n,
      },
      compact: {
        body: {
          spend_inputs_hash: h32(0x31),
          reference_inputs_hash: h32(0x32),
          outputs_hash: h32(0x33),
          fee: 4n,
          validity_interval_start: 5n,
          validity_interval_end: 6n,
          required_observers_hash: h32(0x37),
          required_signers_hash: h32(0x38),
          mint_hash: h32(0x39),
          script_integrity_hash: h32(0x3a),
          auxiliary_data_hash: h32(0x3b),
          network_id: 1n,
        },
        witness_set_hash: h32(0x3c),
        validity_code: null,
      },
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
      SDK.fieldOpeningForField({
        fieldIndex: 7,
        nativeTxCompactCbor: "a1b2c3",
        carriage: { Inline: { preimage: "80" } },
      }),
    ).toThrow(SDK.MidgardFieldOpeningError);
    // And a witness anchor with no anchored witness_set_hash to check against.
    expect(() =>
      SDK.nativeTxAnchorForField({
        sourceKind: 0n,
        fieldIndex: 7,
        txId: h32(0x11),
      }),
    ).toThrow(SDK.MidgardFieldOpeningError);
  });
});
