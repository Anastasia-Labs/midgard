/**
 * `da-hash-preimage` family (Goal task `Q44`) — rule and codec agreement.
 *
 * Every vector here is shared with the Aiken selectors in
 * `onchain/aiken/validators/fraud-proofs/da-hash-preimage/step-01.ak` and
 * `.../step-02.ak`, so the TypeScript builder and the L1 verifier cannot
 * drift.
 */
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  committedLeafBodyCborV1,
  committedLeafIsUnderframedV1,
  DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT,
  DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT,
  DA_HASH_PREIMAGE_COMPACT_V1_TAIL_BYTE_COUNT,
  DA_HASH_PREIMAGE_VIOLATION_ID_V1,
  daHashPreimageEvidenceFromCommittedLeafV1,
  DaHashPreimageStep02Datum,
  DaHashPreimageStep02DatumSchema,
  DaHashPreimageStep02SpendRedeemer,
  DaHashPreimageStep02SpendRedeemerSchema,
  DaHashPreimageStep02State,
  daHashPreimageStep02StateFromEvidenceV1,
  deriveCommittedLeafTxIdV1,
  isDaHashPreimageViolationV1,
} from "../src/fraud-proof/da-hash-preimage.js";

/**
 * Largest canonical compact transaction the Cardano boundary admits (296
 * spend redeemers). Same bytes as
 * `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak:36` and the Aiken
 * selector `da_hash_preimage_rule_holds_at_the_maximum_cardano_transaction`.
 */
const MAXIMUM_CARDANO_COMPACT_CBOR_HEX =
  "84018c58207fac00ce59ee1a8c6f84fe48c8ff61af01e76a9f4cfe210a6245f0cbbe7781265820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58205581fd909e08e4dea9336a8928f0d2731f2f31e1ce31a16cb1f5b2ebc2c9dccf1a000de2ec20205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d5820e650a24c14c0e6a48877805b4185f8ff2ee711e964e6aa63ce05c29ddeb1bd26582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820723dfb187dd11e5d8b44a3ebc9b44da9037807f5ff794e07f962798509df1f6100";

const MAXIMUM_CARDANO_TRANSACTION_ID =
  "82c56f324a18a66255e3d48ddcf80a86f5b7db89dd8f5b1e0c3d3cce02668b40";

const FOREIGN_COMMITTED_KEY =
  "9999999999999999999999999999999999999999999999999999999999999999";

const maximumLeaf = Buffer.from(MAXIMUM_CARDANO_COMPACT_CBOR_HEX, "hex");

describe("Q44 da-hash-preimage rule", () => {
  it("pins the canonical frame constants shared with rule.ak", () => {
    expect(DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT).toBe(2);
    expect(DA_HASH_PREIMAGE_COMPACT_V1_TAIL_BYTE_COUNT).toBe(35);
    expect(DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT).toBe(37);
  });

  it("recovers the committed body preimage without decoding the leaf", () => {
    const decoded = decodeMidgardNativeTxCompactV1(maximumLeaf);
    const decodedTxId = computeMidgardNativeTxIdV1(decoded).toString("hex");
    expect(decodedTxId).toBe(MAXIMUM_CARDANO_TRANSACTION_ID);
    // The decoder-free derivation must agree with the full decoder at the
    // largest leaf the boundary admits.
    expect(deriveCommittedLeafTxIdV1(maximumLeaf).toString("hex")).toBe(
      MAXIMUM_CARDANO_TRANSACTION_ID,
    );
    expect(committedLeafBodyCborV1(maximumLeaf).length).toBe(
      maximumLeaf.length - DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT,
    );
  });

  it("never convicts an honestly keyed committed leaf", () => {
    const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
      committedTxId: MAXIMUM_CARDANO_TRANSACTION_ID,
      committedLeafValue: maximumLeaf,
    });
    expect(evidence.violationId).toBe(DA_HASH_PREIMAGE_VIOLATION_ID_V1);
    expect(evidence.derivedTxId).toBe(MAXIMUM_CARDANO_TRANSACTION_ID);
    expect(evidence.isViolation).toBe(false);
  });

  it("convicts a canonical transaction committed under a foreign key", () => {
    const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
      committedTxId: FOREIGN_COMMITTED_KEY,
      committedLeafValue: maximumLeaf,
    });
    expect(evidence.isViolation).toBe(true);
    expect(evidence.derivedTxId).toBe(MAXIMUM_CARDANO_TRANSACTION_ID);
    expect(evidence.committedLeafByteCount).toBe(maximumLeaf.length);
  });

  it("convicts a committed leaf that is not a transaction at all", () => {
    const garbage = Buffer.from("deadbeef", "hex");
    const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
      committedTxId: FOREIGN_COMMITTED_KEY,
      committedLeafValue: garbage,
    });
    expect(evidence.isViolation).toBe(true);
    expect(committedLeafIsUnderframedV1(garbage.length)).toBe(true);
    // Underframed leaves are convicted even when the clamped derivation would
    // collide with the committed key.
    expect(
      isDaHashPreimageViolationV1({
        committedTxId: deriveCommittedLeafTxIdV1(garbage).toString("hex"),
        derivedTxId: deriveCommittedLeafTxIdV1(garbage).toString("hex"),
        committedLeafByteCount: garbage.length,
      }),
    ).toBe(true);
  });

  it("is exact on the predicate boundary", () => {
    const a = "11".repeat(32);
    const b = "22".repeat(32);
    expect(
      isDaHashPreimageViolationV1({
        committedTxId: a,
        derivedTxId: a,
        committedLeafByteCount: DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT,
      }),
    ).toBe(false);
    expect(
      isDaHashPreimageViolationV1({
        committedTxId: a,
        derivedTxId: b,
        committedLeafByteCount: DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT,
      }),
    ).toBe(true);
    expect(
      isDaHashPreimageViolationV1({
        committedTxId: a,
        derivedTxId: a,
        committedLeafByteCount:
          DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT - 1,
      }),
    ).toBe(true);
  });
});

describe("Q44 da-hash-preimage on-chain schemas", () => {
  const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
    committedTxId: FOREIGN_COMMITTED_KEY,
    committedLeafValue: maximumLeaf,
  });

  it("round-trips the step-02 state exactly as step-01 derives it", () => {
    const state = daHashPreimageStep02StateFromEvidenceV1(evidence);
    expect(state).toEqual({
      committed_tx_id: FOREIGN_COMMITTED_KEY,
      derived_tx_id: MAXIMUM_CARDANO_TRANSACTION_ID,
      committed_leaf_byte_count: BigInt(maximumLeaf.length),
    });
    const cbor = Data.to(state, DaHashPreimageStep02State);
    expect(Data.from(cbor, DaHashPreimageStep02State)).toEqual(state);
  });

  it("round-trips the step-02 datum and redeemer", () => {
    const datum: DaHashPreimageStep02Datum = {
      fraud_prover: "44".repeat(28),
      data: daHashPreimageStep02StateFromEvidenceV1(evidence),
    };
    const datumCbor = Data.to(datum, DaHashPreimageStep02Datum);
    expect(Data.from(datumCbor, DaHashPreimageStep02Datum)).toEqual(datum);

    const redeemer: DaHashPreimageStep02SpendRedeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
        },
      ],
    };
    const redeemerCbor = Data.to(redeemer, DaHashPreimageStep02SpendRedeemer);
    expect(Data.from(redeemerCbor, DaHashPreimageStep02SpendRedeemer)).toEqual(
      redeemer,
    );
  });

  it("rejects a non-32-byte commitment in the step-02 state", () => {
    expect(() =>
      Data.to(
        {
          committed_tx_id: "00",
          derived_tx_id: MAXIMUM_CARDANO_TRANSACTION_ID,
          committed_leaf_byte_count: 1n,
        },
        DaHashPreimageStep02State,
      ),
    ).toThrow();
  });

  it("exposes both step-02 schemas under their schema aliases", () => {
    expect(DaHashPreimageStep02DatumSchema).toBeDefined();
    expect(DaHashPreimageStep02SpendRedeemerSchema).toBeDefined();
  });
});
