/** Cross-language vectors for the total Q44 source-leaf adjudicator. */
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  adjudicateCommittedSourceLeafV1,
  DA_HASH_PREIMAGE_VIOLATION_ID_V1,
  daHashPreimageEvidenceFromCommittedLeafV1,
  DaHashPreimageStep02Datum,
  DaHashPreimageStep02SpendRedeemer,
  DaHashPreimageStep02State,
  daHashPreimageStep02StateFromEvidenceV1,
  isDaHashPreimageViolationV1,
} from "../src/fraud-proof/da-hash-preimage.js";
import {
  L2TransactionSourceV1,
  L2TransactionSourceV1Schema,
} from "../src/ledger-state.js";

const VALID_TX_ID =
  "7b4e4657e0083544359f4398fb092c482766220cd53ad99b598239297d1e9813";
const FOREIGN_TX_ID = "99".repeat(32);

// Same accepted Cardano maximum source as the Aiken max-fit selector.
const COMPACT_CBOR =
  "84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820095c12f5790acc50dbbf52c0b47fe4ebd1dfd9ab308b14701543d6d4d78a06ae1a000d59492020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e2d5bb3b4c4475d516516e5396ec041b553ada06379a53f665b47e1485e0451f582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ad12ff89400f2f7975c77231241032e6a7bf49d0f2ab388425b3de0cefef003000";
const WITNESS_SET_CBOR =
  "835820689afcab7a4406fa8da9a4f97b325f34458bd2114d6b2ae9eb357e681acc0e97582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0";
const FIELD_LENGTHS_CBOR = "89182901183001190e8a01011931e601";

const source = (
  overrides: Partial<L2TransactionSourceV1> = {},
): L2TransactionSourceV1 => ({
  tx_id: VALID_TX_ID,
  source: {
    compact_cbor: COMPACT_CBOR,
    witness_set_compact_cbor: WITNESS_SET_CBOR,
    field_preimage_lengths_cbor: FIELD_LENGTHS_CBOR,
  },
  ...overrides,
});

const sourceBytes = (value: L2TransactionSourceV1): Buffer =>
  Buffer.from(Data.to(value, L2TransactionSourceV1), "hex");

describe("Q44 da-hash-preimage total source verdict", () => {
  it("accepts the exact maximum valid source as the valid-block negative", () => {
    const value = sourceBytes(source());
    const adjudication = adjudicateCommittedSourceLeafV1({
      committedTxId: VALID_TX_ID,
      committedLeafValue: value,
    });
    expect(adjudication).toEqual({
      verdict: "NoViolation",
      embeddedTxId: VALID_TX_ID,
      derivedTxId: VALID_TX_ID,
    });

    const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
      committedTxId: VALID_TX_ID,
      committedLeafValue: value,
    });
    expect(evidence.violationId).toBe(DA_HASH_PREIMAGE_VIOLATION_ID_V1);
    expect(evidence.isViolation).toBe(false);
  });

  it("convicts a valid source committed under a different MPF key", () => {
    expect(
      adjudicateCommittedSourceLeafV1({
        committedTxId: FOREIGN_TX_ID,
        committedLeafValue: sourceBytes(source()),
      }),
    ).toEqual({
      verdict: "KeyMismatch",
      embeddedTxId: VALID_TX_ID,
      derivedTxId: null,
    });
  });

  it.each([
    ["raw garbage", Buffer.from("deadbeef", "hex")],
    [
      "canonical source with trailing bytes",
      Buffer.concat([sourceBytes(source()), Buffer.from("00", "hex")]),
    ],
  ])("convicts malformed source: %s", (_label, committedLeafValue) => {
    expect(
      adjudicateCommittedSourceLeafV1({
        committedTxId: VALID_TX_ID,
        committedLeafValue,
      }).verdict,
    ).toBe("MalformedSource");
  });

  it.each([
    [
      "malformed witness CBOR",
      source({
        source: {
          ...source().source,
          witness_set_compact_cbor: "deadbeef",
        },
      }),
    ],
    [
      "forged witness-set hash binding",
      source({
        source: {
          ...source().source,
          witness_set_compact_cbor: `${WITNESS_SET_CBOR.slice(0, -2)}c1`,
        },
      }),
    ],
    [
      "malformed field lengths",
      source({
        source: {
          ...source().source,
          field_preimage_lengths_cbor: "deadbeef",
        },
      }),
    ],
  ])("convicts malformed proof source: %s", (_label, value) => {
    expect(
      adjudicateCommittedSourceLeafV1({
        committedTxId: VALID_TX_ID,
        committedLeafValue: sourceBytes(value),
      }).verdict,
    ).toBe("MalformedProofSource");
  });

  it("convicts an internally valid source whose embedded id is not its body id", () => {
    const value = source({ tx_id: FOREIGN_TX_ID });
    expect(
      adjudicateCommittedSourceLeafV1({
        committedTxId: FOREIGN_TX_ID,
        committedLeafValue: sourceBytes(value),
      }),
    ).toEqual({
      verdict: "DerivedIdMismatch",
      embeddedTxId: FOREIGN_TX_ID,
      derivedTxId: VALID_TX_ID,
    });
  });

  it("marks exactly the four accusation verdicts as violations", () => {
    expect(isDaHashPreimageViolationV1("MalformedSource")).toBe(true);
    expect(isDaHashPreimageViolationV1("KeyMismatch")).toBe(true);
    expect(isDaHashPreimageViolationV1("MalformedProofSource")).toBe(true);
    expect(isDaHashPreimageViolationV1("DerivedIdMismatch")).toBe(true);
    expect(isDaHashPreimageViolationV1("NoViolation")).toBe(false);
  });
});

describe("Q44 da-hash-preimage on-chain schemas", () => {
  const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
    committedTxId: FOREIGN_TX_ID,
    committedLeafValue: sourceBytes(source()),
  });

  it("round-trips the exact verdict-only step-02 state", () => {
    const state = daHashPreimageStep02StateFromEvidenceV1(evidence);
    expect(state).toEqual({ verdict: "KeyMismatch" });
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

  it("pins the verdict constructor order through CBOR round trips", () => {
    for (const verdict of [
      "MalformedSource",
      "KeyMismatch",
      "MalformedProofSource",
      "DerivedIdMismatch",
      "NoViolation",
    ] as const) {
      const state: DaHashPreimageStep02State = { verdict };
      const cbor = Data.to(state, DaHashPreimageStep02State);
      expect(Data.from(cbor, DaHashPreimageStep02State)).toEqual(state);
    }
  });

  it("keeps the schema alias available to source builders", () => {
    expect(L2TransactionSourceV1Schema).toBeDefined();
  });
});
