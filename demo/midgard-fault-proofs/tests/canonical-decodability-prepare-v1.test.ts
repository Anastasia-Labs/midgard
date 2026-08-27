import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxCompactV1,
} from "@al-ft/midgard-core";
import {
  MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER_V1,
  MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES_V1,
  miscountedMidgardFieldPreimageV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareCanonicalDecodabilityV1 } from "../src/canonical-decodability/index.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const bodyFixture = (preimage: Buffer) => {
  const honest = makeNativeTx({ spendInputCbors: [], fee: 7n });
  const compact = {
    ...honest.compact,
    transactionBody: {
      ...honest.compact.transactionBody,
      outputsHash: computeHash32(preimage),
    },
  };
  return {
    compact,
    compactCbor: encodeMidgardNativeTxCompactV1(compact).toString("hex"),
    txId: computeMidgardNativeTxIdV1(compact).toString("hex"),
  };
};

describe("canonical-decodability preparation", () => {
  it("derives an inline body claim and exact step-02 state", () => {
    const preimage = miscountedMidgardFieldPreimageV1(1, [
      Buffer.from("aa", "hex"),
      Buffer.from("bb", "hex"),
    ]);
    const fixture = bodyFixture(preimage);
    const prepared = prepareCanonicalDecodabilityV1({
      badTxId: fixture.txId,
      nativeTxCompactCbor: fixture.compactCbor,
      fieldIndex: 2,
      committedPreimage: preimage,
    });
    expect(prepared.evidence.verdict).toBe(
      MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES_V1,
    );
    expect(prepared.step02State).toEqual({
      bad_tx_id: fixture.txId,
      field_index: 2n,
      verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES_V1),
    });
    expect(prepared.claim).toEqual({
      BodyFieldClaim: {
        field_index: 2n,
        carriage: { Inline: { preimage: preimage.toString("hex") } },
      },
    });
  });

  it("reaches the empty-preimage missing-header corner", () => {
    const preimage = Buffer.alloc(0);
    const fixture = bodyFixture(preimage);
    expect(
      prepareCanonicalDecodabilityV1({
        badTxId: fixture.txId,
        nativeTxCompactCbor: fixture.compactCbor,
        fieldIndex: 2,
        committedPreimage: preimage,
      }).evidence.verdict,
    ).toBe(MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER_V1);
  });

  it("refuses a grammatical commitment, wrong bytes, wrong id, and wrong half", () => {
    const grammatical = encodeMidgardFieldPreimageV1([]);
    const grammaticalFixture = bodyFixture(grammatical);
    expect(() =>
      prepareCanonicalDecodabilityV1({
        badTxId: grammaticalFixture.txId,
        nativeTxCompactCbor: grammaticalFixture.compactCbor,
        fieldIndex: 2,
        committedPreimage: grammatical,
      }),
    ).toThrow(/verdict 0.*valid block cannot be challenged/u);

    const bad = Buffer.alloc(0);
    const badFixture = bodyFixture(bad);
    expect(() =>
      prepareCanonicalDecodabilityV1({
        badTxId: badFixture.txId,
        nativeTxCompactCbor: badFixture.compactCbor,
        fieldIndex: 2,
        committedPreimage: Buffer.from([0x80]),
      }),
    ).toThrow(/not the positional commitment/u);
    expect(() =>
      prepareCanonicalDecodabilityV1({
        badTxId: "11".repeat(32),
        nativeTxCompactCbor: badFixture.compactCbor,
        fieldIndex: 2,
        committedPreimage: bad,
      }),
    ).toThrow(/re-derives to/u);
    expect(() =>
      prepareCanonicalDecodabilityV1({
        badTxId: badFixture.txId,
        nativeTxCompactCbor: badFixture.compactCbor,
        fieldIndex: 6,
        committedPreimage: bad,
      }),
    ).toThrow(/no compact witness set/u);
  });
});
