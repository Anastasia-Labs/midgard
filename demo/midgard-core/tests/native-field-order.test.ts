import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardAddressWitnessItem,
  encodeMidgardSpendInputItem,
} from "../src/codec/native-tx-field-items.js";
import { midgardFieldItemOrderIsValid } from "../src/codec/native-tx-field-order.js";

// The vectors of `tests-canonical-order.test.ak`
// (`onchain/aiken/lib/midgard/validation-machine-tests/`): the same items in
// the same orders get the same verdict from `canonical_item_order_is_valid`.
const hA = Buffer.alloc(32, 0xaa);
const hB = Buffer.alloc(32, 0xbb);
const input = (txId: Buffer): Buffer =>
  encodeMidgardSpendInputItem({ txId, outputIndex: 0 });
const address = (verificationKey: Buffer, tail: Buffer): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey,
    signature: Buffer.concat([hA, tail]),
  });
const signer = (item: Buffer): Buffer =>
  Buffer.from(blake2b(item.subarray(3, 35), { dkLen: 28 }));

const [lowSigner, highSigner] = [address(hA, hA), address(hB, hA)].sort(
  (left, right) => Buffer.compare(signer(left), signer(right)),
) as [Buffer, Buffer];
const [lowBytes, highBytes] = [address(hA, hA), address(hA, hB)].sort(
  Buffer.compare,
) as [Buffer, Buffer];

describe("CanonicalDecode fixed-field order", () => {
  it.each([0, 1])(
    "field %i: sorted and repeated inputs pass, descending refused",
    (field) => {
      expect(midgardFieldItemOrderIsValid(field, input(hA), input(hB))).toBe(
        true,
      );
      expect(midgardFieldItemOrderIsValid(field, input(hA), input(hA))).toBe(
        true,
      );
      expect(midgardFieldItemOrderIsValid(field, input(hB), input(hA))).toBe(
        false,
      );
    },
  );

  it("field 7: signer hash first, then item bytes, strictly", () => {
    expect(midgardFieldItemOrderIsValid(7, lowSigner, highSigner)).toBe(true);
    expect(midgardFieldItemOrderIsValid(7, highSigner, lowSigner)).toBe(false);
    expect(midgardFieldItemOrderIsValid(7, lowBytes, highBytes)).toBe(true);
    expect(midgardFieldItemOrderIsValid(7, highBytes, lowBytes)).toBe(false);
    expect(midgardFieldItemOrderIsValid(7, lowBytes, lowBytes)).toBe(false);
  });

  it("field 7 orders by signer hash, not by verification key bytes", () => {
    const keyOrder = Buffer.compare(hA, hB) < 0;
    const signerOrder =
      Buffer.compare(signer(address(hA, hA)), signer(address(hB, hA))) < 0;
    expect(
      midgardFieldItemOrderIsValid(7, address(hA, hA), address(hB, hA)),
    ).toBe(signerOrder);
    expect(keyOrder).toBe(true);
  });

  it("leaves every other field unordered", () => {
    for (const field of [2, 3, 4, 5, 6, 8])
      expect(midgardFieldItemOrderIsValid(field, input(hB), input(hA))).toBe(
        true,
      );
  });
});
