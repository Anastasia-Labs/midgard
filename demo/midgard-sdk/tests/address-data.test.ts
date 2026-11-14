import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { AddressData, addressDataFromBech32 } from "../src/common.js";

const keyHash = "cd".repeat(28);
const stakeHash = "ef".repeat(28);

/** A testnet key address of `header` type with `tail` after the payment key
 * hash. */
const bech32Address = (header: number, tail: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.from(
      `${header.toString(16).padStart(2, "0")}${keyHash}${tail}`,
      "hex",
    ),
  ).to_bech32();

describe("AddressData", () => {
  it("encodes a pointer stake credential flat, as the ledger does", () => {
    const address: AddressData = {
      paymentCredential: { PublicKeyCredential: [keyHash] },
      stakeCredential: {
        Pointer: { slotNumber: 1n, transactionIndex: 2n, certificateIndex: 3n },
      },
    };
    const ledger = new Constr(0, [
      new Constr(0, [keyHash]),
      new Constr(0, [new Constr(1, [1n, 2n, 3n])]),
    ]);
    expect(Data.to(address, AddressData)).toBe(Data.to(ledger));
    expect(Data.from(Data.to(ledger), AddressData)).toEqual(address);
    // The nested shape is not an address.
    const nested = new Constr(0, [
      new Constr(0, [keyHash]),
      new Constr(0, [new Constr(1, [new Constr(0, [1n, 2n, 3n])])]),
    ]);
    expect(() => Data.from(Data.to(nested), AddressData)).toThrow();
  });
});

describe("addressDataFromBech32", () => {
  it("maps base and enterprise addresses", async () => {
    await expect(
      Effect.runPromise(addressDataFromBech32(bech32Address(0x00, stakeHash))),
    ).resolves.toEqual({
      paymentCredential: { PublicKeyCredential: [keyHash] },
      stakeCredential: { Inline: [{ PublicKeyCredential: [stakeHash] }] },
    });
    await expect(
      Effect.runPromise(addressDataFromBech32(bech32Address(0x60, ""))),
    ).resolves.toEqual({
      paymentCredential: { PublicKeyCredential: [keyHash] },
      stakeCredential: null,
    });
  });

  it("refuses a pointer address, which admission would not accept", async () => {
    // Pointer (slot 1, transaction 2, certificate 3), one byte each.
    const result = await Effect.runPromise(
      Effect.either(addressDataFromBech32(bech32Address(0x40, "010203"))),
    );
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left._tag).toBe("Bech32DeserializationError");
      expect(result.left.message).toMatch(/Pointer addresses/u);
    }
  });
});
