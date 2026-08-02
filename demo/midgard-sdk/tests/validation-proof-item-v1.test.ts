import { buildMidgardBoundedItemV1 } from "@al-ft/midgard-core";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveValidationProofItemPublicationV1,
  minimumLovelaceForValidationProofItemPublicationV1,
  ValidationProofItemDatumV1,
  type ValidationTraceDisputeFaultProofContracts,
} from "../src/fraud-proof/index.js";

const makeCollectionProof = (itemCbor: string) => {
  const item = buildMidgardBoundedItemV1({
    fieldIndex: 2,
    itemIndex: 0,
    bytes: Buffer.from(itemCbor, "hex"),
  });
  return {
    version: 1n,
    field_index: 2n,
    item_count: 1n,
    item_index: 0n,
    item_length: BigInt(item.bytes.length),
    item_commitment: item.commitment.toString("hex"),
    frontier: [{ height: 0n, hash: "22".repeat(32) }],
    siblings: [],
  };
};

const collectionProof = makeCollectionProof("8200");

describe("validation proof-item publication V1", () => {
  it("encodes the complete canonical item and its authenticated descriptor", () => {
    const publication = deriveValidationProofItemPublicationV1({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      collectionProof,
      itemCbor: "8200",
    });
    expect(
      Data.from(publication.datumCbor, ValidationProofItemDatumV1),
    ).toEqual(publication.datum);
    expect(Data.to(publication.datum, ValidationProofItemDatumV1)).toBe(
      publication.datumCbor,
    );
  });

  it("rejects malformed identities and item bytes before transaction construction", () => {
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(31),
        transactionCommitment: "44".repeat(32),
        collectionProof,
        itemCbor: "8200",
      }),
    ).toThrow(/transaction id/u);
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof,
        itemCbor: "xyz",
      }),
    ).toThrow(/hexadecimal CBOR/u);
  });

  it("rejects compact length, commitment, and index mismatches", () => {
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: { ...collectionProof, item_length: 3n },
        itemCbor: "8200",
      }),
    ).toThrow(/length/u);
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: {
          ...collectionProof,
          item_commitment: "ff".repeat(32),
        },
        itemCbor: "8200",
      }),
    ).toThrow(/commitment/u);
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: { ...collectionProof, item_index: -1n },
        itemCbor: "8200",
      }),
    ).toThrow(/item index/u);
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: {
          ...collectionProof,
          field_index: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
        },
        itemCbor: "8200",
      }),
    ).toThrow(/field index/u);
  });

  it("round-trips the publication datum exactly and rejects mutated wire forms", () => {
    const roundTripItem = "820182018201820082008200";
    const roundTripProof = makeCollectionProof(roundTripItem);
    const publication = deriveValidationProofItemPublicationV1({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      collectionProof: roundTripProof,
      itemCbor: roundTripItem,
    });
    const exactDecode = (cbor: string): ValidationProofItemDatumV1 => {
      const decoded = Data.from(cbor, ValidationProofItemDatumV1);
      if (Data.to(decoded, ValidationProofItemDatumV1) !== cbor) {
        throw new Error("validation proof-item datum is not exact wire CBOR");
      }
      return decoded;
    };
    expect(exactDecode(publication.datumCbor).item_cbor).toBe(roundTripItem);

    // Trailing data after the exact datum rejects.
    expect(() => exactDecode(`${publication.datumCbor}00`)).toThrow();
    // Substitution anywhere in the datum rejects or changes the decoded
    // identity, so the consuming validator's binding check fails closed.
    const flipped = Buffer.from(publication.datumCbor, "hex");
    flipped[10] = flipped[10]! ^ 0x01;
    const flippedHex = flipped.toString("hex");
    let substitutionRejected = false;
    try {
      const decoded = exactDecode(flippedHex);
      substitutionRejected =
        decoded.transaction_id !== "33".repeat(32) ||
        decoded.transaction_commitment !== "44".repeat(32);
    } catch {
      substitutionRejected = true;
    }
    expect(substitutionRejected).toBe(true);
    // Omission of the item bytes fails before a datum is constructed.
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: roundTripProof,
        itemCbor: "",
      }),
    ).toThrow(/length/u);
    // Duplicated item bytes fail the exact-length guard as well.
    expect(() =>
      deriveValidationProofItemPublicationV1({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        collectionProof: roundTripProof,
        itemCbor: roundTripItem.repeat(2),
      }),
    ).toThrow(/length/u);
    // Reordered identities bind differently: swapping the transaction id and
    // commitment cannot reproduce the same publication datum.
    const reordered = deriveValidationProofItemPublicationV1({
      transactionId: "44".repeat(32),
      transactionCommitment: "33".repeat(32),
      collectionProof: roundTripProof,
      itemCbor: roundTripItem,
    });
    expect(reordered.datumCbor).not.toBe(publication.datumCbor);
  });

  it("accepts the complete canonical item up to the publication maximum without chunk inputs", () => {
    const maxItemHex = "a5".repeat(14_396);
    const maxCollectionProof = makeCollectionProof(maxItemHex);
    const publication = deriveValidationProofItemPublicationV1({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      collectionProof: maxCollectionProof,
      itemCbor: maxItemHex,
    });
    const decoded = Data.from(
      publication.datumCbor,
      ValidationProofItemDatumV1,
    );
    expect(decoded.item_cbor).toBe(maxItemHex);
    expect(decoded.collection_proof.item_commitment).toBe(
      maxCollectionProof.item_commitment,
    );
  });

  it("stabilizes the exact inline-datum min-Ada from protocol parameters", () => {
    const publication = deriveValidationProofItemPublicationV1({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      collectionProof,
      itemCbor: "8200",
    });
    const address = CML.Address.from_raw_bytes(
      Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x55)]),
    ).to_bech32();
    const contracts = {
      validationTraceDispute: {
        proofItem: { spendingScriptAddress: address },
      },
    } as unknown as ValidationTraceDisputeFaultProofContracts;
    const coinsPerUtxoByte = 4_310n;
    const lovelace = minimumLovelaceForValidationProofItemPublicationV1({
      contracts,
      publication,
      coinsPerUtxoByte,
    });
    const requiredAt = (coin: bigint): bigint =>
      CML.min_ada_required(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(coin),
          CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(publication.datumCbor),
          ),
          undefined,
        ),
        coinsPerUtxoByte,
      );
    expect(requiredAt(lovelace)).toBeLessThanOrEqual(lovelace);
    expect(requiredAt(lovelace - 1n)).toBeGreaterThan(lovelace - 1n);
  });
});
