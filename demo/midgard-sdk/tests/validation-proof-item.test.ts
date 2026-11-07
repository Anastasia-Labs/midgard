import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveValidationProofItemPublication,
  minimumLovelaceForValidationProofItemPublication,
  ValidationProofItemDatum,
  type ValidationTraceDisputeFaultProofContracts,
} from "../src/fraud-proof/index.js";

/**
 * #597. The publication's unit is the field's whole §5.1 preimage, not one item
 * with an opening into it: under §4 a field commits to a flat `blake2b_256` over
 * its preimage bytes, so a per-item opening has nothing to be checked against.
 * The Aiken twin is `ValidationProofItemDatum` in
 * `onchain/aiken/lib/midgard/validation-machine/`, and
 * `canonical_decode_item_semantic_v1.proof_item_from_reference` is what reads it
 * back — as `Inline { preimage }`, constructed by the validator rather than
 * named by the prover.
 *
 * Every preimage below comes from `encodeMidgardFieldPreimage`, the §5.1
 * envelope encoder, rather than being spelled by hand here.
 */
const preimageOf = (...items: readonly string[]): string =>
  encodeMidgardFieldPreimage(
    items.map((item) => Buffer.from(item, "hex")),
  ).toString("hex");

const fieldPreimage = preimageOf("8200");

describe("validation proof-item publication V1", () => {
  it("encodes the committed field preimage and its dispute binding", () => {
    const publication = deriveValidationProofItemPublication({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      fieldPreimage,
    });
    expect(publication.datum.field_preimage).toBe(fieldPreimage);
    expect(Data.from(publication.datumCbor, ValidationProofItemDatum)).toEqual(
      publication.datum,
    );
    expect(Data.to(publication.datum, ValidationProofItemDatum)).toBe(
      publication.datumCbor,
    );
  });

  it("rejects malformed identities and preimage bytes before transaction construction", () => {
    expect(() =>
      deriveValidationProofItemPublication({
        transactionId: "33".repeat(31),
        transactionCommitment: "44".repeat(32),
        fieldPreimage,
      }),
    ).toThrow(/transaction id/u);
    expect(() =>
      deriveValidationProofItemPublication({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(31),
        fieldPreimage,
      }),
    ).toThrow(/transaction commitment/u);
    expect(() =>
      deriveValidationProofItemPublication({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        fieldPreimage: "xyz",
      }),
    ).toThrow(/hexadecimal CBOR/u);
    // §5.1's empty field is the one-byte header `80`, so a genuinely empty byte
    // string is a caller's mistake in every case and never a publishable field.
    expect(() =>
      deriveValidationProofItemPublication({
        transactionId: "33".repeat(32),
        transactionCommitment: "44".repeat(32),
        fieldPreimage: "",
      }),
    ).toThrow(/must not be empty/u);
  });

  it("round-trips the publication datum exactly and rejects mutated wire forms", () => {
    const roundTripPreimage = preimageOf("820182018201820082008200", "8201");
    const publication = deriveValidationProofItemPublication({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      fieldPreimage: roundTripPreimage,
    });
    const exactDecode = (cbor: string): ValidationProofItemDatum => {
      const decoded = Data.from(cbor, ValidationProofItemDatum);
      if (Data.to(decoded, ValidationProofItemDatum) !== cbor) {
        throw new Error("validation proof-item datum is not exact wire CBOR");
      }
      return decoded;
    };
    expect(exactDecode(publication.datumCbor).field_preimage).toBe(
      roundTripPreimage,
    );

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
    // A preimage that differs only by item order is a different publication:
    // §5.1's envelope is positional, so the datum cannot be reproduced.
    const reorderedItems = deriveValidationProofItemPublication({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      fieldPreimage: preimageOf("8201", "820182018201820082008200"),
    });
    expect(reorderedItems.datumCbor).not.toBe(publication.datumCbor);
    // Reordered identities bind differently: swapping the transaction id and
    // commitment cannot reproduce the same publication datum.
    const reordered = deriveValidationProofItemPublication({
      transactionId: "44".repeat(32),
      transactionCommitment: "33".repeat(32),
      fieldPreimage: roundTripPreimage,
    });
    expect(reordered.datumCbor).not.toBe(publication.datumCbor);
  });

  it("accepts a field preimage up to the single-publication maximum", () => {
    // The publication cap is measured on the complete signed transaction, and
    // under §8 the bytes it carries are the field's preimage rather than one
    // item — so the cap applies to the whole envelope this builds.
    const maxPreimage = preimageOf(
      "a5".repeat(
        MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes - 8,
      ),
    );
    const publication = deriveValidationProofItemPublication({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      fieldPreimage: maxPreimage,
    });
    const decoded = Data.from(publication.datumCbor, ValidationProofItemDatum);
    expect(decoded.field_preimage).toBe(maxPreimage);
    expect(Buffer.from(maxPreimage, "hex").length).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
    );
  });

  it("stabilizes the exact inline-datum min-Ada from protocol parameters", () => {
    const publication = deriveValidationProofItemPublication({
      transactionId: "33".repeat(32),
      transactionCommitment: "44".repeat(32),
      fieldPreimage,
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
    const lovelace = minimumLovelaceForValidationProofItemPublication({
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
