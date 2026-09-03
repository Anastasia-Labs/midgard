import { readFileSync } from "node:fs";

import { MIDGARD_MAX_TIER3_CHUNK_COUNT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { EMPTY_SPEND_INPUTS_HASH } from "../src/fraud-proof/zero-input.js";
import {
  EMPTY_FIELD_COMMITMENT_HEX,
  FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  FIELD_VIEW_CONSTRUCTOR_INDEXES,
  FieldCarriage,
  FieldPreimageCertificate,
  FieldPreimageCertificateMintRedeemer,
  FieldView,
} from "../src/native-tx-field-access.js";

/**
 * The off-chain half of the §8.8 wire contract. It cannot execute Plutus, so it
 * never claims to prove the door's behaviour — the Aiken family in
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.test.ak` does that. What
 * it proves is that the two sides agree on the wire: the constructor order the
 * door decodes, the record field order the certificate datum carries, and the
 * §4/§8.6 values a builder has to reproduce.
 */

const repositoryRoot = new URL("../../../", import.meta.url);
const AIKEN_MODULE = "onchain/aiken/lib/midgard/native-tx-field-access-v1.ak";
/**
 * §8.6's producer half lives with the carriage producer rather than behind the
 * access door, so the mint redeemer's declaration order is read from a second
 * module.
 */
const AIKEN_CARRIAGE_MODULE =
  "onchain/aiken/lib/midgard/native-tx-carriage-v1.ak";

const readRepositoryFile = (relativePath: string): string =>
  readFileSync(new URL(relativePath, repositoryRoot), "utf8");

/**
 * Constructor names of an Aiken sum type, in declaration order.
 *
 * Read out of the Aiken source rather than restated here. Plutus tags a
 * constructor by its *position*, so reordering the declaration silently
 * re-points every encoded carriage at a different tier — a change no type
 * checker on either side would catch.
 *
 * Nullary constructors (`Retire`) are matched as well as record ones, because a
 * parser that only saw `Name {` would read a two-variant sum as a one-variant
 * one and agree with any tag assignment at all.
 */
const aikenConstructorOrderIn = (
  module: string,
  typeName: string,
): readonly string[] => {
  const source = readRepositoryFile(module);
  const opening = new RegExp(`^pub type ${typeName} \\{$`, "mu").exec(source);
  if (opening?.index === undefined) {
    throw new Error(`${typeName} is no longer declared in ${module}`);
  }
  const body = source.slice(opening.index + opening[0].length);
  const end = body.indexOf("\n}");
  if (end === -1) {
    throw new Error(`${typeName} has no closing brace`);
  }
  return [
    ...body.slice(0, end).matchAll(/^ {2}([A-Z][A-Za-z0-9]*)(?: \{|\s*$)/gmu),
  ].map((match) => match[1] as string);
};

const aikenConstructorOrder = (typeName: string): readonly string[] =>
  aikenConstructorOrderIn(AIKEN_MODULE, typeName);

/** Plutus constructor tag for index `i` (i < 7): 121 + i, CBOR tag `d879 + i`. */
const constructorTagPrefix = (index: number): string =>
  `d8${(0x79 + index).toString(16)}`;

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);

describe("§8.8 FieldCarriageV1 wire contract", () => {
  it("keeps the frozen constructor order the Aiken door declares", () => {
    expect(aikenConstructorOrder("FieldCarriageV1")).toEqual([
      "Inline",
      "RawUtxo",
      "Certified",
    ]);
    expect(FIELD_CARRIAGE_CONSTRUCTOR_INDEXES).toEqual({
      Inline: 0,
      RawUtxo: 1,
      Certified: 2,
    });
  });

  it("encodes each tier at its frozen tag with the fields flat", () => {
    // Exact CBOR, not a prefix check. Aiken's variants carry *named* fields, so
    // the payload sits directly in the constructor: `Inline { preimage }` is
    // `Constr 0 [B]`, never `Constr 0 [Constr 0 [B]]`. A `Data.Tuple` payload
    // would produce the second shape and this assertion is what catches it.
    const inline = Data.to({ Inline: { preimage: "80" } }, FieldCarriage);
    const rawUtxo = Data.to(
      { RawUtxo: { ref_input_index: 3n } },
      FieldCarriage,
    );
    const certified = Data.to(
      {
        Certified: {
          cert_ref_input_index: 1n,
          chunk_ref_input_indices: [2n, 3n],
        },
      },
      FieldCarriage,
    );

    expect(inline).toBe("d8799f4180ff");
    expect(rawUtxo).toBe("d87a9f03ff");
    expect(certified).toBe("d87b9f019f0203ffff");

    expect(inline.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(rawUtxo.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(certified.startsWith(constructorTagPrefix(2))).toBe(true);

    expect(Data.from(inline, FieldCarriage)).toEqual({
      Inline: { preimage: "80" },
    });
    expect(Data.from(rawUtxo, FieldCarriage)).toEqual({
      RawUtxo: { ref_input_index: 3n },
    });
    expect(Data.from(certified, FieldCarriage)).toEqual({
      Certified: {
        cert_ref_input_index: 1n,
        chunk_ref_input_indices: [2n, 3n],
      },
    });
  });
});

describe("§8.8 FieldViewV1 wire contract", () => {
  it("keeps the frozen constructor order the Aiken door declares", () => {
    expect(aikenConstructorOrder("FieldViewV1")).toEqual([
      "Whole",
      "Chunked",
      "ProvisionalWhole",
    ]);
    expect(FIELD_VIEW_CONSTRUCTOR_INDEXES).toEqual({
      Whole: 0,
      Chunked: 1,
      ProvisionalWhole: 2,
    });
  });

  it("round-trips both variants at their frozen tags, fields flat", () => {
    const whole = Data.to(
      { Whole: { bytes: "80", count: 0n, stride: 40n } },
      FieldView,
    );
    const chunked = Data.to(
      {
        Chunked: {
          chunks: ["80"],
          chunk_digests: [h32("ab")],
          count: 0n,
          stride: 0n,
        },
      },
      FieldView,
    );
    expect(whole).toBe("d8799f4180001828ff");
    expect(chunked).toBe(`d87a9f9f4180ff9f5820${h32("ab")}ff0000ff`);
    expect(whole.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(chunked.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(Data.from(whole, FieldView)).toEqual({
      Whole: { bytes: "80", count: 0n, stride: 40n },
    });
    expect(Data.from(chunked, FieldView)).toEqual({
      Chunked: {
        chunks: ["80"],
        chunk_digests: [h32("ab")],
        count: 0n,
        stride: 0n,
      },
    });
  });
});

describe("§8.6 FieldPreimageCertificateV1 datum", () => {
  it("keeps the Aiken record's field order", () => {
    const source = readRepositoryFile(AIKEN_MODULE);
    const declaration =
      /^pub type FieldPreimageCertificateV1 \{\n([\s\S]*?)\n\}/mu.exec(source);
    expect(declaration).not.toBeNull();
    const fields = [...declaration![1].matchAll(/^ {2}([a-z_]+):/gmu)].map(
      (match) => match[1],
    );
    expect(fields).toEqual([
      "owner",
      "tx_id",
      "field_index",
      "field_hash",
      "total_length",
      "chunk_digests",
    ]);
  });

  it("round-trips a manifest datum", () => {
    const certificate = {
      owner: h28("11"),
      tx_id: h32("22"),
      field_index: 5n,
      field_hash: h32("66"),
      total_length: 16_417n,
      chunk_digests: [h32("33"), h32("44")],
    };
    const encoded = Data.to(certificate, FieldPreimageCertificate);
    expect(Data.from(encoded, FieldPreimageCertificate)).toEqual(certificate);
  });
});

describe("§8.6 FieldPreimageCertificateMintRedeemerV1 wire contract", () => {
  it("keeps the frozen Certify/Retire order the Aiken producer declares", () => {
    expect(
      aikenConstructorOrderIn(
        AIKEN_CARRIAGE_MODULE,
        "FieldPreimageCertificateMintRedeemerV1",
      ),
    ).toEqual(["Certify", "Retire"]);
    expect(
      FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
    ).toEqual({ Certify: 0, Retire: 1 });
  });

  it("keeps the Aiken Certify arm's field order", () => {
    const source = readRepositoryFile(AIKEN_CARRIAGE_MODULE);
    const declaration = /^ {2}Certify \{\n([\s\S]*?)\n {2}\}/mu.exec(source);
    expect(declaration).not.toBeNull();
    const fields = [...declaration![1].matchAll(/^ {4}([a-z_]+):/gmu)].map(
      (match) => match[1],
    );
    expect(fields).toEqual([
      "compact_cbor",
      "witness_set_compact_cbor",
      "chunk_ref_input_indices",
      "output_index",
    ]);
  });

  it("encodes Certify at Constr 0 with its four fields flat", () => {
    // Exact CBOR, not a prefix check — the same `Data.Object` vs `Data.Tuple`
    // hazard the §8.8 carriage assertions guard. `Certify` carries named
    // fields, so its payload sits directly in the constructor.
    const certify = Data.to(
      {
        Certify: {
          compact_cbor: "a1",
          witness_set_compact_cbor: "b2",
          chunk_ref_input_indices: [0n, 1n, 2n],
          output_index: 0n,
        },
      },
      FieldPreimageCertificateMintRedeemer,
    );
    expect(certify).toBe("d8799f41a141b29f000102ff00ff");
    expect(certify.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(Data.from(certify, FieldPreimageCertificateMintRedeemer)).toEqual({
      Certify: {
        compact_cbor: "a1",
        witness_set_compact_cbor: "b2",
        chunk_ref_input_indices: [0n, 1n, 2n],
        output_index: 0n,
      },
    });
  });

  it("encodes Retire as a bare Constr 1", () => {
    const retire = Data.to("Retire", FieldPreimageCertificateMintRedeemer);
    expect(retire).toBe("d87a80");
    expect(retire.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(Data.from(retire, FieldPreimageCertificateMintRedeemer)).toBe(
      "Retire",
    );
  });

  it("bounds chunk_ref_input_indices at the §8.3 three-chunk ladder", () => {
    // The wire schema is a plain list — the bound is the policy's, not the
    // codec's — so the assertion here is that the *core* constant the builders
    // clamp against is still three, and that a redeemer naming more is a thing
    // an off-chain builder has to refuse rather than something the schema does.
    expect(MIDGARD_MAX_TIER3_CHUNK_COUNT).toBe(3);
    const overlong = Data.to(
      {
        Certify: {
          compact_cbor: "a1",
          witness_set_compact_cbor: "b2",
          chunk_ref_input_indices: [0n, 1n, 2n, 3n],
          output_index: 0n,
        },
      },
      FieldPreimageCertificateMintRedeemer,
    );
    expect(overlong.startsWith(constructorTagPrefix(0))).toBe(true);
  });
});

describe("§4/§8.6 values a builder must reproduce", () => {
  it("pins the field-independent empty-field commitment", () => {
    // The cross-language vector the zero-input step-02 validator names.
    expect(EMPTY_FIELD_COMMITMENT_HEX).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
  });

  it("computes the empty spend-inputs hash as the flat §4 empty-field commitment", () => {
    // The convergence #585 exists to produce. `fraud_proofs/zero_input/step_02`
    // compares its `bad_tx_spend_inputs_hash` against
    // `native_tx_field_access_v1.empty_field_commitment`, and
    // `EMPTY_SPEND_INPUTS_HASH` is what the codec computes for a body that spends
    // nothing — before the nine-field consumer swap the two disagreed
    // (`eb25ed4a…` against `45b0cfc2…`) and a builder could emit a datum the
    // validator rejects.
    //
    // They now agree by construction: `deriveNativeTxBodyCompact` derives §4's
    // flat `blake2b_256` over the §5.1 preimage, and the empty field is exactly
    // `80`. Asserting equality rather than the gap is what keeps a regression from
    // silently re-opening it. The blueprint that pins the on-chain half is
    // regenerated once, in #579.
    expect(EMPTY_SPEND_INPUTS_HASH).toBe(EMPTY_FIELD_COMMITMENT_HEX);
  });

  it("pins the constant certificate token name (#606)", () => {
    // One constant for every certificate of the policy — ASCII
    // "MIDGARD_FIELD_PREIMAGE_CERT", 27 bytes. The identity the retired
    // per-(tx_id, field_index) derivation carried lives in the datum, which
    // now also welds the §4 commitment (`field_hash`).
    expect(FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX).toBe(
      Buffer.from("MIDGARD_FIELD_PREIMAGE_CERT", "ascii").toString("hex"),
    );
    expect(FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX).toMatch(
      /^[0-9a-f]{54}$/u,
    );
  });
});
