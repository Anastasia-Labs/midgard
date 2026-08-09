import { readFileSync } from "node:fs";

import { MIDGARD_MAX_TIER3_CHUNK_COUNT_V1 } from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { EMPTY_SPEND_INPUTS_HASH } from "../src/fraud-proof/zero-input.js";
import {
  EMPTY_FIELD_COMMITMENT_HEX_V1,
  FIELD_CARRIAGE_V1_CONSTRUCTOR_INDEXES,
  FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_V1_CONSTRUCTOR_INDEXES,
  FIELD_VIEW_V1_CONSTRUCTOR_INDEXES,
  FieldCarriageV1,
  fieldPreimageCertificateAssetNameHexV1,
  FieldPreimageCertificateMintRedeemerV1,
  FieldPreimageCertificateV1,
  FieldViewV1,
} from "../src/native-tx-field-access-v1.js";

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
    expect(FIELD_CARRIAGE_V1_CONSTRUCTOR_INDEXES).toEqual({
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
    const inline = Data.to({ Inline: { preimage: "80" } }, FieldCarriageV1);
    const rawUtxo = Data.to(
      { RawUtxo: { ref_input_index: 3n } },
      FieldCarriageV1,
    );
    const certified = Data.to(
      {
        Certified: {
          cert_ref_input_index: 1n,
          chunk_ref_input_indices: [2n, 3n],
        },
      },
      FieldCarriageV1,
    );

    expect(inline).toBe("d8799f4180ff");
    expect(rawUtxo).toBe("d87a9f03ff");
    expect(certified).toBe("d87b9f019f0203ffff");

    expect(inline.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(rawUtxo.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(certified.startsWith(constructorTagPrefix(2))).toBe(true);

    expect(Data.from(inline, FieldCarriageV1)).toEqual({
      Inline: { preimage: "80" },
    });
    expect(Data.from(rawUtxo, FieldCarriageV1)).toEqual({
      RawUtxo: { ref_input_index: 3n },
    });
    expect(Data.from(certified, FieldCarriageV1)).toEqual({
      Certified: {
        cert_ref_input_index: 1n,
        chunk_ref_input_indices: [2n, 3n],
      },
    });
  });
});

describe("§8.8 FieldViewV1 wire contract", () => {
  it("keeps the frozen constructor order the Aiken door declares", () => {
    expect(aikenConstructorOrder("FieldViewV1")).toEqual(["Whole", "Chunked"]);
    expect(FIELD_VIEW_V1_CONSTRUCTOR_INDEXES).toEqual({
      Whole: 0,
      Chunked: 1,
    });
  });

  it("round-trips both variants at their frozen tags, fields flat", () => {
    const whole = Data.to(
      { Whole: { bytes: "80", count: 0n, stride: 40n } },
      FieldViewV1,
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
      FieldViewV1,
    );
    expect(whole).toBe("d8799f4180001828ff");
    expect(chunked).toBe(`d87a9f9f4180ff9f5820${h32("ab")}ff0000ff`);
    expect(whole.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(chunked.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(Data.from(whole, FieldViewV1)).toEqual({
      Whole: { bytes: "80", count: 0n, stride: 40n },
    });
    expect(Data.from(chunked, FieldViewV1)).toEqual({
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
      "total_length",
      "chunk_digests",
    ]);
  });

  it("round-trips a manifest datum", () => {
    const certificate = {
      owner: h28("11"),
      tx_id: h32("22"),
      field_index: 5n,
      total_length: 16_417n,
      chunk_digests: [h32("33"), h32("44")],
    };
    const encoded = Data.to(certificate, FieldPreimageCertificateV1);
    expect(Data.from(encoded, FieldPreimageCertificateV1)).toEqual(certificate);
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
      FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_V1_CONSTRUCTOR_INDEXES,
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
      FieldPreimageCertificateMintRedeemerV1,
    );
    expect(certify).toBe("d8799f41a141b29f000102ff00ff");
    expect(certify.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(Data.from(certify, FieldPreimageCertificateMintRedeemerV1)).toEqual({
      Certify: {
        compact_cbor: "a1",
        witness_set_compact_cbor: "b2",
        chunk_ref_input_indices: [0n, 1n, 2n],
        output_index: 0n,
      },
    });
  });

  it("encodes Retire as a bare Constr 1", () => {
    const retire = Data.to("Retire", FieldPreimageCertificateMintRedeemerV1);
    expect(retire).toBe("d87a80");
    expect(retire.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(Data.from(retire, FieldPreimageCertificateMintRedeemerV1)).toBe(
      "Retire",
    );
  });

  it("bounds chunk_ref_input_indices at the §8.3 three-chunk ladder", () => {
    // The wire schema is a plain list — the bound is the policy's, not the
    // codec's — so the assertion here is that the *core* constant the builders
    // clamp against is still three, and that a redeemer naming more is a thing
    // an off-chain builder has to refuse rather than something the schema does.
    expect(MIDGARD_MAX_TIER3_CHUNK_COUNT_V1).toBe(3);
    const overlong = Data.to(
      {
        Certify: {
          compact_cbor: "a1",
          witness_set_compact_cbor: "b2",
          chunk_ref_input_indices: [0n, 1n, 2n, 3n],
          output_index: 0n,
        },
      },
      FieldPreimageCertificateMintRedeemerV1,
    );
    expect(overlong.startsWith(constructorTagPrefix(0))).toBe(true);
  });
});

describe("§4/§8.6 values a builder must reproduce", () => {
  it("pins the field-independent empty-field commitment", () => {
    // The cross-language vector the zero-input step-02 validator names.
    expect(EMPTY_FIELD_COMMITMENT_HEX_V1).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
  });

  it("records the zero-input residual #575–#578 and #579 close together", () => {
    // The `fraud_proofs/zero_input/step_02` *source* requires the flat §4
    // commitment. `EMPTY_SPEND_INPUTS_HASH` is the value the *codec* computes,
    // and it is still the counted one: #569 added the flat per-field producers
    // and their cross-language vectors, but the nine-field consumer path
    // (`deriveNativeTxBodyCompact` and its downstream callers) has not
    // re-pointed — that is the Phase-5 family rebind (#575–#578) — and the
    // blueprint has not been regenerated (#579). See the constant's own note
    // for why moving it before both would break the fault-proof emulator.
    // Asserting the gap keeps it visible instead of letting a builder quietly
    // emit a datum the validator rejects.
    expect(EMPTY_SPEND_INPUTS_HASH).not.toBe(EMPTY_FIELD_COMMITMENT_HEX_V1);
    expect(EMPTY_SPEND_INPUTS_HASH).toBe(
      "eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec76",
    );
  });

  it("derives the certificate token name from (tx_id, field_index)", () => {
    const txId = h32("22");
    const names = Array.from({ length: 9 }, (_, fieldIndex) =>
      fieldPreimageCertificateAssetNameHexV1({ txId, fieldIndex }),
    );
    expect(new Set(names).size).toBe(9);
    for (const name of names) {
      expect(name).toMatch(/^[0-9a-f]{64}$/u);
    }
    expect(() =>
      fieldPreimageCertificateAssetNameHexV1({ txId, fieldIndex: 9 }),
    ).toThrow();
    expect(() =>
      fieldPreimageCertificateAssetNameHexV1({
        txId: "22".repeat(31),
        fieldIndex: 0,
      }),
    ).toThrow();
  });
});
