import { hex } from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
  FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  FIELD_VIEW_CONSTRUCTOR_INDEXES,
} from "../dist/index.js";
import {
  certificateFieldHash,
  certificateOwner,
  certificateTxId,
  chunkedViewChunkB,
  digestA,
  digestB,
  digestC,
  emptyFieldPreimage,
  smallCompactCbor,
  smallWitnessSetCompactCbor,
  vectors,
  withTrailingItem,
} from "./generate-native-tx-carriage-wire-v1-goldens.vectors.mjs";

/**
 * Each negative vector names the layer that must refuse it, because "something
 * threw" is not evidence that the right thing threw. `cbor-parse` means the
 * bytes are not one well-formed CBOR item at all and `cbor.deserialise` must
 * answer `None`; `data-cast` means the bytes parse cleanly and it is the cast
 * into the Aiken type that must fail — for those the generated module also
 * asserts the parse *succeeds*, so the `fail` test cannot pass because the
 * vector happened to be malformed CBOR too.
 *
 * `typescript` records what `Data.from` really does rather than what one would
 * like it to do. Every wrong-shape vector throws. The trailing-bytes vectors do
 * **not**: `Data.from` decodes the leading item and discards the rest, so the
 * off-chain decoder is tolerant exactly where the on-chain one is strict. That
 * asymmetry is the finding; the suite pins it as `tolerates-trailing-bytes` and
 * asserts the re-encoding is shorter than the vector, which is the property
 * that still makes the vector detectably not producer output.
 */
export const negativeVectors = [
  {
    label: "carriage_trailing_bytes",
    aikenType: "FieldCarriageV1",
    cborHex: withTrailingItem("carriage_inline_empty_field"),
    reason:
      "a valid `Inline` encoding followed by a complete extra CBOR item; a fail-closed decoder consumes the whole payload or refuses it",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "carriage_constructor_index_out_of_range",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(new Constr(3, [hex(emptyFieldPreimage)])),
    reason:
      "constructor index 3; FieldCarriageV1 declares exactly Inline/RawUtxo/Certified at 0/1/2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "carriage_inline_preimage_as_integer",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(new Constr(0, [5n])),
    reason:
      "`Inline.preimage` is a ByteArray; this vector puts an Integer there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "carriage_inline_extra_field",
    aikenType: "FieldCarriageV1",
    cborHex: Data.to(
      new Constr(0, [hex(emptyFieldPreimage), hex(chunkedViewChunkB)]),
    ),
    reason:
      "`Inline` has arity 1; a decoder that reads the first field and ignores the rest accepts this",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_trailing_bytes",
    aikenType: "FieldViewV1",
    cborHex: withTrailingItem("view_whole_empty_field"),
    reason: "a valid `Whole` encoding followed by a complete extra CBOR item",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "view_constructor_index_out_of_range",
    aikenType: "FieldViewV1",
    cborHex: Data.to(new Constr(3, [hex(emptyFieldPreimage), 0n, 40n])),
    reason:
      "constructor index 3; FieldViewV1 declares exactly Whole/Chunked/ProvisionalWhole at 0/1/2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_whole_missing_stride",
    aikenType: "FieldViewV1",
    cborHex: Data.to(new Constr(0, [hex(emptyFieldPreimage), 0n])),
    reason: "`Whole` has arity 3 (bytes, count, stride); this vector carries 2",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_whole_count_as_bytes",
    aikenType: "FieldViewV1",
    cborHex: Data.to(
      new Constr(0, [hex(emptyFieldPreimage), hex(chunkedViewChunkB), 40n]),
    ),
    reason: "`Whole.count` is an Integer; this vector puts a ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "view_chunked_chunks_as_bytes",
    aikenType: "FieldViewV1",
    cborHex: Data.to(
      new Constr(1, [hex(chunkedViewChunkB), [hex(digestA)], 1n, 40n]),
    ),
    reason:
      "`Chunked.chunks` is a List<ByteArray>; this vector puts a bare ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_trailing_bytes",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: withTrailingItem("certificate_three_chunk_corner"),
    reason:
      "a valid certificate datum followed by a complete extra CBOR item; a datum is one value and a manifest that decodes past its own end is not one",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "certificate_missing_chunk_digests",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        5n,
        hex(certificateFieldHash),
        32_763n,
      ]),
    ),
    reason:
      "the certificate record has 6 fields; dropping `chunk_digests` is the shape that would let a manifest certify nothing",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_missing_field_hash",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        5n,
        32_763n,
        [hex(digestA), hex(digestB), hex(digestC)],
      ]),
    ),
    reason:
      "the pre-#606 5-field shape — a datum without the mint-welded `field_hash` is a certificate the door has no anchored equality to hold, and the frozen wire format refuses it at decode",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "certificate_field_index_as_bytes",
    aikenType: "FieldPreimageCertificateV1",
    cborHex: Data.to(
      new Constr(0, [
        hex(certificateOwner),
        hex(certificateTxId),
        "05",
        hex(certificateFieldHash),
        32_763n,
        [hex(digestA), hex(digestB), hex(digestC)],
      ]),
    ),
    reason:
      "`field_index` is an Integer; a one-byte ByteArray with the same value must not pass for it",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_trailing_bytes",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: withTrailingItem("mint_redeemer_retire"),
    reason: "a valid `Retire` encoding followed by a complete extra CBOR item",
    rejectedBy: { aiken: "cbor-parse", typescript: "tolerates-trailing-bytes" },
  },
  {
    label: "mint_redeemer_constructor_index_out_of_range",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(new Constr(2, [])),
    reason:
      "constructor index 2; the mint redeemer declares exactly Certify/Retire at 0/1, and a third arm is how a burn-only policy would be talked into minting",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_certify_indices_as_bytes",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(
      new Constr(0, [
        0n,
        hex(smallCompactCbor),
        hex(smallWitnessSetCompactCbor),
        hex(chunkedViewChunkB),
        2n,
      ]),
    ),
    reason:
      "`Certify.chunk_ref_input_indices` is a List<Int>; this vector puts a ByteArray there",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_certify_missing_output_index",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(
      new Constr(0, [
        0n,
        hex(smallCompactCbor),
        hex(smallWitnessSetCompactCbor),
        [0n, 1n],
      ]),
    ),
    reason:
      "`Certify` has arity 5; without `output_index` the policy has no named output to check",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
  {
    label: "mint_redeemer_retire_extra_field",
    aikenType: "FieldPreimageCertificateMintRedeemerV1",
    cborHex: Data.to(new Constr(1, [0n])),
    reason:
      "`Retire` has arity 0; a decoder that only checks the constructor index accepts this",
    rejectedBy: { aiken: "data-cast", typescript: "throws" },
  },
];

// ---------------------------------------------------------------------------
// The golden
// ---------------------------------------------------------------------------

export const buildGolden = () => ({
  schema: "midgard-native-tx-carriage-wire-golden",
  version: 1,
  specDocument: "docs/spec/midgard-tx.md",
  generator:
    "demo/midgard-sdk/scripts/generate-native-tx-carriage-wire-v1-goldens.mjs",
  constructorIndexes: {
    FieldCarriageV1: FIELD_CARRIAGE_CONSTRUCTOR_INDEXES,
    FieldViewV1: FIELD_VIEW_CONSTRUCTOR_INDEXES,
    FieldPreimageCertificateMintRedeemerV1:
      FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES,
  },
  vectors: vectors.map((vector) => ({
    label: vector.label,
    aikenType: vector.aikenType,
    value: JSON.parse(
      JSON.stringify(vector.value, (_key, entry) =>
        typeof entry === "bigint" ? `${entry}n` : entry,
      ),
    ),
    cborHex: Data.to(vector.value, vector.schema),
  })),
  negativeVectors: negativeVectors.map((vector) => ({
    label: vector.label,
    aikenType: vector.aikenType,
    cborHex: vector.cborHex,
    reason: vector.reason,
    rejectedBy: vector.rejectedBy,
  })),
});

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

/**
 * `///` doc lines wrapped to the same width the rest of the tree reads at.
 * `aiken fmt` reflows code but never comments, so a generator that emits a
 * paragraph on one line leaves one on one line forever.
 */
export const docComment = (text, width = 74) => {
  const lines = [];
  let current = "";
  for (const word of text.split(/\s+/u)) {
    const candidate = current === "" ? word : `${current} ${word}`;
    if (candidate.length + "/// ".length > width && current !== "") {
      lines.push(`/// ${current}`);
      current = word;
    } else {
      current = candidate;
    }
  }
  if (current !== "") {
    lines.push(`/// ${current}`);
  }
  return lines;
};

export const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];
