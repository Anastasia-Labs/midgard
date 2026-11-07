import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenBytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  CanonicalDecodabilityStep02State,
  CommittedFieldClaim,
  miscountedMidgardFieldPreimage,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/canonical-decodability-v1.generated.json",
);

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-canonical-decodability-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

// ---------------------------------------------------------------------------
// Verdict vector inputs
// ---------------------------------------------------------------------------

const bytes = (...values) => Buffer.from(values);

const filler = (length) => Buffer.alloc(length, 0x5a);

/** The honest §5.1 producer, spelled here so the two twins share no code. */
const envelope = (items) => miscountedMidgardFieldPreimage(items.length, items);

/**
 * Each vector is built from **inputs** — a declared count and a list of items,
 * or an explicit head — and never transcribed from a hex string someone read
 * off a failing test. The Aiken side then recomputes the verdict from the same
 * bytes, so the two sides are one construction checked twice rather than a
 * construction and a copy of its output.
 */
export const verdictVectors = [
  {
    label: "empty_field",
    preimage: bytes(0x80),
    note: "§5.1's empty field, and the shortest admissible preimage there is",
  },
  {
    label: "one_item",
    preimage: envelope([bytes(0xde, 0xad, 0xbe, 0xef)]),
    note: "the ordinary shape",
  },
  {
    label: "twenty_three_items",
    preimage: envelope(Array.from({ length: 23 }, () => bytes(0x00))),
    note: "the widest one-byte array header, `97`",
  },
  {
    label: "twenty_four_items",
    preimage: envelope(Array.from({ length: 24 }, () => bytes(0x00))),
    note: "the narrowest two-byte array header, `98 18`",
  },
  {
    label: "two_hundred_fifty_six_items",
    preimage: envelope(Array.from({ length: 256 }, () => bytes(0x00))),
    note: "the narrowest three-byte array header, `99 0100`",
  },
  {
    label: "twenty_four_byte_item",
    preimage: envelope([filler(24)]),
    note: "the narrowest two-byte item header, `58 18`",
  },
  {
    label: "two_hundred_fifty_six_byte_item",
    preimage: envelope([filler(256)]),
    note: "the narrowest three-byte item header, `59 0100`",
  },
  {
    label: "no_bytes",
    preimage: Buffer.alloc(0),
    note: "one byte short of the empty field",
  },
  {
    label: "four_byte_array_head",
    preimage: bytes(0x9a, 0x00, 0x00, 0x00, 0x01),
    note: "well-formed CBOR, outside §5.1's acceptance set",
  },
  {
    label: "four_byte_item_head",
    preimage: bytes(0x81, 0x5a, 0x00, 0x00, 0x00, 0x01, 0x00),
    note: "the same exclusion at the item head",
  },
  {
    label: "non_minimal_array_head_at_23",
    preimage: bytes(0x98, 0x17),
    note: "23 items spelled in the two-byte form",
  },
  {
    label: "non_minimal_array_head_at_255",
    preimage: bytes(0x99, 0x00, 0xff),
    note: "255 items spelled in the three-byte form",
  },
  {
    label: "truncated_array_head_two_byte",
    preimage: bytes(0x98),
    note: "a `98` head with no count byte",
  },
  {
    label: "truncated_array_head_three_byte",
    preimage: bytes(0x99, 0x00),
    note: "a `99` head with one of its two count bytes",
  },
  {
    label: "declares_more_items_than_it_carries",
    preimage: miscountedMidgardFieldPreimage(2, [bytes(0xde, 0xad)]),
    note: "the walk runs out of preimage before it runs out of declared items",
  },
  {
    label: "declares_one_item_and_carries_none",
    preimage: miscountedMidgardFieldPreimage(1, []),
    note: "the same, at the smallest cardinality there is",
  },
  {
    label: "item_head_wrong_major",
    preimage: bytes(0x81, 0x00),
    note: "a CBOR uint where §5.1 requires a byte-string head",
  },
  {
    label: "non_minimal_item_head_at_23",
    preimage: bytes(0x81, 0x58, 0x17),
    note: "a 23-byte item spelled in the two-byte form",
  },
  {
    label: "non_minimal_item_head_at_255",
    preimage: bytes(0x81, 0x59, 0x00, 0xff),
    note: "a 255-byte item spelled in the three-byte form",
  },
  {
    label: "truncated_item_head_two_byte",
    preimage: bytes(0x81, 0x58),
    note: "a `58` item head with no length byte",
  },
  {
    label: "truncated_item_head_three_byte",
    preimage: bytes(0x81, 0x59, 0x01),
    note: "a `59` item head with one of its two length bytes",
  },
  {
    label: "truncated_item_payload",
    preimage: bytes(0x81, 0x42, 0xff),
    note: "an item declaring two payload bytes with one left",
  },
  {
    label: "truncated_item_payload_at_the_end",
    preimage: Buffer.concat([envelope([bytes(0x01)]), bytes(0x42, 0xff)]),
    note: "a complete first item and a truncated second — the walk gets past item 0 before it fails",
  },
  {
    label: "declares_fewer_items_than_it_carries",
    preimage: miscountedMidgardFieldPreimage(1, [
      bytes(0xde, 0xad),
      bytes(0xbe, 0xef),
    ]),
    note: "the preimage runs out of walk before it runs out of bytes",
  },
  {
    label: "trailing_byte_after_the_empty_field",
    preimage: bytes(0x80, 0x41),
    note: "the smallest trailing-content vector there is",
  },
];

// ---------------------------------------------------------------------------
// Wire vector inputs
// ---------------------------------------------------------------------------

const repeatedByte = (byte, length) => Buffer.alloc(length, byte);

const badTxId = repeatedByte(0x22, 32);

const witnessSetHashes = {
  addr_tx_wits_hash: hex(repeatedByte(0x31, 32)),
  script_tx_wits_hash: hex(repeatedByte(0x32, 32)),
  redeemer_tx_wits_hash: hex(repeatedByte(0x33, 32)),
};

/**
 * The two Data-encoded surfaces this family adds. Both are new types rather
 * than moved ones, so what these vectors pin is the shape the two sides agree
 * on from the start — including the one thing a reader cannot check by
 * inspection, which is that `CommittedFieldClaim`'s constructor order is
 * `BodyFieldClaim` 0 and `WitnessFieldClaim` 1 on both sides.
 */
export const wireVectors = [
  {
    label: "claim_body_inline",
    aikenType: "CommittedFieldClaimV1",
    schema: CommittedFieldClaim,
    value: {
      BodyFieldClaim: {
        field_index: 2n,
        carriage: { Inline: { preimage: hex(bytes(0x80, 0x41)) } },
      },
    },
    aiken: [
      "BodyFieldClaim {",
      "  field_index: 2,",
      `  carriage: Inline { preimage: ${aikenBytes(hex(bytes(0x80, 0x41)))} },`,
      "}",
    ].join("\n"),
  },
  {
    label: "claim_body_certified",
    aikenType: "CommittedFieldClaimV1",
    schema: CommittedFieldClaim,
    value: {
      BodyFieldClaim: {
        field_index: 0n,
        carriage: {
          Certified: {
            cert_ref_input_index: 2n,
            chunk_ref_input_indices: [3n, 4n, 5n],
          },
        },
      },
    },
    aiken: [
      "BodyFieldClaim {",
      "  field_index: 0,",
      "  carriage: Certified {",
      "    cert_ref_input_index: 2,",
      "    chunk_ref_input_indices: [3, 4, 5],",
      "  },",
      "}",
    ].join("\n"),
  },
  {
    label: "claim_witness_raw_utxo",
    aikenType: "CommittedFieldClaimV1",
    schema: CommittedFieldClaim,
    value: {
      WitnessFieldClaim: {
        field_index: 6n,
        witness_set: witnessSetHashes,
        carriage: { RawUtxo: { ref_input_index: 2n } },
      },
    },
    aiken: [
      "WitnessFieldClaim {",
      "  field_index: 6,",
      "  witness_set: NativeTxWitnessSetCompact {",
      `    addr_tx_wits_hash: ${aikenBytes(witnessSetHashes.addr_tx_wits_hash)},`,
      `    script_tx_wits_hash: ${aikenBytes(witnessSetHashes.script_tx_wits_hash)},`,
      `    redeemer_tx_wits_hash: ${aikenBytes(witnessSetHashes.redeemer_tx_wits_hash)},`,
      "  },",
      "  carriage: RawUtxo { ref_input_index: 2 },",
      "}",
    ].join("\n"),
  },
  {
    label: "state_convicting",
    aikenType: "State",
    schema: CanonicalDecodabilityStep02State,
    value: {
      bad_tx_id: hex(badTxId),
      field_index: 2n,
      verdict: 10n,
    },
    aiken: [
      "State {",
      `  bad_tx_id: ${aikenBytes(hex(badTxId))},`,
      "  field_index: 2,",
      "  verdict: 10,",
      "}",
    ].join("\n"),
  },
  {
    label: "state_grammatical",
    aikenType: "State",
    schema: CanonicalDecodabilityStep02State,
    value: {
      bad_tx_id: hex(badTxId),
      field_index: 8n,
      verdict: 0n,
    },
    aiken: [
      "State {",
      `  bad_tx_id: ${aikenBytes(hex(badTxId))},`,
      "  field_index: 8,",
      "  verdict: 0,",
      "}",
    ].join("\n"),
  },
];
