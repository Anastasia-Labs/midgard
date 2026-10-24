import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import {
  aikenBytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  CommittedFieldShapeStep02State,
  sizedMidgardFieldEnvelope,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/committed-field-shape-v1.generated.json",
);

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-committed-field-shape-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

// ---------------------------------------------------------------------------
// Constructions
// ---------------------------------------------------------------------------

const literal = (...values) => ({
  kind: "literal",
  hex: hex(Buffer.from(values)),
});

const envelope = (items) => ({
  kind: "envelope",
  items: items.map((item) => hex(item)),
});

const sized = (totalLength, fill) => ({ kind: "sized", totalLength, fill });

const filler = (length, byte) => Buffer.alloc(length, byte);

/** Build a construction's bytes. The Aiken renderer builds the same three ways. */
export const buildPreimage = (construction) => {
  if (construction.kind === "literal") {
    return Buffer.from(construction.hex, "hex");
  }
  if (construction.kind === "envelope") {
    return encodeMidgardFieldPreimage(
      construction.items.map((item) => Buffer.from(item, "hex")),
    );
  }
  return sizedMidgardFieldEnvelope(construction.totalLength, construction.fill);
};

/** The Aiken expression that builds the same bytes. */
export const renderConstruction = (construction) => {
  if (construction.kind === "literal") {
    return aikenBytes(construction.hex);
  }
  if (construction.kind === "envelope") {
    const items = construction.items.map((item) => aikenBytes(item)).join(", ");
    return `encode_field_preimage([${items}])`;
  }
  return `sized_field_envelope_v1(${String(construction.totalLength)}, ${aikenBytes(
    hex(Buffer.from([construction.fill])),
  )})`;
};

// ---------------------------------------------------------------------------
// Verdict vector inputs
// ---------------------------------------------------------------------------

/** §5.3's fixed strides, so a vector's expected length is arithmetic in view. */
const spendInputStride = midgardFieldStride(0);

const addressWitnessStride = midgardFieldStride(7);

const hash28Stride = midgardFieldStride(3);

/**
 * Each vector is a `(field_index, construction)` pair. The Aiken side rebuilds
 * the bytes from the same construction, proves they are the same bytes by §4's
 * own hash, and recomputes both verdicts — so the two sides are one construction
 * checked twice rather than a construction and a copy of its output.
 */
export const verdictVectors = [
  {
    label: "empty_field_at_a_walked_slot",
    fieldIndex: 2,
    construction: literal(0x80),
    note: "§5.1's empty field; every slot admits it",
  },
  {
    label: "empty_field_at_a_fixed_stride_slot",
    fieldIndex: 0,
    construction: literal(0x80),
    note: "header_len + stride·0 = 1, so the empty field is admissible at slot 0 too",
  },
  {
    label: "one_spend_input",
    fieldIndex: 0,
    construction: envelope([filler(38, 0x00)]),
    note: `slot 0's honest one-item shape, 1 + ${String(spendInputStride)} bytes`,
  },
  {
    label: "two_spend_inputs",
    fieldIndex: 0,
    construction: envelope([filler(38, 0x00), filler(38, 0x11)]),
    note: `slot 0 at two items, 1 + 2·${String(spendInputStride)} bytes`,
  },
  {
    label: "one_required_signer",
    fieldIndex: 4,
    construction: envelope([filler(28, 0x22)]),
    note: `slot 4's honest one-item shape, 1 + ${String(hash28Stride)} bytes`,
  },
  {
    label: "one_address_witness",
    fieldIndex: 7,
    construction: envelope([filler(101, 0x33)]),
    note: `slot 7's honest one-item shape, 1 + ${String(addressWitnessStride)} bytes`,
  },
  {
    label: "four_byte_item_at_a_walked_slot",
    fieldIndex: 2,
    construction: envelope([Buffer.from([0xde, 0xad, 0xbe, 0xef])]),
    note: "a variable-width slot has no stride to fail",
  },
  {
    label: "four_byte_item_at_spend_inputs",
    fieldIndex: 0,
    construction: envelope([Buffer.from([0xde, 0xad, 0xbe, 0xef])]),
    note: "the same bytes at a fixed-stride slot: §7.4's arithmetic refuses them",
  },
  {
    label: "four_byte_item_at_address_witnesses",
    fieldIndex: 7,
    construction: envelope([Buffer.from([0xde, 0xad, 0xbe, 0xef])]),
    note: "and at the fixed-stride slot in the witness-set half",
  },
  {
    label: "four_byte_item_at_reference_inputs",
    fieldIndex: 1,
    construction: envelope([Buffer.from([0xde, 0xad, 0xbe, 0xef])]),
    note: "slot 1 shares slot 0's stride and is a separate row of §5.3's table",
  },
  {
    label: "four_byte_item_at_required_observers",
    fieldIndex: 3,
    construction: envelope([Buffer.from([0xde, 0xad, 0xbe, 0xef])]),
    note: "and slot 3 shares slot 4's, so all five fixed-stride slots are covered",
  },
  {
    label: "one_byte_over_the_spend_input_stride",
    fieldIndex: 0,
    construction: envelope([filler(39, 0x00)]),
    note: "§7.4 is an equality: one byte too many refuses",
  },
  {
    label: "one_byte_under_the_spend_input_stride",
    fieldIndex: 0,
    construction: envelope([filler(37, 0x00)]),
    note: "and one byte too few refuses",
  },
  {
    label: "at_the_field_byte_bound",
    fieldIndex: 2,
    construction: sized(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES, 0x00),
    note: "exactly §5.4's per-field bound, which the door opens",
  },
  {
    label: "above_the_field_byte_bound",
    fieldIndex: 2,
    construction: sized(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
      0x00,
    ),
    note: "one byte above it, and well-formed in every other respect",
  },
  {
    label: "above_the_field_byte_bound_at_a_fixed_stride_slot",
    fieldIndex: 0,
    construction: sized(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
      0x00,
    ),
    note: "the byte bound is checked first, so this is one accusation and not two",
  },
  {
    label: "no_bytes",
    fieldIndex: 0,
    construction: { kind: "literal", hex: "" },
    note: "§12.7's fault: not an envelope, deferred rather than convicted",
  },
  {
    label: "four_byte_array_head",
    fieldIndex: 0,
    construction: literal(0x9a, 0x00, 0x00, 0x00, 0x01),
    note: "well-formed CBOR, outside §5.1's acceptance set — §12.7's",
  },
  {
    label: "trailing_byte_at_a_fixed_stride_slot",
    fieldIndex: 0,
    construction: literal(0x80, 0x41),
    note: "§12.7's fault at a slot whose stride would also refuse it; deferring is what keeps the two kinds disjoint",
  },
  {
    label: "miscounted_at_a_walked_slot",
    fieldIndex: 2,
    construction: literal(0x81, 0x42, 0xde, 0xad, 0x42, 0xbe, 0xef),
    note: "a declared count the body contradicts — §12.7's, at a slot with no stride",
  },
];

// ---------------------------------------------------------------------------
// Wire vector inputs
// ---------------------------------------------------------------------------

const badTxId = Buffer.alloc(32, 0x22);

/**
 * The one Data-encoded surface this family adds. `CommittedFieldClaim` is
 * §12.7's type reused unchanged and is pinned by §12.7's own channel; re-pinning
 * it here would be a second copy of one wire form. What is new is the state,
 * whose three members read identically to §12.7's and whose `verdict` means
 * something else — which is exactly why it is a separate type on both sides.
 */
export const wireVectors = [
  {
    label: "state_wrong_stride",
    aikenType: "State",
    schema: CommittedFieldShapeStep02State,
    value: { bad_tx_id: hex(badTxId), field_index: 0n, verdict: 3n },
    aiken: [
      "State {",
      `  bad_tx_id: ${aikenBytes(hex(badTxId))},`,
      "  field_index: 0,",
      "  verdict: 3,",
      "}",
    ].join("\n"),
  },
  {
    label: "state_field_byte_bound",
    aikenType: "State",
    schema: CommittedFieldShapeStep02State,
    value: { bad_tx_id: hex(badTxId), field_index: 2n, verdict: 2n },
    aiken: [
      "State {",
      `  bad_tx_id: ${aikenBytes(hex(badTxId))},`,
      "  field_index: 2,",
      "  verdict: 2,",
      "}",
    ].join("\n"),
  },
  {
    label: "state_not_an_envelope",
    aikenType: "State",
    schema: CommittedFieldShapeStep02State,
    value: { bad_tx_id: hex(badTxId), field_index: 8n, verdict: 1n },
    aiken: [
      "State {",
      `  bad_tx_id: ${aikenBytes(hex(badTxId))},`,
      "  field_index: 8,",
      "  verdict: 1,",
      "}",
    ].join("\n"),
  },
];
