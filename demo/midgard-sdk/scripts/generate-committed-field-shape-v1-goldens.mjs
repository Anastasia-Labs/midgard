#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the `committed-field-shape`
 * family of `docs/spec/midgard-tx.md` §12.8 — the `(field_index, preimage)`
 * shape verdict and the wire type its two steps carry.
 *
 * **Why this channel exists at all.** §12.8's verdict decides whether a block is
 * slashed, and it decides it from two arguments rather than one. Two independent
 * implementations of a two-argument decision procedure drift in a way a
 * one-argument one cannot: a twin that transposes the slot, or reads §5.3's
 * stride table one row differently, agrees with its partner on most inputs and
 * disagrees on exactly the ones the fault kind exists for. Every vector below is
 * therefore a `(slot, bytes)` pair and both sides recompute the verdict from it.
 *
 * **The channel also enforces the partition against §12.7.** Each vector carries
 * the §12.7 envelope verdict alongside this section's, and the generator refuses
 * to emit a set in which any vector is convicted by both fault kinds or by
 * neither-when-the-door-refuses. A cross-section boundary that lives only in
 * prose is a boundary one side can drift out of silently.
 *
 * **Why vectors are built rather than transcribed.** Two of the shapes this
 * family adjudicates are 32,768 and 32,769 bytes long. A hex literal for those
 * would be a 65 KB string in two files that nobody can read and neither side
 * could check for transposition. Each vector therefore carries a *construction*
 * — a literal for the small ones, `sizedFieldEnvelope(totalLength, fill)` for the
 * large — and the byte-level agreement is proved by a `blake2b_256` commitment
 * over the built bytes, which both sides recompute. That is §4's own hash, so
 * the check is the one the door itself would make.
 *
 * Two artifacts, the pair every channel emits:
 *
 *   * `demo/midgard-sdk/tests/fixtures/committed-field-shape-v1.generated.json`
 *     — recomputed by `tests/committed-field-shape.test.ts`, so a drifting
 *     TypeScript twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a divergence
 *     between the two verdicts fails on the Aiken side.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the producers emit today.
 *
 * usage: node scripts/generate-committed-field-shape-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Data } from "@lucid-evolution/lucid";

import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldCommitment,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import {
  CommittedFieldShapeStep02State,
  isCommittedFieldShapeViolation,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_NAMES,
  MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT,
  MIDGARD_FIELD_SHAPE_VERDICT_NAMES,
  MIDGARD_FIXED_STRIDE_FIELD_INDICES,
  midgardCommittedFieldShapeVerdict,
  midgardEnvelopeVerdict,
  sizedMidgardFieldEnvelope,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/committed-field-shape-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-committed-field-shape-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

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
const buildPreimage = (construction) => {
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
const renderConstruction = (construction) => {
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
const verdictVectors = [
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
const wireVectors = [
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

// ---------------------------------------------------------------------------
// The golden
// ---------------------------------------------------------------------------

const buildGolden = () => {
  const vectors = verdictVectors.map((vector) => {
    const preimage = buildPreimage(vector.construction);
    const verdict = midgardCommittedFieldShapeVerdict(
      vector.fieldIndex,
      preimage,
    );
    const envelopeVerdict = midgardEnvelopeVerdict(preimage);
    // The partition against §12.7, checked where it is made. A vector convicted
    // by both fault kinds would let one committed field finalize twice, and a
    // channel that merely recorded the pair would regenerate cleanly around it.
    const convicts = isCommittedFieldShapeViolation({
      fieldIndex: vector.fieldIndex,
      verdict,
    });
    if (convicts && envelopeVerdict !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) {
      throw new Error(
        `vector ${vector.label} is convicted by both §12.7 and §12.8`,
      );
    }
    if (
      envelopeVerdict !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL &&
      verdict !== 1
    ) {
      throw new Error(
        `vector ${vector.label} leaves §5.1 and is not deferred to §12.7`,
      );
    }
    return {
      label: vector.label,
      note: vector.note,
      fieldIndex: vector.fieldIndex,
      fieldStride: midgardFieldStride(vector.fieldIndex),
      construction: vector.construction,
      byteCount: preimage.length,
      preimageCommitment: hex(midgardFieldCommitment(preimage)),
      envelopeVerdict,
      envelopeVerdictName: MIDGARD_ENVELOPE_VERDICT_NAMES[envelopeVerdict],
      verdict,
      verdictName: MIDGARD_FIELD_SHAPE_VERDICT_NAMES[verdict],
      convicts,
    };
  });
  const reached = new Set(vectors.map((vector) => vector.verdict));
  if (reached.size !== MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT) {
    throw new Error(
      `verdict vectors reach ${String(reached.size)} of ` +
        `${String(MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT)} codes`,
    );
  }
  // Every fixed-stride slot has to appear, or a stride table that dropped a row
  // would still regenerate cleanly.
  const slots = new Set(vectors.map((vector) => vector.fieldIndex));
  for (const fieldIndex of MIDGARD_FIXED_STRIDE_FIELD_INDICES) {
    if (!slots.has(fieldIndex)) {
      throw new Error(`no vector at fixed-stride slot ${String(fieldIndex)}`);
    }
  }
  return {
    schema: "midgard-committed-field-shape-golden",
    version: 1,
    specDocument: "docs/spec/midgard-tx.md",
    generator:
      "demo/midgard-sdk/scripts/generate-committed-field-shape-v1-goldens.mjs",
    verdictCodeCount: MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT,
    verdictNames: [...MIDGARD_FIELD_SHAPE_VERDICT_NAMES],
    fixedStrideFieldIndices: [...MIDGARD_FIXED_STRIDE_FIELD_INDICES],
    fieldByteBound: MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    vectors,
    wireVectors: wireVectors.map((vector) => ({
      label: vector.label,
      aikenType: vector.aikenType,
      value: JSON.parse(
        JSON.stringify(vector.value, (_key, entry) =>
          typeof entry === "bigint" ? `${entry}n` : entry,
        ),
      ),
      cborHex: Data.to(vector.value, vector.schema),
    })),
  };
};

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

const docComment = (text, width = 74) => {
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

const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

const renderAiken = (golden) =>
  [
    "//// Generated by",
    `//// ${golden.generator}.`,
    "//// Do not edit; regenerate from the TypeScript twins.",
    "////",
    "//// The §12.8 cross-language goldens. Two sets, and they answer different",
    "//// questions.",
    "////",
    "//// **The verdict set** is the one that matters, and every vector in it is a",
    "//// `(slot, bytes)` pair rather than a byte string. §12.8's verdict is a total",
    "//// decision procedure of two arguments implemented twice — here and in",
    "//// `demo/midgard-sdk/src/fraud-proof/committed-field-shape.ts` — and a twin",
    "//// that transposed the slot or read §5.3's stride table one row differently",
    "//// would agree with its partner on most inputs and disagree on exactly the",
    "//// ones the fault kind exists for. Each test rebuilds the bytes from the same",
    "//// construction the TypeScript side used, proves they are the same bytes by",
    "//// §4's own `blake2b_256`, and then asserts both verdicts: this section's, and",
    "//// §12.7's, because the partition between the two fault kinds is a property",
    "//// of the pair and not of either alone.",
    "////",
    "//// **The wire set** round-trips the one Data-encoded surface the family adds:",
    "//// each test decodes the TypeScript producer's bytes into the Aiken type and",
    "//// then re-serialises what came back. `CommittedFieldClaim` is §12.7's type",
    "//// reused unchanged and is pinned by §12.7's own channel.",
    "",
    "use aiken/cbor",
    "use aiken/primitive/bytearray",
    "use midgard/fraud_proofs/canonical_decodability/rule.{envelope_verdict_v1}",
    "use midgard/fraud_proofs/committed_field_shape/rule.{",
    "  committed_field_shape_verdict_v1, sized_field_envelope_v1,",
    "} as committed_field_shape_rule",
    "use midgard/fraud_proofs/committed_field_shape/step_02.{State}",
    "use midgard/native_tx_field_access_v1.{",
    "  encode_field_preimage, field_commitment,",
    "}",
    "",
    ...section("Verdict vectors"),
    ...golden.vectors.flatMap((vector) => [
      ...docComment(
        `\`${vector.label}\` — ${vector.note}. Slot ${String(vector.fieldIndex)} (stride ${String(vector.fieldStride)}), ${String(vector.byteCount)} bytes, §12.8 verdict ${String(vector.verdict)} (\`${vector.verdictName}\`), §12.7 verdict ${String(vector.envelopeVerdict)} (\`${vector.envelopeVerdictName}\`).`,
      ),
      `test committed_field_shape_golden_${vector.label}() {`,
      `  let preimage = ${renderConstruction(vector.construction)}`,
      "  and {",
      `    bytearray.length(preimage) == ${String(vector.byteCount)},`,
      `    field_commitment(preimage) == ${aikenBytes(vector.preimageCommitment)},`,
      `    committed_field_shape_verdict_v1(${String(vector.fieldIndex)}, preimage) == ${String(vector.verdict)},`,
      `    envelope_verdict_v1(preimage) == ${String(vector.envelopeVerdict)},`,
      "  }",
      "}",
      "",
    ]),
    ...section("Wire vectors"),
    ...golden.wireVectors.flatMap((vector, index) => {
      const source = wireVectors[index];
      return [
        `const ${vector.label}_cbor: ByteArray =`,
        `  ${aikenBytes(vector.cborHex)}`,
        "",
        `fn ${vector.label}_value() -> ${vector.aikenType} {`,
        ...source.aiken.split("\n").map((line) => `  ${line}`),
        "}",
        "",
        `/// ${vector.aikenType} / \`${vector.label}\`: decode the TypeScript`,
        "/// producer's bytes, then re-serialise what came back.",
        `test committed_field_shape_golden_${vector.label}_round_trips() {`,
        `  expect Some(decoded_data) = cbor.deserialise(${vector.label}_cbor)`,
        `  expect decoded: ${vector.aikenType} = decoded_data`,
        `  let rebuilt: Data = ${vector.label}_value()`,
        "  and {",
        `    decoded == ${vector.label}_value(),`,
        `    cbor.serialise(rebuilt) == ${vector.label}_cbor,`,
        "  }",
        "}",
        "",
      ];
    }),
  ].join("\n");

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const golden = buildGolden();
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: renderAiken(golden),
    fileName: "rule-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgard-601-aiken-format-",
  }),
);
