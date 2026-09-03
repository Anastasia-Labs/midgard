#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the `canonical-decodability`
 * family of `docs/spec/midgard-tx.md` §12.7 — the total §5.1 envelope verdict
 * and the two wire types the family's two steps carry.
 *
 * **Why this channel exists at all.** §12.7's verdict is the one predicate in
 * this document that both language sides must agree on *without* either of them
 * being able to fail: on chain it decides whether a block is slashed, off chain
 * it decides whether a prover builds a step that would be refused. Two
 * independent implementations of a decision procedure are exactly the shape
 * that drifts, and the drift would be silent in both directions — a TypeScript
 * twin that called a preimage grammatical would leave a genuine fault
 * unfiled, and one that called it ungrammatical would have a prover burn a
 * transaction on a step step 02 refuses.
 *
 * **Why it lives in the SDK.** Two of the three artifacts it pins are Plutus
 * **Data** encodings, whose off-chain producer is `Data.to` against the schemas
 * in `src/fraud-proof/canonical-decodability-v1.ts`; the verdict twin is in the
 * same module. A vector is only worth having if it is emitted by the thing that
 * will really emit it, so the generator sits beside the producer. The shared
 * channel plumbing is imported from midgard-core rather than copied, so the
 * `--check` contract is one implementation.
 *
 * Two artifacts, the pair every channel emits:
 *
 *   * `demo/midgard-sdk/tests/fixtures/canonical-decodability-v1.generated.json`
 *     — recomputed by `tests/canonical-decodability-v1.test.ts`, so a drifting
 *     TypeScript twin fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak`
 *     — recomputed by the Aiken producers under the fork runner, so a
 *     divergence between the two verdicts fails on the Aiken side.
 *
 * The verdict vector set covers **every one of the eleven codes**, both
 * §5.1 width boundaries (23/24 and 255/256) on both the array head and the item
 * head, the empty field `80`, and the declared-count mismatch in both
 * directions. Coverage is asserted rather than eyeballed: the JSON carries the
 * code each vector earns and both suites check the set of codes reached is the
 * whole code space.
 *
 * Vectors are regenerated, never hand-edited. Run with `--check` to assert the
 * checked-in artifacts are exactly what the producers emit today.
 *
 * usage: node scripts/generate-canonical-decodability-v1-goldens.mjs [--check]
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
  CanonicalDecodabilityStep02State,
  CommittedFieldClaim,
  MIDGARD_ENVELOPE_VERDICT_CODE_COUNT,
  MIDGARD_ENVELOPE_VERDICT_NAMES,
  midgardEnvelopeVerdict,
  miscountedMidgardFieldPreimage,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/canonical-decodability-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-canonical-decodability-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

// ---------------------------------------------------------------------------
// Verdict vector inputs
// ---------------------------------------------------------------------------

const bytes = (...values) => Buffer.from(values);

const filler = (length) => Buffer.alloc(length, 0x5a);

/** The honest §5.1 producer, spelled here so the two twins share no code. */
const envelope = (items) =>
  miscountedMidgardFieldPreimage(items.length, items);

/**
 * Each vector is built from **inputs** — a declared count and a list of items,
 * or an explicit head — and never transcribed from a hex string someone read
 * off a failing test. The Aiken side then recomputes the verdict from the same
 * bytes, so the two sides are one construction checked twice rather than a
 * construction and a copy of its output.
 */
const verdictVectors = [
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
const wireVectors = [
  {
    label: "claim_body_inline",
    aikenType: "CommittedFieldClaim",
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
    aikenType: "CommittedFieldClaim",
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
    aikenType: "CommittedFieldClaim",
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

// ---------------------------------------------------------------------------
// The golden
// ---------------------------------------------------------------------------

const buildGolden = () => {
  const vectors = verdictVectors.map((vector) => {
    const verdict = midgardEnvelopeVerdict(vector.preimage);
    return {
      label: vector.label,
      note: vector.note,
      preimage: hex(vector.preimage),
      byteCount: vector.preimage.length,
      verdict,
      verdictName: MIDGARD_ENVELOPE_VERDICT_NAMES[verdict],
    };
  });
  const reached = new Set(vectors.map((vector) => vector.verdict));
  // The coverage claim is checked where it is made rather than asserted in
  // prose: a vector set that stopped reaching a code would otherwise still
  // regenerate cleanly, and the code it stopped reaching is exactly the one
  // nothing else pins.
  if (reached.size !== MIDGARD_ENVELOPE_VERDICT_CODE_COUNT) {
    throw new Error(
      `verdict vectors reach ${String(reached.size)} of ` +
        `${String(MIDGARD_ENVELOPE_VERDICT_CODE_COUNT)} codes`,
    );
  }
  return {
    schema: "midgard-canonical-decodability-golden",
    version: 1,
    specDocument: "docs/spec/midgard-tx.md",
    generator:
      "demo/midgard-sdk/scripts/generate-canonical-decodability-v1-goldens.mjs",
    verdictCodeCount: MIDGARD_ENVELOPE_VERDICT_CODE_COUNT,
    verdictNames: [...MIDGARD_ENVELOPE_VERDICT_NAMES],
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
    "//// The §12.7 cross-language goldens. Two sets, and they answer different",
    "//// questions.",
    "////",
    "//// **The verdict set** is the one that matters. §12.7's verdict is a total",
    "//// decision procedure implemented twice — here and in",
    "//// `demo/midgard-sdk/src/fraud-proof/canonical-decodability-v1.ts` — and the",
    "//// two must agree on every byte string. A disagreement is silent in both",
    "//// directions: a twin that calls a committed non-envelope grammatical leaves",
    "//// a genuine fault unfiled, and one that calls an envelope ungrammatical has",
    "//// a prover spend a transaction on a step step 02 refuses. Every vector below",
    "//// carries the code the TypeScript twin assigned it, and the test asserts the",
    "//// Aiken verdict is the same number.",
    "////",
    "//// **The wire set** round-trips the two Data-encoded surfaces the family",
    "//// adds: each test decodes the TypeScript producer's bytes into the Aiken",
    "//// type and then re-serialises what came back, so a decoder that accepted a",
    "//// shape the producer never emits fails the second half and an encoder that",
    "//// emitted a shape the decoder tolerates fails it too. `CommittedFieldClaim`",
    "//// constructor order is wire format: `BodyFieldClaim` 0, `WitnessFieldClaim` 1.",
    "",
    "use aiken/cbor",
    "use midgard/fraud_proofs/canonical_decodability/rule.{",
    "  BodyFieldClaim, CommittedFieldClaim, WitnessFieldClaim,",
    "  envelope_verdict_v1,",
    "}",
    "use midgard/fraud_proofs/canonical_decodability/step_02.{State}",
    "use midgard/fraud_proofs/native_tx/types.{NativeTxWitnessSetCompact}",
    "use midgard/native_tx_field_access_v1.{Certified, Inline, RawUtxo}",
    "",
    ...section("Verdict vectors"),
    ...golden.vectors.flatMap((vector) => [
      ...docComment(
        `\`${vector.label}\` — ${vector.note}. ${String(vector.byteCount)} bytes, verdict ${String(vector.verdict)} (\`${vector.verdictName}\`).`,
      ),
      `test canonical_decodability_golden_verdict_${vector.label}() {`,
      `  envelope_verdict_v1(${aikenBytes(vector.preimage)}) == ${String(vector.verdict)}`,
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
        `test canonical_decodability_golden_${vector.label}_round_trips() {`,
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
    tmpPrefix: "midgard-596-aiken-format-",
  }),
);
