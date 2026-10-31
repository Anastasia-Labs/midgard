/**
 * The rows of the `forced-tx-admission-v1` channel, spelled byte by byte from
 * the §2.5 and §5.1 layouts. Read by
 * `generate-forced-tx-admission-v1-goldens.mjs`, which documents the channel.
 */

import {
  ADDRESS_WITNESSES,
  addressWitness,
  base,
  bstr,
  cat,
  DATUM,
  EMPTY,
  field,
  filled,
  hash28,
  head,
  MIDGARD_V1,
  MINT,
  MULTI_ASSET_VALUE,
  NATIVE,
  nativeBeforeScript,
  nativeSignatureScript,
  NONCANONICAL_DATUM,
  output,
  OUTPUTS,
  PLUTUS_V3,
  plutusProgram,
  REDEEMERS,
  REFERENCE_INPUTS,
  REQUIRED_OBSERVERS,
  REQUIRED_SIGNERS,
  SCRIPT_WITNESSES,
  SPEND_INPUTS,
  spendInput,
  UNCHUNKED_BYTES_DATUM,
  valueOfSize,
  versionedScript,
  withFields,
} from "./generate-forced-tx-admission-v1-goldens.encoding.mjs";
import {
  canonicalMintRows,
  mintRows,
} from "./generate-forced-tx-admission-v1-goldens.mint-rows.mjs";

export {
  canonicalMintRows,
  SPEND_INPUTS,
  REFERENCE_INPUTS,
  OUTPUTS,
  REQUIRED_OBSERVERS,
  REQUIRED_SIGNERS,
  MINT,
  SCRIPT_WITNESSES,
  ADDRESS_WITNESSES,
  REDEEMERS,
  head,
};

export const rows = [];
const add = (name, expected, row) => rows.push({ name, expected, ...row });

add("base", null, base());

// Rule 7.
add("upper_bound_present", "has_no_validity_upper_bound(body)", {
  ...base(),
  validityIntervalEnd: 1_000_000n,
});

// Rule 3.
add(
  "no_spend_input",
  "has_a_spend_input(body)",
  withFields(base(), [[SPEND_INPUTS, EMPTY]]),
);
add("auxiliary_data", "has_no_auxiliary_data(body)", {
  ...base(),
  auxiliaryDataHash: filled(32, 0x11),
});
const observed = withFields(base(), [
  [REQUIRED_OBSERVERS, field([hash28(0xee)])],
]);
const integrity = filled(32, 0x22);
add(
  "observed_on_untagged_network",
  "is_not_observed_on_untagged_network(body)",
  {
    ...observed,
    scriptIntegrityHash: integrity,
    networkId: 255n,
  },
);
add("observed_on_tagged_network", null, {
  ...observed,
  scriptIntegrityHash: integrity,
  networkId: 1n,
});
add("observed_without_integrity_hash", null, { ...observed, networkId: 255n });
add("integrity_hash_without_observers", null, {
  ...base(),
  scriptIntegrityHash: integrity,
  networkId: 255n,
});

// Rule 1.
add(
  "redeemer",
  "has_no_redeemers(witness_set)",
  withFields(base(), [
    [REDEEMERS, field([Buffer.from("840000182a820000", "hex")])],
  ]),
);
const withScripts = (scripts) =>
  withFields(base(), [[SCRIPT_WITNESSES, field(scripts)]]);
add(
  "plutus_v3_script_witness",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([versionedScript(PLUTUS_V3, plutusProgram)]),
);
add(
  "midgard_v1_script_witness",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([versionedScript(MIDGARD_V1, plutusProgram)]),
);
add(
  "plutus_script_witness_after_a_native_one",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([
    versionedScript(NATIVE, nativeSignatureScript),
    versionedScript(PLUTUS_V3, plutusProgram),
  ]),
);
add(
  "script_witness_language_head_not_minimal",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x18, 0x00], bstr(nativeSignatureScript))]),
);
add(
  "script_witness_payload_short_of_the_item",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat(versionedScript(NATIVE, nativeSignatureScript), [0x00])]),
);
add(
  "script_witness_not_a_pair",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x83, 0x00], bstr(nativeSignatureScript), [0x00])]),
);
add(
  "script_witness_unknown_language",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([versionedScript(1, nativeSignatureScript)]),
);
add(
  "native_script_witness_with_an_empty_payload",
  null,
  withScripts([versionedScript(NATIVE, Buffer.alloc(0))]),
);
add(
  "native_script_witness",
  null,
  withScripts([versionedScript(NATIVE, nativeSignatureScript)]),
);
add(
  "native_script_witness_satisfiable_only_before",
  null,
  withScripts([versionedScript(NATIVE, nativeBeforeScript)]),
);
// The payload head at each width a field item can need, minimal and not. A
// short item is followed by a native one, so every read the mint makes past
// the short item's end lands in the next item, not past the field.
const NATIVE_ITEM = versionedScript(NATIVE, nativeSignatureScript);
for (const length of [23, 24, 255, 256, 300]) {
  add(
    `native_script_witness_with_a_${length}_byte_payload`,
    null,
    withScripts([versionedScript(NATIVE, filled(length, 0xcc))]),
  );
}
for (const [name, payloadHead, length] of [
  ["one_byte", [0x58, 0x05], 5],
  ["two_byte", [0x59, 0x00, 0xff], 255],
  ["four_byte", [0x5a, 0x00, 0x00, 0x00, 0x05], 5],
  ["eight_byte", [0x5b, 0, 0, 0, 0, 0, 0, 0, 0x05], 5],
]) {
  add(
    `script_witness_payload_${name}_head_not_minimal`,
    "script_witnesses_are_native(script_witnesses)",
    withScripts([cat([0x82, 0x00], payloadHead, filled(length, 0xcc))]),
  );
}
add(
  "script_witness_payload_past_the_item",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00, 0x45], filled(3, 0xcc))]),
);
add(
  "script_witness_payload_one_byte_head_past_the_item",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00, 0x58]), NATIVE_ITEM]),
);
add(
  "script_witness_payload_two_byte_head_past_the_item",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00, 0x59, 0x01]), NATIVE_ITEM]),
);
add(
  "script_witness_without_a_payload",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00]), NATIVE_ITEM]),
);
add(
  "script_witness_of_one_byte",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82]), NATIVE_ITEM]),
);
add(
  "script_witness_payload_not_a_byte_string",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00, 0x20])]),
);
add(
  "script_witness_payload_an_indefinite_byte_string",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x82, 0x00, 0x5f, 0x41, 0xcc, 0xff])]),
);
add(
  "script_witness_an_indefinite_pair",
  "script_witnesses_are_native(script_witnesses)",
  withScripts([cat([0x9f, 0x00, 0x40, 0xff])]),
);

const withOutputs = (outputs) =>
  withFields(base(), [[OUTPUTS, field(outputs)]]);
// Rule 1: no output carries a reference script, native or not. A datum is
// allowed, canonical or not (an output datum is always inline).
const OUTPUT_CHECK = "outputs_are_admissible(outputs)";
add(
  "native_reference_script",
  OUTPUT_CHECK,
  withOutputs([
    output({ scriptRef: versionedScript(NATIVE, nativeSignatureScript) }),
  ]),
);
add(
  "plutus_reference_script",
  OUTPUT_CHECK,
  withOutputs([
    output({ scriptRef: versionedScript(PLUTUS_V3, plutusProgram) }),
  ]),
);
add(
  "midgard_v1_reference_script",
  OUTPUT_CHECK,
  withOutputs([
    output({ scriptRef: versionedScript(MIDGARD_V1, plutusProgram) }),
  ]),
);
add(
  "native_reference_script_after_a_multi_asset_value",
  OUTPUT_CHECK,
  withOutputs([
    output({
      value: MULTI_ASSET_VALUE,
      scriptRef: versionedScript(NATIVE, nativeSignatureScript),
    }),
  ]),
);
add(
  "inline_datum_and_native_reference_script",
  OUTPUT_CHECK,
  withOutputs([
    output({
      datum: DATUM,
      scriptRef: versionedScript(NATIVE, nativeSignatureScript),
    }),
  ]),
);
add(
  "reference_script_in_a_later_output",
  OUTPUT_CHECK,
  withOutputs([
    output({ datum: DATUM }),
    output({ scriptRef: versionedScript(NATIVE, nativeSignatureScript) }),
  ]),
);
add("canonical_inline_datum", null, withOutputs([output({ datum: DATUM })]));
add(
  "noncanonical_inline_datum",
  null,
  withOutputs([output({ datum: NONCANONICAL_DATUM })]),
);
add(
  "unchunked_100_byte_inline_datum",
  null,
  withOutputs([output({ datum: UNCHUNKED_BYTES_DATUM })]),
);
add(
  "inline_datum_after_a_multi_asset_value",
  null,
  withOutputs([output({ value: MULTI_ASSET_VALUE, datum: DATUM })]),
);
add("two_outputs_without_data", null, withOutputs([output({}), output({})]));

// Rule 5: with no datum, the value runs to the end of the output.
const MAX_VALUE_BYTES = 5_000;
add(
  `value_of_${MAX_VALUE_BYTES}_bytes`,
  null,
  withOutputs([output({ value: valueOfSize(MAX_VALUE_BYTES) })]),
);
add(
  `value_of_${MAX_VALUE_BYTES + 1}_bytes`,
  OUTPUT_CHECK,
  withOutputs([output({ value: valueOfSize(MAX_VALUE_BYTES + 1) })]),
);
// With a datum after it, the value's end is found by walking it.
add(
  `value_of_${MAX_VALUE_BYTES}_bytes_before_a_datum`,
  null,
  withOutputs([output({ value: valueOfSize(MAX_VALUE_BYTES), datum: DATUM })]),
);
add(
  `value_of_${MAX_VALUE_BYTES + 1}_bytes_before_a_datum`,
  OUTPUT_CHECK,
  withOutputs([
    output({ value: valueOfSize(MAX_VALUE_BYTES + 1), datum: DATUM }),
  ]),
);

// Rule 4: each fixed byte of each fixed-stride field, broken in one item; then
// one item a byte short beside one a byte long, which keeps the field's length.
const fixedFields = [
  {
    name: "spend_input",
    index: SPEND_INPUTS,
    check: "input_items_are_fixed(spend_inputs)",
    items: () => [spendInput(0xaa, 3), spendInput(0xab, 4)],
    fixedOffsets: [0, 1, 2, 3, 4, 37],
  },
  {
    name: "reference_input",
    index: REFERENCE_INPUTS,
    check: "input_items_are_fixed(reference_inputs)",
    items: () => [spendInput(0xa1, 0), spendInput(0xa2, 1)],
    fixedOffsets: [0, 1, 2, 3, 4, 37],
  },
  {
    name: "required_observer",
    index: REQUIRED_OBSERVERS,
    check: "hash28_items_are_fixed(required_observers)",
    items: () => [hash28(0xe1), hash28(0xe2)],
    fixedOffsets: [0, 1],
  },
  {
    name: "required_signer",
    index: REQUIRED_SIGNERS,
    check: "hash28_items_are_fixed(required_signers)",
    items: () => [hash28(0xb1), hash28(0xb2)],
    fixedOffsets: [0, 1],
  },
  {
    name: "address_witness",
    index: ADDRESS_WITNESSES,
    check: "address_witness_items_are_fixed(address_witnesses)",
    items: () => [addressWitness(0xd1), addressWitness(0xd2)],
    fixedOffsets: [0, 1, 2, 3, 4, 37, 38],
  },
];
for (const { name, index, check, items, fixedOffsets } of fixedFields) {
  const honest = field(items());
  add(`${name}_items`, null, withFields(base(), [[index, honest]]));
  const header = 1;
  const stride = (honest.length - header) / 2;
  for (const offset of fixedOffsets) {
    // Broken in the second item, so the walk must reach past the first.
    const broken = Buffer.from(honest);
    broken[header + stride + offset] ^= 0x01;
    add(
      `${name}_second_item_byte_${offset}_broken`,
      check,
      withFields(base(), [[index, broken]]),
    );
  }
  const [first, second] = items();
  const short = first.subarray(0, first.length - 1);
  const long = cat(second, [0x00]);
  add(
    `${name}_item_a_byte_short`,
    check,
    withFields(base(), [[index, field([short, long])]]),
  );
}

rows.push(...mintRows);
