/**
 * The rows of the `forced-tx-admission-v1` channel, spelled byte by byte from
 * the §2.5 and §5.1 layouts. Read by
 * `generate-forced-tx-admission-v1-goldens.mjs`, which documents the channel.
 */

import { blake2b } from "@noble/hashes/blake2.js";

const fail = (message) => {
  throw new Error(`forced-tx-admission-v1: ${message}`);
};

// ## CBOR, spelled by hand

const cat = (...parts) => Buffer.concat(parts.map((part) => Buffer.from(part)));
const filled = (length, byte) => Buffer.alloc(length, byte);
export const head = (major, value) => {
  const n = BigInt(value);
  const base = major << 5;
  if (n < 24n) return Buffer.from([base | Number(n)]);
  if (n < 0x100n) return Buffer.from([base | 24, Number(n)]);
  if (n < 0x10000n) return cat([base | 25], bigEndian(n, 2));
  if (n < 0x100000000n) return cat([base | 26], bigEndian(n, 4));
  return cat([base | 27], bigEndian(n, 8));
};
const bigEndian = (value, width) => {
  const out = Buffer.alloc(width);
  let n = BigInt(value);
  for (let index = width - 1; index >= 0; index -= 1) {
    out[index] = Number(n & 0xffn);
    n >>= 8n;
  }
  return out;
};
const uint = (value) => head(0, value);
const bstr = (bytes) => cat(head(2, bytes.length), bytes);
const mapHead = (count) => head(5, count);

/** §5.1: the array header, then every item as a definite byte string. */
const field = (items) => cat(head(4, items.length), ...items.map(bstr));
const EMPTY = field([]);

const blake2b256 = (bytes) => Buffer.from(blake2b(bytes, { dkLen: 32 }));
const EMPTY_NULL_HASH = blake2b256(Buffer.from([0xf6]));

// ## Items

const spendInput = (byte, index) =>
  cat([0x82], bstr(filled(32, byte)), [0x19], bigEndian(index, 2));
const hash28 = (byte) => filled(28, byte);
const addressWitness = (byte) =>
  cat([0x82], bstr(filled(32, byte)), bstr(filled(64, byte)));

/** A versioned script: `82 ‖ tag ‖ bytes`. */
const versionedScript = (tag, scriptBytes) =>
  cat([0x82], uint(tag), bstr(scriptBytes));
const NATIVE = 0;
const PLUTUS_V3 = 3;
const MIDGARD_V1 = 128;
/** Native `[0, key hash]`: a signature script. */
const nativeSignatureScript = cat([0x82, 0x00], bstr(hash28(0xcc)));
/** Native `[5, slot]`: InvalidHereafter, satisfiable only by an upper bound. */
const nativeBeforeScript = cat([0x82, 0x05], uint(10_000_000));
const plutusProgram = Buffer.from("4e4d01000033222220051200120011", "hex");

const address = cat([0x61], hash28(0x77));
/** `82 ‖ coin ‖ {}`: a value with no assets. */
const adaValue = cat([0x82], uint(2_000_000), mapHead(0));

/** `{0: address, 1: value, ?2: datum, ?3: script_ref}`. */
const output = ({ value = adaValue, datum, scriptRef }) => {
  const entries = [
    [0, bstr(address)],
    [1, value],
    ...(datum === undefined ? [] : [[2, bstr(datum)]]),
    ...(scriptRef === undefined ? [] : [[3, scriptRef]]),
  ];
  return cat(
    mapHead(entries.length),
    ...entries.map(([key, entry]) => cat(uint(key), entry)),
  );
};

/**
 * A value of exactly `size` bytes, `82 ‖ coin ‖ {policy => {name => 1}}`, with
 * every asset name the same length so the names are ascending however a
 * reader orders them.
 */
const valueOfSize = (size) => {
  for (const coin of [1n, 24n, 256n, 65_536n, 4_294_967_296n]) {
    for (let nameLength = 32; nameLength >= 2; nameLength -= 1) {
      const fixed = 1 + uint(coin).length + 1 + bstr(hash28(0)).length;
      const perAsset = bstr(filled(nameLength, 0)).length + 1;
      for (let assets = 1; assets <= 255; assets += 1) {
        if (fixed + mapHead(assets).length + perAsset * assets !== size) {
          continue;
        }
        const names = Array.from({ length: assets }, (_, index) =>
          bigEndian(index + 1, nameLength),
        );
        return cat(
          [0x82],
          uint(coin),
          mapHead(1),
          bstr(hash28(0x99)),
          mapHead(assets),
          ...names.map((name) => cat(bstr(name), uint(1))),
        );
      }
    }
  }
  return fail(`no value spelling of ${size} bytes`);
};

// ## The admitted base and its neighbours

const NONCANONICAL_DATUM = Buffer.from("19002a", "hex");
const DATUM = Buffer.from("182a", "hex");
/** A 100-byte bytestring left unchunked: Plutus data chunks above 64 bytes. */
const UNCHUNKED_BYTES_DATUM = cat([0x58, 100], filled(100, 0x5a));
/** Two policies, the second with two assets of different name lengths. */
const MULTI_ASSET_VALUE = cat(
  [0x82],
  uint(5_000_000),
  mapHead(2),
  bstr(hash28(0x91)),
  mapHead(1),
  bstr(Buffer.alloc(0)),
  uint(7),
  bstr(hash28(0x92)),
  mapHead(2),
  bstr(Buffer.from("a")),
  uint(1),
  bstr(Buffer.from("bb")),
  uint(4_294_967_296n),
);

const base = () => ({
  fields: [
    field([spendInput(0xaa, 3)]),
    EMPTY,
    field([output({})]),
    EMPTY,
    field([hash28(0xbb)]),
    EMPTY,
    EMPTY,
    field([addressWitness(0xdd)]),
    EMPTY,
  ],
  validityIntervalEnd: -1n,
  scriptIntegrityHash: EMPTY_NULL_HASH,
  auxiliaryDataHash: EMPTY_NULL_HASH,
  networkId: 0n,
});

export const SPEND_INPUTS = 0;
export const REFERENCE_INPUTS = 1;
export const OUTPUTS = 2;
export const REQUIRED_OBSERVERS = 3;
export const REQUIRED_SIGNERS = 4;
export const MINT = 5;
export const SCRIPT_WITNESSES = 6;
export const ADDRESS_WITNESSES = 7;
export const REDEEMERS = 8;

const withFields = (row, entries) => {
  const fields = [...row.fields];
  for (const [index, preimage] of entries) fields[index] = preimage;
  return { ...row, fields };
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

// ## Rule 6: the mint field's items

/** An integer, negative ones included. */
const int = (value) => (value < 0n ? head(1, -1n - value) : head(0, value));
/** `[policy_id, {asset_name => quantity}]`, the entries in the order given. */
const mintItem = (policyId, assets, { arrayHead = [0x82] } = {}) =>
  cat(
    arrayHead,
    bstr(policyId),
    mapHead(assets.length),
    ...assets.map(([name, quantity]) => cat(bstr(name), int(quantity))),
  );
const ascii = (text) => Buffer.from(text, "latin1");
const POLICY_A = hash28(0x11);
const POLICY_B = hash28(0x22);
const MINT_CHECK = "mint_items_are_valid(mint)";
const withMint = (items) => withFields(base(), [[MINT, field(items)]]);

/**
 * The admitted mint rows, each also the bytes the package's own canonical mint
 * encoder writes for the same entries (checked by the generator).
 */
export const canonicalMintRows = [];
const addMint = (name, expected, items, { halts = false } = {}) => {
  add(name, expected, { ...withMint(items), halts });
};
const addCanonicalMint = (name, entries) => {
  canonicalMintRows.push({ name, entries });
  addMint(
    name,
    null,
    entries.map(({ policyId, assets }) =>
      mintItem(
        policyId,
        assets.map(({ assetName, quantity }) => [assetName, quantity]),
      ),
    ),
  );
};

addCanonicalMint("mint_one_asset", [
  { policyId: POLICY_A, assets: [{ assetName: ascii(""), quantity: 1n }] },
]);
// Byte order puts "aa" before "b"; length-first order would not.
addCanonicalMint("mint_names_in_byte_order", [
  {
    policyId: POLICY_A,
    assets: [
      { assetName: ascii("aa"), quantity: 1n },
      { assetName: ascii("b"), quantity: 2n },
    ],
  },
]);
addCanonicalMint("mint_entry_with_the_minimal_array_head", [
  { policyId: POLICY_A, assets: [{ assetName: ascii("a"), quantity: 1n }] },
]);
addCanonicalMint("mint_two_policies_burning", [
  { policyId: POLICY_A, assets: [{ assetName: ascii("a"), quantity: -5n }] },
  {
    policyId: POLICY_B,
    assets: [{ assetName: filled(32, 0x7a), quantity: 0x100000000n }],
  },
]);

addMint("mint_policies_out_of_order", MINT_CHECK, [
  mintItem(POLICY_B, [[ascii("a"), 1n]]),
  mintItem(POLICY_A, [[ascii("a"), 1n]]),
]);
addMint("mint_policy_repeated", MINT_CHECK, [
  mintItem(POLICY_A, [[ascii("a"), 1n]]),
  mintItem(POLICY_A, [[ascii("b"), 1n]]),
]);
addMint("mint_names_in_length_first_order", MINT_CHECK, [
  mintItem(POLICY_A, [
    [ascii("b"), 1n],
    [ascii("aa"), 1n],
  ]),
]);
addMint("mint_name_repeated", MINT_CHECK, [
  mintItem(POLICY_A, [
    [ascii("a"), 1n],
    [ascii("a"), 2n],
  ]),
]);
addMint("mint_zero_quantity", MINT_CHECK, [
  mintItem(POLICY_A, [[ascii("a"), 0n]]),
]);
addMint("mint_name_of_33_bytes", MINT_CHECK, [
  mintItem(POLICY_A, [[filled(33, 0x7a), 1n]]),
]);
addMint("mint_policy_id_of_27_bytes", MINT_CHECK, [
  mintItem(filled(27, 0x11), [[ascii("a"), 1n]]),
]);
addMint("mint_policy_id_of_29_bytes", MINT_CHECK, [
  mintItem(filled(29, 0x11), [[ascii("a"), 1n]]),
]);
addMint("mint_policy_with_no_asset", MINT_CHECK, [mintItem(POLICY_A, [])]);
addMint("mint_byte_after_the_last_asset", MINT_CHECK, [
  cat(mintItem(POLICY_A, [[ascii("a"), 1n]]), [0x00]),
]);
addMint("mint_fewer_assets_than_counted", MINT_CHECK, [
  cat([0x82], bstr(POLICY_A), mapHead(2), bstr(ascii("a")), int(1n)),
]);
// Reads the fold halts on: the mint fails, refusing the order.
addMint(
  "mint_entry_of_three_items",
  MINT_CHECK,
  [mintItem(POLICY_A, [[ascii("a"), 1n]], { arrayHead: [0x83] })],
  { halts: true },
);
// The entry's header must be the minimal two-item array head. Here the head
// is the wide `98` form and its count byte is the policy's own `58` head, so
// the bytes after the head byte decode as the policy and asset map admitted
// under `82` (mint_entry_with_the_minimal_array_head): only the head check
// refuses the entry.
addMint(
  "mint_entry_with_a_wide_array_head",
  MINT_CHECK,
  [mintItem(POLICY_A, [[ascii("a"), 1n]], { arrayHead: [0x98] })],
  { halts: true },
);
addMint(
  "mint_entry_not_an_array",
  MINT_CHECK,
  [mintItem(POLICY_A, [[ascii("a"), 1n]], { arrayHead: [0xa2] })],
  { halts: true },
);
addMint(
  "mint_policy_id_with_a_wide_head",
  MINT_CHECK,
  [
    cat(
      [0x82, 0x59, 0x00, 28],
      POLICY_A,
      mapHead(1),
      bstr(ascii("a")),
      int(1n),
    ),
  ],
  { halts: true },
);
addMint(
  "mint_asset_map_with_a_wide_head",
  MINT_CHECK,
  [cat([0x82], bstr(POLICY_A), [0xb8, 0x01], bstr(ascii("a")), int(1n))],
  { halts: true },
);
addMint(
  "mint_quantity_with_a_wide_head",
  MINT_CHECK,
  [cat([0x82], bstr(POLICY_A), mapHead(1), bstr(ascii("a")), [0x18, 0x05])],
  { halts: true },
);
addMint(
  "mint_quantity_not_an_integer",
  MINT_CHECK,
  [cat([0x82], bstr(POLICY_A), mapHead(1), bstr(ascii("a")), bstr(ascii("x")))],
  { halts: true },
);
