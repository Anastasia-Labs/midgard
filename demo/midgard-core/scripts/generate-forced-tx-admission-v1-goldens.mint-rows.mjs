/**
 * The mint-field rows of the `forced-tx-admission-v1` channel (rule 6), spelled
 * byte by byte. Read by `generate-forced-tx-admission-v1-goldens.rows.mjs`,
 * which appends them after its own rows.
 */

import {
  base,
  bstr,
  cat,
  field,
  filled,
  hash28,
  head,
  mapHead,
  MINT,
  withFields,
} from "./generate-forced-tx-admission-v1-goldens.encoding.mjs";

export const mintRows = [];
const add = (name, expected, row) => mintRows.push({ name, expected, ...row });

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
