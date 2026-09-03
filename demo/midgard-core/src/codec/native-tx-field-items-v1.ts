/**
 * The TypeScript twins of the **nine per-field item encodings** of
 * `docs/spec/midgard-tx.md` §5.3, and of the per-field §5.1 preimage producers
 * built over them.
 *
 * `native-tx-field-access-v1.ts` owns what all nine fields *share* — the §5.1
 * envelope, the §4 flat commitment, the §5.3 stride table, the §8 carriage
 * ladder. This module owns what makes each field itself: the bytes of `enc_i`.
 * Together they are the off-chain half of §1's "Encoders" obligation, and the
 * cross-language golden vectors in
 * `tests/fixtures/native-tx-field-items-v1.generated.json` pin them against the
 * Aiken producers in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx/{preimages,components}.ak`.
 *
 * Two of the nine item encoders already existed as canonical encoders and are
 * **reused** rather than re-spelled — §5.3 says their interiors are "unchanged
 * from the current canonical encoders", so a second spelling here would be a
 * divergence waiting to happen:
 *
 *   * field 2 — {@link encodeMidgardTxOutput} (§5.5);
 *   * field 6 — {@link encodeMidgardVersionedScript} (§5.3's tag table).
 *
 * Reused, not re-exported: both keep their own module as their single export
 * site, for the barrel reason spelled out at the import block below.
 *
 * The rest are new here because the flat reversion changed them: fields 0/1
 * gained the fixed 3-byte output index, fields 3/4 gained an asserted 28-byte
 * width, field 5 moved from a raw map to enveloped per-policy items, and fields
 * 7/8 moved from raw concatenation into the §5.1 envelope.
 *
 * **This module is a producer, not an access idiom.** Reading a *committed*
 * field goes through `authenticatedMidgardFieldViewV1`; what is here builds the
 * bytes that get committed.
 */

import { compareBytes, encodeCborInteger, encodeCborUnsigned } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldPreimage,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
  midgardFieldCommitment,
} from "./native-tx-field-access-v1.js";
import { encodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import {
  encodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "./versioned-script.js";

// `encodeMidgardTxOutput` (field 2) and `encodeMidgardVersionedScript`
// (field 6) are deliberately **not** re-exported here. `codec/index.ts` is a
// `export *` barrel, so a re-export would make each name ambiguous and ESM
// would drop it silently — the two canonical encoders keep their own modules as
// their single export site, and {@link encodeMidgardFieldItems} below is the
// one place that dispatches all nine.

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const exactBytes = (
  value: Uint8Array,
  length: number,
  label: string,
): Buffer => {
  if (value.length !== length) {
    return fail(`${label} must be ${length} bytes`, `got=${value.length}`);
  }
  return Buffer.from(value);
};

// ---------------------------------------------------------------------------
// §5.3 fields 0/1 — spend and reference inputs, fixed 3-byte output index
// ---------------------------------------------------------------------------

/** §5.4/§5.3: the output index is a CBOR uint16, so 0..65,535. */
export const MIDGARD_MAX_OUTPUT_INDEX = 65_535;

/**
 * §5.3's sole deliberately non-minimal encoding: the output index is **always**
 * the fixed 3-byte form `19 XXXX` (CBOR uint16 head, big-endian), even for the
 * values 0–23 that minimal CBOR spells in one byte.
 *
 * That is what makes every input item exactly 38 bytes with wrapper `58 26`,
 * giving stride 40 and pure arithmetic access. Picking a different canon does
 * not waive uniqueness (§6.1): `18 XX`, the minimal one-byte forms and any
 * wider form all reject on the way back in.
 *
 * Twin of `encode_fixed_output_index`.
 */
export const encodeMidgardFixedOutputIndex = (outputIndex: number): Buffer => {
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0 ||
    outputIndex > MIDGARD_MAX_OUTPUT_INDEX
  ) {
    return fail(
      "output index must be 0..65,535 (§5.3 fixed uint16 form)",
      `output_index=${outputIndex}`,
    );
  }
  const encoded = Buffer.alloc(3);
  encoded[0] = 0x19;
  encoded.writeUInt16BE(outputIndex, 1);
  return encoded;
};

export type MidgardTxInput = {
  /** The referenced transaction's id (32 bytes). */
  readonly txId: Uint8Array;
  /** 0..65,535. */
  readonly outputIndex: number;
};

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — fixed 38 bytes.
 * Twin of `encode_midgard_tx_input`.
 */
export const encodeMidgardSpendInputItem = (input: MidgardTxInput): Buffer => {
  const encoded = Buffer.concat([
    Buffer.from([0x82]),
    encodeMidgardDefiniteBytes(exactBytes(input.txId, 32, "input tx_id")),
    encodeMidgardFixedOutputIndex(input.outputIndex),
  ]);
  // The width is the whole point of the fixed index; asserting it here means a
  // future edit to either half cannot quietly break stride-40 arithmetic.
  if (encoded.length !== MIDGARD_SPEND_INPUT_ITEM_BYTES) {
    return fail(
      "spend/reference input item is not the §5.3 fixed width",
      `length=${encoded.length}`,
    );
  }
  return encoded;
};

// ---------------------------------------------------------------------------
// §5.3 fields 3/4 — required observers and signers, asserted 28-byte width
// ---------------------------------------------------------------------------

/**
 * §5.3 fields 3/4: the item *is* the raw 28-byte hash — no interior CBOR. The
 * width is asserted by both encoder twins, which is what fixes stride 30.
 * Twin of `expect_hash28`.
 */
export const encodeMidgardHash28Item = (hash: Uint8Array): Buffer =>
  exactBytes(hash, MIDGARD_HASH28_ITEM_BYTES, "observer/signer item");

// ---------------------------------------------------------------------------
// §5.6 field 5 — mint, enveloped per-policy items
// ---------------------------------------------------------------------------

/** §5.6 caps asset names at the ledger's 32 bytes. */
const MIDGARD_MAX_ASSET_NAME_BYTES = 32;

/**
 * The definite CBOR map header `map(k)`, minimal width. Twin of
 * `encode_definite_map_header`; kept here rather than in the shared field-access
 * module because §5.1's envelope has no maps in it — only §5.6's mint items do.
 */
const encodeMidgardDefiniteMapHeader = (length: number): Buffer => {
  if (!Number.isSafeInteger(length) || length < 0) {
    return fail("map length must be a non-negative integer", `${length}`);
  }
  if (length <= 23) {
    return Buffer.from([0xa0 + length]);
  }
  if (length <= 0xff) {
    return Buffer.from([0xb8, length]);
  }
  if (length <= 0xffff) {
    return Buffer.from([0xb9, (length >> 8) & 0xff, length & 0xff]);
  }
  const header = Buffer.alloc(5);
  header[0] = 0xba;
  header.writeUInt32BE(length, 1);
  return header;
};

export type MidgardMintAsset = {
  /** ≤ 32 bytes. */
  readonly assetName: Uint8Array;
  /** Non-zero: positive mints, negative burns. */
  readonly quantity: bigint;
};

export type MidgardMintPolicyItem = {
  /** 28-byte policy id. */
  readonly policyId: Uint8Array;
  /** Non-empty, in canonical key order (length-first, then byte-lexicographic). */
  readonly assets: readonly MidgardMintAsset[];
};

/**
 * §5.6's canonical key order: length first, then byte-lexicographic. Twin of
 * `canonical_bytes_key_precedes`.
 *
 * Exported because §5.5's Value maps and §5.6's mint items share this one
 * ordering, so a producer and the decoder that checks it can never disagree about
 * what "canonical" means. Producers of field-5 items do not call it directly —
 * {@link sortMidgardMintItems} is the one spelling of the two-level sort they
 * need, and {@link encodeMidgardFieldItems} then enforces the result rather than
 * trusting it. This is for the ordering itself, wherever else §5.5/§5.6 keys are
 * compared.
 */
export const compareMidgardCanonicalKeyBytes = (
  left: Uint8Array,
  right: Uint8Array,
): number => left.length - right.length || compareBytes(left, right);

/**
 * §5.6's canonical order applied to a whole field-5 item list: policy items by
 * policy id, and each policy's assets by asset name.
 *
 * {@link encodeMidgardMintPolicyItem} and {@link encodeMidgardFieldItems}
 * *enforce* this order but deliberately never impose it, because a silent sort
 * would hide a producer that had lost track of its own ordering. That left every
 * producer spelling the two-level sort out for itself — four of them did, one per
 * source of mint intent — and four spellings of one consensus rule is four places
 * for it to drift. This is the one spelling; the enforcement downstream is what
 * keeps it honest.
 *
 * Both levels are sorted, never deduplicated: two entries with the same key are a
 * producer bug, and the encoder rejects them by name rather than letting a sort
 * quietly pick a winner.
 */
export const sortMidgardMintItems = (
  items: readonly MidgardMintPolicyItem[],
): readonly MidgardMintPolicyItem[] =>
  [...items]
    .map((item) => ({
      ...item,
      assets: [...item.assets].sort((left, right) =>
        compareMidgardCanonicalKeyBytes(left.assetName, right.assetName),
      ),
    }))
    .sort((left, right) =>
      compareMidgardCanonicalKeyBytes(left.policyId, right.policyId),
    );

/**
 * §5.6's ordering rule applied to one run of keys: strictly ascending under
 * {@link compareMidgardCanonicalKeyBytes}, so both "out of order" and
 * "duplicated" reject.
 *
 * It is used at both levels the spec names — asset names within a policy, and
 * policy ids across the field — because the §5.6 decoders check both, and an
 * encoder that let either past would hand a builder bytes that never decode.
 */
const assertCanonicalKeyOrder = (
  keys: readonly Uint8Array[],
  label: string,
): void => {
  for (let index = 1; index < keys.length; index += 1) {
    const order = compareMidgardCanonicalKeyBytes(
      keys[index - 1]!,
      keys[index]!,
    );
    if (order > 0) {
      return void fail(
        `§5.6 ${label} must be in canonical key order`,
        `index=${index}`,
      );
    }
    if (order === 0) {
      return void fail(`§5.6 ${label} must not repeat`, `index=${index}`);
    }
  }
};

/**
 * §5.6: `82 ‖ 58 1C policy_id(28) ‖ map(k) ‖ asset entries`, where each entry is
 * `bytes(asset_name ≤ 32) ‖ int(quantity ≠ 0)`.
 *
 * Ordering and duplicate rejection are enforced here rather than assumed of the
 * caller: the decoders check both, so an encoder that let them past would emit
 * bytes that never decode. Twin of `encode_mint_policy_item`.
 */
export const encodeMidgardMintPolicyItem = (
  item: MidgardMintPolicyItem,
): Buffer => {
  const policyId = exactBytes(item.policyId, 28, "mint policy id");
  if (item.assets.length === 0) {
    return fail(
      "§5.6 mint policy item must carry at least one asset",
      `policy_id=${policyId.toString("hex")}`,
    );
  }
  const entries = item.assets.map((asset, index) => {
    const assetName = Buffer.from(asset.assetName);
    if (assetName.length > MIDGARD_MAX_ASSET_NAME_BYTES) {
      return fail(
        "§5.6 mint asset name exceeds 32 bytes",
        `index=${index},length=${assetName.length}`,
      );
    }
    if (asset.quantity === 0n) {
      return fail(
        "§5.6 mint quantity must be non-zero",
        `index=${index},asset_name=${assetName.toString("hex")}`,
      );
    }
    return { assetName, quantity: asset.quantity };
  });
  assertCanonicalKeyOrder(
    entries.map((entry) => entry.assetName),
    "mint asset names",
  );
  return Buffer.concat([
    Buffer.from([0x82]),
    encodeMidgardDefiniteBytes(policyId),
    encodeMidgardDefiniteMapHeader(entries.length),
    ...entries.map((entry) =>
      Buffer.concat([
        encodeMidgardDefiniteBytes(entry.assetName),
        encodeCborInteger(entry.quantity),
      ]),
    ),
  ]);
};

/**
 * §5.6's field-level rule: the *policy items* of field 5 appear in canonical key
 * order and duplicates reject — the same rule
 * {@link encodeMidgardMintPolicyItem} applies to asset names one level down.
 *
 * It lives here rather than in the per-item encoder because ordering is a
 * property of the run, not of any one item, and the §5.6 decoder
 * (`decode_mint_policy_items_at`) enforces it across the whole field. Without
 * this the module would happily produce a descending or duplicated policy list
 * that no decoder on either side accepts — a producer handing back an
 * uncommittable preimage with no error.
 */
const encodeMidgardMintFieldItems = (
  items: readonly MidgardMintPolicyItem[],
): readonly Buffer[] => {
  assertCanonicalKeyOrder(
    items.map((item) => exactBytes(item.policyId, 28, "mint policy id")),
    "mint policy ids",
  );
  return items.map(encodeMidgardMintPolicyItem);
};

// ---------------------------------------------------------------------------
// §5.3 field 7 — address (vkey) witnesses
// ---------------------------------------------------------------------------

export type MidgardAddressWitness = {
  /** 32-byte Ed25519 verification key. */
  readonly verificationKey: Uint8Array;
  /** 64-byte Ed25519 signature. */
  readonly signature: Uint8Array;
};

/**
 * §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)` — fixed 101 bytes,
 * stride 103. Twin of `encode_midgard_address_witness`.
 */
export const encodeMidgardAddressWitnessItem = (
  witness: MidgardAddressWitness,
): Buffer => {
  const encoded = Buffer.concat([
    Buffer.from([0x82]),
    encodeMidgardDefiniteBytes(
      exactBytes(witness.verificationKey, 32, "witness verification key"),
    ),
    encodeMidgardDefiniteBytes(
      exactBytes(witness.signature, 64, "witness signature"),
    ),
  ]);
  if (encoded.length !== MIDGARD_ADDRESS_WITNESS_ITEM_BYTES) {
    return fail(
      "address witness item is not the §5.3 fixed width",
      `length=${encoded.length}`,
    );
  }
  return encoded;
};

// ---------------------------------------------------------------------------
// §5.3 field 8 — redeemer witnesses
// ---------------------------------------------------------------------------

/**
 * §5.3's `purpose_tag` value set — exactly seven values, every one ≤ 23 so each
 * occupies one byte equal to its value. Values 0–5 reuse Cardano's own
 * `RedeemerTag` numbering; 6 (`Receive`) is Midgard-only.
 *
 * Two narrower sets sit inside this one and are deliberately **not** the
 * format's bound: the Midgard builder emits only `Spend`/`Mint`/`Reward`/
 * `Receive`, and the Cardano conversion bridge admits only `Spend`/`Mint`/
 * `Reward`. Twin of `midgard_redeemer_purpose_to_tag`.
 */
export const MIDGARD_REDEEMER_PURPOSE_TAGS = {
  Spend: 0,
  Mint: 1,
  Cert: 2,
  Reward: 3,
  Vote: 4,
  Propose: 5,
  Receive: 6,
} as const;

export type MidgardRedeemerPurpose = keyof typeof MIDGARD_REDEEMER_PURPOSE_TAGS;

// The inverse map (tag → purpose) deliberately has no twin here. This module is
// a producer; a tag-to-purpose reader belongs on the decoding side, and §5.3
// names `midgard_redeemer_purpose_from_tag` in
// `fraud-proofs/native-tx/components.ak` as the place that rejects an
// out-of-set tag. Spelling a second one here — with no caller — would put a
// decoder in a producer module and give the value set two homes.

export type MidgardExecutionUnits = {
  readonly memory: bigint;
  readonly steps: bigint;
};

export type MidgardRedeemerWitness = {
  readonly purpose: MidgardRedeemerPurpose;
  /** Non-negative, canonical minimal CBOR uint. */
  readonly index: bigint;
  readonly redeemerCbor: Uint8Array;
  readonly executionUnits: MidgardExecutionUnits;
};

const exactNonNegative = (value: bigint, label: string): bigint => {
  if (value < 0n) {
    return fail(`${label} must be non-negative`, `${value}`);
  }
  return value;
};

/**
 * §5.3 field 8:
 * `84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)`.
 *
 * Note the shape: a four-element array whose last element is a two-element
 * array, spelled inline rather than nested through a helper — that is what the
 * Aiken twin emits, and the §5.1 envelope wraps the whole thing. Twin of
 * `encode_midgard_redeemer_witness`.
 */
export const encodeMidgardRedeemerWitnessItem = (
  witness: MidgardRedeemerWitness,
): Buffer =>
  Buffer.concat([
    Buffer.from([0x84]),
    encodeCborUnsigned(BigInt(MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose])),
    encodeCborUnsigned(exactNonNegative(witness.index, "redeemer index")),
    encodeMidgardDefiniteBytes(witness.redeemerCbor),
    Buffer.from([0x82]),
    encodeCborUnsigned(
      exactNonNegative(witness.executionUnits.memory, "ex_memory"),
    ),
    encodeCborUnsigned(
      exactNonNegative(witness.executionUnits.steps, "ex_steps"),
    ),
  ]);

// ---------------------------------------------------------------------------
// The nine fields, dispatched by §2.5 index
// ---------------------------------------------------------------------------

/**
 * A single field's items, tagged by the §2.5 field index they belong to. The
 * tag is the field index rather than a name because §4 makes field identity
 * *positional*: fields 0/1 and 3/4 share item encoders, so identical content
 * aliases across those pairs, and the index is the only thing that says which
 * slot a preimage was built for.
 */
export type MidgardFieldItems =
  | { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInput[] }
  | { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] }
  | { readonly fieldIndex: 3 | 4; readonly items: readonly Uint8Array[] }
  | {
      readonly fieldIndex: 5;
      readonly items: readonly MidgardMintPolicyItem[];
    }
  | {
      readonly fieldIndex: 6;
      readonly items: readonly MidgardVersionedScript[];
    }
  | {
      readonly fieldIndex: 7;
      readonly items: readonly MidgardAddressWitness[];
    }
  | {
      readonly fieldIndex: 8;
      readonly items: readonly MidgardRedeemerWitness[];
    };

/**
 * The §5.3 `enc_i` bytes of one field's items — the per-item half of the
 * grammar, before the §5.1 envelope goes on.
 */
export const encodeMidgardFieldItems = (
  field: MidgardFieldItems,
): readonly Buffer[] => {
  switch (field.fieldIndex) {
    case 0:
    case 1:
      return field.items.map(encodeMidgardSpendInputItem);
    case 2:
      return field.items.map(encodeMidgardTxOutput);
    case 3:
    case 4:
      return field.items.map(encodeMidgardHash28Item);
    case 5:
      return encodeMidgardMintFieldItems(field.items);
    case 6:
      return field.items.map(encodeMidgardVersionedScript);
    case 7:
      return field.items.map(encodeMidgardAddressWitnessItem);
    case 8:
      return field.items.map(encodeMidgardRedeemerWitnessItem);
  }
};

/**
 * The §5.1 preimage of one field: `definite_array_header(N)` followed by one
 * definite byte-string-wrapped `enc_i` per item. An empty field is exactly `80`
 * — all nine, including mint (§5.6), which under the retired scheme spelled it
 * `a0`.
 *
 * Twin of the six `encode_*_preimage` producers in `preimages.ak`, which differ
 * from each other only in the item encoder they map.
 */
export const encodeMidgardFieldPreimageForField = (
  field: MidgardFieldItems,
): Buffer => encodeMidgardFieldPreimage(encodeMidgardFieldItems(field));

/**
 * §4 — the flat commitment of one field: `blake2b_256(preimage)`, plain, with
 * no domain tag, version prefix or field index in the hash input.
 */
export const midgardFieldCommitmentForField = (
  field: MidgardFieldItems,
): Buffer => midgardFieldCommitment(encodeMidgardFieldPreimageForField(field));

/**
 * The §2.5 field names, positionally indexed, for diagnostics and vector
 * labelling. §4 makes field identity positional, so the index — not the name —
 * is what any encoder dispatches on; this array only ever labels one.
 */
export const MIDGARD_FIELD_NAMES = [
  "spend_inputs",
  "reference_inputs",
  "outputs",
  "required_observers",
  "required_signers",
  "mint",
  "script_witnesses",
  "address_witnesses",
  "redeemers",
] as const;
