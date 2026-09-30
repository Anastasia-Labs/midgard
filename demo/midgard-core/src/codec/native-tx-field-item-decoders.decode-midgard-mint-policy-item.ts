import { readCborBytes, readCborInteger, readCborMapHeader } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
} from "./native-tx-field-access.js";
import {
  compareMidgardCanonicalKeyBytes,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  type MidgardAddressWitness,
  type MidgardMintAsset,
  type MidgardMintPolicyItem,
  type MidgardRedeemerPurpose,
  type MidgardTxInput,
} from "./native-tx-field-items.js";

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.CborDecode,
    message,
    detail,
  );
};

/**
 * Every item decoder here ends with this: §5.1 hands each item its exact
 * payload span, so a reader that stops short has accepted bytes the committed
 * preimage carries and this decode ignored.
 */
export const expectFullyConsumed = (
  offset: number,
  item: Uint8Array,
  label: string,
): void => {
  if (offset !== item.length) {
    return void fail(
      `${label} has trailing bytes`,
      `consumed=${offset},length=${item.length}`,
    );
  }
};

export const expectByte = (
  item: Uint8Array,
  offset: number,
  expected: number,
  label: string,
): number => {
  if (item[offset] !== expected) {
    return fail(
      `${label} must start with 0x${expected.toString(16).padStart(2, "0")}`,
      `offset=${offset},got=${item[offset]?.toString(16) ?? "eof"}`,
    );
  }
  return offset + 1;
};

const expectExactLength = (
  item: Uint8Array,
  expected: number,
  label: string,
): void => {
  if (item.length !== expected) {
    return void fail(
      `${label} must be exactly ${expected} bytes`,
      `length=${item.length}`,
    );
  }
};

// ---------------------------------------------------------------------------
// §5.3 fields 0/1 — spend and reference inputs
// ---------------------------------------------------------------------------

/**
 * §5.3's fixed 3-byte output index, `19 XXXX`.
 *
 * This is the one place in the codec that must **not** go through
 * `readCborUnsigned`: that reader enforces minimal CBOR and so rejects
 * `19 0000`, while §5.3 requires exactly that spelling. Picking a different
 * canon does not waive uniqueness — the `0x19` head is asserted, so `18 XX`,
 * the minimal one-byte forms and every wider form all reject here.
 *
 * Twin of `decode_fixed_output_index_at`.
 */
const decodeMidgardFixedOutputIndexAt = (
  item: Uint8Array,
  offset: number,
): { readonly outputIndex: number; readonly nextOffset: number } => {
  const head = expectByte(item, offset, 0x19, "§5.3 output index");
  const high = item[head];
  const low = item[head + 1];
  if (high === undefined || low === undefined) {
    return fail("§5.3 output index is truncated", `offset=${offset}`);
  }
  return { outputIndex: high * 256 + low, nextOffset: head + 2 };
};

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — fixed 38 bytes.
 * Twin of `decode_midgard_tx_input_cbor`.
 */
export const decodeMidgardSpendInputItem = (
  item: Uint8Array,
): MidgardTxInput => {
  expectExactLength(
    item,
    MIDGARD_SPEND_INPUT_ITEM_BYTES,
    "§5.3 spend/reference input item",
  );
  let offset = expectByte(item, 0, 0x82, "§5.3 spend/reference input item");
  const txId = readCborBytes(item, offset, "input tx_id");
  if (txId.value.length !== 32) {
    return fail("§5.3 input tx_id must be 32 bytes", `${txId.value.length}`);
  }
  offset = txId.nextOffset;
  const index = decodeMidgardFixedOutputIndexAt(item, offset);
  expectFullyConsumed(
    index.nextOffset,
    item,
    "§5.3 spend/reference input item",
  );
  return { txId: txId.value, outputIndex: index.outputIndex };
};

// ---------------------------------------------------------------------------
// §5.3 fields 3/4 — required observers and signers
// ---------------------------------------------------------------------------

/**
 * §5.3 fields 3/4: the item *is* the raw 28-byte hash, no interior CBOR. The
 * asserted width is what fixes stride 30, so it is checked rather than assumed.
 * Twin of `expect_hash28`.
 */
export const decodeMidgardHash28Item = (item: Uint8Array): Buffer => {
  expectExactLength(
    item,
    MIDGARD_HASH28_ITEM_BYTES,
    "§5.3 observer/signer item",
  );
  return Buffer.from(item);
};

// ---------------------------------------------------------------------------
// §5.6 field 5 — mint policy items
// ---------------------------------------------------------------------------

const MIDGARD_MAX_ASSET_NAME_BYTES = 32;

/**
 * §5.6's ordering rule on one run of keys: strictly ascending, so "out of
 * order" and "duplicated" both reject. Applied at both levels the spec names —
 * asset names inside a policy item, policy ids across the field.
 */
const expectCanonicalKeyOrder = (
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
 * §5.6: `82 ‖ 58 1C policy_id(28) ‖ map(k) ‖ asset entries`, each entry
 * `bytes(asset_name ≤ 32) ‖ int(quantity ≠ 0)`.
 *
 * Twin of `decode_mint_policy_item_cbor`.
 */
export const decodeMidgardMintPolicyItem = (
  item: Uint8Array,
): MidgardMintPolicyItem => {
  let offset = expectByte(item, 0, 0x82, "§5.6 mint policy item");
  const policyId = readCborBytes(item, offset, "mint policy id");
  if (policyId.value.length !== 28) {
    return fail(
      "§5.6 mint policy id must be 28 bytes",
      `${policyId.value.length}`,
    );
  }
  offset = policyId.nextOffset;
  const assetHeader = readCborMapHeader(item, offset, "mint assets");
  if (assetHeader.length === 0) {
    return fail("§5.6 mint policy item must carry at least one asset");
  }
  offset = assetHeader.nextOffset;
  const assets: MidgardMintAsset[] = [];
  for (let index = 0; index < assetHeader.length; index += 1) {
    const assetName = readCborBytes(item, offset, "mint asset name");
    if (assetName.value.length > MIDGARD_MAX_ASSET_NAME_BYTES) {
      return fail(
        "§5.6 mint asset name exceeds 32 bytes",
        `index=${index},length=${assetName.value.length}`,
      );
    }
    const quantity = readCborInteger(
      item,
      assetName.nextOffset,
      "mint quantity",
    );
    if (quantity.value === 0n) {
      return fail("§5.6 mint quantity must be non-zero", `index=${index}`);
    }
    assets.push({ assetName: assetName.value, quantity: quantity.value });
    offset = quantity.nextOffset;
  }
  expectCanonicalKeyOrder(
    assets.map((asset) => asset.assetName),
    "mint asset names",
  );
  expectFullyConsumed(offset, item, "§5.6 mint policy item");
  return { policyId: policyId.value, assets };
};

/**
 * §5.6's field-level rule, which no single item can see: the policy items
 * appear in canonical key order and duplicates reject. Twin of
 * `decode_mint_policy_items_at`, which makes the same split between the run and
 * the item.
 */
export const decodeMidgardMintFieldItems = (
  items: readonly Uint8Array[],
): readonly MidgardMintPolicyItem[] => {
  const decoded = items.map(decodeMidgardMintPolicyItem);
  expectCanonicalKeyOrder(
    decoded.map((item) => item.policyId),
    "mint policy ids",
  );
  return decoded;
};

// ---------------------------------------------------------------------------
// §5.3 field 7 — address (vkey) witnesses
// ---------------------------------------------------------------------------

/**
 * §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)` — fixed 101 bytes.
 * Twin of `decode_midgard_address_witness_cbor`.
 */
export const decodeMidgardAddressWitnessItem = (
  item: Uint8Array,
): MidgardAddressWitness => {
  expectExactLength(
    item,
    MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
    "§5.3 address witness item",
  );
  let offset = expectByte(item, 0, 0x82, "§5.3 address witness item");
  const verificationKey = readCborBytes(
    item,
    offset,
    "witness verification key",
  );
  if (verificationKey.value.length !== 32) {
    return fail(
      "§5.3 witness verification key must be 32 bytes",
      `${verificationKey.value.length}`,
    );
  }
  offset = verificationKey.nextOffset;
  const signature = readCborBytes(item, offset, "witness signature");
  if (signature.value.length !== 64) {
    return fail(
      "§5.3 witness signature must be 64 bytes",
      `${signature.value.length}`,
    );
  }
  expectFullyConsumed(signature.nextOffset, item, "§5.3 address witness item");
  return {
    verificationKey: verificationKey.value,
    signature: signature.value,
  };
};

// ---------------------------------------------------------------------------
// §5.3 field 8 — redeemer witnesses
// ---------------------------------------------------------------------------

const MIDGARD_REDEEMER_PURPOSES_BY_TAG: readonly MidgardRedeemerPurpose[] = (
  Object.keys(MIDGARD_REDEEMER_PURPOSE_TAGS) as MidgardRedeemerPurpose[]
).reduce<MidgardRedeemerPurpose[]>((byTag, purpose) => {
  byTag[MIDGARD_REDEEMER_PURPOSE_TAGS[purpose]] = purpose;
  return byTag;
}, []);

/**
 * §5.3's `purpose_tag` value set, read back. Exactly seven values are
 * admissible and every one is ≤ 23, so the tag occupies one byte equal to its
 * value; any other value rejects. Twin of `midgard_redeemer_purpose_from_tag`.
 */
export const midgardRedeemerPurposeFromTag = (
  tag: number,
): MidgardRedeemerPurpose =>
  MIDGARD_REDEEMER_PURPOSES_BY_TAG[tag] ??
  fail("§5.3 redeemer purpose tag is out of set", `purpose_tag=${tag}`);
