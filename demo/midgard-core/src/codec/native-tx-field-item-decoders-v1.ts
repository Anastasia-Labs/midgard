/**
 * The decode twins of `native-tx-field-items-v1.ts` — one reader per §5.3
 * `enc_i` form, plus the per-field dispatch that turns an authenticated §5.1
 * preimage back into typed items.
 *
 * `native-tx-field-items-v1.ts` deliberately holds no decoders: it is a
 * producer, and §5.3 names the Aiken *reader* functions
 * (`decode_midgard_tx_input_cbor`, `midgard_redeemer_purpose_from_tag`, …) as
 * the places that reject an out-of-set value. This module is where those
 * readers' twins live, so the producer module keeps one direction and this one
 * keeps the other, and neither has to spell the value sets twice.
 *
 * Two of the nine item decoders already exist as canonical decoders and are
 * **reused** rather than re-spelled, mirroring the producer module's own reuse:
 *
 *   * field 2 — {@link decodeMidgardTxOutput} (§5.5);
 *   * field 6 — {@link decodeMidgardVersionedScript} (§5.3's tag table).
 *
 * Everything here reads a *single item's* `enc_i` bytes. Splitting a preimage
 * into items is not this module's job — that is §5.1's one uniform byte-list
 * decode, `decodeMidgardFieldPreimageV1`, which all nine fields share. The
 * per-field entry point {@link decodeMidgardFieldItemsV1} composes the two.
 */

import {
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
  readCborUnsigned,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  decodeMidgardFieldPreimageV1,
  exactMidgardFieldIndexV1,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
  MIDGARD_HASH28_ITEM_BYTES_V1,
  MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
} from "./native-tx-field-access-v1.js";
import {
  compareMidgardCanonicalKeyBytesV1,
  MIDGARD_REDEEMER_PURPOSE_TAGS_V1,
  type MidgardAddressWitnessV1,
  type MidgardMintAssetV1,
  type MidgardMintPolicyItemV1,
  type MidgardRedeemerPurposeV1,
  type MidgardRedeemerWitnessV1,
  type MidgardTxInputV1,
} from "./native-tx-field-items-v1.js";
import { decodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import {
  decodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "./versioned-script.js";

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
const expectFullyConsumed = (
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

const expectByte = (
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
const decodeMidgardFixedOutputIndexV1At = (
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
export const decodeMidgardSpendInputItemV1 = (
  item: Uint8Array,
): MidgardTxInputV1 => {
  expectExactLength(
    item,
    MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
    "§5.3 spend/reference input item",
  );
  let offset = expectByte(item, 0, 0x82, "§5.3 spend/reference input item");
  const txId = readCborBytes(item, offset, "input tx_id");
  if (txId.value.length !== 32) {
    return fail("§5.3 input tx_id must be 32 bytes", `${txId.value.length}`);
  }
  offset = txId.nextOffset;
  const index = decodeMidgardFixedOutputIndexV1At(item, offset);
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
export const decodeMidgardHash28ItemV1 = (item: Uint8Array): Buffer => {
  expectExactLength(
    item,
    MIDGARD_HASH28_ITEM_BYTES_V1,
    "§5.3 observer/signer item",
  );
  return Buffer.from(item);
};

// ---------------------------------------------------------------------------
// §5.6 field 5 — mint policy items
// ---------------------------------------------------------------------------

const MIDGARD_MAX_ASSET_NAME_BYTES_V1 = 32;

/**
 * §5.6's ordering rule on one run of keys: strictly ascending, so "out of
 * order" and "duplicated" both reject. Applied at both levels the spec names —
 * asset names inside a policy item, policy ids across the field.
 */
const expectCanonicalKeyOrderV1 = (
  keys: readonly Uint8Array[],
  label: string,
): void => {
  for (let index = 1; index < keys.length; index += 1) {
    const order = compareMidgardCanonicalKeyBytesV1(
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
export const decodeMidgardMintPolicyItemV1 = (
  item: Uint8Array,
): MidgardMintPolicyItemV1 => {
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
  const assets: MidgardMintAssetV1[] = [];
  for (let index = 0; index < assetHeader.length; index += 1) {
    const assetName = readCborBytes(item, offset, "mint asset name");
    if (assetName.value.length > MIDGARD_MAX_ASSET_NAME_BYTES_V1) {
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
  expectCanonicalKeyOrderV1(
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
const decodeMidgardMintFieldItemsV1 = (
  items: readonly Uint8Array[],
): readonly MidgardMintPolicyItemV1[] => {
  const decoded = items.map(decodeMidgardMintPolicyItemV1);
  expectCanonicalKeyOrderV1(
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
export const decodeMidgardAddressWitnessItemV1 = (
  item: Uint8Array,
): MidgardAddressWitnessV1 => {
  expectExactLength(
    item,
    MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
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

const MIDGARD_REDEEMER_PURPOSES_BY_TAG_V1: readonly MidgardRedeemerPurposeV1[] =
  (
    Object.keys(MIDGARD_REDEEMER_PURPOSE_TAGS_V1) as MidgardRedeemerPurposeV1[]
  ).reduce<MidgardRedeemerPurposeV1[]>((byTag, purpose) => {
    byTag[MIDGARD_REDEEMER_PURPOSE_TAGS_V1[purpose]] = purpose;
    return byTag;
  }, []);

/**
 * §5.3's `purpose_tag` value set, read back. Exactly seven values are
 * admissible and every one is ≤ 23, so the tag occupies one byte equal to its
 * value; any other value rejects. Twin of `midgard_redeemer_purpose_from_tag`.
 */
export const midgardRedeemerPurposeFromTagV1 = (
  tag: number,
): MidgardRedeemerPurposeV1 =>
  MIDGARD_REDEEMER_PURPOSES_BY_TAG_V1[tag] ??
  fail("§5.3 redeemer purpose tag is out of set", `purpose_tag=${tag}`);

/**
 * §5.3 field 8:
 * `84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)`.
 *
 * Twin of `decode_midgard_redeemer_witness_at`.
 */
export const decodeMidgardRedeemerWitnessItemV1 = (
  item: Uint8Array,
): MidgardRedeemerWitnessV1 => {
  let offset = expectByte(item, 0, 0x84, "§5.3 redeemer witness item");
  const purposeTag = readCborUnsigned(item, offset, "redeemer purpose tag");
  offset = purposeTag.nextOffset;
  const index = readCborUnsigned(item, offset, "redeemer index");
  offset = index.nextOffset;
  const redeemerCbor = readCborBytes(item, offset, "redeemer cbor");
  offset = expectByte(
    item,
    redeemerCbor.nextOffset,
    0x82,
    "§5.3 redeemer execution units",
  );
  const memory = readCborUnsigned(item, offset, "ex_memory");
  const steps = readCborUnsigned(item, memory.nextOffset, "ex_steps");
  expectFullyConsumed(steps.nextOffset, item, "§5.3 redeemer witness item");
  return {
    purpose: midgardRedeemerPurposeFromTagV1(Number(purposeTag.value)),
    index: index.value,
    redeemerCbor: redeemerCbor.value,
    executionUnits: { memory: memory.value, steps: steps.value },
  };
};

// ---------------------------------------------------------------------------
// The nine fields, dispatched by §2.5 index
// ---------------------------------------------------------------------------

/**
 * One field's decoded items, tagged by the §2.5 field index — the read-back
 * counterpart of `MidgardFieldItemsV1`. The tag is the index rather than a name
 * because §4 makes field identity positional.
 */
export type MidgardDecodedFieldItemsV1 =
  | { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInputV1[] }
  | { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] }
  | { readonly fieldIndex: 3 | 4; readonly items: readonly Buffer[] }
  | {
      readonly fieldIndex: 5;
      readonly items: readonly MidgardMintPolicyItemV1[];
    }
  | {
      readonly fieldIndex: 6;
      readonly items: readonly MidgardVersionedScript[];
    }
  | {
      readonly fieldIndex: 7;
      readonly items: readonly MidgardAddressWitnessV1[];
    }
  | {
      readonly fieldIndex: 8;
      readonly items: readonly MidgardRedeemerWitnessV1[];
    };

/**
 * The seven per-field entry points, named so a caller that knows its field gets
 * a typed item list without narrowing a union. Each is §5.1's uniform byte-list
 * decode followed by that field's `enc_i` reader; fields 0/1 and 3/4 share an
 * encoder, so they share a decoder too, exactly as §5.3's table does.
 */
export const decodeMidgardInputFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardTxInputV1[] =>
  decodeMidgardFieldPreimageV1(preimage).map(decodeMidgardSpendInputItemV1);

export const decodeMidgardOutputFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardTxOutput[] =>
  decodeMidgardFieldPreimageV1(preimage).map(decodeMidgardTxOutput);

export const decodeMidgardHash28FieldPreimageV1 = (
  preimage: Uint8Array,
): readonly Buffer[] =>
  decodeMidgardFieldPreimageV1(preimage).map(decodeMidgardHash28ItemV1);

export const decodeMidgardMintFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardMintPolicyItemV1[] =>
  decodeMidgardMintFieldItemsV1(decodeMidgardFieldPreimageV1(preimage));

export const decodeMidgardScriptWitnessFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardVersionedScript[] =>
  decodeMidgardFieldPreimageV1(preimage).map(decodeMidgardVersionedScript);

export const decodeMidgardAddressWitnessFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardAddressWitnessV1[] =>
  decodeMidgardFieldPreimageV1(preimage).map(decodeMidgardAddressWitnessItemV1);

export const decodeMidgardRedeemerWitnessFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly MidgardRedeemerWitnessV1[] =>
  decodeMidgardFieldPreimageV1(preimage).map(
    decodeMidgardRedeemerWitnessItemV1,
  );

/**
 * §5.1 then §5.3: split the preimage into items with the one uniform byte-list
 * decode all nine fields share, then read each item's `enc_i`.
 *
 * The inverse of `encodeMidgardFieldPreimageForFieldV1`. Round-tripping is the
 * property the cross-language vectors pin: a preimage that decodes here
 * re-encodes to the same bytes, and one that does not is not §5.1 canonical.
 *
 * The overloads exist so a **literal** field index narrows the result to that
 * field's item type. §4 makes field identity positional, so the index is the
 * only thing that says which of the seven readers applies, and a caller that
 * passes a literal should not have to re-narrow a seven-way union afterwards.
 * The `number` signature stays for the genuinely field-generic callers, which
 * discriminate on `fieldIndex`.
 */
export function decodeMidgardFieldItemsV1(
  fieldIndex: 0 | 1,
  preimage: Uint8Array,
): { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInputV1[] };
export function decodeMidgardFieldItemsV1(
  fieldIndex: 2,
  preimage: Uint8Array,
): { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] };
export function decodeMidgardFieldItemsV1(
  fieldIndex: 3 | 4,
  preimage: Uint8Array,
): { readonly fieldIndex: 3 | 4; readonly items: readonly Buffer[] };
export function decodeMidgardFieldItemsV1(
  fieldIndex: 5,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 5;
  readonly items: readonly MidgardMintPolicyItemV1[];
};
export function decodeMidgardFieldItemsV1(
  fieldIndex: 6,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 6;
  readonly items: readonly MidgardVersionedScript[];
};
export function decodeMidgardFieldItemsV1(
  fieldIndex: 7,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 7;
  readonly items: readonly MidgardAddressWitnessV1[];
};
export function decodeMidgardFieldItemsV1(
  fieldIndex: 8,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 8;
  readonly items: readonly MidgardRedeemerWitnessV1[];
};
export function decodeMidgardFieldItemsV1(
  fieldIndex: number,
  preimage: Uint8Array,
): MidgardDecodedFieldItemsV1;
export function decodeMidgardFieldItemsV1(
  fieldIndex: number,
  preimage: Uint8Array,
): MidgardDecodedFieldItemsV1 {
  const exact = exactMidgardFieldIndexV1(fieldIndex);
  const items = decodeMidgardFieldPreimageV1(preimage);
  switch (exact) {
    case 0:
    case 1:
      return {
        fieldIndex: exact,
        items: items.map(decodeMidgardSpendInputItemV1),
      };
    case 2:
      return { fieldIndex: 2, items: items.map(decodeMidgardTxOutput) };
    case 3:
    case 4:
      return { fieldIndex: exact, items: items.map(decodeMidgardHash28ItemV1) };
    case 5:
      return { fieldIndex: 5, items: decodeMidgardMintFieldItemsV1(items) };
    case 6:
      return {
        fieldIndex: 6,
        items: items.map(decodeMidgardVersionedScript),
      };
    case 7:
      return {
        fieldIndex: 7,
        items: items.map(decodeMidgardAddressWitnessItemV1),
      };
    default:
      return {
        fieldIndex: 8,
        items: items.map(decodeMidgardRedeemerWitnessItemV1),
      };
  }
}

/**
 * §5.1's array header is the **only** place a field's item count exists (§5.2),
 * so a count-consuming rule reads it back from the preimage rather than from a
 * mirrored field. This is the cheap form of {@link decodeMidgardFieldItemsV1}
 * for callers that need the count and the raw item spans but not typed items.
 */
export const decodeMidgardFieldItemBytesV1 = (
  preimage: Uint8Array,
): readonly Buffer[] => decodeMidgardFieldPreimageV1(preimage);
