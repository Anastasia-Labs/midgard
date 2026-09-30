import { readCborBytes, readCborUnsigned } from "./cbor.js";
import {
  decodeMidgardFieldPreimage,
  exactMidgardFieldIndex,
} from "./native-tx-field-access.js";
import {
  decodeMidgardAddressWitnessItem,
  decodeMidgardHash28Item,
  decodeMidgardMintFieldItems,
  decodeMidgardSpendInputItem,
  expectByte,
  expectFullyConsumed,
  midgardRedeemerPurposeFromTag,
} from "./native-tx-field-item-decoders.decode-midgard-mint-policy-item.js";
import {
  type MidgardAddressWitness,
  type MidgardMintPolicyItem,
  type MidgardRedeemerWitness,
  type MidgardTxInput,
} from "./native-tx-field-items.js";
import { decodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import {
  decodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "./versioned-script.js";

/**
 * §5.3 field 8:
 * `84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)`.
 *
 * Twin of `decode_midgard_redeemer_witness_at`.
 */
export const decodeMidgardRedeemerWitnessItem = (
  item: Uint8Array,
): MidgardRedeemerWitness => {
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
    purpose: midgardRedeemerPurposeFromTag(Number(purposeTag.value)),
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
export type MidgardDecodedFieldItems =
  | { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInput[] }
  | { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] }
  | { readonly fieldIndex: 3 | 4; readonly items: readonly Buffer[] }
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
 * The seven per-field entry points, named so a caller that knows its field gets
 * a typed item list without narrowing a union. Each is §5.1's uniform byte-list
 * decode followed by that field's `enc_i` reader; fields 0/1 and 3/4 share an
 * encoder, so they share a decoder too, exactly as §5.3's table does.
 */
export const decodeMidgardInputFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardTxInput[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardSpendInputItem);

export const decodeMidgardOutputFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardTxOutput[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardTxOutput);

export const decodeMidgardHash28FieldPreimage = (
  preimage: Uint8Array,
): readonly Buffer[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardHash28Item);

export const decodeMidgardMintFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardMintPolicyItem[] =>
  decodeMidgardMintFieldItems(decodeMidgardFieldPreimage(preimage));

export const decodeMidgardScriptWitnessFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardVersionedScript[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardVersionedScript);

export const decodeMidgardAddressWitnessFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardAddressWitness[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardAddressWitnessItem);

export const decodeMidgardRedeemerWitnessFieldPreimage = (
  preimage: Uint8Array,
): readonly MidgardRedeemerWitness[] =>
  decodeMidgardFieldPreimage(preimage).map(decodeMidgardRedeemerWitnessItem);

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
export function decodeMidgardFieldItems(
  fieldIndex: 0 | 1,
  preimage: Uint8Array,
): { readonly fieldIndex: 0 | 1; readonly items: readonly MidgardTxInput[] };

export function decodeMidgardFieldItems(
  fieldIndex: 2,
  preimage: Uint8Array,
): { readonly fieldIndex: 2; readonly items: readonly MidgardTxOutput[] };

export function decodeMidgardFieldItems(
  fieldIndex: 3 | 4,
  preimage: Uint8Array,
): { readonly fieldIndex: 3 | 4; readonly items: readonly Buffer[] };

export function decodeMidgardFieldItems(
  fieldIndex: 5,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 5;
  readonly items: readonly MidgardMintPolicyItem[];
};

export function decodeMidgardFieldItems(
  fieldIndex: 6,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 6;
  readonly items: readonly MidgardVersionedScript[];
};

export function decodeMidgardFieldItems(
  fieldIndex: 7,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 7;
  readonly items: readonly MidgardAddressWitness[];
};

export function decodeMidgardFieldItems(
  fieldIndex: 8,
  preimage: Uint8Array,
): {
  readonly fieldIndex: 8;
  readonly items: readonly MidgardRedeemerWitness[];
};

export function decodeMidgardFieldItems(
  fieldIndex: number,
  preimage: Uint8Array,
): MidgardDecodedFieldItems;

export function decodeMidgardFieldItems(
  fieldIndex: number,
  preimage: Uint8Array,
): MidgardDecodedFieldItems {
  const exact = exactMidgardFieldIndex(fieldIndex);
  const items = decodeMidgardFieldPreimage(preimage);
  switch (exact) {
    case 0:
    case 1:
      return {
        fieldIndex: exact,
        items: items.map(decodeMidgardSpendInputItem),
      };
    case 2:
      return { fieldIndex: 2, items: items.map(decodeMidgardTxOutput) };
    case 3:
    case 4:
      return { fieldIndex: exact, items: items.map(decodeMidgardHash28Item) };
    case 5:
      return { fieldIndex: 5, items: decodeMidgardMintFieldItems(items) };
    case 6:
      return {
        fieldIndex: 6,
        items: items.map(decodeMidgardVersionedScript),
      };
    case 7:
      return {
        fieldIndex: 7,
        items: items.map(decodeMidgardAddressWitnessItem),
      };
    default:
      return {
        fieldIndex: 8,
        items: items.map(decodeMidgardRedeemerWitnessItem),
      };
  }
}

/**
 * §5.1's array header is the **only** place a field's item count exists (§5.2),
 * so a count-consuming rule reads it back from the preimage rather than from a
 * mirrored field. This is the cheap form of {@link decodeMidgardFieldItems}
 * for callers that need the count and the raw item spans but not typed items.
 */
export const decodeMidgardFieldItemBytes = (
  preimage: Uint8Array,
): readonly Buffer[] => decodeMidgardFieldPreimage(preimage);
