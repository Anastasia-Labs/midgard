import { midgardBlake2b } from "./blake2b.js";

const addressWitnessSigner = (item: Uint8Array): Buffer =>
  Buffer.from(midgardBlake2b(item.subarray(3, 35), { dkLen: 28 }));

/**
 * Field 7's order: by signer hash `blake2b_224(vkey)`, then by item bytes. The
 * verification key is payload bytes 3..35 of `82 5820 <vkey> 5840 <signature>`.
 * Producers sort address witnesses with this before encoding field 7.
 */
export const compareMidgardAddressWitnessItems = (
  left: Uint8Array,
  right: Uint8Array,
): number =>
  Buffer.compare(addressWitnessSigner(left), addressWitnessSigner(right)) ||
  Buffer.compare(left, right);

/**
 * Whether item `current` may follow item `previous` in field `fieldIndex`.
 *
 * Fields 0 and 1 ascend by item bytes and admit repeats (InputSets reports a
 * repeat as `DuplicateInput`). Field 7 ascends strictly under
 * {@link compareMidgardAddressWitnessItems}. Every other field is unordered
 * here. Aiken twin: `canonical_item_order_is_valid` in
 * `onchain/aiken/lib/midgard/validation-machine/canonical-decode.ak`.
 */
export const midgardFieldItemOrderIsValid = (
  fieldIndex: number,
  previous: Uint8Array,
  current: Uint8Array,
): boolean => {
  if (fieldIndex === 0 || fieldIndex === 1)
    return Buffer.compare(previous, current) <= 0;
  if (fieldIndex !== 7) return true;
  return compareMidgardAddressWitnessItems(previous, current) < 0;
};
