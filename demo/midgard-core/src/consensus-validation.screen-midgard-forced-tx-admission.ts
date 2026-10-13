import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
} from "./codec/native-constants.js";
import { compareMidgardAssetKeyBytes } from "./codec/value.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import { parseMidgardVersionedScriptHeader } from "./native-script-decoding-engine.parse-midgard-versioned-script-header.js";

/**
 * The parts of a forced transaction the admission screen reads: the nine §2.5
 * field preimages (each a §5.1 envelope, `80` when empty) and the three body
 * scalars that are not field commitments.
 */
export type MidgardForcedTxAdmissionInput = {
  readonly body: {
    readonly spendInputsPreimageCbor: Uint8Array;
    readonly referenceInputsPreimageCbor: Uint8Array;
    readonly outputsPreimageCbor: Uint8Array;
    readonly validityIntervalEnd: bigint;
    readonly requiredObserversPreimageCbor: Uint8Array;
    readonly requiredSignersPreimageCbor: Uint8Array;
    readonly mintPreimageCbor: Uint8Array;
    readonly scriptIntegrityHash: Uint8Array;
    readonly auxiliaryDataHash: Uint8Array;
    readonly networkId: bigint;
  };
  readonly witnessSet: {
    readonly addrTxWitsPreimageCbor: Uint8Array;
    readonly scriptTxWitsPreimageCbor: Uint8Array;
    readonly redeemerTxWitsPreimageCbor: Uint8Array;
  };
};

/**
 * The checks of `forced_tx_is_admissible_v1`
 * (onchain/aiken/lib/midgard/user-events/tx-order-v1.ak), in the order the
 * mint runs them. Each name is the expression the mint traces when that check
 * fails, so a traced refusal and this screen's refusal name the same check.
 */
export const MIDGARD_FORCED_TX_ADMISSION_CHECKS = [
  "has_no_validity_upper_bound(body)",
  "has_a_spend_input(body)",
  "has_no_auxiliary_data(body)",
  "is_not_observed_on_untagged_network(body)",
  "has_no_redeemers(witness_set)",
  "script_witnesses_are_native(script_witnesses)",
  "outputs_are_admissible(outputs)",
  "input_items_are_fixed(spend_inputs)",
  "input_items_are_fixed(reference_inputs)",
  "hash28_items_are_fixed(required_observers)",
  "hash28_items_are_fixed(required_signers)",
  "address_witness_items_are_fixed(address_witnesses)",
  "mint_items_are_valid(mint)",
] as const;

export type MidgardForcedTxAdmissionCheck =
  (typeof MIDGARD_FORCED_TX_ADMISSION_CHECKS)[number];

export type MidgardForcedTxAdmissionRefusal = {
  readonly check: MidgardForcedTxAdmissionCheck;
  readonly detail: string;
};

const NATIVE_LANGUAGE_TAG = 0;
const TWO_ENTRY_OUTPUT_MAP_HEAD = 0xa2;
const THREE_ENTRY_OUTPUT_MAP_HEAD = 0xa3;
const DATUM_KEY = 2;
const VALUE_ARRAY_HEAD = 0x82;
const INPUT_ITEM_PREFIX = [0x58, 0x26, 0x82, 0x58, 0x20] as const;
const INPUT_ITEM_INDEX_HEAD_OFFSET = 37;
const INPUT_ITEM_INDEX_HEAD = 0x19;
const INPUT_ITEM_STRIDE = 40;
const HASH28_ITEM_PREFIX = [0x58, 0x1c] as const;
const HASH28_ITEM_STRIDE = 30;
const ADDRESS_WITNESS_ITEM_PREFIX = [0x58, 0x65, 0x82, 0x58, 0x20] as const;
const ADDRESS_WITNESS_SIGNATURE_HEAD_OFFSET = 37;
const ADDRESS_WITNESS_SIGNATURE_HEAD = [0x58, 0x40] as const;
const ADDRESS_WITNESS_ITEM_STRIDE = 103;
const MINT_POLICY_ID_BYTES = 28;
const MINT_MAX_ASSET_NAME_BYTES = 32;

/** A read the mint would fail on. Caught and reported as a refusal. */
class Malformed extends Error {}

const byteAt = (bytes: Uint8Array, offset: number): number => {
  const value = bytes[offset];
  if (value === undefined) throw new Malformed(`no byte at ${offset}`);
  return value;
};

const startsWithAt = (
  bytes: Uint8Array,
  offset: number,
  prefix: readonly number[],
): boolean =>
  offset + prefix.length <= bytes.length &&
  prefix.every((value, index) => bytes[offset + index] === value);

const bigEndian = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): number => {
  let value = 0;
  for (let index = 0; index < width; index += 1) {
    value = value * 256 + byteAt(bytes, offset + index);
  }
  return value;
};

/** §5.1 array header: the offset of the first item and the item count. */
const fieldHeader = (preimage: Uint8Array): [number, number] => {
  const tag = byteAt(preimage, 0);
  if (tag >= 0x80 && tag <= 0x97) return [1, tag - 0x80];
  if (tag === 0x98) return [2, byteAt(preimage, 1)];
  if (tag === 0x99) return [3, bigEndian(preimage, 1, 2)];
  throw new Malformed(`field header ${tag.toString(16)}`);
};

const isEmptyField = (preimage: Uint8Array): boolean =>
  preimage.length === 1 && preimage[0] === 0x80;

/**
 * The width of the definite CBOR head at `offset`, which must have major type
 * `major`: the twin of the mint's `head_width`.
 */
const headWidth = (
  bytes: Uint8Array,
  offset: number,
  major: number,
): number => {
  const additional = byteAt(bytes, offset) - major * 32;
  if (additional < 24) {
    if (additional < 0) throw new Malformed(`head below major ${major}`);
    return 1;
  }
  if (additional === 24) return 2;
  if (additional === 25) return 3;
  if (additional === 26) return 5;
  if (additional === 27) return 9;
  throw new Malformed(`head ${byteAt(bytes, offset)} at ${offset}`);
};

/** The argument of the CBOR head of width `width` at `offset`. */
const headArgument = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): number => {
  if (width === 1) return byteAt(bytes, offset) % 32;
  if (width === 2) return byteAt(bytes, offset + 1);
  if (offset + width > bytes.length) {
    throw new Malformed(`head cut short at ${offset}`);
  }
  return bigEndian(bytes, offset + 1, width - 1);
};

/**
 * Offset one past the byte string whose head is at `offset`, the twin of the
 * mint's `skip_bytes`, short paths included: below `40` it reads another major
 * type by the same arithmetic.
 */
const skipBytes = (bytes: Uint8Array, offset: number): number => {
  const tag = byteAt(bytes, offset);
  if (tag < 0x58) return offset + tag - 0x3f;
  if (tag === 0x58) return offset + 2 + byteAt(bytes, offset + 1);
  const width = headWidth(bytes, offset, 2);
  return offset + width + headArgument(bytes, offset, width);
};

/** Offset one past the unsigned integer whose head is at `offset`. */
const skipUnsigned = (bytes: Uint8Array, offset: number): number =>
  byteAt(bytes, offset) < 24
    ? offset + 1
    : offset + headWidth(bytes, offset, 0);

/** Offset one past the value `[coin, {policy => {name => quantity}}]`. */
const skipValue = (bytes: Uint8Array, offset: number): number => {
  if (byteAt(bytes, offset) !== VALUE_ARRAY_HEAD) {
    throw new Malformed(`value head at ${offset}`);
  }
  const policies = skipUnsigned(bytes, offset + 1);
  const policiesWidth = headWidth(bytes, policies, 5);
  let cursor = policies + policiesWidth;
  for (
    let policy = headArgument(bytes, policies, policiesWidth);
    policy > 0;
    policy -= 1
  ) {
    const assets = skipBytes(bytes, cursor);
    const assetsWidth = headWidth(bytes, assets, 5);
    cursor = assets + assetsWidth;
    for (
      let asset = headArgument(bytes, assets, assetsWidth);
      asset > 0;
      asset -= 1
    ) {
      cursor = skipUnsigned(bytes, skipBytes(bytes, cursor));
    }
  }
  return cursor;
};

/**
 * Rules 1 and 5 on the output whose payload is `[start, end)`, the twin of the
 * mint's `output_is_admissible`. The output is the map
 * `{0: address, 1: value, ?2: datum, ?3: reference script}`, read by position.
 * Rule 1: no reference script; a three-entry output's third key is the datum's
 * `2`, and a four-entry output always carries one. Rule 5: the value's bytes
 * are at most the maximum value size; in a two-entry output the value runs to
 * the end of the item. Returns the reason an output is refused, or null.
 */
const outputRefusal = (
  bytes: Uint8Array,
  start: number,
  end: number,
): string | null => {
  const mapHead = byteAt(bytes, start);
  const value = skipBytes(bytes, start + 2) + 1;
  const maxValueBytes = MIDGARD_CONSENSUS_LIMITS.maxOutputValueCborBytes;
  if (mapHead === TWO_ENTRY_OUTPUT_MAP_HEAD) {
    const valueBytes = end - value;
    return valueBytes <= maxValueBytes ? null : `value is ${valueBytes} bytes`;
  }
  if (mapHead !== THREE_ENTRY_OUTPUT_MAP_HEAD) {
    return "output carries a reference script";
  }
  const valueEnd = skipValue(bytes, value);
  const valueBytes = valueEnd - value;
  if (valueBytes > maxValueBytes) return `value is ${valueBytes} bytes`;
  return byteAt(bytes, valueEnd) === DATUM_KEY
    ? null
    : "output carries a reference script";
};

/**
 * Visits each §5.1 item's payload `[start, end)` from the end of the header to
 * the end of the field, the twin of the mint's `variable_items_admitted`.
 */
const eachVariableItem = (
  preimage: Uint8Array,
  visit: (start: number, end: number) => string | null,
): string | null => {
  const [header] = fieldHeader(preimage);
  for (let offset = header, index = 0; offset < preimage.length; index += 1) {
    const tag = byteAt(preimage, offset);
    const start = offset + (tag < 0x58 ? 1 : tag === 0x58 ? 2 : 3);
    const end = skipBytes(preimage, offset);
    const reason = visit(start, end);
    if (reason !== null) return `item ${index}: ${reason}`;
    offset = end;
  }
  return null;
};

/** Visits each fixed-stride item, from the end of the header to the end. */
const eachFixedItem = (
  preimage: Uint8Array,
  stride: number,
  isFixed: (offset: number) => boolean,
): string | null => {
  const [header] = fieldHeader(preimage);
  for (
    let offset = header, index = 0;
    offset < preimage.length;
    offset += stride, index += 1
  ) {
    if (!isFixed(offset)) return `item ${index} has the wrong prefix`;
  }
  return null;
};

const inputItemsRefusal = (preimage: Uint8Array): string | null =>
  eachFixedItem(
    preimage,
    INPUT_ITEM_STRIDE,
    (offset) =>
      startsWithAt(preimage, offset, INPUT_ITEM_PREFIX) &&
      preimage[offset + INPUT_ITEM_INDEX_HEAD_OFFSET] === INPUT_ITEM_INDEX_HEAD,
  );

const hash28ItemsRefusal = (preimage: Uint8Array): string | null =>
  eachFixedItem(preimage, HASH28_ITEM_STRIDE, (offset) =>
    startsWithAt(preimage, offset, HASH28_ITEM_PREFIX),
  );

const addressWitnessItemsRefusal = (preimage: Uint8Array): string | null =>
  eachFixedItem(
    preimage,
    ADDRESS_WITNESS_ITEM_STRIDE,
    (offset) =>
      startsWithAt(preimage, offset, ADDRESS_WITNESS_ITEM_PREFIX) &&
      startsWithAt(
        preimage,
        offset + ADDRESS_WITNESS_SIGNATURE_HEAD_OFFSET,
        ADDRESS_WITNESS_SIGNATURE_HEAD,
      ),
  );

/**
 * The big-endian argument of `width` bytes after the head at `offset`, the
 * mint's decoders' arithmetic; a read past the field fails as the mint's does.
 */
const headValue = (
  bytes: Uint8Array,
  offset: number,
  width: number,
): bigint => {
  let value = 0n;
  for (let index = 1; index <= width; index += 1) {
    value = value * 256n + BigInt(byteAt(bytes, offset + index));
  }
  return value;
};

/**
 * A definite array header, the twin of `decode_definite_array_header_at`: any
 * width up to four argument bytes, minimal or not.
 */
const definiteArrayHeaderAt = (
  bytes: Uint8Array,
  offset: number,
): [number, bigint] => {
  const tag = byteAt(bytes, offset);
  if (tag >= 0x80 && tag <= 0x97) return [offset + 1, BigInt(tag - 0x80)];
  if (tag === 0x98) return [offset + 2, headValue(bytes, offset, 1)];
  if (tag === 0x99) return [offset + 3, headValue(bytes, offset, 2)];
  if (tag === 0x9a) return [offset + 5, headValue(bytes, offset, 4)];
  throw new Malformed(`array head ${tag.toString(16)} at ${offset}`);
};

/**
 * A canonical (minimal) head of `major` with at most four argument bytes, the
 * shared arithmetic of `decode_canonical_bytes_at` and
 * `decode_canonical_map_header_at`: the offset after the head and its argument.
 */
const canonicalHeadAt = (
  bytes: Uint8Array,
  offset: number,
  major: number,
): [number, bigint] => {
  const additional = byteAt(bytes, offset) - major * 32;
  if (additional >= 0 && additional < 24) {
    return [offset + 1, BigInt(additional)];
  }
  const widths = new Map([
    [24, [1, 24n]],
    [25, [2, 0x100n]],
    [26, [4, 0x10000n]],
  ] as const);
  const shape = widths.get(additional as 24 | 25 | 26);
  if (shape === undefined) {
    throw new Malformed(`head ${byteAt(bytes, offset)} at ${offset}`);
  }
  const [width, least] = shape;
  const value = headValue(bytes, offset, width);
  if (value < least) throw new Malformed(`non-minimal head at ${offset}`);
  return [offset + 1 + width, value];
};

/**
 * A canonical byte string, the twin of `decode_canonical_bytes_at`: the offset
 * after it and its bytes, cut short at the end of the field as the mint's
 * slice is.
 */
const canonicalBytesAt = (
  bytes: Uint8Array,
  offset: number,
): [number, Uint8Array] => {
  const [start, length] = canonicalHeadAt(bytes, offset, 2);
  const end = start + Number(length);
  return [end, bytes.subarray(start, end)];
};

/**
 * A canonical integer, the twin of `decode_canonical_int_at`: a minimal
 * unsigned or negative integer with at most eight argument bytes.
 */
const canonicalIntAt = (
  bytes: Uint8Array,
  offset: number,
): [number, bigint] => {
  const tag = byteAt(bytes, offset);
  const major = tag < 0x20 ? 0 : 1;
  const additional = tag - major * 32;
  if (tag >= 0x40 || additional > 27) {
    throw new Malformed(`integer head ${tag.toString(16)} at ${offset}`);
  }
  let next: number;
  let argument: bigint;
  if (additional < 24) {
    [next, argument] = [offset + 1, BigInt(additional)];
  } else {
    const width = 2 ** (additional - 24);
    argument = headValue(bytes, offset, width);
    const least = additional === 24 ? 24n : 1n << BigInt(4 * width);
    if (argument < least) {
      throw new Malformed(`non-minimal integer at ${offset}`);
    }
    next = offset + 1 + width;
  }
  return [next, major === 0 ? argument : -1n - argument];
};

/**
 * Rule 6 on the mint field, the twin of the mint's `mint_items_are_valid`: the
 * checks the validation machine's stage-6 mint fold makes, so an admitted mint
 * field never stalls it. Each item is `[policy_id, {asset_name => quantity}]`:
 * a 28-byte policy id strictly after the previous item's, at least one asset,
 * each name at most 32 bytes and strictly after the previous name, each
 * quantity non-zero, the last asset ending exactly at the end of the item.
 * Keys are ordered by `compareMidgardAssetKeyBytes`. Returns the reason the
 * field is refused, or null.
 */
const mintItemsRefusal = (preimage: Uint8Array): string | null => {
  let previousPolicy: Uint8Array | null = null;
  return eachVariableItem(preimage, (start, end) => {
    const [policyOffset, itemCount] = definiteArrayHeaderAt(preimage, start);
    if (itemCount !== 2n) {
      throw new Malformed(`policy entry of ${itemCount} items`);
    }
    const [assetsHeaderOffset, policyId] = canonicalBytesAt(
      preimage,
      policyOffset,
    );
    const [assetsOffset, assetCount] = canonicalHeadAt(
      preimage,
      assetsHeaderOffset,
      5,
    );
    if (policyId.length !== MINT_POLICY_ID_BYTES) {
      return `policy id of ${policyId.length} bytes`;
    }
    if (assetCount === 0n) return "policy with no asset";
    if (
      previousPolicy !== null &&
      compareMidgardAssetKeyBytes(previousPolicy, policyId) >= 0
    ) {
      return "policy ids out of order";
    }
    previousPolicy = policyId;
    let cursor = assetsOffset;
    let previousAsset: Uint8Array | null = null;
    for (let remaining = assetCount; remaining > 0n; remaining -= 1n) {
      const [quantityOffset, assetName] = canonicalBytesAt(preimage, cursor);
      const [next, quantity] = canonicalIntAt(preimage, quantityOffset);
      if (assetName.length > MINT_MAX_ASSET_NAME_BYTES) {
        return `asset name of ${assetName.length} bytes`;
      }
      if (quantity === 0n) return "zero quantity";
      if (
        previousAsset !== null &&
        compareMidgardAssetKeyBytes(previousAsset, assetName) >= 0
      ) {
        return "asset names out of order";
      }
      if (remaining === 1n ? next !== end : next >= end) {
        return "assets do not end with the item";
      }
      previousAsset = assetName;
      cursor = next;
    }
    return null;
  });
};

const equalBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const checksOf = (
  tx: MidgardForcedTxAdmissionInput,
): ReadonlyArray<
  readonly [MidgardForcedTxAdmissionCheck, () => string | null]
> => {
  const { body, witnessSet } = tx;
  const whenCarried =
    (preimage: Uint8Array, screen: (preimage: Uint8Array) => string | null) =>
    () => (isEmptyField(preimage) ? null : screen(preimage));
  return [
    [
      "has_no_validity_upper_bound(body)",
      () =>
        body.validityIntervalEnd === MIDGARD_POSIX_TIME_NONE
          ? null
          : `validity upper bound ${body.validityIntervalEnd}`,
    ],
    [
      "has_a_spend_input(body)",
      () =>
        isEmptyField(body.spendInputsPreimageCbor) ? "no spend input" : null,
    ],
    [
      "has_no_auxiliary_data(body)",
      () =>
        equalBytes(body.auxiliaryDataHash, EMPTY_NULL_ROOT)
          ? null
          : "auxiliary data hash present",
    ],
    [
      "is_not_observed_on_untagged_network(body)",
      () =>
        equalBytes(body.scriptIntegrityHash, EMPTY_NULL_ROOT) ||
        isEmptyField(body.requiredObserversPreimageCbor) ||
        body.networkId !== MIDGARD_NATIVE_NETWORK_ID_NONE
          ? null
          : "script integrity hash and required observers with no network id",
    ],
    [
      "has_no_redeemers(witness_set)",
      () =>
        isEmptyField(witnessSet.redeemerTxWitsPreimageCbor)
          ? null
          : "redeemers present",
    ],
    [
      "script_witnesses_are_native(script_witnesses)",
      whenCarried(witnessSet.scriptTxWitsPreimageCbor, (preimage) =>
        eachVariableItem(preimage, (start, end) =>
          parseMidgardVersionedScriptHeader(
            preimage.subarray(start, end),
            end - start,
          )?.languageTag === NATIVE_LANGUAGE_TAG
            ? null
            : "script is not a native versioned script",
        ),
      ),
    ],
    [
      "outputs_are_admissible(outputs)",
      whenCarried(body.outputsPreimageCbor, (preimage) =>
        eachVariableItem(preimage, (start, end) =>
          outputRefusal(preimage, start, end),
        ),
      ),
    ],
    [
      "input_items_are_fixed(spend_inputs)",
      whenCarried(body.spendInputsPreimageCbor, inputItemsRefusal),
    ],
    [
      "input_items_are_fixed(reference_inputs)",
      whenCarried(body.referenceInputsPreimageCbor, inputItemsRefusal),
    ],
    [
      "hash28_items_are_fixed(required_observers)",
      whenCarried(body.requiredObserversPreimageCbor, hash28ItemsRefusal),
    ],
    [
      "hash28_items_are_fixed(required_signers)",
      whenCarried(body.requiredSignersPreimageCbor, hash28ItemsRefusal),
    ],
    [
      "address_witness_items_are_fixed(address_witnesses)",
      whenCarried(
        witnessSet.addrTxWitsPreimageCbor,
        addressWitnessItemsRefusal,
      ),
    ],
    [
      "mint_items_are_valid(mint)",
      whenCarried(body.mintPreimageCbor, mintItemsRefusal),
    ],
  ];
};

/**
 * Whether a forced transaction is one the tx-order mint admits: the off-chain
 * twin of `forced_tx_is_admissible_v1`, run by the SDK before it signs an order
 * and by the node on every order the chain admitted.
 *
 * It assumes what the mint has already established before it runs: every
 * field preimage is a §5.1 envelope whose item count is consistent with its
 * bytes. It returns the first check that fails, in the mint's order, or null
 * when every check passes. A read the mint would fail on is a refusal of the
 * check that made it.
 */
export const screenMidgardForcedTxAdmission = (
  tx: MidgardForcedTxAdmissionInput,
): MidgardForcedTxAdmissionRefusal | null => {
  for (const [check, run] of checksOf(tx)) {
    let reason: string | null;
    try {
      reason = run();
    } catch (error) {
      if (!(error instanceof Malformed)) throw error;
      reason = `malformed: ${error.message}`;
    }
    if (reason !== null) return { check, detail: reason };
  }
  return null;
};

/**
 * A forced transaction the tx-order mint would refuse. The SDK throws it before
 * signing, so no order is built for a transaction the chain cannot admit.
 */
export class MidgardForcedTxAdmissionRefused extends Error {
  readonly _tag = "MidgardForcedTxAdmissionRefused";
  readonly retryable = false;
  readonly refusal: MidgardForcedTxAdmissionRefusal;

  constructor(refusal: MidgardForcedTxAdmissionRefusal) {
    super(`Forced admission refused at ${refusal.check}: ${refusal.detail}`);
    this.name = "MidgardForcedTxAdmissionRefused";
    this.refusal = refusal;
  }
}
