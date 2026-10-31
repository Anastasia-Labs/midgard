import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
} from "./codec/native-constants.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import {
  byteAt,
  eachVariableItem,
  fieldHeader,
  headArgument,
  headWidth,
  isEmptyField,
  Malformed,
  skipBytes,
  startsWithAt,
  TWO_ENTRY_OUTPUT_MAP_HEAD,
} from "./consensus-validation.forced-tx-admission-reads.js";
import { mintItemsRefusal } from "./consensus-validation.screen-midgard-forced-tx-mint-items.js";
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
 *
 * The keys are read by position, not through `decodeMidgardTxOutput`: the
 * output scan (`buildMidgardLedgerOutputScanTrace`, twin of
 * `ledger_output_scan_v1`) refuses every other shape as non-canonical, so this
 * read must agree with that scan on every output it accepts.
 * `tests/forced-tx-admission-v1.output-scan-fuzz.test.ts` checks it.
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
