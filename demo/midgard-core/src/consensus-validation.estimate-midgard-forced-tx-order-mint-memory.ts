import {
  byteAt,
  canonicalBytesAt,
  canonicalHeadAt,
  canonicalIntAt,
  definiteArrayHeaderAt,
  eachVariableItem,
  fieldHeader,
  headArgument,
  headWidth,
  isEmptyField,
  skipBytes,
  TWO_ENTRY_OUTPUT_MAP_HEAD,
} from "./consensus-validation.forced-tx-admission-reads.js";
import type { MidgardForcedTxAdmissionInput } from "./consensus-validation.screen-midgard-forced-tx-admission.js";

/**
 * The tx-order mint's memory budget for one order: the §3.3 reliability basis
 * (20% under the supported L1 floor of 16,500,000), as the tx-order mint
 * execution ledger (onchain/aiken/scripts/tx-order-mint-exec-ledger-v1.json)
 * prices it.
 */
export const MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_BASIS = 13_200_000;

/**
 * Per-shape memory the tx-order mint spends on a forced transaction's material
 * (its field door plus the admission rules), read off the measured rows of
 * `forced-tx-admission-v1-cost.test.ak` and the tx-order mint execution
 * ledger. Rule slopes subtract each row's fixture-only control; the door's
 * whole-test slope is conservative. Every rate rounds its measured slope up:
 *
 *   * `base`: the door at the aggregate bound with fixed-stride fields
 *     (2,370,675) plus the rules on a minimal order (603,349 - 338,205).
 *   * `variableItem`: the door's walk per item of a variable-width field,
 *     (12,660,422 - 7,319,914) / 250.
 *   * `fixedItem`: rule 4 per input, hash or address-witness item plus the
 *     door's share; the address-witness slope, the widest, is
 *     (2,474,726 - 408,908) / 316 rounded up with the door's 2,000.
 *   * `twoEntryOutput` / `threeEntryOutput`: rules 1 and 5 per output, the
 *     250-to-500 slopes with an empty asset map.
 *   * `outputPolicy` / `outputAsset`: the value walk of a three-entry output.
 *     The 20-to-40 policy slope (32,838.15) includes one asset per policy;
 *     the 50-to-100 asset slope (16,548) gives the asset charge, leaving
 *     16,290.15 per policy. Rows use 32-byte names and 8-byte quantities.
 *   * `nativeWitness`: rule 1 per field-6 item.
 *   * `mintPolicy` / `mintAsset`: rule 6 per single-asset policy and per
 *     further asset, the 250-to-500 policy slope and the 500-to-1,000 asset
 *     slope. The widths below add the work absent from these narrow rows.
 *   * `mintPositiveQuantity` / `mintNegativeQuantity`: surcharge per asset
 *     for direct / 1 / 2 / 4 / 8 argument bytes, from 100-to-200 asset slopes
 *     minus the narrow unsigned slope 26,218. Positive deltas are
 *     0 / 2,910 / 5,022 / 8,544 / 15,486; negative deltas are
 *     1,502 / 4,010 / 6,322 / 9,844 / 16,786.
 *   * `mintPairHead`: surcharge per policy for 82 / 98 / 99 / 9a pair heads,
 *     from the 50-to-100 policy slopes: 0 / 1,306 / 3,618 / 7,140.
 *   * `mintFirstAssetName`: the 32-byte first name versus the empty name
 *     costs 3,720 per policy; conservatively charged for any nonempty first
 *     name. `mintLongAssetName` adds 1,008 per further name of 24-to-32 bytes.
 *     Combined maximum rows measure 91,734 per policy and 44,012 per further
 *     asset, below the corresponding combined charges.
 *
 * These are the P1 forced-transaction capacity limits: an order whose estimate
 * exceeds the basis is one the mint is not expected to fit. They are an
 * estimate from controlled shapes, including the admitted signed integer and
 * nonminimal pair-head widths. They are not a proof of full transaction fit:
 * local L1 evaluation remains authoritative for combined memory, CPU, size
 * and carriage overhead that these admission-rule rows do not measure.
 */
export const MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_RATES = {
  base: 2_640_000,
  variableItem: 21_400,
  fixedItem: 8_600,
  twoEntryOutput: 15_300,
  threeEntryOutput: 31_900,
  outputPolicy: 16_300,
  outputAsset: 16_600,
  nativeWitness: 16_400,
  mintPolicy: 65_100,
  mintAsset: 26_300,
  mintPositiveQuantity: [0, 3_000, 5_100, 8_600, 15_500],
  mintNegativeQuantity: [1_600, 4_100, 6_400, 9_900, 16_800],
  mintPairHead: [0, 1_400, 3_700, 7_200],
  mintFirstAssetName: 3_800,
  mintLongAssetName: 1_100,
} as const;

export type MidgardForcedTxOrderMintEstimate = {
  readonly memory: number;
  readonly outputs: number;
  readonly outputPolicies: number;
  readonly outputAssets: number;
  readonly mintPolicies: number;
  readonly mintAssets: number;
};

const fieldItemCount = (preimage: Uint8Array): number =>
  isEmptyField(preimage) ? 0 : fieldHeader(preimage)[1];

/** Item counts of the value walk already admitted by the screen. The read
 * follows the mint's permissive skip_value, without decoding the inline datum. */
const outputValueItemCounts = (bytes: Uint8Array, start: number) => {
  const skipUnsigned = (offset: number) =>
    offset + (byteAt(bytes, offset) < 24 ? 1 : headWidth(bytes, offset, 0));
  const value = skipBytes(bytes, start + 2) + 1;
  const policiesOffset = skipUnsigned(value + 1);
  const policiesWidth = headWidth(bytes, policiesOffset, 5);
  const policies = headArgument(bytes, policiesOffset, policiesWidth);
  let cursor = policiesOffset + policiesWidth;
  let assets = 0;
  for (let policy = 0; policy < policies; policy += 1) {
    const assetsOffset = skipBytes(bytes, cursor);
    const assetsWidth = headWidth(bytes, assetsOffset, 5);
    const assetCount = headArgument(bytes, assetsOffset, assetsWidth);
    assets += assetCount;
    cursor = assetsOffset + assetsWidth;
    for (let asset = 0; asset < assetCount; asset += 1) {
      cursor = skipUnsigned(skipBytes(bytes, cursor));
    }
  }
  return { policies, assets };
};

/** The canonical integer has already been read before this price lookup. */
const mintQuantityMemory = (tag: number): number => {
  const rates = MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_RATES;
  const signedRates =
    tag < 0x20 ? rates.mintPositiveQuantity : rates.mintNegativeQuantity;
  switch (tag & 0x1f) {
    case 24:
      return signedRates[1];
    case 25:
      return signedRates[2];
    case 26:
      return signedRates[3];
    case 27:
      return signedRates[4];
    default:
      return signedRates[0];
  }
};

/**
 * The tx-order mint's estimated memory for a forced transaction the screen
 * admits, from {@link MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_RATES}. Run it only
 * on a transaction `screenMidgardForcedTxAdmission` admitted: it reads the
 * outputs and the mint field by the shapes the screen has already checked.
 */
export const estimateMidgardForcedTxOrderMintMemory = (
  tx: MidgardForcedTxAdmissionInput,
): MidgardForcedTxOrderMintEstimate => {
  const rates = MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_RATES;
  const { body, witnessSet } = tx;
  let memory = rates.base;
  let outputs = 0;
  let outputPolicies = 0;
  let outputAssets = 0;
  if (!isEmptyField(body.outputsPreimageCbor)) {
    eachVariableItem(body.outputsPreimageCbor, (start) => {
      outputs += 1;
      const twoEntry =
        body.outputsPreimageCbor[start] === TWO_ENTRY_OUTPUT_MAP_HEAD;
      memory +=
        rates.variableItem +
        (twoEntry ? rates.twoEntryOutput : rates.threeEntryOutput);
      if (!twoEntry) {
        const counts = outputValueItemCounts(body.outputsPreimageCbor, start);
        outputPolicies += counts.policies;
        outputAssets += counts.assets;
        memory +=
          counts.policies * rates.outputPolicy +
          counts.assets * rates.outputAsset;
      }
      return null;
    });
  }
  let mintPolicies = 0;
  let mintAssets = 0;
  if (!isEmptyField(body.mintPreimageCbor)) {
    const mint = body.mintPreimageCbor;
    eachVariableItem(mint, (start) => {
      const [policyOffset] = definiteArrayHeaderAt(mint, start);
      const [assetsHeaderOffset] = canonicalBytesAt(mint, policyOffset);
      const [assetsOffset, assetCount] = canonicalHeadAt(
        mint,
        assetsHeaderOffset,
        5,
      );
      mintPolicies += 1;
      mintAssets += Number(assetCount);
      memory +=
        rates.variableItem +
        rates.mintPolicy +
        (Number(assetCount) - 1) * rates.mintAsset;
      const pairHead = byteAt(mint, start);
      memory +=
        pairHead === 0x82
          ? rates.mintPairHead[0]
          : pairHead === 0x98
            ? rates.mintPairHead[1]
            : pairHead === 0x99
              ? rates.mintPairHead[2]
              : rates.mintPairHead[3];
      let cursor = assetsOffset;
      for (let asset = 0; asset < Number(assetCount); asset += 1) {
        const [quantityOffset, name] = canonicalBytesAt(mint, cursor);
        const [next] = canonicalIntAt(mint, quantityOffset);
        memory += mintQuantityMemory(byteAt(mint, quantityOffset));
        if (asset === 0 && name.length > 0) {
          memory += rates.mintFirstAssetName;
        } else if (name.length >= 24) {
          memory += rates.mintLongAssetName;
        }
        cursor = next;
      }
      return null;
    });
  }
  const nativeWitnesses = fieldItemCount(witnessSet.scriptTxWitsPreimageCbor);
  memory += nativeWitnesses * (rates.variableItem + rates.nativeWitness);
  const fixedItems = [
    body.spendInputsPreimageCbor,
    body.referenceInputsPreimageCbor,
    body.requiredObserversPreimageCbor,
    body.requiredSignersPreimageCbor,
    witnessSet.addrTxWitsPreimageCbor,
  ].reduce((sum, preimage) => sum + fieldItemCount(preimage), 0);
  memory += fixedItems * rates.fixedItem;
  return {
    memory,
    outputs,
    outputPolicies,
    outputAssets,
    mintPolicies,
    mintAssets,
  };
};

/**
 * A forced transaction the rules admit but whose order the tx-order mint is
 * not expected to fit in its execution budget. The SDK throws it before
 * signing.
 */
export class MidgardForcedTxOrderMintCapacityExceeded extends Error {
  readonly _tag = "MidgardForcedTxOrderMintCapacityExceeded";
  readonly retryable = false;
  readonly estimate: MidgardForcedTxOrderMintEstimate;

  constructor(estimate: MidgardForcedTxOrderMintEstimate) {
    super(
      `Forced transaction exceeds the tx-order mint's capacity: estimated ${estimate.memory} memory units, above the ${MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_BASIS} basis (${estimate.outputs} outputs, ${estimate.mintPolicies} mint policies, ${estimate.mintAssets} mint assets, ${estimate.outputPolicies} output policies, ${estimate.outputAssets} output assets). Split it into smaller forced transactions.`,
    );
    this.name = "MidgardForcedTxOrderMintCapacityExceeded";
    this.estimate = estimate;
  }
}
