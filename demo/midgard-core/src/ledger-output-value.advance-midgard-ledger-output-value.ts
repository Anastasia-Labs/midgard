import {
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "./cek-semantic.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  buildMidgardLedgerOutputAssetFrontier,
  hashMidgardLedgerOutputAssetLeaf,
  type MidgardLedgerOutputAsset,
} from "./ledger-output-commitment.js";
import {
  advanced,
  exactUint64,
  finalizeCurrentPolicy,
  initialMidgardLedgerOutputValueControl,
  isWellFormedMidgardLedgerOutputValueControl,
  type MidgardLedgerOutputValueControl,
  type MidgardLedgerOutputValueHead,
  MidgardLedgerOutputValueStages,
  type MidgardLedgerOutputValueTrace,
  type MidgardLedgerOutputValueTraceStep,
  type MidgardLedgerOutputValueWitness,
  sequencesEqual,
  summarizeBytes,
  summarizeInteger,
  UINT64_MAX,
} from "./ledger-output-value.is-well-formed-midgard-ledger-output-value-control.js";
import {
  buildMidgardValidationMerkleMembership,
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

/**
 * Strict descending execution order over independently authenticated source
 * indices, mirroring the on-chain `order_is_valid`. The native frontier
 * commits assets in canonical (length-then-bytes) key order while the
 * evaluated script context orders asset names lexicographically; the fold
 * proves that permutation instead of changing either ordering contract.
 */
const orderIsValid = ({
  control,
  assetCount,
  policyId,
  assetName,
  previous,
}: {
  readonly control: MidgardLedgerOutputValueControl;
  readonly assetCount: number;
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly previous: MidgardLedgerOutputValueHead | null;
}): boolean => {
  if (control.currentPolicy.length === 0) {
    return previous === null && control.assetRemaining === assetCount;
  }
  if (policyId.equals(control.currentPolicy)) {
    if (previous === null) return false;
    return (
      Buffer.compare(assetName, previous.assetName) < 0 &&
      sequencesEqual(
        prependMidgardCekDataPairSummary(
          summarizeBytes(previous.assetName),
          summarizeInteger(previous.quantity),
          previous.tail,
        ),
        control.currentAssets,
      )
    );
  }
  return (
    previous === null && Buffer.compare(policyId, control.currentPolicy) < 0
  );
};

export const advanceMidgardLedgerOutputValue = ({
  control,
  assetFrontier,
  lovelace,
  witness,
}: {
  readonly control: MidgardLedgerOutputValueControl;
  readonly assetFrontier: MidgardValidationMerkleFrontier;
  readonly lovelace: bigint;
  readonly witness: MidgardLedgerOutputValueWitness | null;
}): MidgardLedgerOutputValueControl | null => {
  try {
    if (
      !isWellFormedMidgardLedgerOutputValueControl(control) ||
      control.assetRemaining > assetFrontier.count
    ) {
      return null;
    }
    exactUint64(lovelace, "lovelace");
    if (control.stage === MidgardLedgerOutputValueStages.Assets) {
      if (control.assetRemaining === 0) {
        return witness === null
          ? advanced({
              ...control,
              stage: MidgardLedgerOutputValueStages.Finalize,
            })
          : null;
      }
      if (
        witness === null ||
        witness.policyId.length !== 28 ||
        witness.assetName.length > 32 ||
        witness.quantity <= 0n ||
        witness.quantity > UINT64_MAX
      ) {
        return null;
      }
      if (
        !orderIsValid({
          control,
          assetCount: assetFrontier.count,
          policyId: witness.policyId,
          assetName: witness.assetName,
          previous: witness.previous,
        })
      ) {
        return null;
      }
      const leafHash = hashMidgardLedgerOutputAssetLeaf({
        policyId: witness.policyId,
        assetName: witness.assetName,
        quantity: witness.quantity,
      });
      if (
        !verifyMidgardValidationMerkleMembership({
          frontier: assetFrontier,
          leafIndex: witness.assetIndex,
          leafHash,
          siblings: witness.siblings.map((sibling) =>
            ensureHash32(sibling, "ledger_output_value_v1.sibling"),
          ),
        })
      ) {
        return null;
      }
      const policyChanged = !witness.policyId.equals(control.currentPolicy);
      const valueEntries =
        policyChanged && control.currentPolicy.length !== 0
          ? finalizeCurrentPolicy(control)
          : control.valueEntries;
      const currentAssets = policyChanged
        ? emptyMidgardCekDataPairSummary()
        : control.currentAssets;
      return advanced({
        ...control,
        assetRemaining: control.assetRemaining - 1,
        currentPolicy: Buffer.from(witness.policyId),
        currentAssets: prependMidgardCekDataPairSummary(
          summarizeBytes(witness.assetName),
          summarizeInteger(witness.quantity),
          currentAssets,
        ),
        valueEntries,
      });
    }
    if (control.stage === MidgardLedgerOutputValueStages.Finalize) {
      if (witness !== null) return null;
      let valueEntries = finalizeCurrentPolicy(control);
      if (lovelace !== 0n) {
        const emptyBytes = summarizeBytes(Buffer.alloc(0));
        const coinAssets = prependMidgardCekDataPairSummary(
          emptyBytes,
          summarizeInteger(lovelace),
          emptyMidgardCekDataPairSummary(),
        );
        valueEntries = prependMidgardCekDataPairSummary(
          emptyBytes,
          summarizeMidgardCekMapData(coinAssets),
          valueEntries,
        );
      }
      return advanced({
        ...control,
        stage: MidgardLedgerOutputValueStages.Terminal,
        currentPolicy: Buffer.alloc(0),
        currentAssets: emptyMidgardCekDataPairSummary(),
        valueEntries: emptyMidgardCekDataPairSummary(),
        result: summarizeMidgardCekMapData(valueEntries),
      });
    }
    return null;
  } catch {
    return null;
  }
};

export const finalizeMidgardLedgerOutputValue = (
  control: MidgardLedgerOutputValueControl,
): MidgardCekDataSummary | null =>
  isWellFormedMidgardLedgerOutputValueControl(control) &&
  control.stage === MidgardLedgerOutputValueStages.Terminal
    ? control.result
    : null;

export const buildMidgardLedgerOutputValueTrace = ({
  assets,
  lovelace,
}: {
  readonly assets: readonly MidgardLedgerOutputAsset[];
  readonly lovelace: bigint;
}): MidgardLedgerOutputValueTrace => {
  exactUint64(lovelace, "lovelace");
  const material = buildMidgardLedgerOutputAssetFrontier(assets);
  const initial = initialMidgardLedgerOutputValueControl(material.count);
  const steps: MidgardLedgerOutputValueTraceStep[] = [];
  let control = initial;
  // The frontier commits assets in canonical (length-then-bytes) key order;
  // the evaluated script context map is lexicographic. Sort the traversal
  // lexicographically while retaining each asset's original frontier index,
  // then fold in descending order so the prepending accumulator emits the
  // ascending lexicographic map the evaluated context commits to.
  const traversal = assets
    .map((asset, assetIndex) => ({ asset, assetIndex }))
    .sort(
      (left, right) =>
        Buffer.compare(
          Buffer.from(left.asset.policyId),
          Buffer.from(right.asset.policyId),
        ) ||
        Buffer.compare(
          Buffer.from(left.asset.assetName),
          Buffer.from(right.asset.assetName),
        ),
    )
    .reverse();
  let lastHead: MidgardLedgerOutputValueHead | null = null;
  for (const { asset, assetIndex } of traversal) {
    const membership = buildMidgardValidationMerkleMembership(
      material.leaves,
      assetIndex,
    );
    const policyId = Buffer.from(asset.policyId);
    const samePolicy =
      control.currentPolicy.length !== 0 &&
      policyId.equals(control.currentPolicy);
    const witness: MidgardLedgerOutputValueWitness = {
      assetIndex,
      policyId,
      assetName: Buffer.from(asset.assetName),
      quantity: asset.quantity,
      siblings: membership.siblings,
      previous: samePolicy ? lastHead : null,
    };
    lastHead = {
      assetName: Buffer.from(asset.assetName),
      quantity: asset.quantity,
      tail: samePolicy
        ? control.currentAssets
        : emptyMidgardCekDataPairSummary(),
    };
    const next = advanceMidgardLedgerOutputValue({
      control,
      assetFrontier: material.frontier,
      lovelace,
      witness,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output Value fold failed");
    }
    steps.push({ control, witness, next });
    control = next;
  }
  for (let localStep = 0; localStep < 2; localStep += 1) {
    const next = advanceMidgardLedgerOutputValue({
      control,
      assetFrontier: material.frontier,
      lovelace,
      witness: null,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output Value close failed");
    }
    steps.push({ control, witness: null, next });
    control = next;
  }
  if (finalizeMidgardLedgerOutputValue(control) === null) {
    throw new Error("Canonical V1 ledger output Value did not terminate");
  }
  return {
    assets,
    frontier: material.frontier,
    initial,
    steps,
    terminal: control,
  };
};
