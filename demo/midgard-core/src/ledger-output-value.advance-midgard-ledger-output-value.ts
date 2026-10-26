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
  MidgardLedgerOutputValueStages,
  type MidgardLedgerOutputValueTrace,
  type MidgardLedgerOutputValueTraceStep,
  type MidgardLedgerOutputValueWitness,
  summarizeBytes,
  summarizeInteger,
  UINT64_MAX,
} from "./ledger-output-value.is-well-formed-midgard-ledger-output-value-control.js";
import {
  buildMidgardValidationMerkleMembership,
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

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
      const leafHash = hashMidgardLedgerOutputAssetLeaf({
        policyId: witness.policyId,
        assetName: witness.assetName,
        quantity: witness.quantity,
      });
      if (
        !verifyMidgardValidationMerkleMembership({
          frontier: assetFrontier,
          // The control fixes the leaf: the fold walks the frontier from its
          // last leaf to its first (on-chain `asset_step`).
          leafIndex: control.assetRemaining - 1,
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
  // The frontier commits assets in plain byte order, the order of the
  // evaluated script context map. The fold opens leaf `assetRemaining - 1`
  // each step, so it walks them in descending order and the prepending
  // accumulator emits that ascending map.
  for (let assetIndex = assets.length - 1; assetIndex >= 0; assetIndex -= 1) {
    const asset = assets[assetIndex]!;
    const membership = buildMidgardValidationMerkleMembership(
      material.leaves,
      assetIndex,
    );
    const witness: MidgardLedgerOutputValueWitness = {
      policyId: Buffer.from(asset.policyId),
      assetName: Buffer.from(asset.assetName),
      quantity: asset.quantity,
      siblings: membership.siblings,
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
