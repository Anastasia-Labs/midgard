/**
 * CEK context stage 8: the mint fold, one authenticated mint asset per step.
 *
 * TypeScript twin of `verify_cek_mint_context_item`
 * (`onchain/aiken/lib/midgard/validation-machine/cek.ak`) and `next_mint_item`
 * (`onchain/aiken/lib/midgard/cek-context-step.ak`). Stage 6 appends the mint
 * leaves in strictly ascending (policy id, asset name) byte order, so the step
 * at `mintCursor` opens leaf `mintCount - 1 - mintCursor`: the fold walks the
 * frontier descending, each policy's assets are adjacent, and the prepended
 * summaries end in the evaluated context's mint map.
 */
import {
  type MidgardCekContextControl,
  summarizeMidgardCekLucidData,
} from "./cek-context.midgard-cek-context-control.js";
import {
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "./script-context-proof.js";

/** One mint-frontier leaf, in frontier (ascending) order. */
export type MidgardCekMintAsset = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
};

/** The frontier leaf the step at `mintCursor` opens. */
export const midgardCekMintItemIndex = (
  mintCursor: number,
  mintCount: number,
): number => mintCount - 1 - mintCursor;

const finalizeCurrentPolicy = (
  control: MidgardCekContextControl,
): MidgardCekDataSequenceSummary =>
  prependMidgardCekDataPairSummary(
    summarizeMidgardCekLucidData(control.currentMintPolicy.toString("hex")),
    summarizeMidgardCekMapData(control.currentMintAssets),
    control.mintPolicies,
  );

/** The control after the step that opens `asset`. */
export const advanceMidgardCekMintItem = (
  control: MidgardCekContextControl,
  asset: MidgardCekMintAsset,
): MidgardCekContextControl => {
  const policyChanged =
    control.currentMintPolicy.length !== 0 &&
    !control.currentMintPolicy.equals(asset.policyId);
  return {
    ...control,
    mintCursor: control.mintCursor + 1,
    currentMintPolicy: asset.policyId,
    currentMintAssets: prependMidgardCekDataPairSummary(
      summarizeMidgardCekLucidData(asset.assetName.toString("hex")),
      summarizeMidgardCekLucidData(asset.quantity),
      policyChanged
        ? emptyMidgardCekDataPairSummary()
        : control.currentMintAssets,
    ),
    mintPolicies: policyChanged
      ? finalizeCurrentPolicy(control)
      : control.mintPolicies,
  };
};

/** The terminal step: stage 9 with the finished mint map. */
export const finalizeMidgardCekMint = (
  control: MidgardCekContextControl,
): MidgardCekContextControl => {
  const mintPolicies = finalizeCurrentPolicy(control);
  return {
    ...control,
    stage: 9,
    currentMintPolicy: Buffer.alloc(0),
    currentMintAssets: emptyMidgardCekDataPairSummary(),
    mintPolicies,
    mintSummary: summarizeMidgardCekMapData(mintPolicies),
  };
};

/**
 * Runs stage 8 over a non-empty mint frontier. `onItem` sees each item step's
 * pre-control and the leaf it opens; `onTerminal` sees the terminal step's
 * pre-control. Returns the stage-9 control.
 */
export const runMidgardCekMintFold = (
  control: MidgardCekContextControl,
  assets: readonly MidgardCekMintAsset[],
  onItem?: (pre: MidgardCekContextControl, mintIndex: number) => void,
  onTerminal?: (pre: MidgardCekContextControl) => void,
): MidgardCekContextControl => {
  let next = control;
  while (next.mintCursor < assets.length) {
    const mintIndex = midgardCekMintItemIndex(next.mintCursor, assets.length);
    onItem?.(next, mintIndex);
    next = advanceMidgardCekMintItem(next, assets[mintIndex]!);
  }
  onTerminal?.(next);
  return finalizeMidgardCekMint(next);
};
