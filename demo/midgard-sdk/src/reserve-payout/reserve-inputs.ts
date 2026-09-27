import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  hasNonZeroAssetQuantity,
  minPositiveAssets,
  subtractAssets,
} from "./assets.js";

/** Why the reserve and payout validators can never spend this reserve UTxO.
 * Both require a NoDatum, script-free reserve input, and anyone can pay to the
 * reserve address, so such a UTxO must never be selected. */
export const reserveInputShapeRejection = (utxo: UTxO): string | undefined =>
  utxo.datum != null
    ? "carries an inline datum"
    : utxo.datumHash != null
      ? "carries a datum hash"
      : utxo.scriptRef != null
        ? "carries a reference script"
        : undefined;

/** Why this reserve UTxO cannot fund `neededAssets` in one AddFunds step. The
 * builder takes the per-unit minimum and returns the rest as exact reserve
 * change, which must itself be a ledger-valid output. */
export const reserveFundingRejection = (
  utxo: UTxO,
  neededAssets: Assets,
  coinsPerUtxoByte: bigint,
): string | undefined => {
  const shape = reserveInputShapeRejection(utxo);
  if (shape !== undefined) return shape;
  const taken = minPositiveAssets(utxo.assets, neededAssets);
  if (Object.keys(taken).length === 0)
    return "contributes no still-needed payout asset";
  const change = subtractAssets(utxo.assets, taken);
  if (
    hasNonZeroAssetQuantity(change) &&
    (change.lovelace ?? 0n) <
      calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        address: utxo.address,
        assets: change,
      })
  )
    return "leaves reserve change below the minimum UTxO lovelace";
  return undefined;
};

/** Deterministic choice among fundable reserve UTxOs: the largest lovelace
 * contribution first, then canonical out-ref order. */
export const selectReserveFundingInput = (
  reserveUtxos: readonly UTxO[],
  neededAssets: Assets,
  coinsPerUtxoByte: bigint,
): UTxO | undefined => {
  const takenLovelace = (utxo: UTxO) =>
    minPositiveAssets(utxo.assets, neededAssets).lovelace ?? 0n;
  return reserveUtxos
    .filter(
      (utxo) =>
        reserveFundingRejection(utxo, neededAssets, coinsPerUtxoByte) ===
        undefined,
    )
    .sort((left, right) => {
      const difference = takenLovelace(right) - takenLovelace(left);
      return difference === 0n
        ? compareOutRefs(left, right)
        : difference > 0n
          ? 1
          : -1;
    })[0];
};
