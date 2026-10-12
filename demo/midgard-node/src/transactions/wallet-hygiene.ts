import type { Assets, UTxO } from "@lucid-evolution/lucid";

export const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

export const positiveAssetEntries = (
  assets: Readonly<Assets>,
): readonly (readonly [string, bigint])[] =>
  Object.entries(assets).filter(([, amount]) => amount > 0n);

export const hasPositiveNonLovelaceAsset = (utxo: UTxO): boolean =>
  positiveAssetEntries(utxo.assets).some(([unit]) => unit !== "lovelace");

export const isPlainAdaOnlyUtxo = (utxo: UTxO): boolean => {
  if (utxo.datum !== undefined || utxo.datumHash !== undefined) {
    return false;
  }
  if (utxo.scriptRef !== undefined) {
    return false;
  }
  const positiveAssets = positiveAssetEntries(utxo.assets);
  return (
    positiveAssets.length === 1 &&
    positiveAssets[0]?.[0] === "lovelace" &&
    positiveAssets[0][1] > 0n
  );
};
