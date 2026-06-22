import { LOVELACE_UNIT } from "@al-ft/midgard-core/assets";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import type { Assets, TxOutput, UTxO } from "@lucid-evolution/lucid";

export const outputDatumCborMatches = (
  output: Pick<TxOutput, "datum">,
  datumCbor: string,
): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const positiveAssetEntries = (
  assets: Readonly<Assets>,
): readonly (readonly [string, bigint])[] =>
  Object.entries(assets).filter(([, amount]) => amount > 0n);

export const isPlainPositiveAdaOnlyUtxo = (utxo: UTxO): boolean => {
  if (utxo.datum !== undefined || utxo.datumHash !== undefined) {
    return false;
  }
  if (utxo.scriptRef !== undefined) {
    return false;
  }
  const positiveAssets = positiveAssetEntries(utxo.assets);
  return (
    positiveAssets.length === 1 &&
    positiveAssets[0]?.[0] === LOVELACE_UNIT &&
    positiveAssets[0][1] > 0n
  );
};
