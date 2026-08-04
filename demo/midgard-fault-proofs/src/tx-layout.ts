import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { type TxOutput } from "@lucid-evolution/lucid";

export const outputWithDatumAndUnitPredicate = ({
  address,
  datum,
  unit,
}: {
  readonly address: string;
  readonly datum: string;
  readonly unit: string;
}) => {
  const expectedDatum = canonicalPlutusDataCbor(datum);
  return (output: TxOutput): boolean =>
    output.address === address &&
    output.datum != null &&
    canonicalPlutusDataCbor(output.datum) === expectedDatum &&
    (output.assets[unit] ?? 0n) === 1n;
};

export const computationThreadOutputPredicate = outputWithDatumAndUnitPredicate;
