import { CML } from "@lucid-evolution/lucid";
import {
  decodeMidgardTxOutput as decodeCoreMidgardTxOutput,
  encodeMidgardTxOutput as encodeCoreMidgardTxOutput,
  hashMidgardVersionedScript,
  protectMidgardAddress,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { encodeMidgardTxOutput } from "@al-ft/lucid-midgard";

export type TestMidgardTxOutput = MidgardTxOutput & {
  readonly to_cbor_bytes: () => Buffer;
};

const datumOptionToPlutusData = (
  datum?: InstanceType<typeof CML.DatumOption>,
): InstanceType<typeof CML.PlutusData> | undefined => {
  if (datum === undefined) {
    return undefined;
  }
  const inlineDatum = datum.as_datum();
  if (inlineDatum !== undefined) {
    return inlineDatum;
  }
  throw new Error("Midgard test outputs do not support datum hashes");
};

export const makeCardanoTxOutput = (
  address: InstanceType<typeof CML.Address>,
  value: InstanceType<typeof CML.Value>,
  datum?: InstanceType<typeof CML.DatumOption>,
  scriptRef?: InstanceType<typeof CML.Script>,
): InstanceType<typeof CML.TransactionOutput> => {
  const output = CML.ConwayFormatTxOut.new(address, value);
  if (datum !== undefined) {
    output.set_datum_option(datum);
  }
  if (scriptRef !== undefined) {
    output.set_script_reference(scriptRef);
  }
  return CML.TransactionOutput.new_conway_format_tx_out(output);
};

export const makeMidgardTxOutput = (
  address: InstanceType<typeof CML.Address> | string,
  value: InstanceType<typeof CML.Value>,
  datum?: InstanceType<typeof CML.DatumOption>,
  scriptRef?: InstanceType<typeof CML.Script>,
): TestMidgardTxOutput => {
  const cbor = encodeMidgardTxOutput(address, value, {
    datum: datumOptionToPlutusData(datum),
    scriptRef,
  });
  return {
    ...decodeCoreMidgardTxOutput(cbor),
    to_cbor_bytes: () => Buffer.from(cbor),
  };
};

export const protectOutputAddressBytes = (outputCbor: Uint8Array): Buffer => {
  const output = decodeCoreMidgardTxOutput(outputCbor);
  return encodeCoreMidgardTxOutput({
    ...output,
    address: protectMidgardAddress(output.address),
  });
};

export const hashPlutusV3Script = (scriptBytes: Uint8Array): string =>
  hashMidgardVersionedScript({
    language: "PlutusV3",
    scriptBytes: Buffer.from(scriptBytes),
  });

export const hashMidgardV1Script = (scriptBytes: Uint8Array): string =>
  hashMidgardVersionedScript({
    language: "MidgardV1",
    scriptBytes: Buffer.from(scriptBytes),
  });
