import { CML } from "@lucid-evolution/lucid";
import {
  hashMidgardVersionedScript,
  protectMidgardAddress,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
// Validation-side `encode/decodeMidgardTxOutput` operate on midgard-ts binary
// bytes (the post-Phase 5 wire format) but expose the core `MidgardTxOutput`
// shape. `encodeMidgardTxOutput` from `@al-ft/lucid-midgard` also emits the
// same midgard-ts binary, so the encode/decode round-trip stays self-consistent.
import {
  decodeMidgardTxOutput as decodeMidgardTsTxOutput,
  encodeMidgardTxOutput as encodeMidgardTsTxOutputStruct,
} from "@/validation/midgard-output.js";
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

// Builds a Midgard test output backed by midgard-ts binary bytes. `to_cbor_bytes`
// is kept as the accessor name for back-compat with existing tests, but the
// bytes are now midgard-ts wire-format, not CBOR.
export const makeMidgardTxOutput = (
  address: InstanceType<typeof CML.Address> | string,
  value: InstanceType<typeof CML.Value>,
  datum?: InstanceType<typeof CML.DatumOption>,
  scriptRef?: InstanceType<typeof CML.Script>,
): TestMidgardTxOutput => {
  const bytes = encodeMidgardTxOutput(address, value, {
    datum: datumOptionToPlutusData(datum),
    scriptRef,
  });
  return {
    ...decodeMidgardTsTxOutput(bytes),
    to_cbor_bytes: () => Buffer.from(bytes),
  };
};

export const protectOutputAddressBytes = (outputBytes: Uint8Array): Buffer => {
  const output = decodeMidgardTsTxOutput(outputBytes);
  return encodeMidgardTsTxOutputStruct({
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
