import { encodeMidgardTxOutput } from "@al-ft/lucid-midgard";
import {
  decodeMidgardTxOutput as decodeCoreMidgardTxOutput,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput as encodeCoreMidgardTxOutput,
  hashMidgardVersionedScript,
  type MidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

export type TestMidgardTxOutput = MidgardTxOutput & {
  readonly to_cbor_bytes: () => Buffer;
};

/**
 * The canonical out-ref bytes for tests: the §5.3 field-0/1 item encoding
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — a fixed 38 bytes with the
 * deliberately non-minimal 3-byte output index.
 *
 * The same bytes are the field-0/1 preimage item, the ledger MPF trie key, and
 * the ledger DB `outref` column, so tests must derive them from the shared
 * encoder rather than hand-rolling CML's minimal-index `TransactionInput` CBOR
 * (36 bytes for output indices 0–23) or literal `825820…00` hex.
 *
 * `txId` accepts the 32 raw bytes, a 64-char hex string, or a single repeated
 * fill byte, which is how the existing test fixtures spell their tx ids.
 */
export const makeOutRefCbor = (
  txId: Uint8Array | string | number,
  outputIndex: number | bigint = 0,
): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId:
      typeof txId === "number"
        ? Buffer.alloc(32, txId)
        : typeof txId === "string"
          ? Buffer.from(txId, "hex")
          : txId,
    outputIndex: Number(outputIndex),
  });

const datumOptionToPlutusData = (
  datum?: CML.DatumOption,
): CML.PlutusData | undefined => {
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
  address: CML.Address,
  value: CML.Value,
  datum?: CML.DatumOption,
  scriptRef?: CML.Script,
): CML.TransactionOutput => {
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
  address: CML.Address | string,
  value: CML.Value,
  datum?: CML.DatumOption,
  scriptRef?: CML.Script,
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
