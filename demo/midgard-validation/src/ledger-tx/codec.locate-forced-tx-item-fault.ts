import {
  classifyMidgardWitnessScriptItem,
  isCanonicalMidgardCekData,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";

/** A faulty item of a forced transaction, by field and item position. */
export type MidgardForcedTxItemFault = Readonly<{
  field: 2;
  itemIndex: number;
}>;

/** A faulty field-6 item of a forced transaction and its proof class. */
export type MidgardForcedTxWitnessScriptFault = Readonly<{
  itemIndex: number;
  itemClass: "headerMalformed" | "nativeMalformed";
}>;

/**
 * The first output of a forced transaction that is not a canonical ledger
 * output: its address, value or datum does not decode, or its inline datum is
 * not canonical Data as the on-chain streamed traverse decides it. Returns
 * null when every output is canonical, or when the field-2 list itself is not
 * a §5.1 envelope (the tx-order mint refuses such an order).
 *
 * Any item this names is faulty, so a rejection that cites it cannot be
 * disproved; choosing the lowest index keeps the verdict deterministic.
 */
export const locateMidgardForcedTxOutputFault = (
  txCbor: Uint8Array,
): MidgardForcedTxItemFault | null => {
  let outputs: readonly Uint8Array[];
  try {
    outputs = decodeMidgardNativeByteListPreimage(
      deriveMidgardForcedTxFaultEvidenceMaterial(txCbor).canonical.body
        .outputsPreimageCbor,
      "native.outputs",
    );
  } catch {
    return null;
  }
  for (let itemIndex = 0; itemIndex < outputs.length; itemIndex += 1) {
    let datum: Uint8Array | undefined;
    try {
      datum = decodeMidgardTxOutput(outputs[itemIndex]!).datum?.cbor;
    } catch {
      return { field: 2, itemIndex };
    }
    if (datum !== undefined && !isCanonicalMidgardCekData(datum))
      return { field: 2, itemIndex };
  }
  return null;
};

/**
 * The first field-6 item of a forced transaction that the witness-script
 * decoding proof classifies as faulty, with its class. The classifier is the
 * one the proof twin uses, so the named item is the one the proof finds
 * faulty. Returns null when no item is faulty, when the field is not a §5.1
 * envelope, or when the first faulty item is over a node or depth limit (an
 * item that size exceeds the field size cap the tx-order mint enforces).
 */
export const locateMidgardForcedTxWitnessScriptFault = (
  txCbor: Uint8Array,
): MidgardForcedTxWitnessScriptFault | null => {
  let items: readonly Uint8Array[];
  try {
    items = decodeMidgardNativeByteListPreimage(
      deriveMidgardForcedTxFaultEvidenceMaterial(txCbor).canonical.witnessSet
        .scriptTxWitsPreimageCbor,
      "native.script_tx_wits",
    );
  } catch {
    return null;
  }
  for (let itemIndex = 0; itemIndex < items.length; itemIndex += 1) {
    const { itemClass } = classifyMidgardWitnessScriptItem(items[itemIndex]!);
    if (itemClass === "noFault") continue;
    return itemClass === "headerMalformed" || itemClass === "nativeMalformed"
      ? { itemIndex, itemClass }
      : null;
  }
  return null;
};
