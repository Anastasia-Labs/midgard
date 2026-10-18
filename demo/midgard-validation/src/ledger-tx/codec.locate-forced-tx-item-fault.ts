import {
  classifyMidgardWitnessScriptItem,
  type MidgardLedgerOutputProofRefusalKind,
  MidgardLedgerOutputProofResultKinds,
  traceMidgardLedgerOutputProof,
} from "@al-ft/midgard-core";
import { decodeMidgardNativeByteListPreimage } from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";

import type { RejectSubject } from "../reject-subject.js";
import { scriptDecodeRecordOfScript } from "../script-decode.step-walk.js";
import { type RejectCode, RejectCodes } from "../types.js";

/**
 * The class of a faulty output of a forced transaction. The first four are
 * the output proof's refusals; `plutusScriptUndecodable` is a Plutus
 * reference script whose decode record refuses it.
 */
export type MidgardForcedTxOutputFaultClass =
  | "invalidOutput"
  | "referenceScriptMalformed"
  | "referenceScriptNodeLimit"
  | "referenceScriptDepthLimit"
  | "plutusScriptUndecodable";

/** A faulty output of a forced transaction, by position, with its class. */
export type MidgardForcedTxOutputFault = Readonly<{
  itemIndex: number;
  itemClass: MidgardForcedTxOutputFaultClass;
  /** The decode record's class (1-17), for `plutusScriptUndecodable`. */
  refusalClass?: number;
}>;

/**
 * The classes validation does not refuse yet: an output of such a class is
 * admitted, and a later faulty output is the first.
 */
export const MIDGARD_HELD_FORCED_TX_OUTPUT_FAULT_CLASSES: ReadonlySet<MidgardForcedTxOutputFaultClass> =
  new Set(["plutusScriptUndecodable"]);

const CLASSES_OF_REFUSALS: Readonly<
  Record<MidgardLedgerOutputProofRefusalKind, MidgardForcedTxOutputFaultClass>
> = {
  [MidgardLedgerOutputProofResultKinds.InvalidOutput]: "invalidOutput",
  [MidgardLedgerOutputProofResultKinds.InvalidReferenceScript]:
    "referenceScriptMalformed",
  [MidgardLedgerOutputProofResultKinds.NativeScriptNodeLimit]:
    "referenceScriptNodeLimit",
  [MidgardLedgerOutputProofResultKinds.NativeScriptDepthLimit]:
    "referenceScriptDepthLimit",
};

/** The class of an output the output proof refuses with `refusal`. */
export const outputProofRefusalClass = (
  refusal: MidgardLedgerOutputProofRefusalKind,
): MidgardForcedTxOutputFaultClass => CLASSES_OF_REFUSALS[refusal];

/**
 * The class of the output at field position `itemIndex`, or null when it is
 * not faulty. The output proof the machine runs at the ScriptSources output
 * scan decides the class; an output it admits is faulty only when it carries
 * a Plutus reference script whose decode record refuses it.
 */
export const classifyMidgardForcedTxOutputItem = (
  itemIndex: number,
  outputCbor: Uint8Array,
): MidgardForcedTxOutputFault | null => {
  const outcome = traceMidgardLedgerOutputProof({
    outputIndex: itemIndex,
    outputCbor,
  });
  if (outcome.kind === "refused") {
    return { itemIndex, itemClass: outputProofRefusalClass(outcome.refusal) };
  }
  const scan = outcome.trace.terminal.outputScan;
  if (scan.referenceScriptLanguage <= 0) return null;
  const record = scriptDecodeRecordOfScript(
    scan.referenceScriptLanguage,
    outputCbor.subarray(
      scan.referenceScriptOffset,
      scan.referenceScriptOffset + scan.referenceScriptLength,
    ),
  );
  return record.k === "refused"
    ? {
        itemIndex,
        itemClass: "plutusScriptUndecodable",
        refusalClass: record.class,
      }
    : null;
};

/**
 * The rejection that refuses a faulty output: the output proof's refusal,
 * as the machine emits it at the ScriptSources output scan.
 */
export const forcedTxOutputFaultRejection = (
  fault: MidgardForcedTxOutputFault,
): { readonly code: RejectCode; readonly subject: RejectSubject } => {
  const index = BigInt(fault.itemIndex);
  switch (fault.itemClass) {
    case "invalidOutput":
      return {
        code: RejectCodes.InvalidOutput,
        subject: { arm: "OutputNonCanonical", index },
      };
    case "referenceScriptMalformed":
      return {
        code: RejectCodes.InvalidFieldType,
        subject: { arm: "OutputReferenceScriptMalformed", index },
      };
    case "referenceScriptNodeLimit":
      return {
        code: RejectCodes.NativeScriptNodeCount,
        subject: { arm: "OutputReferenceScriptNodeLimit", index },
      };
    case "referenceScriptDepthLimit":
      return {
        code: RejectCodes.NativeScriptDepth,
        subject: { arm: "OutputReferenceScriptDepthLimit", index },
      };
    case "plutusScriptUndecodable":
      return {
        code: RejectCodes.ScriptProgramEncoding,
        subject: {
          arm: "PlutusScriptUndecodable",
          itemKind: 1n,
          index,
          refusalClass: BigInt(fault.refusalClass!),
        },
      };
  }
};

/** A faulty field-6 item of a forced transaction and its proof class. */
export type MidgardForcedTxWitnessScriptFault = Readonly<{
  itemIndex: number;
  itemClass: "headerMalformed" | "nativeMalformed";
}>;

/**
 * The first faulty output of a forced transaction whose class is not in
 * `held`, with its class (see {@link classifyMidgardForcedTxOutputItem}).
 * Returns null when no such output exists, or when the field-2 list itself is
 * not a §5.1 envelope (the tx-order mint refuses such an order).
 *
 * Any item this names is faulty, so a rejection that cites it cannot be
 * disproved; choosing the lowest index keeps the verdict deterministic.
 */
export const locateMidgardForcedTxOutputFault = (
  txCbor: Uint8Array,
  held: ReadonlySet<MidgardForcedTxOutputFaultClass> = MIDGARD_HELD_FORCED_TX_OUTPUT_FAULT_CLASSES,
): MidgardForcedTxOutputFault | null => {
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
    const fault = classifyMidgardForcedTxOutputItem(
      itemIndex,
      outputs[itemIndex]!,
    );
    if (fault !== null && !held.has(fault.itemClass)) return fault;
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
