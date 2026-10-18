import { parseMidgardVersionedScriptHeader } from "@al-ft/midgard-core";
import {
  type ScriptDecodeRecord,
  scriptDecodeRecordOfScript,
  scriptDecodeScriptHash,
} from "@al-ft/midgard-validation";

/**
 * A Plutus item whose decode record refuses it with class `c` closes the
 * script-decoding families as `PLUTUS_SCRIPT_REFUSED_RESULT_BASE + c`, the
 * twin of the on-chain `result_plutus_refused_base`.
 */
export const PLUTUS_SCRIPT_REFUSED_RESULT_BASE = 100;

/**
 * `PLUTUS_SCRIPT_REFUSED_RESULT_BASE + class` for a refused Plutus item; the
 * base admits any class the reason may name.
 */
export type PlutusScriptRefusedResultClass = number;

/** `item_kind` of `PlutusScriptUndecodable` for a witness field-6 item. */
export const PLUTUS_SCRIPT_ITEM_KIND_WITNESS = 0n;
/** `item_kind` of `PlutusScriptUndecodable` for an output's reference script. */
export const PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT = 1n;

/** The script a Plutus item carries, and how Cardano decodes it. */
export type PlutusItemScriptDecode = Readonly<{
  language: number;
  scriptBytes: Buffer;
  scriptHashHex: string;
  record: ScriptDecodeRecord;
}>;

/**
 * The script a versioned-script item carries when its language is a Plutus
 * one (3 or 128), with the record a decode walk of it ends in; undefined for
 * a native or malformed item. The script hash is the one the on-chain item
 * check (`item_record_class_v1`) requires of the record.
 */
export const plutusItemScriptDecode = (
  item: Uint8Array,
): PlutusItemScriptDecode | undefined => {
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  if (header === null || header.languageTag === 0) return undefined;
  const scriptBytes = Buffer.from(item.subarray(header.payloadOffset));
  return Object.freeze({
    language: header.languageTag,
    scriptBytes,
    scriptHashHex: scriptDecodeScriptHash(
      header.languageTag,
      scriptBytes,
    ).toString("hex"),
    record: scriptDecodeRecordOfScript(header.languageTag, scriptBytes),
  });
};

/**
 * The class a decode record closes a Plutus item with: undefined when the
 * record is `Decoded` (no fault), `PLUTUS_SCRIPT_REFUSED_RESULT_BASE + class`
 * when it is `Refused`.
 */
export const plutusRecordResultClass = (
  record: ScriptDecodeRecord,
): number | undefined =>
  record.k === "decoded"
    ? undefined
    : PLUTUS_SCRIPT_REFUSED_RESULT_BASE + record.class;

/**
 * The result class `PlutusScriptUndecodable { item_kind, item_index, class }`
 * accuses, when it names `kind` and `index`; the on-chain binding requires
 * `class >= 1`.
 */
export const plutusUndecodableAccusedClass = (
  reason: unknown,
  kind: bigint,
  index: number,
): number | undefined => {
  if (
    typeof reason !== "object" ||
    reason === null ||
    !("PlutusScriptUndecodable" in reason)
  )
    return undefined;
  const payload = (
    reason as {
      readonly PlutusScriptUndecodable: {
        readonly item_kind: bigint;
        readonly item_index: bigint;
        readonly class: bigint;
      };
    }
  ).PlutusScriptUndecodable;
  if (
    payload.item_kind !== kind ||
    payload.item_index !== BigInt(index) ||
    payload.class < 1n ||
    payload.class > BigInt(Number.MAX_SAFE_INTEGER - 100)
  )
    throw new Error(
      "PlutusScriptUndecodable reason names another item or no class",
    );
  return PLUTUS_SCRIPT_REFUSED_RESULT_BASE + Number(payload.class);
};
