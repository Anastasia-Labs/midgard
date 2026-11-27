import { parseMidgardVersionedScriptHeader } from "@al-ft/midgard-core";
import {
  type ScriptDecodeRecord,
  scriptDecodeScriptHash,
} from "@al-ft/midgard-validation/script-decode.step";
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation/script-decode.step-walk";

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

/** The script a Plutus item carries, without its decode record. */
export type PlutusItemScript = Omit<PlutusItemScriptDecode, "record">;

/**
 * The script a versioned-script item carries when its language is a Plutus
 * one (3 or 128); undefined for a native or malformed item. The script hash
 * is the one the on-chain item check (`item_record_class_v1`) requires of
 * the record. Unlike `plutusItemScriptDecode`, it does not walk the script.
 */
export const plutusItemScript = (
  item: Uint8Array,
): PlutusItemScript | undefined => {
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
  });
};

/**
 * `plutusItemScript`, with the record a decode walk of the script ends in.
 */
export const plutusItemScriptDecode = (
  item: Uint8Array,
): PlutusItemScriptDecode | undefined => {
  const script = plutusItemScript(item);
  if (script === undefined) return undefined;
  return Object.freeze({
    ...script,
    record: scriptDecodeRecordOfScript(script.language, script.scriptBytes),
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
 * The class a rejection naming a Plutus item kind no family decodes, or no
 * decode class, accuses: no item closes with it, so any replay contradicts
 * it. Twin of the on-chain `result_unreachable_class`.
 */
export const PLUTUS_SCRIPT_UNREACHABLE_ACCUSED_CLASS = 100n;

/**
 * The result class `PlutusScriptUndecodable { item_kind, item_index, class }`
 * accuses when the family that decodes `kind` binds it at `index`: exact twin
 * of the on-chain binding. The witness family binds every item kind but the
 * reference-script one; the output family binds only its own. A reason naming
 * `kind` with `class >= 1` accuses `100 + class`; any other admitted reason
 * accuses `PLUTUS_SCRIPT_UNREACHABLE_ACCUSED_CLASS`. Undefined when the reason
 * is not `PlutusScriptUndecodable`; throws when the family does not bind it.
 */
export const plutusUndecodableAccusedClass = (
  reason: unknown,
  kind: bigint,
  index: bigint,
): bigint | undefined => {
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
  const bindsKind =
    kind === PLUTUS_SCRIPT_ITEM_KIND_WITNESS
      ? payload.item_kind !== PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT
      : payload.item_kind === kind;
  if (!bindsKind || payload.item_index !== index)
    throw new Error("PlutusScriptUndecodable reason names another item");
  return payload.item_kind === kind && payload.class >= 1n
    ? BigInt(PLUTUS_SCRIPT_REFUSED_RESULT_BASE) + payload.class
    : PLUTUS_SCRIPT_UNREACHABLE_ACCUSED_CLASS;
};
