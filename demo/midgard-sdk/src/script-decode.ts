import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { H32Schema, ScriptHashSchema } from "./common.js";

/** Twin of midgard/script_decode_v1.ScriptDecodeRecordV1. */
export const ScriptDecodeRecordSchema = Data.Enum([
  Data.Object({
    Decoded: Data.Object({
      script_hash: ScriptHashSchema,
      language: Data.Integer(),
      program_version: Data.Integer({ minimum: -1, maximum: 1 }),
      term_root: H32Schema,
      phase2_class: Data.Integer(),
    }),
  }),
  Data.Object({
    Refused: Data.Object({
      script_hash: ScriptHashSchema,
      language: Data.Integer(),
      class: Data.Integer({ minimum: 1, maximum: 17 }),
      bit_offset: Data.Integer({ minimum: 0 }),
    }),
  }),
]);
export type ScriptDecodeRecord = Data.Static<typeof ScriptDecodeRecordSchema>;
export const ScriptDecodeRecord = asDataType<ScriptDecodeRecord>(
  ScriptDecodeRecordSchema,
);

/**
 * Authenticate the inline record selected by a canonical transaction reference-input
 * index. The decoder policy mints its permanent record token under the script hash;
 * the selection successor also binds that hash and language to the selected script.
 */
export const scriptDecodeRecordFromReferenceInput = ({
  referenceInputs,
  referenceInputIndex,
  scriptDecodePolicyId,
  scriptHash,
  language,
}: {
  readonly referenceInputs: readonly UTxO[];
  readonly referenceInputIndex: number;
  readonly scriptDecodePolicyId: string;
  readonly scriptHash: string;
  readonly language: 3 | 128;
}): ScriptDecodeRecord => {
  if (
    !/^[0-9a-f]{56}$/u.test(scriptDecodePolicyId) ||
    !/^[0-9a-f]{56}$/u.test(scriptHash)
  ) {
    throw new Error(
      "Script-decode policy and script hash must each be 28 bytes of hexadecimal",
    );
  }
  if (!Number.isSafeInteger(referenceInputIndex) || referenceInputIndex < 0) {
    throw new Error(
      "Script-decode record reference input index must be a non-negative safe integer",
    );
  }
  const input = referenceInputs[referenceInputIndex];
  if (typeof input?.datum !== "string" || input.datumHash != null) {
    throw new Error(
      "Script-decode record reference input must carry an inline datum",
    );
  }
  const record = Data.from(input.datum, ScriptDecodeRecord);
  const fields = "Decoded" in record ? record.Decoded : record.Refused;
  if (
    fields.script_hash !== scriptHash ||
    fields.language !== BigInt(language)
  ) {
    throw new Error(
      "Script-decode record does not match the selected script hash and language",
    );
  }
  if (input.assets[scriptDecodePolicyId + fields.script_hash] !== 1n) {
    throw new Error(
      "Script-decode record reference input lacks its unique authentication token",
    );
  }
  return record;
};
