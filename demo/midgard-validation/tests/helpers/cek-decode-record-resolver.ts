import type { MidgardVersionedScript } from "@al-ft/midgard-core";

import { scriptDecodeRecordOfScript } from "../../src/script-decode.step-walk.js";
import type { CekSelectionDecodeRecordResolver } from "../../src/validation-machine-data.js";

/**
 * The decoded records a dispute composer would reference for a transaction's
 * carried Plutus scripts: each one is the record walk's own verdict on the
 * script's stored bytes.
 */
export const decodeRecordResolverOf =
  (
    scripts: readonly MidgardVersionedScript[],
  ): CekSelectionDecodeRecordResolver =>
  ({ scriptHash, languageTag }) => {
    for (const script of scripts) {
      if (script.language === "NativeCardano") continue;
      const language = script.language === "PlutusV3" ? 3 : 128;
      if (language !== languageTag) continue;
      const record = scriptDecodeRecordOfScript(language, script.scriptBytes);
      if (record.k !== "decoded" || !record.scriptHash.equals(scriptHash))
        continue;
      if (![-1, 0, 1].includes(record.programVersion))
        throw new Error("decoded record has no V1 program version");
      return {
        scriptHash: record.scriptHash,
        languageTag,
        programVersion: record.programVersion as -1 | 0 | 1,
        termRoot: record.termRoot,
        phase2Class: record.phase2Class,
      };
    }
    throw new Error("no decoded record for the selected script");
  };
