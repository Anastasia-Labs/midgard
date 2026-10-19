import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { buildMidgardRetainedDaCanonicalScriptProjection } from "./helpers/retained-da-boundary.build-midgard-retained-da-canonical-script-projection.js";
import { makeNativeTx } from "./validation-fixtures.js";

describe("retained DA stored script identity", () => {
  it.each(["46010000480081", "5806010000480081"])(
    "preserves the exact stored spelling %s through the schema projection",
    (storedHex) => {
      const script = {
        language: "PlutusV3" as const,
        scriptBytes: Buffer.from(storedHex, "hex"),
      };
      const source = makeNativeTx({
        scriptWitnesses: [script],
        scriptLanguages: ["PlutusV3"],
      });
      const projection = buildMidgardRetainedDaCanonicalScriptProjection({
        canonicalTransactionCbor: source.txCbor,
      });
      const projected = decodeMidgardNativeTxFullFromCanonicalCbor(
        projection.canonicalTransactionCbor,
      );
      const scripts = decodeMidgardVersionedScriptListPreimage(
        projected.witnessSet.scriptTxWitsPreimageCbor,
      );
      expect(scripts).toEqual([script]);
      expect(projection.sourceScriptHash).toBe(
        hashMidgardVersionedScript(script),
      );
      expect(projected.witnessSet.addrTxWitsPreimageCbor).toEqual(
        Buffer.from([0x80]),
      );
    },
  );
});
