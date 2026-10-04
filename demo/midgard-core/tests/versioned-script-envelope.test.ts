import { describe, expect, it } from "vitest";

import {
  decodeMidgardVersionedScript,
  decodeMidgardVersionedScriptEnvelope,
  EMPTY_CBOR_LIST,
  encodeCbor,
} from "../src/codec/index.js";
import {
  collectMidgardAttachedProgramEnvelopes,
  collectMidgardEventProgramEnvelopes,
} from "../src/script-proof.js";
import { canonical } from "./consensus-validation.canonical.js";
// The new raw envelope projection is distinct from the strict script decoder;
// its nearest source-proof suite already fills the 500-line module budget.
describe("forced canonical script envelope projection", () => {
  it("preserves exact forced native envelopes without widening normal or reference-output admission", () => {
    const malformedNative = Buffer.from("820043820700", "hex");
    const base = canonical();
    const tx = {
      ...base,
      body: {
        ...base.body,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      },
      witnessSet: {
        ...base.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([malformedNative]),
      },
    };
    const envelope = decodeMidgardVersionedScriptEnvelope(malformedNative);
    expect(envelope).toEqual({
      language: "NativeCardano",
      scriptBytes: Buffer.from("820700", "hex"),
    });
    expect(collectMidgardAttachedProgramEnvelopes(tx, "forced")).toEqual([]);
    expect(
      collectMidgardEventProgramEnvelopes(tx, () => undefined, "forced"),
    ).toEqual([]);
    expect(() => collectMidgardAttachedProgramEnvelopes(tx)).toThrow();
    expect(() => decodeMidgardVersionedScript(malformedNative)).toThrow();
    for (const hex of [
      "82180043820700",
      "8200420700ff",
      "820143820700",
      "9f0043820700ff",
      "82005803820700",
    ]) {
      expect(
        () => decodeMidgardVersionedScriptEnvelope(Buffer.from(hex, "hex")),
        hex,
      ).toThrow();
    }
  });
});
