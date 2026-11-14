import { describe, expect, it } from "vitest";

import {
  decodeMidgardVersionedScript,
  decodeMidgardVersionedScriptEnvelope,
} from "../src/codec/index.js";
// The new raw envelope projection is distinct from the strict script decoder;
// its nearest source-proof suite already fills the 500-line module budget.
describe("forced canonical script envelope projection", () => {
  it("keeps native raw-envelope projection separate from the strict native decoder", () => {
    const malformedNative = Buffer.from("820043820700", "hex");
    const envelope = decodeMidgardVersionedScriptEnvelope(malformedNative);
    expect(envelope).toEqual({
      language: "NativeCardano",
      scriptBytes: Buffer.from("820700", "hex"),
    });
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
