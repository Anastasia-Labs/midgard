import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { describe, expect, it } from "vitest";

import { scriptDecodeScriptHash } from "../src/script-decode.step.js";
import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
const decode = (bytes: Uint8Array) => scriptDecodeRecordOfScript(3, bytes);

const canonical = Buffer.from("0101004c0101000001", "hex");
const stored = (flat: Uint8Array): Buffer => encodeCbor(Buffer.from(flat));

describe("terminal-record stored-script framing", () => {
  it.each([3, 128])(
    "preserves exact non-minimal wrapper identity for language %i",
    (language) => {
      const bytes = Buffer.concat([
        Buffer.from([0x58, canonical.length]),
        canonical,
      ]);
      const record = scriptDecodeRecordOfScript(language, bytes);
      expect(record).toMatchObject({
        k: "decoded",
        phase2Class: 0,
        language,
        scriptHash: scriptDecodeScriptHash(language, bytes),
      });
      const minimal = scriptDecodeRecordOfScript(language, stored(canonical));
      expect(record.scriptHash).not.toEqual(minimal.scriptHash);
      if (record.k !== "decoded" || minimal.k !== "decoded")
        throw new Error("expected decoded records");
      expect(record.termRoot).toEqual(minimal.termRoot);
    },
  );
  it.each(["", "00", "5f40ff", "590001", "430101"])(
    "refuses malformed or truncated wrapper %s",
    (hex) => {
      expect(decode(Buffer.from(hex, "hex"))).toMatchObject({
        k: "refused",
        class: 1,
        bitOffset: 0,
      });
    },
  );
  it("decodes a valid Data constant without evaluating its non-unit result", () => {
    expect(decode(stored(canonical))).toMatchObject({
      k: "decoded",
      phase2Class: 0,
    });
  });
  it.each(["00", "ff"])("refuses trailing Flat %s", (suffix) => {
    expect(
      decode(stored(Buffer.concat([canonical, Buffer.from(suffix, "hex")]))),
    ).toMatchObject({ k: "refused", class: 4 });
  });
  it.each(["03", "ff", "80"])("refuses final pad %s", (pad) => {
    expect(
      decode(
        stored(
          Buffer.concat([canonical.subarray(0, -1), Buffer.from(pad, "hex")]),
        ),
      ),
    ).toMatchObject({ k: "refused", class: 4 });
  });
  it("accepts Cardano's multi-byte inner filler without changing the program", () => {
    expect(
      decode(stored(Buffer.from("0101004c000101000001", "hex"))),
    ).toMatchObject({ k: "decoded", phase2Class: 0 });
  });
  // M6(a): Flat Data constants decode their first CBOR item and ignore the
  // remaining bytes. The stand-alone Data decoders retain exact consumption.
  it.each(["0101004c01020000000001", "0101004c010200ff0001"])(
    "accepts trailing Data-constant bytes in %s, matching Haskell Data I 0",
    (flatHex) => {
      expect(decode(stored(Buffer.from(flatHex, "hex")))).toMatchObject({
        k: "decoded",
        phase2Class: 0,
      });
    },
  );
  it("refuses bytes outside the stored CBOR wrapper", () => {
    expect(
      decode(Buffer.concat([stored(canonical), Buffer.from([0])])),
    ).toMatchObject({ k: "refused", class: 2 });
  });
});
