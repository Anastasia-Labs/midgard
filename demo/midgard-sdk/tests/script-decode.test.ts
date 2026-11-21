import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ScriptDecodeRecord,
  scriptDecodeRecordFromReferenceInput,
} from "../src/index.js";

const policy = "ab".repeat(28);
const hash = "cd".repeat(28);
const root = "ef".repeat(32);
const decoded: ScriptDecodeRecord = {
  Decoded: {
    script_hash: hash,
    language: 3n,
    program_version: 1n,
    term_root: root,
    phase2_class: 0n,
  },
};
const refused: ScriptDecodeRecord = {
  Refused: { script_hash: hash, language: 3n, class: 4n, bit_offset: 63n },
};
const referenceInput = (record: ScriptDecodeRecord = decoded): UTxO => ({
  txHash: "12".repeat(32),
  outputIndex: 0,
  address: "unused-by-record-authentication",
  assets: { lovelace: 2_000_000n, [policy + hash]: 1n },
  datum: Data.to(record, ScriptDecodeRecord),
});
const read = (referenceInputs: readonly UTxO[], referenceInputIndex = 0) =>
  scriptDecodeRecordFromReferenceInput({
    referenceInputs,
    referenceInputIndex,
    scriptDecodePolicyId: policy,
    scriptHash: hash,
    language: 3,
  });

describe("authenticated script-decode records", () => {
  it.each([
    [decoded, new Constr(0, [hash, 3n, 1n, root, 0n])],
    [refused, new Constr(1, [hash, 3n, 4n, 63n])],
  ] as const)(
    "matches the on-chain constructor and selects the named reference input: %#",
    (record, raw) => {
      expect(Data.to(record, ScriptDecodeRecord)).toBe(Data.to(raw));
      expect(read([referenceInput(), referenceInput(record)], 1)).toEqual(
        record,
      );
    },
  );

  it.each([-1, 0.5, 1, Number.MAX_SAFE_INTEGER + 1])(
    "refuses absent or invalid reference index %s",
    (index) => {
      expect(() => read([referenceInput()], index)).toThrow();
    },
  );

  it("requires inline data and the exact record token quantity", () => {
    const input = referenceInput();
    for (const datum of [undefined, null]) {
      expect(() => read([{ ...input, datum }])).toThrow(/inline datum/u);
    }
    expect(read([{ ...input, datumHash: null }])).toEqual(decoded);
    expect(() => read([{ ...input, datumHash: "11".repeat(32) }])).toThrow(
      /inline datum/u,
    );
    for (const assets of [
      { lovelace: 2_000_000n },
      { ...input.assets, [policy + hash]: 2n },
      { lovelace: 2_000_000n, ["aa".repeat(28) + hash]: 1n },
    ]) {
      expect(() => read([{ ...input, assets }])).toThrow(
        /authentication token/u,
      );
    }
  });

  it("binds the token-backed record to the selected script hash and language", () => {
    for (const record of [
      { Decoded: { ...decoded.Decoded, script_hash: "11".repeat(28) } },
      { Decoded: { ...decoded.Decoded, language: 128n } },
      { Refused: { ...refused.Refused, script_hash: "11".repeat(28) } },
    ]) {
      expect(() => read([referenceInput(record)])).toThrow(/selected script/u);
    }
  });

  it("refuses malformed record fields before exposing a decoded root", () => {
    const input = referenceInput();
    for (const raw of [
      new Constr(0, [hash, 3n, 1n, "00", 0n]),
      new Constr(1, [hash, 3n, 4n]),
      new Constr(2, []),
    ]) {
      expect(() => read([{ ...input, datum: Data.to(raw) }])).toThrow();
    }
  });
});
