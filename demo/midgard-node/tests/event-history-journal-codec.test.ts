import { describe, expect, it } from "vitest";

import {
  decodeJournalIncarnation,
  decodeJournalOutput,
  encodeJournalIncarnation,
  encodeJournalOutput,
} from "../src/database/eventHistoryJournalCodec.js";
import { stageHistoryProvenance } from "../src/l1-event-history-provenance.js";

const output = {
  txHash: "aa".repeat(32),
  outputIndex: 0,
  address: "local-storage-codec-fixture",
  assets: { lovelace: 9_007_199_254_740_993n, ["bb".repeat(28)]: 2n },
  datum: "d87980",
  hasReferenceScript: false,
};

describe("private history journal storage codecs", () => {
  it("round trips exact quantities and gives Value entries deterministic order", () => {
    const encoded = encodeJournalOutput(output);
    expect(decodeJournalOutput(encoded)).toEqual(output);
    expect(Object.isFrozen(decodeJournalOutput(encoded).assets)).toBe(true);
    expect(JSON.parse(encoded).assets.lovelace).toBe("9007199254740993");
    expect(
      encodeJournalOutput({
        ...output,
        assets: Object.fromEntries(Object.entries(output.assets).reverse()),
      }),
    ).toBe(encoded);
  });

  it("preserves hashed datum and reference-script presence separately", () => {
    const value = {
      ...output,
      datum: undefined,
      datumHash: "cc".repeat(32),
      hasReferenceScript: true,
    };
    expect(decodeJournalOutput(encodeJournalOutput(value))).toEqual(value);
  });

  it.each([
    { outputIndex: -1 },
    { outputIndex: Number.MAX_SAFE_INTEGER + 1 },
    { txHash: "AA".repeat(32) },
    { assets: { lovelace: "-1" } },
    { assets: { lovelace: "0x10" } },
    { assets: { lovelace: "+1" } },
    { assets: { lovelace: "01" } },
    { assets: { lovelace: " 1 " } },
    { assets: { lovelace: "" } },
    { assets: { lovelace: 9007199254740992 } },
    { assets: { lovelace: "1", badUnit: "2" } },
    { datum: "not hex" },
    { datumHash: "cc".repeat(32) },
    { hasReferenceScript: "false" },
    { unrecognized: true },
  ])("refuses malformed stored output shape %j", (patch) => {
    const stored = { ...JSON.parse(encodeJournalOutput(output)), ...patch };
    expect(() => decodeJournalOutput(JSON.stringify(stored))).toThrow();
  });

  it("round trips immutable admission and nullable orphan placement losslessly", () => {
    const bindingDigest = "dd".repeat(32);
    const [change] = stageHistoryProvenance({
      bindingDigest,
      block: { point: { id: "ee".repeat(32), slot: 1, height: 1 } },
      incarnations: [],
      transitions: [
        {
          transactionIndex: 0,
          transition: {
            kind: "deposit",
            operation: "InsertOrder",
            transactionHash: output.txHash,
            consumed: [],
            produced: [],
            continuations: [],
            admission: {
              key: "ff".repeat(32),
              idCbor: "01",
              factsCbor: "02",
              payloadCbor: "03",
              originalAssetsCbor: "04",
              inclusionTime: 9_007_199_254_740_993n,
              outRef: { txHash: output.txHash, outputIndex: 0 },
            },
          },
        },
      ],
    });
    const value = change!.after;
    expect(decodeJournalIncarnation(encodeJournalIncarnation(value))).toEqual(
      value,
    );
    expect(
      Object.isFrozen(
        decodeJournalIncarnation(encodeJournalIncarnation(value)).placement!
          .current!.outRef,
      ),
    ).toBe(true);
    expect(() =>
      encodeJournalIncarnation({
        ...value,
        placement: { ...value.placement!, current: null },
      }),
    ).toThrow(/live or explicitly retired/);
    const orphan = { ...value, placement: null };
    expect(decodeJournalIncarnation(encodeJournalIncarnation(orphan))).toEqual(
      orphan,
    );
    const encoded = JSON.parse(encodeJournalIncarnation(value));
    encoded.event.inclusionTime = 9007199254740992;
    expect(() => decodeJournalIncarnation(JSON.stringify(encoded))).toThrow();
    expect(() =>
      decodeJournalIncarnation(JSON.stringify({ ...encoded, kind: "unknown" })),
    ).toThrow();
  });
});
