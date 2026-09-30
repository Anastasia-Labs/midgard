import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import type { MidgardLedgerRedeemer } from "../src/ledger-tx/types.js";
import type { MidgardScriptPurpose } from "../src/midgard-redeemers.js";
import {
  buildMidgardScriptContext,
  buildPlutusV3ScriptContext,
  type ScriptContextView,
} from "../src/script-context.js";
import { outRefFromByte } from "./validation-fixtures.js";

// Ruling 1: a Midgard receiving script's redeemer is part of the MidgardV1
// transaction info, and a receive purpose never reaches a PlutusV3 context
// (phase B refuses it before building one; phase-b.test.ts pins that arm).
// These tests pin the context side, so moving the context's Data reads and
// pair construction cannot drop or reorder the receive entry.

const RECEIVE_HASH = "11".repeat(28);
const SPEND_HASH = "22".repeat(28);
const SPEND_REF = outRefFromByte(0x33).toString("hex");

const redeemer = (tag: number, value: bigint): MidgardLedgerRedeemer => ({
  tag,
  index: 0n,
  dataCbor: Buffer.from([0x18, Number(value)]),
  exUnits: { memory: 1n, steps: 1n },
});

const receive: MidgardScriptPurpose = {
  kind: "receive",
  scriptHash: RECEIVE_HASH,
};
const spend: MidgardScriptPurpose = {
  kind: "spend",
  scriptHash: SPEND_HASH,
  outRefHex: SPEND_REF,
};

const view: ScriptContextView = {
  txId: Buffer.alloc(32, 0x44),
  inputs: [],
  referenceInputs: [],
  outputs: [],
  fee: 0n,
  observers: [],
  signatories: [],
  mint: new Map(),
  redeemers: [
    { purpose: spend, redeemer: redeemer(0, 40n) },
    { purpose: receive, redeemer: redeemer(4, 41n) },
  ],
};

const constrAt = (value: Data, ...path: number[]): Data => {
  let current = value;
  for (const index of path) {
    if (!(current instanceof DataConstr)) {
      throw new Error("expected a constructor on the path");
    }
    current = current.fields[index]!;
  }
  return current;
};

const redeemerEntries = (
  map: Data,
): { readonly purpose: Data; readonly value: bigint }[] => {
  if (!(map instanceof DataMap)) throw new Error("expected a redeemer map");
  return (map as DataMap<Data, Data>).map.map((entry) => {
    if (!(entry.snd instanceof DataI)) throw new Error("expected an integer");
    return { purpose: entry.fst, value: entry.snd.int };
  });
};

const purposeTag = (purpose: Data): bigint => {
  if (!(purpose instanceof DataConstr)) throw new Error("expected a purpose");
  return purpose.constr;
};

describe("receive redeemers in the script context (ruling 1)", () => {
  it("keeps the receive redeemer in the MidgardV1 txInfo redeemer map, in witness order", () => {
    const context = buildMidgardScriptContext(view, receive, redeemer(4, 41n));
    // MidgardV1 txInfo field 8 is the redeemer map.
    const entries = redeemerEntries(constrAt(context, 0, 8));
    expect(entries.map((entry) => entry.value)).toEqual([40n, 41n]);
    expect(entries.map((entry) => purposeTag(entry.purpose))).toEqual([1n, 3n]);
    const receivePurpose = entries[1]!.purpose as DataConstr;
    expect(receivePurpose.fields[0]).toBeInstanceOf(DataB);
    expect(
      Buffer.from((receivePurpose.fields[0] as DataB).bytes.toBuffer()),
    ).toEqual(Buffer.from(RECEIVE_HASH, "hex"));
    // The script's own purpose is the receive purpose.
    expect(purposeTag(constrAt(context, 2))).toBe(3n);
  });

  it("omits receive redeemers from a PlutusV3 txInfo and refuses a PlutusV3 receive purpose", () => {
    const context = buildPlutusV3ScriptContext(view, spend, redeemer(0, 40n));
    // PlutusV3 txInfo field 9 is the redeemer map.
    const entries = redeemerEntries(constrAt(context, 0, 9));
    expect(entries.map((entry) => entry.value)).toEqual([40n]);
    expect(() =>
      buildPlutusV3ScriptContext(view, receive, redeemer(4, 41n)),
    ).toThrow("Receiving scripts require MidgardV1 context");
  });
});
