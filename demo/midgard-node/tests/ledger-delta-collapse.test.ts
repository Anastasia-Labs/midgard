import { describe, expect, it } from "vitest";

import { collapseLedgerDelta } from "../src/mpf/ledger-delta.js";
import type { MpfBatchOp } from "../src/mpf/types.js";

const outref = (txByte: number, index: number): Buffer =>
  Buffer.from([0x82, 0x58, 0x20, ...Array(32).fill(txByte), 0x19, 0, index]);
const output = (tag: number): Buffer => Buffer.from([0xa2, tag]);
const insert = (key: Buffer): MpfBatchOp => ({
  type: "insert",
  key,
  value: Buffer.from([0xd8, key[3]!, key[37]!]),
});
const del = (key: Buffer): MpfBatchOp => ({ type: "delete", key });
const values = (...keys: Buffer[]): ReadonlyMap<string, Buffer> =>
  new Map(keys.map((key, i) => [key.toString("hex"), output(i)]));
const hex = (buffers: readonly Buffer[]): string[] =>
  buffers.map((buffer) => buffer.toString("hex")).sort();

/**
 * The confirmed-ledger finalizer's rule: every spent outref must be in the
 * base, and no produced outref may already be.
 */
const applyToBase = (
  base: readonly Buffer[],
  delta: ReturnType<typeof collapseLedgerDelta>,
): string[] => {
  const ledger = new Set(hex(base));
  for (const spent of delta.spent) {
    if (!ledger.delete(spent.toString("hex"))) {
      throw new Error(`spends an outref absent from the base`);
    }
  }
  for (const { outref: produced } of delta.produced) {
    if (ledger.has(produced.toString("hex"))) {
      throw new Error(`substitutes an existing unspent outref`);
    }
    ledger.add(produced.toString("hex"));
  }
  return [...ledger].sort();
};

describe("collapseLedgerDelta", () => {
  const deposit = outref(0x6c, 1);
  const transfer1Payment = outref(0x65, 0);
  const transfer1Change = outref(0x65, 1);
  const transfer2Payment = outref(0xbb, 0);
  const transfer2Change = outref(0xbb, 1);

  it("nets out an output a later transaction of the same block spends", () => {
    // Two chained transfers in one block: the second spends the first's
    // change, which the base never held.
    const ops = [
      del(deposit),
      insert(transfer1Payment),
      insert(transfer1Change),
      del(transfer1Change),
      insert(transfer2Payment),
      insert(transfer2Change),
    ];
    const delta = collapseLedgerDelta(
      ops,
      values(
        transfer1Payment,
        transfer1Change,
        transfer2Payment,
        transfer2Change,
      ),
    );
    expect(hex(delta.spent)).toEqual(hex([deposit]));
    expect(hex(delta.produced.map((entry) => entry.outref))).toEqual(
      hex([transfer1Payment, transfer2Payment, transfer2Change]),
    );
    expect(applyToBase([deposit], delta)).toEqual(
      hex([transfer1Payment, transfer2Payment, transfer2Change]),
    );
  });

  it("keeps base spends and new outputs of unchained transactions", () => {
    const other = outref(0x01, 0);
    const delta = collapseLedgerDelta(
      [del(deposit), insert(transfer1Payment), insert(transfer1Change)],
      values(transfer1Payment, transfer1Change),
    );
    expect(hex(delta.spent)).toEqual(hex([deposit]));
    expect(applyToBase([deposit, other], delta)).toEqual(
      hex([other, transfer1Payment, transfer1Change]),
    );
  });

  it("carries the full output bytes of each produced outref", () => {
    const delta = collapseLedgerDelta(
      [insert(transfer1Payment)],
      new Map([[transfer1Payment.toString("hex"), output(7)]]),
    );
    expect(delta.produced).toEqual([
      { outref: transfer1Payment, output: output(7) },
    ]);
  });

  it("refuses a base outref that is spent and then present again", () => {
    expect(() =>
      collapseLedgerDelta([del(deposit), insert(deposit)], values(deposit)),
    ).toThrow(/spends and re-creates base outref/);
  });

  it("refuses a produced outref without its full output bytes", () => {
    expect(() =>
      collapseLedgerDelta([insert(transfer1Payment)], new Map()),
    ).toThrow(/Missing full output bytes/);
  });
});
