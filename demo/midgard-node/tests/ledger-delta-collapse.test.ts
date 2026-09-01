import { describe, expect, it } from "vitest";

import { collapseLedgerDelta, type MpfBatchOp } from "@/workers/utils/mpf.js";

const key = (byte: number): Buffer => Buffer.alloc(36, byte);

describe("ledger delta collapse", () => {
  it("omits an output produced and spent within the same block", () => {
    const baseSpend = key(1);
    const intermediate = key(2);
    const finalOutput = key(3);
    const finalValue = Buffer.from("final-output");
    const ops: readonly MpfBatchOp[] = [
      { type: "delete", key: baseSpend },
      { type: "insert", key: intermediate, value: Buffer.from("change") },
      { type: "delete", key: intermediate },
      { type: "insert", key: finalOutput, value: Buffer.from("leaf") },
    ];

    const delta = collapseLedgerDelta(
      ops,
      new Map([[finalOutput.toString("hex"), finalValue]]),
    );

    expect(delta.spent).toEqual([baseSpend]);
    expect(delta.produced).toEqual([
      { outref: finalOutput, output: finalValue },
    ]);
  });

  it("rejects replacement of an authenticated UTxO", () => {
    const outref = key(4);
    expect(() =>
      collapseLedgerDelta(
        [
          { type: "delete", key: outref },
          { type: "insert", key: outref, value: Buffer.from("leaf") },
        ],
        new Map([[outref.toString("hex"), Buffer.from("replacement")]]),
      ),
    ).toThrow(/cannot replace an authenticated UTxO/u);
  });
});
