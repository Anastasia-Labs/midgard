import { describe, expect, it } from "vitest";
import {
  compareOutRefs,
  resolveReferenceInputIndexFromSet,
} from "@/tx-context.js";

describe("tx context ordering", () => {
  it("orders out refs lexicographically by tx hash then output index", () => {
    const unsorted = [
      { txHash: "bb".repeat(32), outputIndex: 3 },
      { txHash: "aa".repeat(32), outputIndex: 9 },
      { txHash: "aa".repeat(32), outputIndex: 1 },
    ];

    expect([...unsorted].sort(compareOutRefs)).toEqual([
      { txHash: "aa".repeat(32), outputIndex: 1 },
      { txHash: "aa".repeat(32), outputIndex: 9 },
      { txHash: "bb".repeat(32), outputIndex: 3 },
    ]);
  });

  it("derives reference input indexes from canonical ledger ordering", () => {
    const schedulerRef = {
      txHash: "cc".repeat(32),
      outputIndex: 1,
    };
    const hubOracleRef = {
      txHash: "aa".repeat(32),
      outputIndex: 9,
    };
    const activeNodeRef = {
      txHash: "bb".repeat(32),
      outputIndex: 0,
    };

    const referenceInputs = [schedulerRef, activeNodeRef, hubOracleRef];

    expect(
      resolveReferenceInputIndexFromSet(hubOracleRef, referenceInputs),
    ).toEqual(0n);
    expect(
      resolveReferenceInputIndexFromSet(activeNodeRef, referenceInputs),
    ).toEqual(1n);
    expect(
      resolveReferenceInputIndexFromSet(schedulerRef, referenceInputs),
    ).toEqual(2n);
  });
});
