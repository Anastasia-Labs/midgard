import { describe, expect, it } from "vitest";

import { compareOutRefs } from "../src/tx-context.js";

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
});
