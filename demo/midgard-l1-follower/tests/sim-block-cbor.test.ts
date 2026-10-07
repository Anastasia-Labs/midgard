import { describe, expect, it } from "vitest";

import { decodeBlock } from "../src/index.js";
import { encodeBlock } from "../src/testing/index.js";

describe("simulated block encoding", () => {
  it("encodes burns as negative quantities and withdrawals in body order", () => {
    const policy = "c1".repeat(28);
    const script = Buffer.concat([Buffer.of(0xf0), Buffer.alloc(28, 0x0a)]);
    const key = Buffer.concat([Buffer.of(0xe0), Buffer.alloc(28, 0x0b)]);
    const { raw } = encodeBlock({
      height: 1,
      slot: 10,
      prevHash: null,
      branch: 0,
      txs: [
        {
          inputs: [{ txHash: Buffer.alloc(32, 1), index: 0 }],
          outputs: [],
          mint: new Map([
            [
              policy,
              new Map([
                ["aa", -1n],
                ["bb", -300n],
                ["cc", 2n],
              ]),
            ],
          ]),
          withdrawals: [
            { rewardAccount: key, amount: 0n },
            { rewardAccount: script, amount: 5n },
          ],
          nonce: 1,
        },
      ],
    });
    const [tx] = decodeBlock(raw).txs;
    expect(tx?.mint.get(policy)).toEqual(
      new Map([
        ["aa", -1n],
        ["bb", -300n],
        ["cc", 2n],
      ]),
    );
    expect(
      tx?.withdrawals.map((w) => [w.rewardAccount.toString("hex"), w.amount]),
    ).toEqual([
      [key.toString("hex"), 0n],
      [script.toString("hex"), 5n],
    ]);
  });
});
