import { describe, expect, it } from "vitest";

import {
  hashMidgardOutputItemLeaf,
  hashMidgardRedeemerItemLeaf,
} from "../src/script-proof.js";

describe("script context item leaves", () => {
  it("commits the item length in the redeemer and output item leaves", () => {
    const redeemerCommitment = Buffer.alloc(32, 0x22);
    const outputCommitment = Buffer.alloc(32, 0x33);
    const redeemerLeaf = (itemLength: number) =>
      hashMidgardRedeemerItemLeaf({
        redeemerIndex: 1,
        itemLength,
        itemCommitment: redeemerCommitment,
      }).toString("hex");
    const outputLeaf = (itemLength: number) =>
      hashMidgardOutputItemLeaf({
        outputIndex: 2,
        itemLength,
        itemCommitment: outputCommitment,
      }).toString("hex");

    expect([11, 12, 13].map(redeemerLeaf)).toEqual([
      "cd34746a5753b1bb6482027b62bcfbae878dd36d844c2a8f32bc1c7f8578ae76",
      "6e4216c9dfc27679570eac18d26b1c4a846df21cd63a9292a87addebc7784fa0",
      "bcf77738b9539c6f1e1d46400d1a3deff04afc7b58e213c60b0b12cdce2d6cd4",
    ]);
    expect([1, 2, 3].map(outputLeaf)).toEqual([
      "1dcdc9af06d505101bcc33b05aba13b76fa3ba7c325cf424ccfb111f2f5a264b",
      "316409222864a51a6e1c1c122de63724bcf0c4f00ddedd1799688721d99e4509",
      "016116d6ca052045fb1f8fa7d2fd2bc9745401cd6bdad8443f5769169b50517b",
    ]);
    expect(() =>
      hashMidgardRedeemerItemLeaf({
        redeemerIndex: 1,
        itemLength: -1,
        itemCommitment: redeemerCommitment,
      }),
    ).toThrow();
  });
});
