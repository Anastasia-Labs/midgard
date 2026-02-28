import { describe, expect, it } from "vitest";
import {
  stringifyJsonWithBigIntNumbers,
  toBlockfrostAdditionalValue,
} from "@/services/lucid.js";

describe("blockfrost additional UTxO serialization", () => {
  it("preserves bigint token quantities in additionalUtxoSet values", () => {
    const policyId = "ab".repeat(28);
    const assetName = Buffer.from("token", "utf8").toString("hex");
    const unit = `${policyId}${assetName}`;
    const assets = {
      lovelace: 12_345_678_901_234_567n,
      [unit]: 98_765_432_109_876_543n,
    };

    const value = toBlockfrostAdditionalValue(assets);
    expect(value.coins).toEqual(12_345_678_901_234_567n);
    expect((value[policyId] as Record<string, bigint>)[assetName]).toEqual(
      98_765_432_109_876_543n,
    );
  });

  it("emits bigints as JSON numeric literals (not quoted strings)", () => {
    const body = stringifyJsonWithBigIntNumbers({
      coins: 123n,
      nested: {
        amount: 456n,
      },
    });

    expect(body).toContain('"coins":123');
    expect(body).toContain('"amount":456');
    expect(body).not.toContain('"123"');
    expect(body).not.toContain('"456"');
  });
});
