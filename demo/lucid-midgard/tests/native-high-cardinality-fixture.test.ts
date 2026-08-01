import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildHighCardinalityNativeTxFixture,
  HIGH_CARDINALITY_COUNTS,
  type HighCardinalityNativeTxFixture,
  stableFixtureJson,
} from "./fixtures/native-high-cardinality.js";

const fixturePath = path.join(
  path.dirname(fileURLToPath(import.meta.url)),
  "fixtures/native-high-cardinality.json",
);

const readFixture = (): HighCardinalityNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(fixturePath, "utf8"),
  ) as HighCardinalityNativeTxFixture;

describe("native high-cardinality conformance fixture", () => {
  it("rebuilds the checked-in fixture through current LucidMidgard V1", async () => {
    const rebuilt = await buildHighCardinalityNativeTxFixture();

    expect(rebuilt.counts).toEqual(HIGH_CARDINALITY_COUNTS);
    expect(rebuilt.mintPolicyIdsInTxInfoOrder).toHaveLength(
      HIGH_CARDINALITY_COUNTS.mintPolicies,
    );
    expect(rebuilt.redeemerPointers).toEqual([
      "0:1",
      "0:4",
      "0:7",
      "1:0",
      "1:1",
      "1:2",
      "1:3",
      "1:4",
      "1:5",
      "3:0",
      "3:1",
      "6:0",
      "6:1",
    ]);
    expect(rebuilt.fullTxCborHex).not.toBe(rebuilt.compactTxCborHex);
    expect(rebuilt.txIdHex).toHaveLength(64);
    expect(rebuilt.hashes.witnessSetHashHex).toHaveLength(64);
    expect(rebuilt.sizes.fullTxCborBytes).toBe(
      rebuilt.fullTxCborHex.length / 2,
    );

    expect(stableFixtureJson(rebuilt)).toBe(
      fs.readFileSync(fixturePath, "utf8"),
    );
    expect(readFixture()).toEqual(rebuilt);
  });
});
