import { describe, expect, it } from "vitest";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  buildHighCardinalityNativeTxFixture,
  HIGH_CARDINALITY_COUNTS,
  renderHighCardinalityAikenTest,
  stableFixtureJson,
  type HighCardinalityNativeTxFixture,
} from "./fixtures/native-high-cardinality.js";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(testDir, "..");
const repoRoot = path.resolve(packageRoot, "../..");
const fixtureJsonPath = path.join(
  testDir,
  "fixtures/native-high-cardinality.json",
);
const aikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
);

const readFixture = (): HighCardinalityNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(fixtureJsonPath, "utf8"),
  ) as HighCardinalityNativeTxFixture;

const aikenByteStringLiterals = (source: string): readonly string[] =>
  [...source.matchAll(/#"([0-9a-f]*)"/g)].map((match) => match[1]!);

describe("native high-cardinality conformance fixture", () => {
  it("is produced by lucid-midgard and exercises many tx fields together", async () => {
    const fixture = await buildHighCardinalityNativeTxFixture();

    expect(fixture.counts).toEqual(HIGH_CARDINALITY_COUNTS);
    expect(fixture.mintPolicyIdsInTxInfoOrder).toHaveLength(6);
    expect(fixture.redeemerPointers).toEqual([
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
    expect(fixture.fullTxCborHex).not.toEqual(fixture.compactTxCborHex);
    expect(fixture.txIdHex).toHaveLength(64);
    expect(fixture.hashes.witnessSetHashHex).toHaveLength(64);
  });

  it("keeps checked-in JSON and Aiken fixture artifacts fresh", async () => {
    const fixture = await buildHighCardinalityNativeTxFixture();

    expect(stableFixtureJson(fixture)).toBe(
      fs.readFileSync(fixtureJsonPath, "utf8"),
    );
    const renderedAiken = renderHighCardinalityAikenTest(fixture);
    const checkedInAiken = fs.readFileSync(aikenTestPath, "utf8");

    expect(checkedInAiken).toContain(
      "test high_cardinality_lucid_midgard_native_tx_decodes",
    );
    expect(aikenByteStringLiterals(checkedInAiken)).toEqual(
      aikenByteStringLiterals(renderedAiken),
    );
    expect(readFixture()).toEqual(fixture);
  });
});
