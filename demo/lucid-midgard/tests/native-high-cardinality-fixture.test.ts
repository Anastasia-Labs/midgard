import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildHighCardinalityNativeTxFixture,
  buildSizeBalancedNativeTxFixture,
  HIGH_CARDINALITY_COUNTS,
  type HighCardinalityNativeTxFixture,
  renderHighCardinalityAikenTest,
  renderSizeBalancedAikenTest,
  SIZE_BALANCED_COUNTS,
  SIZE_BALANCED_FEE,
  SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES,
  SIZE_BALANCED_MAX_FEE,
  SIZE_BALANCED_MAX_LIST_LENGTH,
  SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES,
  type SizeBalancedNativeTxFixture,
  stableFixtureJson,
} from "./fixtures/native-high-cardinality.js";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(testDir, "..");
const repoRoot = path.resolve(packageRoot, "../..");
const fixtureJsonPath = path.join(
  testDir,
  "fixtures/native-high-cardinality.json",
);
const sizeBalancedFixtureJsonPath = path.join(
  testDir,
  "fixtures/native-size-balanced-15_5k.json",
);
const aikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
);
const sizeBalancedAikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
);

const readFixture = (): HighCardinalityNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(fixtureJsonPath, "utf8"),
  ) as HighCardinalityNativeTxFixture;

const readSizeBalancedFixture = (): SizeBalancedNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(sizeBalancedFixtureJsonPath, "utf8"),
  ) as SizeBalancedNativeTxFixture;

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
    expect(fixture.sizes.fullTxCborBytes).toBe(
      fixture.fullTxCborHex.length / 2,
    );
  });

  it("builds a near-15.5KiB size-balanced native tx fixture from lucid-midgard", async () => {
    const fixture = await buildSizeBalancedNativeTxFixture();

    expect(fixture.counts).toEqual(SIZE_BALANCED_COUNTS);
    expect(fixture.sizes.fullTxCborBytes).toBe(
      fixture.fullTxCborHex.length / 2,
    );
    expect(fixture.sizes.compactTxCborBytes).toBe(
      fixture.compactTxCborHex.length / 2,
    );
    expect(fixture.sizes.fee).toBe(SIZE_BALANCED_FEE.toString(10));
    expect(BigInt(fixture.sizes.fee)).toBeLessThanOrEqual(
      SIZE_BALANCED_MAX_FEE,
    );
    expect(
      Math.abs(
        fixture.sizes.fullTxCborBytes - SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES,
      ),
    ).toBeLessThanOrEqual(SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES);
    for (const count of Object.values(fixture.counts)) {
      expect(count).toBeLessThanOrEqual(SIZE_BALANCED_MAX_LIST_LENGTH);
    }
    expect(fixture.counts.outputs).toBeLessThanOrEqual(
      SIZE_BALANCED_MAX_LIST_LENGTH,
    );
    expect(fixture.mintPolicyIdsInTxInfoOrder).toHaveLength(
      SIZE_BALANCED_COUNTS.mintPolicies,
    );
    expect(fixture.redeemerPointers).toHaveLength(
      SIZE_BALANCED_COUNTS.totalRedeemers,
    );

    const majorBodyPreimageSizes = [
      fixture.sizes.preimages.spendInputs,
      fixture.sizes.preimages.referenceInputs,
      fixture.sizes.preimages.outputs,
      fixture.sizes.preimages.requiredObservers,
      fixture.sizes.preimages.requiredSigners,
      fixture.sizes.preimages.mint,
      fixture.sizes.preimages.scriptTxWits,
      fixture.sizes.preimages.addrTxWits,
      fixture.sizes.preimages.redeemerTxWits,
    ];
    expect(Math.max(...majorBodyPreimageSizes)).toBeLessThanOrEqual(
      Math.floor(fixture.sizes.fullTxCborBytes * 0.35),
    );
    expect(Math.min(...majorBodyPreimageSizes)).toBeGreaterThanOrEqual(250);
  });

  it("keeps checked-in JSON and Aiken fixture artifacts fresh", async () => {
    const fixture = await buildHighCardinalityNativeTxFixture();
    const sizeBalancedFixture = await buildSizeBalancedNativeTxFixture();

    expect(stableFixtureJson(fixture)).toBe(
      fs.readFileSync(fixtureJsonPath, "utf8"),
    );
    expect(stableFixtureJson(sizeBalancedFixture)).toBe(
      fs.readFileSync(sizeBalancedFixtureJsonPath, "utf8"),
    );
    const renderedAiken = renderHighCardinalityAikenTest(fixture);
    const checkedInAiken = fs.readFileSync(aikenTestPath, "utf8");
    const renderedSizeBalancedAiken =
      renderSizeBalancedAikenTest(sizeBalancedFixture);
    const checkedInSizeBalancedAiken = fs.readFileSync(
      sizeBalancedAikenTestPath,
      "utf8",
    );

    expect(checkedInAiken).toContain(
      "test high_cardinality_lucid_midgard_native_tx_decodes",
    );
    expect(checkedInSizeBalancedAiken).toContain(
      "test size_balanced_lucid_midgard_native_tx_decodes",
    );
    expect(aikenByteStringLiterals(checkedInAiken)).toEqual(
      aikenByteStringLiterals(renderedAiken),
    );
    expect(aikenByteStringLiterals(checkedInSizeBalancedAiken)).toEqual(
      aikenByteStringLiterals(renderedSizeBalancedAiken),
    );
    expect(readFixture()).toEqual(fixture);
    expect(readSizeBalancedFixture()).toEqual(sizeBalancedFixture);
  });
});
