import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildHighCardinalityNativeTxFixture,
  HIGH_CARDINALITY_COUNTS,
  type HighCardinalityNativeTxFixture,
} from "./fixtures/native-high-cardinality.js";
import { stableNativeTxFixtureJson } from "./fixtures/native-tx-fixture-shape.js";

const fixturePath = path.join(
  path.dirname(fileURLToPath(import.meta.url)),
  "fixtures/native-high-cardinality.json",
);

const readFixture = (): HighCardinalityNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(fixturePath, "utf8"),
  ) as HighCardinalityNativeTxFixture;

/**
 * `buildHighCardinalityNativeTxFixture` is this fixture's producer, but until
 * now nothing wrote its output back, so a wire-format change left the only route
 * to a fresh fixture being a hand-edit — exactly what the golden discipline
 * forbids. `MIDGARD_SYNC_FIXTURES=1` closes that: it writes the producer's own
 * bytes. Use the package script
 * `pnpm run fixtures:native-high-cardinality:sync`, then regenerate the Aiken
 * goldens derived from it with `pnpm run fixtures:native-compact`.
 *
 * Sync mode deliberately does **not** then compare the producer against the file
 * it just wrote: that comparison would be against its own output and so could
 * not fail. The fixture-matches-producer assertion is a check-mode assertion
 * only, and check mode is the default — sync mode has to be asked for by name.
 * Everything above it (counts, redeemer pointers, derived sizes) is a claim about
 * the producer itself and runs in both modes.
 */
const syncing = process.env.MIDGARD_SYNC_FIXTURES === "1";

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

    if (syncing) {
      fs.writeFileSync(fixturePath, stableNativeTxFixtureJson(rebuilt));
      return;
    }

    expect(stableNativeTxFixtureJson(rebuilt)).toBe(
      fs.readFileSync(fixturePath, "utf8"),
    );
    expect(readFixture()).toEqual(rebuilt);
  });
});
