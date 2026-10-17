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
import { expectNativeTxFixtureFacetsSatisfySpec } from "./fixtures/native-tx-fixture-spec.js";

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
    // The redeemer pointer set is the fixture's reason to exist: three of the
    // eight spend inputs are script-witnessed (indices 1, 4 and 7 of the
    // *sorted* input list), all six mint policies carry a redeemer, and the
    // two observers and two receives follow under their own purpose tags.
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
    // Everything the fixture advertises about its own bytes — the nine field
    // commitments, the witness-set hash, the compact-body layout, the
    // transaction id, the canonical out-ref lists, the mint policy order —
    // re-derived from the specification rather than from the codec that
    // produced them.
    expectNativeTxFixtureFacetsSatisfySpec(rebuilt, {
      label: "high-cardinality",
      version: 1n,
      spendInputs: HIGH_CARDINALITY_COUNTS.spendInputs,
      referenceInputs: HIGH_CARDINALITY_COUNTS.referenceInputs,
      mintPolicies: HIGH_CARDINALITY_COUNTS.mintPolicies,
      redeemers: HIGH_CARDINALITY_COUNTS.totalRedeemers,
      sortedInputs: true,
    });

    if (syncing) {
      fs.writeFileSync(fixturePath, stableNativeTxFixtureJson(rebuilt));
      return;
    }

    expect(stableNativeTxFixtureJson(rebuilt)).toBe(
      fs.readFileSync(fixturePath, "utf8"),
    );
    // The checked-in vector is what the Aiken goldens and the cross-language
    // conformance consumers read, so it is held to the same specification as
    // the freshly built one rather than only to byte-equality with it.
    const checkedIn = readFixture();
    expect(checkedIn).toEqual(rebuilt);
    expectNativeTxFixtureFacetsSatisfySpec(checkedIn, {
      label: "high-cardinality (checked-in file)",
      version: 1n,
      spendInputs: HIGH_CARDINALITY_COUNTS.spendInputs,
      referenceInputs: HIGH_CARDINALITY_COUNTS.referenceInputs,
      mintPolicies: HIGH_CARDINALITY_COUNTS.mintPolicies,
      redeemers: HIGH_CARDINALITY_COUNTS.totalRedeemers,
      sortedInputs: true,
    });
  });
});
