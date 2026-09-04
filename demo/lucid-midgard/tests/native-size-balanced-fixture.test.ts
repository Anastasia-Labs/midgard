import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildSizeBalancedNativeTxFixture,
  SIZE_BALANCED_COUNTS,
  SIZE_BALANCED_PARAMETERS,
  SIZE_BALANCED_PRODUCER,
  type SizeBalancedNativeTxFixture,
} from "./fixtures/native-size-balanced.js";
import { stableNativeTxFixtureJson } from "./fixtures/native-tx-fixture-shape.js";

const fixturePath = path.join(
  path.dirname(fileURLToPath(import.meta.url)),
  "fixtures/native-size-balanced-15_5k.json",
);

const readFixture = (): SizeBalancedNativeTxFixture =>
  JSON.parse(
    fs.readFileSync(fixturePath, "utf8"),
  ) as SizeBalancedNativeTxFixture;

/**
 * `native-size-balanced-15_5k.json` had no producer at all until #588: its
 * `fullTxCborHex` was an opaque ~16 kB blob, and the only route to a fresh one
 * was to hand-edit it. `fixtures/native-size-balanced.ts` is now its declared
 * construction and this suite is its writer, on the same contract as the
 * high-cardinality sibling: `MIDGARD_SYNC_FIXTURES=1` writes the construction's
 * own bytes, check mode — the default — asserts the checked-in file is exactly
 * what the construction produces today.
 *
 * Sync mode deliberately does not then compare the construction against the file
 * it just wrote; that comparison is against its own output and so cannot fail.
 * Everything above it is a claim about the construction itself and runs in both
 * modes.
 */
const syncing = process.env.MIDGARD_SYNC_FIXTURES === "1";

describe("native size-balanced conformance fixture", () => {
  it("rebuilds the checked-in fixture from its declared construction", () => {
    const rebuilt = buildSizeBalancedNativeTxFixture();

    expect(rebuilt.counts).toEqual(SIZE_BALANCED_COUNTS);
    expect(rebuilt.producer).toBe(SIZE_BALANCED_PRODUCER);
    // The band, not one exact size, is what "size-balanced" declares — but the
    // band is asserted here as well as inside the construction so a widened
    // tolerance cannot quietly become the fixture's definition.
    expect(rebuilt.sizes.fullTxCborBytes).toBeGreaterThanOrEqual(
      SIZE_BALANCED_PARAMETERS.targetFullTxCborBytes -
        SIZE_BALANCED_PARAMETERS.fullTxCborToleranceBytes,
    );
    expect(rebuilt.sizes.fullTxCborBytes).toBeLessThanOrEqual(
      SIZE_BALANCED_PARAMETERS.targetFullTxCborBytes +
        SIZE_BALANCED_PARAMETERS.fullTxCborToleranceBytes,
    );
    expect(rebuilt.sizes.fullTxCborBytes).toBe(
      rebuilt.fullTxCborHex.length / 2,
    );
    expect(BigInt(rebuilt.sizes.fee)).toBeLessThanOrEqual(
      SIZE_BALANCED_PARAMETERS.maxFee,
    );
    expect(rebuilt.mintPolicyIdsInTxInfoOrder).toHaveLength(
      SIZE_BALANCED_COUNTS.mintPolicies,
    );
    expect(rebuilt.redeemerPointers).toHaveLength(
      SIZE_BALANCED_COUNTS.totalRedeemers,
    );
    // The eight spend redeemers point at the script-witnessed tail of the
    // sorted input list — a consequence of the key/script split in the
    // parameters, checked here so a reordering cannot pass silently.
    expect(rebuilt.redeemerPointers.slice(0, 8)).toEqual(
      Array.from(
        { length: SIZE_BALANCED_PARAMETERS.scriptSpendInputs },
        (_unused, index) =>
          `0:${String(SIZE_BALANCED_PARAMETERS.pubKeySpendInputs + index)}`,
      ),
    );
    expect(rebuilt.txIdHex).toHaveLength(64);
    expect(rebuilt.hashes.witnessSetHashHex).toHaveLength(64);

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
