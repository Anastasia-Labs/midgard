import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildSizeBalancedNativeTxFixture,
  SIZE_BALANCED_PRODUCER,
  type SizeBalancedNativeTxFixture,
} from "./fixtures/native-size-balanced.js";
import { stableNativeTxFixtureJson } from "./fixtures/native-tx-fixture-shape.js";
import { expectNativeTxFixtureFacetsSatisfySpec } from "./fixtures/native-tx-fixture-spec.js";

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

/**
 * What the fixture is declared to be, written out here rather than imported
 * from `SIZE_BALANCED_PARAMETERS` / `SIZE_BALANCED_COUNTS`.
 *
 * Those two constants are the construction's own inputs — `SIZE_BALANCED_COUNTS`
 * is literally arithmetic over `SIZE_BALANCED_PARAMETERS` — so comparing the
 * construction's output against them cannot fail: a parameter edit moves both
 * sides together. The declaration below is a second, independent statement of
 * the same shape, so an edit to the parameters has to be made here too and is
 * therefore reviewed.
 */
const DECLARED_COUNTS = {
  spendInputs: 48,
  referenceInputs: 32,
  outputs: 48,
  mintPolicies: 24,
  spendRedeemers: 8,
  mintRedeemers: 24,
  observerRedeemers: 18,
  receiveRedeemers: 18,
  totalRedeemers: 68,
  requiredSigners: 17,
  addrWitnesses: 17,
  scriptWitnesses: 68,
} as const;

/** `targetFullTxCborBytes ± fullTxCborToleranceBytes`, stated independently. */
const DECLARED_SIZE_BAND = { min: 16_000, max: 16_256 } as const;

/** The fee ceiling the Aiken consumer of this vector asserts. */
const DECLARED_MAX_FEE = 10_000_000n;

/** The 40 key-witnessed inputs sort ahead of the 8 script-witnessed ones. */
const DECLARED_SPEND_REDEEMER_POINTERS = Array.from(
  { length: 8 },
  (_unused, index) => `0:${String(40 + index)}`,
);

describe("native size-balanced conformance fixture", () => {
  it("rebuilds the checked-in fixture from its declared construction", () => {
    const rebuilt = buildSizeBalancedNativeTxFixture();

    expect(rebuilt.counts).toEqual(DECLARED_COUNTS);
    expect(rebuilt.producer).toBe(SIZE_BALANCED_PRODUCER);
    // The band, not one exact size, is what "size-balanced" declares.
    expect(rebuilt.sizes.fullTxCborBytes).toBeGreaterThanOrEqual(
      DECLARED_SIZE_BAND.min,
    );
    expect(rebuilt.sizes.fullTxCborBytes).toBeLessThanOrEqual(
      DECLARED_SIZE_BAND.max,
    );
    expect(BigInt(rebuilt.sizes.fee)).toBeLessThanOrEqual(DECLARED_MAX_FEE);
    // The eight spend redeemers point at the script-witnessed tail of the
    // sorted input list, so a reordering cannot pass silently.
    expect(rebuilt.redeemerPointers.slice(0, 8)).toEqual(
      DECLARED_SPEND_REDEEMER_POINTERS,
    );
    // Everything the fixture advertises about its own bytes — the nine field
    // commitments, the witness-set hash, the compact-body layout, the
    // transaction id, the canonical out-ref lists, the mint policy order —
    // re-derived from the specification rather than from the codec that
    // produced them.
    expectNativeTxFixtureFacetsSatisfySpec(rebuilt, {
      label: "size-balanced",
      version: 1n,
      spendInputs: DECLARED_COUNTS.spendInputs,
      referenceInputs: DECLARED_COUNTS.referenceInputs,
      mintPolicies: DECLARED_COUNTS.mintPolicies,
      redeemers: DECLARED_COUNTS.totalRedeemers,
    });

    if (syncing) {
      fs.writeFileSync(fixturePath, stableNativeTxFixtureJson(rebuilt));
      return;
    }

    expect(stableNativeTxFixtureJson(rebuilt)).toBe(
      fs.readFileSync(fixturePath, "utf8"),
    );
    // The checked-in vector is what the Aiken goldens read, so it is held to
    // the same specification as the freshly built one rather than only to
    // byte-equality with it.
    const checkedIn = readFixture();
    expect(checkedIn).toEqual(rebuilt);
    expectNativeTxFixtureFacetsSatisfySpec(checkedIn, {
      label: "size-balanced (checked-in file)",
      version: 1n,
      spendInputs: DECLARED_COUNTS.spendInputs,
      referenceInputs: DECLARED_COUNTS.referenceInputs,
      mintPolicies: DECLARED_COUNTS.mintPolicies,
      redeemers: DECLARED_COUNTS.totalRedeemers,
    });
  });
});
