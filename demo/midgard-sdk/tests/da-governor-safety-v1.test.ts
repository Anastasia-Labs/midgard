import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  daParamsFloorViolations,
  governedThresholdFloor,
  MIN_DA_GOVERNED_THRESHOLD,
  MIN_DA_OWNER_COUNT,
} from "../src/index.js";

const repositoryRoot = new URL("../../../", import.meta.url);

const readRepositoryFile = (relativePath: string): string =>
  readFileSync(new URL(relativePath, repositoryRoot), "utf8");

/**
 * `max_indexed_signer_count` is read out of the Aiken constant rather than
 * restated here, so this suite's range endpoint cannot silently drift from the
 * on-chain one. The value is additionally pinned to 256 in the provenance test
 * below, so changing it stays a deliberate, visible edit on both sides.
 */
const aikenMaxIndexedSignerCount = (): number => {
  const source = readRepositoryFile(
    "onchain/aiken/lib/midgard/da-attestation-types.ak",
  );
  const match = /^pub const max_indexed_signer_count: Int = (\d+)$/mu.exec(
    source,
  );
  if (match?.[1] === undefined) {
    throw new Error(
      "max_indexed_signer_count is no longer declared in onchain/aiken/lib/midgard/da-attestation-types.ak",
    );
  }
  return Number(match[1]);
};

const MAX_INDEXED_SIGNER_COUNT = aikenMaxIndexedSignerCount();

// Q63 / decision row D-DA5.
//
// Every number here is the F04 governed floor from
// `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md` §4:
//
//   da_threshold     >= max(2, ceil(2*committee_len/3))
//   update_threshold >= max(2, ceil(2*owner_len/3))
//
// The vectors are byte-for-byte the same absolute pins asserted between
// `da_params_governor_invariant_da_threshold_majority_floor` and
// `da_params_governor_invariant_update_threshold_two_of_n_floor` in
// `onchain/aiken/validators/da-params-governor.ak` (the `[1, 2]` row is the
// second test's; the rest are the first's), so a drift between the off-chain
// guard and the on-chain governor fails here rather than at a script
// evaluation.
const SHARED_FLOOR_VECTORS: ReadonlyArray<readonly [number, number]> = [
  [1, 2],
  [2, 2],
  [3, 2],
  [4, 3],
  [5, 4],
  [6, 4],
  [7, 5],
  [9, 6],
  [16, 11],
  [100, 67],
  [255, 170],
  [256, 171],
];

describe("Q63 governed floor/drain invariants", () => {
  it("invariant 1 — floors da_threshold at the F04 two-thirds ceiling", () => {
    const measured = SHARED_FLOOR_VECTORS.map(
      ([setLength]) => [setLength, governedThresholdFloor(setLength)] as const,
    );

    expect(measured).toStrictEqual(
      SHARED_FLOOR_VECTORS.map(([setLength, floor]) => [setLength, floor]),
    );
  });

  it("invariant 2 — never admits a single-key threshold, and stays satisfiable", () => {
    const belowTwo: number[] = [];
    const unsatisfiable: number[] = [];

    for (
      let setLength = 1;
      setLength <= MAX_INDEXED_SIGNER_COUNT;
      setLength++
    ) {
      const floor = governedThresholdFloor(setLength);
      if (floor < MIN_DA_GOVERNED_THRESHOLD) {
        belowTwo.push(setLength);
      }
      if (setLength >= MIN_DA_OWNER_COUNT && floor > setLength) {
        unsatisfiable.push(setLength);
      }
    }

    expect(belowTwo).toStrictEqual([]);
    expect(unsatisfiable).toStrictEqual([]);
  });

  it("invariant 3 — protects the owner set from draining to one or zero", () => {
    expect(MIN_DA_OWNER_COUNT).toBe(2);
    // An owner set must always be large enough to carry the threshold floor,
    // otherwise no owner configuration would be representable at all. Mirrors
    // `min_owner_count >= min_governed_threshold` on-chain.
    expect(MIN_DA_OWNER_COUNT).toBeGreaterThanOrEqual(MIN_DA_GOVERNED_THRESHOLD);
    expect(governedThresholdFloor(1)).toBeGreaterThan(1);

    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 0,
        updateThreshold: 2,
      }),
    ).toContain("owner_set_below_minimum");
  });
});

describe("Q63 valid-boundary controls", () => {
  it("control 1 — accepts da_threshold exactly on the floor", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 3,
        updateThreshold: 2,
      }),
    ).toStrictEqual([]);
  });

  it("control 2 — accepts update_threshold exactly on the floor", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 3,
        daThreshold: 2,
        ownerCount: 4,
        updateThreshold: 3,
      }),
    ).toStrictEqual([]);
  });
});

describe("Q63 below-floor and drain rejection classes", () => {
  it("rejection 1 — a bare committee majority is below the two-thirds floor", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 3,
        ownerCount: 3,
        updateThreshold: 2,
      }),
    ).toStrictEqual(["da_threshold_below_floor"]);
  });

  it("rejection 2 — a single-key update_threshold is below the 2-of-N floor", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 3,
        updateThreshold: 1,
      }),
    ).toStrictEqual(["update_threshold_below_floor"]);
  });

  it("rejection 3 — an owner set drained to one key is unrepresentable", () => {
    const violations = daParamsFloorViolations({
      committeeLength: 6,
      daThreshold: 4,
      ownerCount: 1,
      updateThreshold: 1,
    });

    // Two overlapping dispositions, deliberately: no one-owner set can violate
    // exactly one bound. The Aiken twin
    // `da_params_governor_rejects_owner_set_drained_to_one_by_overlapping_bounds`
    // is named for the same overlap.
    expect(violations).toContain("owner_set_below_minimum");
    expect(violations).toContain("update_threshold_below_floor");
  });
});

// Review finding I2. `governedThresholdFloor(1) === 2 > 1`, so a one-member DA
// committee is unrepresentable: there is no `da_threshold` it could name. This
// is a real deployment consequence — any topology that assumed a single-signer
// DA committee is now invalid by construction — so it is pinned rather than
// left to be rediscovered.
describe("Q63 single-member-committee consequence", () => {
  it("consequence 1 — a one-member committee admits no representable da_threshold", () => {
    const representable: number[] = [];

    for (let daThreshold = 0; daThreshold <= 1; daThreshold++) {
      const violations = daParamsFloorViolations({
        committeeLength: 1,
        daThreshold,
        ownerCount: 4,
        updateThreshold: 3,
      });
      if (violations.length === 0) {
        representable.push(daThreshold);
      }
    }

    expect(representable).toStrictEqual([]);
  });

  it("consequence 2 — a one-member committee datum is rejected at the committee bound", () => {
    // Single disposition: the owner set sits on its own floor and
    // `daThreshold === governedThresholdFloor(1)`, so the committee bound is
    // the only one left to fail.
    expect(
      daParamsFloorViolations({
        committeeLength: 1,
        daThreshold: 2,
        ownerCount: 4,
        updateThreshold: 3,
      }),
    ).toStrictEqual(["da_threshold_exceeds_committee"]);
  });
});

// Review finding E3. Quoting F04's floor text proves the decision record still
// says what Q63 cited; it does not prove the implementation computes it. This
// binds the SDK's floor to a digest of the whole floor table published in the
// Q63 evidence artifact, and
// `demo/scripts/verify-canonical-v1-q63-da-governor-safety.mjs` independently
// recomputes `max(2, ceil(2n/3))` and asserts the same digest. A floor/ceil
// transcription error on either side breaks one of the two legs.
describe("Q63 floor arithmetic provenance", () => {
  it("provenance — the SDK floor reproduces the F04 arithmetic over every representable set size", () => {
    const evidence: unknown = JSON.parse(
      readRepositoryFile(
        "docs/exec-plans/evidence/canonical-v1-q63-da-governor-safety-v1.json",
      ),
    );
    const { floorTable } = evidence as {
      floorTable: { range: readonly number[]; sha256: string };
    };

    // The Aiken constant, the artifact's published range, and the size the
    // attested-signer bitmap can index are all the same number.
    expect(MAX_INDEXED_SIGNER_COUNT).toBe(256);
    expect(floorTable.range).toStrictEqual([0, MAX_INDEXED_SIGNER_COUNT]);

    const table: number[] = [];
    for (
      let setLength = 0;
      setLength <= MAX_INDEXED_SIGNER_COUNT;
      setLength++
    ) {
      table.push(governedThresholdFloor(setLength));
    }

    expect(
      createHash("sha256").update(JSON.stringify(table)).digest("hex"),
    ).toBe(floorTable.sha256);
  });
});
