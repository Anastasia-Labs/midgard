import { describe, expect, it } from "vitest";

import {
  daParamsFloorViolations,
  governedThresholdFloor,
  MIN_DA_OWNER_COUNT,
} from "../src/index.js";

// Q63 / decision row D-DA5.
//
// Every number here is the F04 governed floor from
// `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md` §4:
//
//   da_threshold     >= max(2, ceil(2*committee_len/3))
//   update_threshold >= max(2, ceil(2*owner_len/3))
//
// The vectors are byte-for-byte the same absolute pins asserted by
// `da_params_governor_invariant_da_threshold_majority_floor` in
// `onchain/aiken/validators/da-params-governor.ak`, so a drift between the
// off-chain guard and the on-chain governor fails here rather than at a
// script evaluation.
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
];

const MAX_INDEXED_SIGNER_COUNT = 256;

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
      if (floor < MIN_DA_OWNER_COUNT) {
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

    expect(violations).toContain("owner_set_below_minimum");
    expect(violations).toContain("update_threshold_below_floor");
  });
});
