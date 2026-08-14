import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  daParamsFloorViolations,
  governedThresholdFloor,
  MIN_DA_COMMITTEE_SIZE,
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
// `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md` §4,
// as amended by the 2026-08-11 owner ruling (the floor's clamp) and the
// 2026-08-13 owner ruling (the owner-set minimum):
//
//   da_threshold     >= ceil(2*committee_len/3)   (committee_len >= 1)
//   update_threshold >= ceil(2*owner_len/3)       (owner_len >= 1)
//
// The vectors are byte-for-byte the same absolute pins asserted by
// `da_params_governor_invariant_da_threshold_majority_floor` in
// `onchain/aiken/validators/da-params-governor.ak`, so a drift between the
// off-chain guard and the on-chain governor fails here rather than at a script
// evaluation. The `[1, 1]` row is the amendment itself: under the retired
// `max(2, ceil(2n/3))` clamp it read `[1, 2]`, a floor no one-member committee
// could satisfy.
const SHARED_FLOOR_VECTORS: ReadonlyArray<readonly [number, number]> = [
  [1, 1],
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

  // Satisfiability is the load-bearing half now. The floor carries no lower
  // clamp and no set-size minimum beside it, so "never a single key" is simply
  // false at n = 1 after the 2026-08-13 ruling; what remains true at every set
  // size is that the floor never exceeds its own argument. Renamed rather than
  // weakened in place, mirroring the on-chain rename.
  it("invariant 2 — floors update_threshold at the same ceiling and stays satisfiable at every set size", () => {
    const unsatisfiable: number[] = [];

    for (
      let setLength = 1;
      setLength <= MAX_INDEXED_SIGNER_COUNT;
      setLength++
    ) {
      if (governedThresholdFloor(setLength) > setLength) {
        unsatisfiable.push(setLength);
      }
    }

    expect(unsatisfiable).toStrictEqual([]);
    expect(governedThresholdFloor(1)).toBe(1);
    expect(governedThresholdFloor(2)).toBe(2);
    expect(governedThresholdFloor(3)).toBe(2);
    expect(governedThresholdFloor(4)).toBe(3);
  });

  it("invariant 3 — protects the owner set from draining to zero or to a zero threshold", () => {
    // What survives of drain protection after both rulings. The owner-set
    // minimum is one, so a lone owner governs; the floor is at least one at
    // every set size, so no set can name a threshold of zero — governance with
    // no signature at all is the one drain neither ruling licensed.
    expect(MIN_DA_OWNER_COUNT).toBe(1);

    const zeroThresholdAdmitted: number[] = [];
    for (
      let setLength = 1;
      setLength <= MAX_INDEXED_SIGNER_COUNT;
      setLength++
    ) {
      if (governedThresholdFloor(setLength) < 1) {
        zeroThresholdAdmitted.push(setLength);
      }
    }
    expect(zeroThresholdAdmitted).toStrictEqual([]);

    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 0,
        updateThreshold: 1,
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

  it("rejection 3 — an owner set drained to zero keys is unrepresentable", () => {
    // Re-scoped twice, tracking what the rulings left standing. It first
    // asserted that a one-owner set was rejected on two overlapping bounds;
    // then, after 2026-08-11, on the owner-set minimum alone; the 2026-08-13
    // ruling removed that minimum, so a one-owner set is now *accepted* and is
    // pinned positively in the single-owner consequence block below. The drain
    // that survives is the drain to zero. On-chain the twin is
    // `da_params_governor_rejects_empty_owner_set`, where the refusal is
    // structural (`expect [first, ..rest] = keys`); here it is this explicit
    // class, because `ownerCount` is caller-supplied and can be zero.
    //
    // `updateThreshold: 0` is the only value that isolates the disposition: over
    // an empty set every threshold of one or more also exceeds the set, so any
    // other choice would report two classes and this would stop being a
    // single-disposition test. Zero satisfies both threshold bounds vacuously
    // (`0 >= governedThresholdFloor(0) === 0` and `0 <= 0`), leaving the empty
    // set itself as the only thing wrong with these params.
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 0,
        updateThreshold: 0,
      }),
    ).toStrictEqual(["owner_set_below_minimum"]);
  });

  // Review finding: uncited by the Q63 artifact, and the committee-side twin of
  // the row above. The threshold bounds say nothing about an empty committee —
  // `governedThresholdFloor(0) === 0`, so `daThreshold: 0` sits exactly on that
  // floor and does not exceed the committee either — so before this class an
  // empty committee was reported as no violation at all, while the governor
  // refuses it structurally. Same disposition-isolating choice as the owner
  // row: zero is the only threshold that leaves the empty committee itself as
  // the only thing wrong with these params.
  it("rejection 4 — a committee drained to zero keys is unrepresentable", () => {
    expect(MIN_DA_COMMITTEE_SIZE).toBe(1);
    expect(
      daParamsFloorViolations({
        committeeLength: 0,
        daThreshold: 0,
        ownerCount: 4,
        updateThreshold: 3,
      }),
    ).toStrictEqual(["committee_below_minimum"]);
  });

  it("rejection 4 control — the empty-committee class is silent at every representable committee size", () => {
    // The added class is diagnostic only: it must fire on zero and nowhere
    // else, so no previously reported result changed. Swept over every
    // representable committee size, at the floor, one above it, and at the
    // committee size itself.
    const firedAtNonzero: number[] = [];
    for (
      let committeeLength = 1;
      committeeLength <= MAX_INDEXED_SIGNER_COUNT;
      committeeLength++
    ) {
      const floor = governedThresholdFloor(committeeLength);
      for (const daThreshold of [floor, floor + 1, committeeLength]) {
        const violations = daParamsFloorViolations({
          committeeLength,
          daThreshold,
          ownerCount: 4,
          updateThreshold: 3,
        });
        if (violations.includes("committee_below_minimum")) {
          firedAtNonzero.push(committeeLength);
        }
      }
    }

    expect(firedAtNonzero).toStrictEqual([]);
    // And the two shapes the rest of this suite pins are byte-for-byte what
    // they were before the class existed.
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 3,
        updateThreshold: 2,
      }),
    ).toStrictEqual([]);
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 3,
        ownerCount: 3,
        updateThreshold: 2,
      }),
    ).toStrictEqual(["da_threshold_below_floor"]);
  });
});

// Review finding I2, inverted by the 2026-08-11 owner ruling. This block used
// to pin that `governedThresholdFloor(1) === 2 > 1` made a one-member DA
// committee unrepresentable. The ruling lifted that prohibition, so the floor
// at one is one and a single-key attest loop is a representable — warned-about,
// not refused — deployment shape. The consequence is pinned in its new
// direction rather than deleted, because it is the only place the lift is
// measured end-to-end off-chain.
describe("Q63 single-member-committee consequence", () => {
  it("consequence 1 — a one-member committee admits exactly one representable da_threshold", () => {
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

    // Enumerated rather than existence-checked: the admissible threshold is 1
    // and nothing else crept in, so the lift is the single intended value
    // rather than a bound that stopped constraining.
    expect(representable).toStrictEqual([1]);
  });

  it("consequence 2 — a one-member committee attesting 1-of-1 is representable", () => {
    // The single-key attest-loop configuration the ruling accepted: committee
    // of one at threshold one, owner set on its own floor.
    expect(
      daParamsFloorViolations({
        committeeLength: 1,
        daThreshold: 1,
        ownerCount: 4,
        updateThreshold: 3,
      }),
    ).toStrictEqual([]);
  });

  it("consequence 3 — the lift did not disable the committee upper bound", () => {
    // Single disposition: the owner set sits on its own floor and `2 >=
    // governedThresholdFloor(1) === 1`, so the committee bound is the only one
    // left to fail.
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

// The owner-side counterpart of the block above, and the newer of the two
// lifts. The 2026-08-13 in-session owner ruling recorded on #602 dropped the
// governor's owner-set minimum to one, so a lone owner carrying
// `updateThreshold === 1` is representable and can rotate the committee and
// both thresholds on its own signature. That is accepted behaviour by owner
// decision, so it is pinned positively here rather than inferred from a missing
// rejection; the Aiken twin goes further and runs the real `spend` handler
// through an actual single-signature rotation.
describe("Q63 single-owner governance consequence", () => {
  it("consequence 1 — a one-owner set admits exactly one representable update_threshold", () => {
    const representable: number[] = [];

    for (let updateThreshold = 0; updateThreshold <= 1; updateThreshold++) {
      const violations = daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 1,
        updateThreshold,
      });
      if (violations.length === 0) {
        representable.push(updateThreshold);
      }
    }

    // Enumerated, not existence-checked: the admissible threshold is 1 and
    // nothing else crept in — a threshold of 0 stays below the floor even for a
    // set of one, so the lift did not open governance with no signature.
    expect(representable).toStrictEqual([1]);
  });

  it("consequence 2 — a one-owner set governing at update_threshold 1 is representable", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 1,
        updateThreshold: 1,
      }),
    ).toStrictEqual([]);
  });

  it("consequence 3 — the lift did not disable the owner-set upper bound", () => {
    expect(
      daParamsFloorViolations({
        committeeLength: 6,
        daThreshold: 4,
        ownerCount: 1,
        updateThreshold: 2,
      }),
    ).toStrictEqual(["update_threshold_exceeds_owner_set"]);
  });
});

// Review finding E3. Quoting F04's floor text proves the decision record still
// says what Q63 cited; it does not prove the implementation computes it. This
// binds the SDK's floor to a digest of the whole floor table published in the
// Q63 evidence artifact, and
// `demo/scripts/verify-canonical-v1-q63-da-governor-safety.mjs` independently
// recomputes `ceil(2n/3)` and asserts the same digest. A floor/ceil
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
