import assert from "node:assert/strict";
import test from "node:test";

import { checkRecoveryWindow } from "../scripts/check-recovery-window.mjs";

const genesis = {
  systemStart: "2026-09-11T00:00:00Z",
  slotLength: 1,
  activeSlotsCoeff: 0.05,
  securityParam: 2160,
};
const start = Date.parse(genesis.systemStart);

test("a recent fork remains eligible for bounded local rollback", () => {
  assert.equal(
    checkRecoveryWindow(genesis, { slot: 100 }, start + 160_000).remainingSlots,
    129540,
  );
});

test("frozen recovery refuses the actual forecast boundary and the former 72-hour reuse", () => {
  for (const ageSeconds of [129600, 72 * 3600])
    assert.throws(
      () =>
        checkRecoveryWindow(
          genesis,
          { slot: 100 },
          start + (100 + ageSeconds) * 1000,
        ),
      /synchronizing from a canonical peer/,
    );
});

test("forecast eligibility does not treat sparse slots as produced blocks", () => {
  const result = checkRecoveryWindow(
    genesis,
    { slot: 10_000 },
    start + 20_000_000,
  );
  assert.equal(result.remainingSlots, 119600);
});

test("clock rewind and malformed snapshot points fail closed", () => {
  assert.throws(
    () => checkRecoveryWindow(genesis, { slot: 100 }, start),
    /ahead/,
  );
  assert.throws(
    () => checkRecoveryWindow(genesis, { slot: -1 }, start),
    /Invalid/,
  );
});
