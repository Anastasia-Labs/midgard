import assert from "node:assert/strict";
import { test } from "node:test";
import {
  minimumDaResponseBudgetMs,
  profileDigest,
  readProfiles,
  renderAiken,
  validateProfile,
} from "./deployment-profiles.mjs";

test("confirmation policy is explicit, validated, and bound into profile identity", () => {
  const profiles = readProfiles();
  assert.equal(
    profiles["local-devnet-testing"].l1_finality.confirmation_depth,
    3,
  );
  assert.equal(profiles["preprod-testing"].l1_finality.confirmation_depth, 3);
  for (const name of ["mainnet", "preprod-public"]) {
    assert.equal(profiles[name].l1_finality.confirmation_depth, 30);
  }
  const profile = structuredClone(profiles["preprod-testing"]);
  const originalDigest = profileDigest(profile);
  profile.l1_finality.confirmation_depth += 1;
  assert.notEqual(profileDigest(profile), originalDigest);
  validateProfile(profile, profile.name);
  for (const invalid of [0, -1, 1.5, "3", Number.MAX_SAFE_INTEGER + 1]) {
    profile.l1_finality.confirmation_depth = invalid;
    assert.throws(
      () => validateProfile(profile, profile.name),
      /confirmation_depth/u,
    );
  }
  delete profile.l1_finality;
  assert.throws(() => validateProfile(profile, profile.name), /l1_finality/u);
});

test("all four profiles have explicit networks and independent deployment identities", () => {
  const profiles = readProfiles();
  assert.equal(
    profiles["preprod-public"].network,
    profiles["preprod-testing"].network,
  );
  assert.notEqual(
    profiles["preprod-public"].economics.requiredBondLovelace,
    profiles["preprod-testing"].economics.requiredBondLovelace,
  );
  assert.equal(new Set(Object.values(profiles).map(profileDigest)).size, 4);
  for (const profile of Object.values(profiles)) {
    const rendered = renderAiken(profile, "");
    assert.ok(
      rendered.includes(
        `pub const block_maturity_duration_v1: Int = ${profile.timing.block_maturity_ms.toLocaleString("en-US").replaceAll(",", "_")}`,
      ),
    );
    assert.ok(
      rendered.includes(
        `pub const required_bond: Int = ${profile.economics.requiredBondLovelace.toLocaleString("en-US").replaceAll(",", "_")}`,
      ),
    );
  }
});

test("fast testing profiles exclude interactive disputes without weakening public profiles", () => {
  const profiles = readProfiles();
  assert.deepEqual(
    profiles["local-devnet-testing"].timing,
    profiles["preprod-testing"].timing,
  );
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    const testing = structuredClone(profiles[name]);
    assert.equal(testing.timing.block_maturity_ms, 900_000);
    assert.equal(testing.timing.da_attestation_timeout_ms, 600_000);
    assert.equal(testing.timing.operator_shift_ms, 600_000);
    assert.equal(testing.timing.registration_ms, 30_000);
    assert.equal(testing.timing.da_small_response_window_ms, 720_000);
    assert.equal(testing.timing.da_full_response_window_ms, 840_000);
    assert.equal(testing.limits.max_bisection_rounds, 32);
    validateProfile(testing, name);
    testing.timing.dispute_response_window_ms = 1_000;
    assert.throws(
      () => validateProfile(testing, name),
      /must exclude interactive/u,
    );
  }
  for (const name of ["mainnet", "preprod-public"]) {
    const profile = structuredClone(profiles[name]);
    profile.timing = structuredClone(profiles["preprod-testing"].timing);
    assert.throws(() => validateProfile(profile, name), /Dispute schedule/u);
  }
});

test("every DA response window covers the minimum response budget, and the bound is exact", () => {
  const profiles = readProfiles();
  // (confirmation depth + 5 chained 64 KiB publications + 1 poll block)
  // × 20 s × 2, derived by hand.
  const expected = {
    mainnet: 1_440_000,
    "preprod-public": 1_440_000,
    "preprod-testing": 360_000,
    "local-devnet-testing": 360_000,
  };
  for (const [name, profile] of Object.entries(profiles)) {
    const budget = minimumDaResponseBudgetMs(profile);
    assert.equal(budget, expected[name]);
    assert.ok(profile.timing.da_small_response_window_ms >= budget);
    assert.ok(profile.timing.da_full_response_window_ms >= budget);
    const atBound = structuredClone(profile);
    atBound.timing.da_small_response_window_ms = budget;
    validateProfile(atBound, name);
    atBound.timing.da_small_response_window_ms = budget - 1;
    assert.throws(
      () => validateProfile(atBound, name),
      new RegExp(`minimum response budget of ${budget} ms`, "u"),
    );
  }
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    const previous = structuredClone(profiles[name]);
    previous.timing.da_small_response_window_ms = 60_000;
    previous.timing.da_full_response_window_ms = 120_000;
    assert.throws(
      () => validateProfile(previous, name),
      /minimum response budget/u,
    );
  }
});

test("public profiles leave the whole dispute schedule after the latest DA response; testing profiles are exempt", () => {
  const profiles = readProfiles();
  // (2 × rounds + 2) × dispute response window, as validation-dispute-v1.ak
  // max_dispute_duration computes it.
  const disputeDuration = ({ limits, timing }) =>
    (2 * limits.max_bisection_rounds + 2) * timing.dispute_response_window_ms;
  for (const name of ["mainnet", "preprod-public"]) {
    const profile = structuredClone(profiles[name]);
    const { timing } = profile;
    assert.equal(disputeDuration(profile), 19_800_000);
    const latestFullWindow =
      timing.block_maturity_ms -
      timing.da_attestation_timeout_ms -
      timing.max_validity_range_ms -
      disputeDuration(profile);
    assert.ok(timing.da_full_response_window_ms < latestFullWindow);
    timing.da_full_response_window_ms = latestFullWindow - 1;
    validateProfile(profile, name);
    timing.da_full_response_window_ms = latestFullWindow;
    assert.throws(
      () => validateProfile(profile, name),
      /dispute schedule must end before block maturity/u,
    );
    // A window that clears maturity only without the dispute schedule, which
    // the half-maturity dispute check alone does not catch.
    timing.da_full_response_window_ms =
      latestFullWindow + disputeDuration(profile) - 1;
    assert.throws(
      () => validateProfile(profile, name),
      /dispute schedule must end before block maturity/u,
    );
  }
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    const profile = profiles[name];
    const { timing } = profile;
    assert.ok(
      timing.da_attestation_timeout_ms +
        timing.max_validity_range_ms +
        timing.da_full_response_window_ms +
        disputeDuration(profile) >=
        timing.block_maturity_ms,
    );
    validateProfile(structuredClone(profile), name);
  }
});

test("digest is independent of YAML key order and changes with timing or economics", () => {
  const profile = readProfiles()["preprod-testing"];
  assert.equal(
    profileDigest(profile),
    profileDigest(Object.fromEntries(Object.entries(profile).reverse())),
  );
  const changed = structuredClone(profile);
  changed.timing.operator_shift_ms += 1;
  assert.notEqual(profileDigest(profile), profileDigest(changed));
  changed.economics.proverCollateralFloorLovelace += 1;
  assert.notEqual(profileDigest(profile), profileDigest(changed));
});

test("profile validation rejects unsafe timing, economics, networks, and unknown fields", () => {
  const mutations = [
    (profile) => {
      profile.timing.dispute_response_window_ms = 1;
    },
    (profile) => {
      profile.timing.block_maturity_ms = 0;
    },
    (profile) => {
      profile.timing.registration_ms = Number.MAX_SAFE_INTEGER + 1;
    },
    (profile) => {
      profile.timing.operator_shift_ms = 30;
    },
    (profile) => {
      profile.timing.da_small_response_window_ms =
        profile.timing.da_full_response_window_ms + 1;
    },
    (profile) => {
      profile.economics.requiredBondLovelace += 1;
    },
    (profile) => {
      profile.economics.inactivitySlashingPenaltyLovelace =
        profile.economics.slashingPenaltyLovelace;
    },
    (profile) => {
      profile.network = "Mainnet";
    },
    (profile) => {
      profile.timing.block_maturity = 123;
    },
  ];
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    for (const mutate of mutations) {
      const profile = structuredClone(readProfiles()[name]);
      mutate(profile);
      assert.throws(() => validateProfile(profile, name));
    }
  }
});

test("dispute schedule accepts the exact half-maturity bound", () => {
  const profile = structuredClone(readProfiles()["preprod-public"]);
  profile.timing.dispute_response_window_ms = 4_581_818;
  profile.timing.block_maturity_ms =
    2 *
    (2 * profile.limits.max_bisection_rounds + 2) *
    profile.timing.dispute_response_window_ms;
  validateProfile(profile, "preprod-public");
  profile.timing.block_maturity_ms -= 1;
  assert.throws(
    () => validateProfile(profile, "preprod-public"),
    /Dispute schedule/u,
  );
});
