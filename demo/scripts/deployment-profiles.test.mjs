import assert from "node:assert/strict";
import { test } from "node:test";
import {
  publicProfiles,
  testingProfiles,
  withDelayAtFloor,
} from "./deployment-profiles.test-helpers.mjs";
import {
  daBondWithdrawDelayFloorMs,
  emulatorOnlyProfileNames,
  generateProfiles,
  minimumDaResponseBudgetMs,
  minimumPublicEventWaitMs,
  minimumPublicOpenAfterApplyMarginMs,
  profileDigest,
  profileNames,
  readProfiles,
  renderAiken,
  specDaBondWithdrawDelayFloorMs,
  validateProfile,
} from "./deployment-profiles.mjs";

test("confirmation policy is explicit, validated, and bound into profile identity", () => {
  const profiles = readProfiles();
  const depth = (name) => profiles[name].l1_finality.confirmation_depth;
  assert.equal(depth("local-devnet-testing"), 10);
  assert.equal(depth("preprod-testing"), 10);
  assert.equal(depth("preprod-emulator-testing"), 3);
  for (const name of ["mainnet", "preprod-public"]) {
    assert.equal(depth(name), 30);
  }
  const profile = structuredClone(profiles.mainnet);
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

test("all profiles have explicit networks and independent deployment identities", () => {
  const profiles = readProfiles();
  assert.equal(
    profiles["preprod-public"].network,
    profiles["preprod-testing"].network,
  );
  assert.notEqual(
    profiles["preprod-public"].economics.requiredBondLovelace,
    profiles["preprod-testing"].economics.requiredBondLovelace,
  );
  assert.equal(
    new Set(Object.values(profiles).map(profileDigest)).size,
    Object.keys(profiles).length,
  );
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

test("emulator-only profiles cannot be selected for generation, checks or builds", async () => {
  assert.deepEqual(emulatorOnlyProfileNames, ["preprod-emulator-testing"]);
  for (const name of emulatorOnlyProfileNames) {
    assert.ok(profileNames.includes(name));
    // Check mode writes nothing, so a missing guard fails on the message
    // instead of rewriting the checkout's selected profile.
    await assert.rejects(generateProfiles(name, true), /emulator-only/u);
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
    assert.equal(testing.timing.operator_shift_ms, 1_800_000);
    assert.equal(testing.timing.registration_ms, 30_000);
    assert.equal(testing.timing.da_small_response_window_ms, 880_000);
    assert.equal(testing.timing.da_full_response_window_ms, 880_000);
    assert.equal(testing.timing.da_challenge_window_ms, 720_000);
    assert.equal(testing.timing.da_slash_grace_ms, 300_000);
    assert.equal(testing.timing.da_bond_withdraw_delay_ms, 2_380_000);
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
    "preprod-testing": 640_000,
    "local-devnet-testing": 640_000,
    "preprod-emulator-testing": 360_000,
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
      timing.da_challenge_window_ms -
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
    // The rule counts from the challenge window, not the attestation timeout:
    // a full window that fits after the timeout but not after the challenge
    // window is refused.
    timing.da_full_response_window_ms = latestFullWindow;
    assert.ok(
      timing.da_attestation_timeout_ms +
        timing.max_validity_range_ms +
        timing.da_full_response_window_ms +
        disputeDuration(profile) <
        timing.block_maturity_ms,
    );
    assert.throws(
      () => validateProfile(profile, name),
      /DA challenge window, maximum validity range, full response window and dispute schedule/u,
    );
  }
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    const profile = profiles[name];
    const { timing } = profile;
    assert.ok(
      timing.da_challenge_window_ms +
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
  const daBondChanged = structuredClone(profile);
  daBondChanged.da_bond.da_bond_lovelace += 1;
  assert.notEqual(profileDigest(profile), profileDigest(daBondChanged));
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

test("every profile keeps the negligence timeout at or above the commitment gap, and the bound is exact", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const profile = structuredClone(original);
    profile.timing.user_events_negligence_timeout_ms =
      profile.timing.max_inactivity_between_block_commitments_ms;
    validateProfile(profile, name);
    profile.timing.user_events_negligence_timeout_ms -= 1;
    assert.throws(
      () => validateProfile(profile, name),
      /negligence timeout must be at least/u,
    );
  }
});

test("every profile keeps the commitment gap shorter than the operator shift, and the bound is exact", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const profile = structuredClone(original);
    profile.timing.max_inactivity_between_block_commitments_ms =
      profile.timing.operator_shift_ms - 1;
    profile.timing.user_events_negligence_timeout_ms = Math.max(
      profile.timing.user_events_negligence_timeout_ms,
      profile.timing.max_inactivity_between_block_commitments_ms,
    );
    validateProfile(profile, name);
    profile.timing.max_inactivity_between_block_commitments_ms += 1;
    profile.timing.user_events_negligence_timeout_ms = Math.max(
      profile.timing.user_events_negligence_timeout_ms,
      profile.timing.max_inactivity_between_block_commitments_ms,
    );
    assert.throws(
      () => validateProfile(profile, name),
      /inactivity between block commitments must be shorter than the operator shift/u,
    );
  }
});

test("public profiles wait out the validity range plus confirmation depth before requiring an event; testing profiles are exempt", () => {
  const profiles = readProfiles();
  // 480 s maximum validity range + 30 blocks × 20 s × 2, derived by hand.
  for (const name of ["mainnet", "preprod-public"]) {
    const profile = structuredClone(profiles[name]);
    assert.equal(minimumPublicEventWaitMs(profile), 1_680_000);
    assert.ok(profile.timing.event_wait_ms >= 1_680_000);
    profile.timing.event_wait_ms = 1_680_000;
    validateProfile(profile, name);
    profile.timing.event_wait_ms -= 1;
    assert.throws(
      () => validateProfile(profile, name),
      /Event wait must cover the maximum validity range plus confirmation depth, at least 1680000 ms/u,
    );
    // A deeper finality assumption raises the floor.
    profile.timing.event_wait_ms = 1_680_000;
    profile.l1_finality.confirmation_depth += 1;
    assert.throws(() => validateProfile(profile, name), /Event wait/u);
  }
  for (const name of ["preprod-testing", "local-devnet-testing"]) {
    const profile = structuredClone(profiles[name]);
    assert.ok(profile.timing.event_wait_ms < minimumPublicEventWaitMs(profile));
    validateProfile(profile, name);
  }
});

test("dispute schedule accepts the exact half-maturity bound", () => {
  const profile = structuredClone(readProfiles()["preprod-public"]);
  // A half-maturity schedule leaves no room for the public 3 d challenge window
  // plus the 2 d full response window, so shrink the full window to keep the
  // DA rule from masking the bound under test.
  profile.timing.da_full_response_window_ms =
    profile.timing.da_small_response_window_ms;
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

test("every profile carries the pooled DA bond amounts and timing, rendered into env", () => {
  const profiles = readProfiles();
  const expected = {
    public: {
      da_bond: {
        da_bond_lovelace: 100_000_000_000,
        da_slash_penalty_lovelace: 25_000_000_000,
        da_bond_min_top_up_lovelace: 1_000_000_000,
        da_bond_pool_floor_lovelace: 5_000_000,
        challenge_record_lovelace: 27_000_000,
      },
      timing: {
        da_challenge_window_ms: 259_200_000,
        da_slash_grace_ms: 172_800_000,
        da_bond_withdraw_delay_ms: 778_080_000,
      },
    },
    testing: {
      da_bond: {
        da_bond_lovelace: 500_000_000,
        da_slash_penalty_lovelace: 100_000_000,
        da_bond_min_top_up_lovelace: 5_000_000,
        da_bond_pool_floor_lovelace: 5_000_000,
        challenge_record_lovelace: 27_000_000,
      },
      timing: {
        da_challenge_window_ms: 720_000,
        da_slash_grace_ms: 300_000,
        da_bond_withdraw_delay_ms: 2_380_000,
      },
    },
  };
  const envNames = {
    da_bond_lovelace: "da_bond_lovelace_v1",
    da_slash_penalty_lovelace: "da_slash_penalty_lovelace_v1",
    da_bond_min_top_up_lovelace: "da_bond_min_top_up_lovelace_v1",
    da_bond_pool_floor_lovelace: "da_bond_pool_floor_lovelace_v1",
    challenge_record_lovelace: "challenge_record_lovelace_v1",
    da_challenge_window_ms: "da_challenge_window_ms_v1",
    da_slash_grace_ms: "da_slash_grace_ms_v1",
    da_bond_withdraw_delay_ms: "da_bond_withdraw_delay_ms_v1",
  };
  for (const [name, profile] of Object.entries(profiles)) {
    const want =
      name === "preprod-emulator-testing"
        ? {
            da_bond: expected.testing.da_bond,
            timing: {
              da_challenge_window_ms: 1_800_000,
              da_slash_grace_ms: 300_000,
              da_bond_withdraw_delay_ms: 15_180_000,
            },
          }
        : expected[publicProfiles.includes(name) ? "public" : "testing"];
    assert.deepEqual(profile.da_bond, want.da_bond);
    const rendered = renderAiken(profile, "");
    for (const [key, value] of Object.entries({
      ...want.da_bond,
      ...want.timing,
    })) {
      if (key in want.timing) assert.equal(profile.timing[key], value);
      assert.ok(
        rendered.includes(
          `pub const ${envNames[key]}: Int = ${value.toLocaleString("en-US").replaceAll(",", "_")}`,
        ),
        `${name} renders ${envNames[key]}`,
      );
    }
  }
});

test("every profile orders attestation timeout < challenge window <= block maturity, and both bounds are exact", () => {
  const profiles = readProfiles();
  for (const [name, original] of Object.entries(profiles)) {
    const profile = structuredClone(original);
    profile.timing.da_challenge_window_ms =
      profile.timing.da_attestation_timeout_ms;
    assert.throws(
      () => validateProfile(withDelayAtFloor(profile), name),
      /DA challenge window must be longer than the attestation timeout and at most block maturity/u,
    );
    profile.timing.da_challenge_window_ms =
      profile.timing.block_maturity_ms + 1;
    assert.throws(
      () => validateProfile(withDelayAtFloor(profile), name),
      /DA challenge window must be longer than the attestation timeout and at most block maturity/u,
    );
  }
  // The public profiles also carry the dispute-schedule and late-Apply rules,
  // which forbid either extreme, so the exact bounds are shown on the testing
  // profiles.
  for (const name of testingProfiles) {
    const profile = structuredClone(profiles[name]);
    profile.timing.da_challenge_window_ms =
      profile.timing.da_attestation_timeout_ms + 1;
    validateProfile(withDelayAtFloor(profile), name);
    profile.timing.da_challenge_window_ms = profile.timing.block_maturity_ms;
    validateProfile(withDelayAtFloor(profile), name);
  }
});

test("every profile's withdrawal delay meets the spec relation and the head-removal relation, and both bounds are exact", () => {
  const profiles = readProfiles();
  const floors = {
    // 480,000 + 259,200,000 + 172,800,000 + 172,800,000 (spec) and
    // 480,000 + max(432,000,000, 604,800,000) + 172,800,000 (enforced).
    public: { spec: 605_280_000, enforced: 778_080_000 },
    // 480,000 + 720,000 + 880,000 + 300,000; the challenge path dominates
    // the 900,000 maturity, so the two forms coincide.
    testing: { spec: 2_380_000, enforced: 2_380_000 },
  };
  for (const [name, original] of Object.entries(profiles)) {
    const want =
      name === "preprod-emulator-testing"
        ? { spec: 3_420_000, enforced: 15_180_000 }
        : floors[publicProfiles.includes(name) ? "public" : "testing"];
    const profile = structuredClone(original);
    assert.equal(specDaBondWithdrawDelayFloorMs(profile.timing), want.spec);
    assert.equal(daBondWithdrawDelayFloorMs(profile.timing), want.enforced);
    assert.equal(profile.timing.da_bond_withdraw_delay_ms, want.enforced);
    validateProfile(profile, name);
    profile.timing.da_bond_withdraw_delay_ms = want.spec - 1;
    assert.throws(
      () => validateProfile(profile, name),
      new RegExp(
        `challenge window, full response window and slash grace, at least ${want.spec} ms`,
        "u",
      ),
    );
    profile.timing.da_bond_withdraw_delay_ms = want.enforced - 1;
    assert.throws(
      () => validateProfile(profile, name),
      want.enforced === want.spec
        ? /slash grace, at least 2380000 ms/u
        : new RegExp(
            `the later of the challenge response deadline and block maturity, and the slash grace, at least ${want.enforced} ms`,
            "u",
          ),
    );
  }
  // The spec's own public figure is exactly the spec floor, and it is refused:
  // it would let a committee withdraw the instant the challenged block
  // becomes removable at the queue head.
  for (const name of publicProfiles) {
    const profile = structuredClone(profiles[name]);
    profile.timing.da_bond_withdraw_delay_ms = 605_280_000;
    assert.throws(
      () => validateProfile(profile, name),
      /the later of the challenge response deadline and block maturity/u,
    );
  }
  // On a testing profile whose maturity outlasts the response deadline, the
  // enforced form binds there too.
  for (const name of testingProfiles) {
    const profile = structuredClone(profiles[name]);
    profile.timing.block_maturity_ms = 2_000_000;
    assert.equal(specDaBondWithdrawDelayFloorMs(profile.timing), 2_380_000);
    assert.equal(daBondWithdrawDelayFloorMs(profile.timing), 2_780_000);
    assert.throws(
      () => validateProfile(profile, name),
      /the later of the challenge response deadline and block maturity, and the slash grace, at least 2780000 ms/u,
    );
    profile.timing.da_bond_withdraw_delay_ms = 2_780_000;
    validateProfile(profile, name);
  }
});

test("every profile keeps 0 < DA slash penalty < DA bond and positive top-up, floor and record amounts", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const atBound = structuredClone(original);
    atBound.da_bond.da_slash_penalty_lovelace =
      atBound.da_bond.da_bond_lovelace - 1;
    validateProfile(atBound, name);
    atBound.da_bond.da_slash_penalty_lovelace =
      atBound.da_bond.da_bond_lovelace;
    assert.throws(
      () => validateProfile(atBound, name),
      /DA slash penalty must be smaller than the DA bond/u,
    );
    for (const key of [
      "da_bond_lovelace",
      "da_slash_penalty_lovelace",
      "da_bond_min_top_up_lovelace",
      "da_bond_pool_floor_lovelace",
      "challenge_record_lovelace",
    ]) {
      for (const invalid of [0, -1, 1.5, "5000000"]) {
        const profile = structuredClone(original);
        profile.da_bond[key] = invalid;
        assert.throws(
          () => validateProfile(profile, name),
          new RegExp(`da_bond\\.${key} must be a positive safe integer`, "u"),
        );
      }
      const missing = structuredClone(original);
      delete missing.da_bond[key];
      assert.throws(
        () => validateProfile(missing, name),
        /da_bond must contain exactly/u,
      );
    }
    const extra = structuredClone(original);
    extra.da_bond.challenger_bond_lovelace = 10_000_000_000;
    assert.throws(
      () => validateProfile(extra, name),
      /da_bond must contain exactly/u,
    );
    const absent = structuredClone(original);
    delete absent.da_bond;
    assert.throws(() => validateProfile(absent, name), /profile must contain/u);
    for (const key of [
      "da_challenge_window_ms",
      "da_slash_grace_ms",
      "da_bond_withdraw_delay_ms",
    ]) {
      const profile = structuredClone(original);
      profile.timing[key] = 0;
      assert.throws(
        () => validateProfile(profile, name),
        new RegExp(`timing\\.${key} must be a positive safe integer`, "u"),
      );
    }
  }
});

test("public profiles leave confirmation depth plus two validity ranges between the latest Apply and the challenge deadline; testing profiles are exempt", () => {
  const profiles = readProfiles();
  for (const name of publicProfiles) {
    const profile = structuredClone(profiles[name]);
    const { timing } = profile;
    // 2 x 480 s maximum validity range + 30 blocks x 20 s x 2, derived by hand.
    const floor = minimumPublicOpenAfterApplyMarginMs(profile);
    assert.equal(floor, 2_160_000);
    assert.ok(
      timing.da_challenge_window_ms - timing.da_attestation_timeout_ms >= floor,
    );
    timing.da_challenge_window_ms = timing.da_attestation_timeout_ms + floor;
    validateProfile(profile, name);
    timing.da_challenge_window_ms -= 1;
    assert.throws(
      () => validateProfile(profile, name),
      /DA challenge window must exceed the attestation timeout by two maximum validity ranges plus confirmation depth, at least 2160000 ms/u,
    );
    // Regression: the old two-validity-range stand-in (960 s) accepted a
    // 4,560,000 ms window after a 3,600,000 ms attestation timeout, which
    // leaves a challenger less than the confirmation-depth budget.
    assert.equal(timing.da_attestation_timeout_ms, 3_600_000);
    timing.da_challenge_window_ms = 4_560_000;
    assert.throws(
      () => validateProfile(profile, name),
      /DA challenge window must exceed the attestation timeout by two maximum validity ranges plus confirmation depth/u,
    );
  }
  for (const name of testingProfiles) {
    const profile = structuredClone(profiles[name]);
    const { timing } = profile;
    // 120 s of slack, under the public floor of two maximum validity ranges
    // plus confirmation depth: accepted on testing.
    assert.equal(
      timing.da_challenge_window_ms - timing.da_attestation_timeout_ms,
      120_000,
    );
    assert.ok(120_000 < minimumPublicOpenAfterApplyMarginMs(profile));
    validateProfile(profile, name);
  }
});
