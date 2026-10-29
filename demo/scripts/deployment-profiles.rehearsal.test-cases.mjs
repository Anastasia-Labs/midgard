import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { renderAiken } from "./deployment-profile-aiken.mjs";
import {
  generateProfiles,
  readProfiles,
  validateProfile,
} from "./deployment-profiles.mjs";

// preprod-rehearsal: preprod-public's timing and every other value, with
// preprod-testing's economics (the operator `economics` section and the DA
// bond amounts) under the same bounded schedule identifier.
test("preprod-rehearsal is preprod-public with preprod-testing's economics", () => {
  const profiles = readProfiles();
  const rehearsal = profiles["preprod-rehearsal"];
  const publicProfile = profiles["preprod-public"];
  const testing = profiles["preprod-testing"];
  const withoutEconomics = ({
    name: _name,
    economics: _economics,
    da_bond: _daBond,
    ...rest
  }) => rest;
  // Network, finality, every timing value, limits, treasury, allow-list,
  // catalogue and DA retention: all preprod-public's.
  assert.deepEqual(
    withoutEconomics(rehearsal),
    withoutEconomics(publicProfile),
  );
  // Every amount, and the schedule identifier, from preprod-testing.
  assert.deepEqual(rehearsal.economics, testing.economics);
  assert.deepEqual(rehearsal.da_bond, testing.da_bond);
  assert.notDeepEqual(rehearsal.economics, publicProfile.economics);
  assert.notDeepEqual(rehearsal.da_bond, publicProfile.da_bond);
  validateProfile(structuredClone(rehearsal), "preprod-rehearsal");
  // The schedule identifier follows the economics, not the name.
  const relabelled = structuredClone(rehearsal);
  relabelled.economics.profile = publicProfile.economics.profile;
  assert.throws(
    () => validateProfile(relabelled, "preprod-rehearsal"),
    /economics\.profile must equal bounded-acceptance-v1/u,
  );
});

test("preprod-rehearsal is held to every production rule preprod-public is", () => {
  const profiles = readProfiles();
  const production =
    /event_wait_ms - max_validity_range_ms >= 3 d slot_length_ms \/ active_slot_coeff on a production profile; the largest such d is 22$/u;
  for (const name of ["preprod-public", "preprod-rehearsal"]) {
    const deeper = structuredClone(profiles[name]);
    deeper.l1_finality.commit_event_depth += 1;
    assert.throws(() => validateProfile(deeper, name), production);
    const open = structuredClone(profiles[name]);
    open.allow_lists.operators = null;
    assert.throws(
      () => validateProfile(open, name),
      /must list the admitted keys on a production profile/u,
    );
    const noRetention = structuredClone(profiles[name]);
    delete noRetention.da_transport;
    assert.throws(
      () => validateProfile(noRetention, name),
      /profile must contain exactly/u,
    );
    // The 900 s testing timing passes only under a testing name.
    const testingTiming = structuredClone(profiles[name]);
    testingTiming.timing = structuredClone(profiles["preprod-testing"].timing);
    testingTiming.l1_finality = structuredClone(
      profiles["preprod-testing"].l1_finality,
    );
    testingTiming.excluded_fraud_proof_categories = ["validationTraceDispute"];
    assert.throws(() => validateProfile(testingTiming, name));
  }
});

test("preprod-rehearsal generates its own Aiken environment", async () => {
  const profiles = readProfiles();
  const template = readFileSync(
    new URL("../../config/deployments/env.ak.template", import.meta.url),
    "utf8",
  );
  assert.equal(
    readFileSync(
      new URL("../../onchain/aiken/env/preprod-rehearsal.ak", import.meta.url),
      "utf8",
    ),
    renderAiken(profiles["preprod-rehearsal"], template),
  );
  // Check mode regenerates every output and compares; nothing is written.
  await generateProfiles("preprod-testing", true);
});
