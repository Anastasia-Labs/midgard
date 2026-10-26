import assert from "node:assert/strict";
import { test } from "node:test";

import {
  profileDigest,
  readProfiles,
  validateProfile,
} from "./deployment-profiles.mjs";

test("the public Preprod profile digest commits its DA bootstrap history", () => {
  const profiles = readProfiles();
  const profile = profiles["preprod-public"];
  const changed = structuredClone(profile);
  changed.da_transport.retention_days = 121;
  validateProfile(changed, changed.name);
  assert.notEqual(profileDigest(profile), profileDigest(changed));
});

test("public Preprod refuses missing, shorter or malformed DA retention", () => {
  const original = readProfiles()["preprod-public"];
  for (const days of [0, 15, 119, 120.5, "120", Number.MAX_SAFE_INTEGER + 1]) {
    const profile = structuredClone(original);
    profile.da_transport = { retention_days: days };
    assert.throws(() => validateProfile(profile, profile.name));
  }
  const missing = structuredClone(original);
  delete missing.da_transport;
  assert.throws(() => validateProfile(missing, missing.name));
  const extra = structuredClone(original);
  extra.da_transport = { retention_days: 120, compatibility: true };
  assert.throws(() => validateProfile(extra, extra.name));
});
