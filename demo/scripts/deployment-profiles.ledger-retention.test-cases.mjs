import assert from "node:assert/strict";
import { test } from "node:test";

import { readProfiles, validateProfile } from "./deployment-profiles.mjs";

// timing.ledger_retention_ms: how long each L2 block's ledger root stays
// readable off-chain. Its only floor is block maturity (owner ruling
// 2026-11-21).
test("every profile refuses a ledger retention below block maturity, naming both values", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const profile = structuredClone(original);
    const maturity = profile.timing.block_maturity_ms;
    profile.timing.ledger_retention_ms = maturity - 1;
    assert.throws(
      () => validateProfile(profile, name),
      (error) =>
        error.message.includes("ledger_retention_ms") &&
        error.message.includes(String(maturity - 1)) &&
        error.message.includes("block_maturity_ms") &&
        error.message.includes(String(maturity)),
    );
  }
});

test("every profile accepts a ledger retention equal to block maturity", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const profile = structuredClone(original);
    profile.timing.ledger_retention_ms = profile.timing.block_maturity_ms;
    validateProfile(profile, name);
  }
});

test("every profile refuses a missing or non-positive-integer ledger retention", () => {
  for (const [name, original] of Object.entries(readProfiles())) {
    const missing = structuredClone(original);
    delete missing.timing.ledger_retention_ms;
    assert.throws(
      () => validateProfile(missing, name),
      /timing must contain exactly .*ledger_retention_ms/u,
    );
    for (const value of [0, -1, 1.5, "604800000"]) {
      const profile = structuredClone(original);
      profile.timing.ledger_retention_ms = value;
      assert.throws(() => validateProfile(profile, name));
    }
  }
});
