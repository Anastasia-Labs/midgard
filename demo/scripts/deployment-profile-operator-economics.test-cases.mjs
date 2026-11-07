import assert from "node:assert/strict";
import { test } from "node:test";

import { readProfiles, validateProfile } from "./deployment-profiles.mjs";

for (const [name, original] of Object.entries(readProfiles())) {
  test(`${name} accepts its shipped operator economics and exact boundaries`, () => {
    assert.doesNotThrow(() => validateProfile(structuredClone(original), name));
    const profile = structuredClone(original);
    const economics = profile.economics;
    economics.fraudProverRewardLovelace =
      economics.inactivitySlashingPenaltyLovelace;
    economics.requiredBondLovelace =
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace;
    assert.doesNotThrow(() => validateProfile(profile, name));
    economics.inactivitySlashingPenaltyLovelace =
      economics.slashingPenaltyLovelace - 1;
    economics.fraudProverRewardLovelace =
      economics.inactivitySlashingPenaltyLovelace;
    economics.requiredBondLovelace =
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace;
    assert.doesNotThrow(() => validateProfile(profile, name));
  });

  test(`${name} refuses an operator bond that differs from slash plus reward`, () => {
    for (const delta of [-1, 1]) {
      const profile = structuredClone(original);
      profile.economics.requiredBondLovelace += delta;
      assert.throws(() => validateProfile(profile, name), {
        message:
          "economics.requiredBondLovelace must equal economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace",
      });
    }
  });

  test(`${name} refuses a fraud prover reward below the inactivity penalty`, () => {
    const profile = structuredClone(original);
    const economics = profile.economics;
    economics.fraudProverRewardLovelace =
      economics.inactivitySlashingPenaltyLovelace - 1;
    economics.requiredBondLovelace =
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace;
    assert.throws(() => validateProfile(profile, name), {
      message:
        "economics.fraudProverRewardLovelace must be at least economics.inactivitySlashingPenaltyLovelace",
    });
  });

  test(`${name} refuses an inactivity penalty equal to the slash`, () => {
    const profile = structuredClone(original);
    const economics = profile.economics;
    economics.inactivitySlashingPenaltyLovelace =
      economics.slashingPenaltyLovelace;
    economics.fraudProverRewardLovelace = Math.max(
      economics.fraudProverRewardLovelace,
      economics.inactivitySlashingPenaltyLovelace,
    );
    economics.requiredBondLovelace =
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace;
    assert.throws(() => validateProfile(profile, name), {
      message:
        "economics.inactivitySlashingPenaltyLovelace must be smaller than economics.slashingPenaltyLovelace",
    });
  });
}
