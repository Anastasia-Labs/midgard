import { daBondWithdrawDelayFloorMs } from "./deployment-profiles.mjs";

// Public timing and the public validation rules; preprod-rehearsal differs
// from preprod-public only in its (bounded) economics.
export const publicProfiles = [
  "mainnet",
  "preprod-public",
  "preprod-rehearsal",
];
export const testingProfiles = ["preprod-testing", "local-devnet-testing"];
// Moves the withdrawal delay onto its enforced floor, so a test can vary one
// timing input without also tripping the delay relation.
export const withDelayAtFloor = (profile) => {
  profile.timing.da_bond_withdraw_delay_ms = daBondWithdrawDelayFloorMs(
    profile.timing,
  );
  return profile;
};
