// Every deployment profile, its Cardano network, and the classes the profile
// validation and the release tooling branch on.
export const profileNames = [
  "mainnet",
  "preprod-public",
  "preprod-rehearsal",
  "preprod-testing",
  "local-devnet-testing",
  "preprod-emulator-testing",
  "preprod-emulator-curated",
];
export const networks = [
  "Mainnet",
  "Preprod",
  "Preprod",
  "Preprod",
  "Custom",
  "Preprod",
  "Preprod",
];
// Generated and compiled for the interactive Vitest projects only; never the
// checkout's selected deployment, and never built as the default blueprint.
export const emulatorOnlyProfileNames = [
  "preprod-emulator-testing",
  "preprod-emulator-curated",
];
// Deployed as public releases. preprod-rehearsal is preprod-public with the
// bounded testing economics, and is classed as production in every other
// respect.
export const productionProfileNames = [
  "mainnet",
  "preprod-public",
  "preprod-rehearsal",
];
// The Preprod P1 profiles that bootstrap participants from retained DA inputs.
export const retainedDaProfileNames = ["preprod-public", "preprod-rehearsal"];
// Test deployments, plus the public rehearsal: the bounded economics schedule.
export const boundedEconomicsProfileNames = [
  "preprod-testing",
  "local-devnet-testing",
  "preprod-rehearsal",
  ...emulatorOnlyProfileNames,
];
