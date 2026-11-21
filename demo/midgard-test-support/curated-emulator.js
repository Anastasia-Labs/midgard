import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { emulatorProfileBlueprint } from "./interactive-emulator.js";

/**
 * `preprod-emulator-curated`: the interactive emulator profile with the stage-0
 * operator allow-list and the P1 fraud-proof catalogue. Its Vitest project runs the
 * curated-deployment journeys against a blueprint compiled for it.
 */
const curated = emulatorProfileBlueprint(
  "preprod-emulator-curated",
  "curated-emulator",
);
export const curatedEmulatorProfile = curated.profile;
export const curatedEmulatorBlueprint = curated.blueprint;
export const curatedEmulatorSetup = fileURLToPath(import.meta.url);
export const curatedEmulatorPlugin = curated.plugin;

export default curated.setup;

// CLI: `node demo/midgard-test-support/curated-emulator.js` builds the
// blueprint, or leaves a fresh one in place, outside Vitest.
if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  await curated.setup();
