import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { emulatorProfileBlueprint } from "./interactive-emulator.js";

/**
 * `preprod-public`: the public Preprod profile, selected only inside the
 * Vitest project that signs a public release and reopens it with the node's
 * public release verifiers, against a blueprint compiled for it. The
 * checkout's selected profile and default blueprint never change.
 */
const preprodPublic = emulatorProfileBlueprint(
  "preprod-public",
  "preprod-public",
);
export const preprodPublicProfile = preprodPublic.profile;
export const preprodPublicBlueprint = preprodPublic.blueprint;
export const preprodPublicSetup = fileURLToPath(import.meta.url);
export const preprodPublicPlugin = preprodPublic.plugin;

export default preprodPublic.setup;

// CLI: `node demo/midgard-test-support/preprod-public-profile.js` builds the
// blueprint, or leaves a fresh one in place, outside Vitest.
if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  await preprodPublic.setup();
