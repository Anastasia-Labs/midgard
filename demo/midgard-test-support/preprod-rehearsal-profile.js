import { emulatorProfileBlueprint } from "./interactive-emulator.js";

/**
 * `preprod-rehearsal`: the public Preprod profile with testing economics,
 * selected only inside the Vitest project that loads a rehearsal stack
 * configuration, which needs only the compiled profile. The checkout's
 * selected profile and default blueprint never change.
 */
export const preprodRehearsalPlugin = emulatorProfileBlueprint(
  "preprod-rehearsal",
  "preprod-rehearsal",
).plugin;
