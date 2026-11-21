// fixture-path: midgard-validation/tests/support/root-entry.ts
// A package's own files are held to no-own-package-barrel, not this rule.
// ok: midgard/no-cross-package-root
import { runPhaseAValidation } from "@al-ft/midgard-validation";

export const phaseA = runPhaseAValidation;
