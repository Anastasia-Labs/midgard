import { evaluateMidgardCekVerdict } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import type { ValidationPoolService } from "../../src/services/validation-pool.js";

/** Explicit unit/benchmark behavior; no HTTP advisory work in this fixture. */
export const testingInlineCekVerdicts = {
  tryEvaluateCekVerdict: () => Effect.succeed(undefined),
  evaluateCekVerdict: (request) =>
    Effect.sync(() => evaluateMidgardCekVerdict(request)),
} satisfies Pick<
  ValidationPoolService,
  "tryEvaluateCekVerdict" | "evaluateCekVerdict"
>;
