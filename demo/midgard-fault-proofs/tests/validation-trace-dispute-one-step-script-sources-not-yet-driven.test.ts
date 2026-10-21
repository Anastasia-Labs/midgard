/** The validation-trace dispute runner at the script-sources steps its
 * deployment does not yet drive: the stage-four begin step of the failing
 * transaction, and the stage-six fold-asset step of the failing transaction
 * that also mints under a native script's policy. Each raises the
 * one-step-required alarm and the run keeps going
 * (support/validation-trace-dispute-one-step-undriven.ts). */
import { describeUndrivenStep } from "./support/validation-trace-dispute-one-step-undriven.js";

describeUndrivenStep({
  role: "stage-four begin",
  phase: "scriptSources",
  semantic: 0,
  step: 58,
});

describeUndrivenStep({
  role: "stage-six fold-asset",
  phase: "scriptSources",
  semantic: 0,
  step: 98,
  installs: { failingNativeMint: true },
});
