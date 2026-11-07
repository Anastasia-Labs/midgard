/** The validation-trace dispute runner at the script-sources redeemer
 * normalization step after state 49 of our trace, over a deployment that
 * publishes the phase's resolvers but not the redeemer item chain past the
 * redeemer normalization semantic resolver: the step raises the
 * one-step-required alarm and the run keeps going
 * (support/validation-trace-dispute-one-step-undriven.ts). */
import { REDEEMER_NORMALIZATION_SEMANTIC } from "./support/validation-trace-dispute-one-step-script-sources-redeemer-drive.js";
import { describeUndrivenStep } from "./support/validation-trace-dispute-one-step-undriven.js";

describeUndrivenStep({
  role: "redeemer normalization",
  phase: "scriptSources",
  semantic: REDEEMER_NORMALIZATION_SEMANTIC,
  step: 49,
});
