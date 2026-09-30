import { type SpendingValidator as SdkSpendingValidator } from "@al-ft/midgard-sdk";

import { makeSpendingValidator } from "./validators.js";

type EmulatorStepTuple = readonly [
  Pick<SdkSpendingValidator, "spendingScript">,
  ...Pick<SdkSpendingValidator, "spendingScript">[],
];

type SdkStepTuple<Steps extends EmulatorStepTuple> = {
  readonly [Index in keyof Steps]: SdkSpendingValidator;
};

export const chainFromSteps = <const Steps extends EmulatorStepTuple>(
  steps: Steps,
) => {
  const sdkSteps = steps.map((step) =>
    makeSpendingValidator(step.spendingScript.script),
  ) as unknown as SdkStepTuple<Steps>;
  return { firstStep: sdkSteps[0], steps: sdkSteps };
};
