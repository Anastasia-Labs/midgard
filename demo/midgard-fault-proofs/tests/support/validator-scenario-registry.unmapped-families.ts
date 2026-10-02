import "./validator-scenario-registry.unmapped-validators.js";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export const UNMAPPED_FAMILIES: readonly Readonly<{
  reason: string;
  families: readonly FraudProofCatalogueCategoryName[];
}>[] = [
  {
    reason:
      "No emulator scenario found in which the family's validators refuse an honest block; its negatives are off-chain (planner, submitter or classifier refusals) or absent.",
    families: [
      "doubleSpend",
      "nonExistentInputNoIndex",
      "invalidRange",
      "zeroInput",
      "crossBlockDuplicateEvent",
      "missingNativeScriptUtxo",
      "redeemerCanonicity",
      "unusedRedeemer",
      "withdrawalMistag",
    ],
  },
];

/** Only ever lowered. */
export const UNMAPPED_VALIDATOR_COUNT = 563;

/** Only ever lowered. */
export const UNMAPPED_FAMILY_COUNT = 9;
