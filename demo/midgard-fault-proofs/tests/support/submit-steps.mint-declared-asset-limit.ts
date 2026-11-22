/**
 * The mint-declared-asset-limit builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-steps.ts).
 */
import {
  buildMintDeclaredAssetLimitStep01Accepted,
  buildMintDeclaredAssetLimitStep01Forced,
  buildMintDeclaredAssetLimitStep01ForcedRaw,
} from "../../src/mint-declared-asset-limit/submit-step-01.js";
import {
  buildMintDeclaredAssetLimitStep02,
  buildMintDeclaredAssetLimitStep02Raw,
} from "../../src/mint-declared-asset-limit/submit-step-02.js";
import {
  buildMintDeclaredAssetLimitStep03,
  buildMintDeclaredAssetLimitStep03Raw,
} from "../../src/mint-declared-asset-limit/submit-step-03.js";
import {
  buildMintDeclaredAssetLimitStep04,
  buildMintDeclaredAssetLimitStep04Raw,
} from "../../src/mint-declared-asset-limit/submit-step-04.js";
import { type StepSubmitter, stepSubmitter } from "./submit-steps.js";

export { submitCommittedFieldShapeInit } from "./submit-steps.committed-field-shape.js";

export const submitMintDeclaredAssetLimitStep01Accepted = stepSubmitter(
  buildMintDeclaredAssetLimitStep01Accepted,
);
export const submitMintDeclaredAssetLimitStep01Forced = stepSubmitter(
  buildMintDeclaredAssetLimitStep01Forced,
);
export const submitMintDeclaredAssetLimitStep01ForcedRaw = stepSubmitter(
  buildMintDeclaredAssetLimitStep01ForcedRaw,
);
export const submitMintDeclaredAssetLimitStep02 = stepSubmitter(
  buildMintDeclaredAssetLimitStep02,
);
export const submitMintDeclaredAssetLimitStep02Raw: StepSubmitter<
  typeof buildMintDeclaredAssetLimitStep02Raw
> = stepSubmitter(buildMintDeclaredAssetLimitStep02Raw);
export const submitMintDeclaredAssetLimitStep03 = stepSubmitter(
  buildMintDeclaredAssetLimitStep03,
);
export const submitMintDeclaredAssetLimitStep03Raw = stepSubmitter(
  buildMintDeclaredAssetLimitStep03Raw,
);
export const submitMintDeclaredAssetLimitStep04 = stepSubmitter(
  buildMintDeclaredAssetLimitStep04,
);
export const submitMintDeclaredAssetLimitStep04Raw = stepSubmitter(
  buildMintDeclaredAssetLimitStep04Raw,
);
