/**
 * The mint-declared-asset-limit builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
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
import {
  unjournaled,
  type UnjournaledSubmitter,
} from "./submit-unjournaled.js";

export { submitCommittedFieldShapeInit } from "./submit-unjournaled.committed-field-shape.js";

export const submitMintDeclaredAssetLimitStep01Accepted = unjournaled(
  buildMintDeclaredAssetLimitStep01Accepted,
);
export const submitMintDeclaredAssetLimitStep01Forced = unjournaled(
  buildMintDeclaredAssetLimitStep01Forced,
);
export const submitMintDeclaredAssetLimitStep01ForcedRaw = unjournaled(
  buildMintDeclaredAssetLimitStep01ForcedRaw,
);
export const submitMintDeclaredAssetLimitStep02 = unjournaled(
  buildMintDeclaredAssetLimitStep02,
);
export const submitMintDeclaredAssetLimitStep02Raw: UnjournaledSubmitter<
  typeof buildMintDeclaredAssetLimitStep02Raw
> = unjournaled(buildMintDeclaredAssetLimitStep02Raw);
export const submitMintDeclaredAssetLimitStep03 = unjournaled(
  buildMintDeclaredAssetLimitStep03,
);
export const submitMintDeclaredAssetLimitStep03Raw = unjournaled(
  buildMintDeclaredAssetLimitStep03Raw,
);
export const submitMintDeclaredAssetLimitStep04 = unjournaled(
  buildMintDeclaredAssetLimitStep04,
);
export const submitMintDeclaredAssetLimitStep04Raw = unjournaled(
  buildMintDeclaredAssetLimitStep04Raw,
);
