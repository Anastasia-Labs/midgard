/** The `missingRedeemer` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { MISSING_REDEEMER_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { MISSING_REDEEMER_FAMILY } from "./contracts.js";
import { MISSING_REDEEMER_ARTIFACT_FAMILY } from "./replay.js";

export const MISSING_REDEEMER_DIRECT_WALK = defineDirectWalkFamily({
  spec: MISSING_REDEEMER_CURSOR_SPEC,
  family: MISSING_REDEEMER_FAMILY,
  artifact: MISSING_REDEEMER_ARTIFACT_FAMILY,
  replay: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
});
