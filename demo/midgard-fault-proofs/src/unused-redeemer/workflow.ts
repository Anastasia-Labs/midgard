/** The `unusedRedeemer` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { UNUSED_REDEEMER_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { UNUSED_REDEEMER_FAMILY } from "./contracts.js";
import { UNUSED_REDEEMER_ARTIFACT_FAMILY } from "./replay.js";

export const UNUSED_REDEEMER_DIRECT_WALK = defineDirectWalkFamily({
  spec: UNUSED_REDEEMER_CURSOR_SPEC,
  family: UNUSED_REDEEMER_FAMILY,
  artifact: UNUSED_REDEEMER_ARTIFACT_FAMILY,
  replay: UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
});
