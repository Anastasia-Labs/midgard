/** The `missingScriptSource` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { MISSING_SCRIPT_SOURCE_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { MISSING_SCRIPT_SOURCE_FAMILY } from "./contracts.js";
import { MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY } from "./replay.js";

export const MISSING_SCRIPT_SOURCE_DIRECT_WALK = defineDirectWalkFamily({
  spec: MISSING_SCRIPT_SOURCE_CURSOR_SPEC,
  family: MISSING_SCRIPT_SOURCE_FAMILY,
  artifact: MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY,
  replay: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
});
