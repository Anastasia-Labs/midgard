/** The `receivePurposeLanguage` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { RECEIVE_PURPOSE_LANGUAGE_FAMILY } from "./contracts.js";
import { RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY } from "./replay.js";

export const RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK = defineDirectWalkFamily({
  spec: RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
  family: RECEIVE_PURPOSE_LANGUAGE_FAMILY,
  artifact: RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY,
  replay: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
});
