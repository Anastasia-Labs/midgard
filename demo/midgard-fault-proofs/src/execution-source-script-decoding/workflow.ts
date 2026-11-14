/** The `executionSourceScriptDecoding` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY } from "./contracts.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY } from "./replay.js";

export const EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK =
  defineDirectWalkFamily({
    spec: EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    family: EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
    artifact: EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY,
    replay: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    fields: [],
    proofChunk: false,
  });
