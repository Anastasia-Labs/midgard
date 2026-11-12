/** The `executionNativeScriptInvalid` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY } from "./contracts.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS } from "./custody.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY } from "./replay.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK =
  defineDirectWalkFamily({
    spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    family: EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY,
    artifact: EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY,
    replay: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    fields: EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS,
  });
