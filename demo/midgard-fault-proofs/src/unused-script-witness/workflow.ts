/** The `unusedScriptWitness` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { UNUSED_SCRIPT_WITNESS_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import {
  UNUSED_SCRIPT_WITNESS_FAMILY,
  unusedScriptWitnessTerminalFamily,
} from "./contracts.js";
import { UNUSED_SCRIPT_WITNESS_FIELDS } from "./custody.js";
import { UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY } from "./replay.js";

export const UNUSED_SCRIPT_WITNESS_DIRECT_WALK = defineDirectWalkFamily({
  spec: UNUSED_SCRIPT_WITNESS_CURSOR_SPEC,
  family: UNUSED_SCRIPT_WITNESS_FAMILY,
  artifact: UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY,
  replay: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  fields: UNUSED_SCRIPT_WITNESS_FIELDS,
  terminal: ({ proof }) => unusedScriptWitnessTerminalFamily(proof.selection),
});
