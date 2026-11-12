/** The `scriptIntegrityHashMismatch` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY } from "./contracts.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS } from "./custody.js";
import { scriptIntegrityHashMismatchTerminalFamily } from "./prover.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY } from "./replay.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK =
  defineDirectWalkFamily({
    spec: SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
    family: SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY,
    artifact: SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY,
    replay: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    fields: SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS,
    terminal: ({ admitted }) =>
      scriptIntegrityHashMismatchTerminalFamily(admitted.material),
  });
