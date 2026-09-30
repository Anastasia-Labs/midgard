// The preflight check registry: which local checks a change can break, and the
// exact command that proves it did not.
//
// DOCTRINE — selection is never the final gate.
//   * CI runs every check on every pull request before merge. Preflight exists
//     to catch a red build before the push, not to decide what CI may skip.
//   * Selection favours recall over precision. When in doubt, add a trigger or
//     a FULL_RUN entry; a check that runs needlessly costs minutes, a check
//     that was skipped because a glob was too tight costs a red CI run.
//   * A file in FULL_RUN, a moved compiler pin, `--full`, or
//     MIDGARD_PREFLIGHT_FULL=1 selects every check at full scope.
//   * "Could not look" is never "passed": a check whose capability is missing
//     is SKIPPED WITH A REASON and the run exits 3, not 0.
//   * When CI catches a failure preflight selection missed, record it in the
//     misses log described in docs/agents/required-checks.md and widen the
//     triggers that missed it.
//
// Trigger lists are derived from the tool that owns them (see derive.mjs), so
// they cannot drift from what the tool actually reads.

import "node:fs";
import "node:path";
import "../../onchain/aiken/scripts/guard-focused-selector.mjs";
import "./derive.mjs";
import "./registry.demo-checks.mjs";
import "./registry.tooling-checks.mjs";
import "./registry.select-checks.mjs";
export { IGNORED_PATHS } from "./derive.mjs";
export {
  formatCommand,
  FULL_RUN,
  FULL_RUN_ENV,
} from "./registry.demo-checks.mjs";
export { buildRegistry, selectChecks } from "./registry.select-checks.mjs";
