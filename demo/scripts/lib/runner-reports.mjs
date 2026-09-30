// Runner-backed check execution for the canonical-V1 evidence verifiers.
//
// The house style these verifiers grew out of published *existence* as
// *passage*: a count of `it(` lines in a test file, or of `test <name>(`
// declarations in an Aiken module, was reported as the number of checks that
// passed. Nine throwing test bodies, an `it.skip`, or a selector that collects
// nothing all leave those counts untouched (issue #519, finding V-2).
//
// the canonical V1 domain checks
// established the remedy for the Vitest side: spawn the runner, read its
// machine-readable report, and derive every published number from that report
// alone. This module is that same mechanism, generalized so the sibling gates
// share one implementation, plus the Aiken equivalent — `aiken check` prints
// its structured JSON report whenever stdout is not a TTY, which makes the same
// derivation possible for focused on-chain selectors.
//
// Nothing here reads test *source*. Every function below takes a runner report
// and either returns measured counts or throws with the exact reason the run
// may not be published as a pass.

import "node:child_process";
import "node:fs";
import "node:module";
import "node:os";
import "node:path";
import "../../../onchain/aiken/scripts/pinned-compiler.mjs";
import "./runner-reports.derive-vitest-outcome.mjs";
import "./runner-reports.derive-aiken-outcome.mjs";
export {
  aikenPublishedCommand,
  deriveAikenOutcome,
  runAikenCheck,
} from "./runner-reports.derive-aiken-outcome.mjs";
export {
  aikenBinary,
  aikenCompilerVersion,
  aikenModuleIndex,
  aikenModuleName,
  aikenSelectorPattern,
  deriveVitestOutcome,
  forkAikenBinary,
  RunnerCheckError,
  runVitest,
  vitestPublishedCommand,
} from "./runner-reports.derive-vitest-outcome.mjs";
