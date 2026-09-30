#!/usr/bin/env node
// Compares the failures in vitest JSON reports with a program's list of
// accepted failures, so a gate run investigates only the failures that are new.
//
// Usage:
//   node diff-test-reds.mjs --accepted <list.json> --suite <suite> [--log <run.log>]... <report.json> [<report.json>...] [--json]
//
//   --accepted  the accepted-failures list (format below)
//   --suite     which list entries apply: the package directory name the
//               reports come from, e.g. midgard-watcher (not demo/midgard-watcher)
//   --log       the default reporter's output of a run, repeatable. The JSON
//               report leaves out errors raised outside any test (an unhandled
//               rejection fails the run while every test passes), so a log
//               whose summary counts errors, or that has no summary because
//               the run did not finish, is a NEW failure, unless the list
//               accepts the errors (see "(run)" below). Without --log the
//               tool cannot see either, and warns.
//   --json      print the result as JSON instead of text
//
// Produce a report and its log with (delete the old report first: vitest
// writes it only at the end, so a killed run leaves the previous one):
//   rm -f <report.json>
//   <env> pnpm --dir demo/<package> exec vitest run <files> <flags> --reporter=default --reporter=json --outputFile=<report.json> 2>&1 | tee <run.log>
// where <env> and <flags> are what the package's `test` script sets (watcher:
// env MALLOC_MMAP_THRESHOLD_=131072; node and node-tools: NODE_ENV=emulator
// and --disableConsoleIntercept, node-tools after its node --test preludes).
//
// The list:
//   { "version": 1, "program": string, "base": string, "generatedFrom": [string],
//     "entries": [ { "suite": string, "file": string, "name": string,
//                    "kind": "deterministic" | "flaky", "reason": string } ] }
// `file` is relative to the package directory ("tests/x.test.ts"); `name` is
// the test's `assertionResults[].fullName`, or "*" for every test in the file
// and for a file that failed as a whole (failed to load, or a hook threw).
// A "*" entry also accepts any new failure in its file: failures it accepts
// are marked "via *" (`wildcard: true` in JSON), and a "*" entry whose file
// also has passing tests is warned about.
// An entry with file "(run)" records the base's errors outside any test. Its
// name is the tool's own wording, "errors outside any test: vitest reported
// <M> error(s); they are not in the JSON report", and it accepts a log that
// reports N such errors when N <= M. A "(run)" entry whose name carries no
// count is malformed (exit 2). A run that printed no summary is always NEW.
// Names are compared with surrounding whitespace trimmed: vitest 3.0.7 puts a
// leading space in `fullName` when the run has no configured project name.
//
// It prints the NEW failures (no entry), the ACCEPTED failures, the
// ACCEPTED-FLAKY failures, the STALE deterministic entries (their tests ran
// and passed), and counts the flaky entries that passed and the entries with no
// result in these reports.
//
// Exit codes:
//   0  no new failure
//   1  at least one new failure
//   2  bad arguments, or a list or report that is unreadable or malformed:
//      nothing was compared

import "./diff-test-reds.validate-accepted-list.mjs";
import "node:fs";
import "node:path";
import "node:url";
import "./diff-test-reds.validate-accepted-list.mjs";
import "./diff-test-reds.diff-reds.mjs";

import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { main } from "./diff-test-reds.diff-reds.mjs";

if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
export { diffReds, main } from "./diff-test-reds.diff-reds.mjs";
export {
  collectResults,
  fileMatches,
  InputError,
  parseArgs,
  runErrorCount,
  runLevelFailures,
  validateAcceptedList,
  validateReport,
  WHOLE_FILE,
} from "./diff-test-reds.validate-accepted-list.mjs";
