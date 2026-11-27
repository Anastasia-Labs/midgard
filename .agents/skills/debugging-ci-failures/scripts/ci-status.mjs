#!/usr/bin/env node
// Which GitHub Actions workflows ran on the latest head of a branch or pull
// request, which did not and why, and where each red run stopped.
//
// A missing run is not a pass. GitHub creates no pull_request run while a pull
// request has merge conflicts, pushes trigger only on the branches a workflow
// names, and a `paths:` filter can skip a workflow outright. This script reads
// each workflow's triggers at the head commit, decides which workflows should
// have run, and reports every expected workflow that has no run.
//
// Usage:
//   node ci-status.mjs <pr-number|branch> [--repo owner/name] [--json]
//                      [--wait [--timeout <minutes>|<N>m|<N>h]]
//
// --wait looks again every minute while runs on the head are in progress (and
// for the first five minutes while an expected run has not appeared), stops at
// the first failure, and gives up after --timeout (bare digits or `m` are
// minutes, `h` hours; default 60 minutes, at most 360). It exits with the
// code below for its last look; progress goes to stderr. See
// ci-status.wait.mjs.
//
// Exit codes (distinct, so a caller never mistakes "could not look" for
// "looked and found nothing"):
//   0  every expected workflow ran on the head commit and passed
//   1  at least one run on the head commit failed (or was cancelled)
//   2  at least one expected workflow has no run on the head commit, or no
//      workflow covers the head at all
//   3  could not query GitHub (gh missing, not authenticated, network, a
//      response that did not parse)
//   4  nothing failed and nothing is missing, but runs are still in progress
//      (with --wait: still in progress when the timeout ran out)
//   64 usage error
//
// Read-only: every gh call is a GET. The gh calls go through an injectable
// runner so the tests use fixture JSON instead of the network.

import "node:child_process";
import "node:url";
import "./ci-status.parse-workflow.mjs";
import "./ci-status.expectation.mjs";
import "./ci-status.collect-status.mjs";
import "./ci-status.render-text.mjs";
import "./ci-status.wait.mjs";

import { fileURLToPath } from "node:url";

import { main } from "./ci-status.render-text.mjs";

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
export { collectStatus, verdict } from "./ci-status.collect-status.mjs";
export { expectation, failedJobs } from "./ci-status.expectation.mjs";
export {
  DEFAULT_REPO,
  EXIT,
  ghRunner,
  matchesPatterns,
  parseWorkflow,
  PATH_FILTER_FILE_LIMIT,
  QueryError,
} from "./ci-status.parse-workflow.mjs";
export { main, renderText } from "./ci-status.render-text.mjs";
export {
  DEFAULT_TIMEOUT_MINUTES,
  MAX_QUERY_FAILURES,
  MISSING_GRACE_MS,
  POLL_INTERVAL_MS,
  waitForStatus,
} from "./ci-status.wait.mjs";
