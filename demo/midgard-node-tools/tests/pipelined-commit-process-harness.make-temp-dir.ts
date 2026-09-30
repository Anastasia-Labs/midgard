import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";

import { type PipelinedCommitNodeProcessSpec } from "../src/e2e/pipelined-commit-process-harness.js";

/**
 * What this suite protects — and what it deliberately does not.
 *
 * The supervised processes here are stub scripts written by the test, so the
 * node's own lease acquisition, journal preparation and candidate
 * invalidation are *not* executed: proving those requires running the real
 * `midgard-node` build against a real Postgres, which is the acceptance
 * lane's job, not this file's. Asserting that a stub printed the line the
 * stub was told to print would be a test of the test.
 *
 * What *is* real here is the harness itself, and it makes decisions of its
 * own: it arms a one-shot crash file and refuses to re-arm one, it requires
 * exactly one supervised checkpoint termination and requires it to be a
 * SIGKILL, it classifies which of two concurrent processes is the winner and
 * which the loser from the marker each one terminated on, it refuses to
 * return a contention result whose loser did not record the evidence the gate
 * exists to collect, and it refuses contention specs that are not actually
 * contending. Every case below names one of those harness decisions, and the
 * stub scripts are the controlled inputs that drive it — including into its
 * refusals.
 */

export const makeTempDir = createTrackedTempDirFactory(
  "midgard-pipelined-commit-process-",
);

export const MID_BUILD_MARKER =
  "pipeline_trace phase=e2e_crash_checkpoint checkpoint=speculative_mid_build";

export const JOURNAL_MARKER =
  "pipeline_trace phase=e2e_crash_checkpoint checkpoint=journal_prepared_before_submit";

export const SUBMITTED_MARKER = "pipeline_trace phase=candidate_submitted";

export const LEASE_BUSY_LINE =
  "pipeline_trace phase=speculative_submission_deferred reason=state_queue_lease_busy";

export const makeNodeSpec = ({
  nodeId,
  script,
  rawLogPath,
}: {
  readonly nodeId: string;
  readonly script: string;
  readonly rawLogPath: string;
}): PipelinedCommitNodeProcessSpec => ({
  nodeId,
  postgresIdentity: "shared-test-postgres",
  ledgerMpfDbPath: `/tmp/${nodeId}-ledger-mpf`,
  transactionsMpfDbPath: `/tmp/${nodeId}-transactions-mpf`,
  stateQueueMutationLeaseTtlMs: 250,
  process: {
    service: "pipelined-node-probe",
    command: process.execPath,
    args: [script],
    cwd: process.cwd(),
    envInheritance: "process",
    rawLogPath,
    timeoutMs: 2_000,
  },
});

/**
 * A stub that races for the one-shot arm file exactly as the real node does —
 * whoever unlinks it first is the process the harness must class as the
 * journal winner — and then plays the lines the caller chose.
 */
export const armRaceScript = (
  winnerLines: string,
  loserLines: string,
): string =>
  [
    "import { unlinkSync } from 'node:fs';",
    "let winner = false;",
    "try {",
    "  unlinkSync(process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE);",
    "  winner = true;",
    "} catch (error) {",
    "  if (error?.code !== 'ENOENT') throw error;",
    "}",
    "if (winner) {",
    winnerLines,
    "} else {",
    loserLines,
    "}",
  ].join("\n");

/** A stub that races for a shared `wx` lock file instead of the arm file. */
export const lockRaceScript = (
  winnerLines: string,
  loserLines: string,
): string =>
  [
    "import { writeFileSync } from 'node:fs';",
    "let winner = false;",
    "try {",
    "  writeFileSync(process.env.SHARED_LOCK_FILE, String(process.pid), { flag: 'wx' });",
    "  winner = true;",
    "} catch (error) {",
    "  if (error?.code !== 'EEXIST') throw error;",
    "}",
    "if (winner) {",
    winnerLines,
    "} else {",
    loserLines,
    "}",
  ].join("\n");

export const withSharedLock = (
  spec: PipelinedCommitNodeProcessSpec,
  lockFile: string,
): PipelinedCommitNodeProcessSpec => ({
  ...spec,
  process: {
    ...spec.process,
    env: { ...spec.process.env, SHARED_LOCK_FILE: lockFile },
  },
});
