import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";

import { type JournalKillNodeProcessSpec } from "../src/e2e/journal-kill-process-harness.js";

/**
 * What this suite protects, and what it deliberately does not.
 *
 * The supervised processes here are stub scripts written by the test, so the
 * node's own lease acquisition, journal preparation and unsubmitted-journal
 * recovery are *not* executed: proving those requires running the real
 * `midgard-node` build against a real Postgres, which is the acceptance
 * lane's job, not this file's. Asserting that a stub printed the line the
 * stub was told to print would be a test of the test.
 *
 * What *is* real here is the harness itself, and it makes decisions of its
 * own: it arms a one-shot crash file and refuses to re-arm one, it requires
 * exactly one SIGKILLed journal winner, it classifies which of two concurrent
 * processes is the winner and which the survivor from the marker each one
 * terminated on, it refuses to return a result whose survivor did not log the
 * default-path recovery lines in order, and it refuses specs that are not
 * actually contending. Every case below names one of those harness
 * decisions, and the stub scripts are the controlled inputs that drive it,
 * including into its refusals.
 */

export const makeTempDir = createTrackedTempDirFactory(
  "midgard-journal-kill-process-",
);

export const makeNodeSpec = ({
  nodeId,
  script,
  rawLogPath,
}: {
  readonly nodeId: string;
  readonly script: string;
  readonly rawLogPath: string;
}): JournalKillNodeProcessSpec => ({
  nodeId,
  postgresIdentity: "shared-test-postgres",
  ledgerMpfDbPath: `/tmp/${nodeId}-ledger-mpf`,
  transactionsMpfDbPath: `/tmp/${nodeId}-transactions-mpf`,
  stateQueueMutationLeaseTtlMs: 250,
  process: {
    service: "journal-kill-node-probe",
    command: process.execPath,
    args: [script],
    cwd: process.cwd(),
    envInheritance: "process",
    rawLogPath,
    timeoutMs: 2_000,
  },
});

/**
 * A stub that races for the one-shot arm file exactly as the real node does
 * (whoever unlinks it first is the process the harness must class as the
 * journal winner) and then plays the lines the caller chose. It first checks
 * that the harness armed the journal-before-submit checkpoint, so a harness
 * that armed any other checkpoint fails every case.
 */
export const armRaceScript = (
  winnerLines: string,
  loserLines: string,
): string =>
  [
    "import { unlinkSync } from 'node:fs';",
    "if (process.env.MIDGARD_E2E_COMMIT_CRASH_CHECKPOINT !== 'journal_prepared_before_submit') {",
    "  console.error('wrong checkpoint armed: ' + process.env.MIDGARD_E2E_COMMIT_CRASH_CHECKPOINT);",
    "  process.exit(3);",
    "}",
    "let winner = false;",
    "try {",
    "  unlinkSync(process.env.MIDGARD_E2E_COMMIT_CRASH_ARM_FILE);",
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

/** One `console.log` statement per line, for the stub scripts. */
export const logLines = (lines: readonly string[]): string =>
  lines.map((line) => `  console.log(${JSON.stringify(line)});`).join("\n");
