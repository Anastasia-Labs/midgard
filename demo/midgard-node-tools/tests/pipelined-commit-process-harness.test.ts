import { access } from "node:fs/promises";
import { join } from "node:path";

import {
  createTrackedTempDirFactory,
  writeScript,
} from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import {
  type PipelinedCommitNodeProcessSpec,
  runPipelinedCommitCheckpointCrash,
  runPipelinedCommitLeaseContention,
  runPipelinedCommitNormalLeaseContention,
} from "../src/e2e/pipelined-commit-process-harness.js";

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

const makeTempDir = createTrackedTempDirFactory(
  "midgard-pipelined-commit-process-",
);

const MID_BUILD_MARKER =
  "pipeline_trace phase=e2e_crash_checkpoint checkpoint=speculative_mid_build";
const JOURNAL_MARKER =
  "pipeline_trace phase=e2e_crash_checkpoint checkpoint=journal_prepared_before_submit";
const SUBMITTED_MARKER = "pipeline_trace phase=candidate_submitted";
const LEASE_BUSY_LINE =
  "pipeline_trace phase=speculative_submission_deferred reason=state_queue_lease_busy";

const makeNodeSpec = ({
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
const armRaceScript = (winnerLines: string, loserLines: string): string =>
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
const lockRaceScript = (winnerLines: string, loserLines: string): string =>
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

const withSharedLock = (
  spec: PipelinedCommitNodeProcessSpec,
  lockFile: string,
): PipelinedCommitNodeProcessSpec => ({
  ...spec,
  process: {
    ...spec.process,
    env: { ...spec.process.env, SHARED_LOCK_FILE: lockFile },
  },
});

describe("pipelined commit real-process harness", () => {
  it("arms one crash file, SIGKILLs at the checkpoint marker, and leaves the arm consumed", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "checkpoint-node.mjs",
      [
        "import { unlinkSync } from 'node:fs';",
        "unlinkSync(process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE);",
        `console.log('${MID_BUILD_MARKER} pid=' + process.pid);`,
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );
    const armFile = join(dir, "arms", "mid-build.arm");
    const summary = await runPipelinedCommitCheckpointCrash({
      spec: makeNodeSpec({
        nodeId: "node-a",
        script,
        rawLogPath: join(dir, "node-a.log"),
      }),
      checkpoint: "speculative_mid_build",
      armFile,
    });

    // One supervised attempt, terminated externally at the checkpoint the
    // caller asked for — not by the process exiting on its own, and not by
    // the supervisor's bounded timeout.
    expect(summary.attempts).toHaveLength(1);
    expect(summary.attempts[0]?.signal).toBe("SIGKILL");
    expect(summary.attempts[0]?.outputTermination).toMatchObject({
      marker: MID_BUILD_MARKER,
      signal: "SIGKILL",
    });
    // The arm is one-shot: the child consumed it, so a restart of the same
    // spec runs without crashing again.
    await expect(access(armFile)).rejects.toMatchObject({ code: "ENOENT" });
  });

  it("refuses to re-arm a checkpoint crash over an existing arm file", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(dir, "idle.mjs", "process.exit(0);\n");
    const armFile = join(dir, "arms", "mid-build.arm");
    const spec = makeNodeSpec({
      nodeId: "node-a",
      script,
      rawLogPath: join(dir, "node-a.log"),
    });
    // A stub that never consumes the arm leaves it behind…
    await expect(
      runPipelinedCommitCheckpointCrash({
        spec,
        checkpoint: "speculative_mid_build",
        armFile,
      }),
    ).rejects.toThrow(/supervised checkpoint termination/);
    // …and the next arming attempt must refuse rather than silently reuse it,
    // which would arm a crash the previous run had already claimed.
    await expect(
      runPipelinedCommitCheckpointCrash({
        spec,
        checkpoint: "speculative_mid_build",
        armFile: join(dir, "arms", "mid-build.arm"),
      }),
    ).rejects.toMatchObject({ code: "EEXIST" });
  });

  it("fails closed when the supervised process terminates at the wrong checkpoint", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "wrong-checkpoint-node.mjs",
      [
        "import { unlinkSync } from 'node:fs';",
        "unlinkSync(process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE);",
        `console.log('${JOURNAL_MARKER} pid=' + process.pid);`,
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );

    await expect(
      runPipelinedCommitCheckpointCrash({
        spec: makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "node-a.log"),
        }),
        checkpoint: "speculative_mid_build",
        armFile: join(dir, "arms", "mid-build.arm"),
      }),
    ).rejects.toThrow(
      /Expected exactly one supervised checkpoint termination for pipeline_trace phase=e2e_crash_checkpoint checkpoint=speculative_mid_build; observed 0/,
    );
  });

  it("classes the arm-file winner as the SIGKILLed journal holder and the other as the survivor", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "contention-node.mjs",
      armRaceScript(
        [
          `  console.log('${JOURNAL_MARKER} pid=' + process.pid);`,
          "  setInterval(() => {}, 1000);",
        ].join("\n"),
        [
          `  console.log('${LEASE_BUSY_LINE}');`,
          "  console.log('abandoning unsubmitted journal after lease expiry');",
          `  console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'bb'.repeat(32));`,
        ].join("\n"),
      ),
    );
    const result = await runPipelinedCommitLeaseContention({
      left: makeNodeSpec({
        nodeId: "node-a",
        script,
        rawLogPath: join(dir, "node-a.log"),
      }),
      right: makeNodeSpec({
        nodeId: "node-b",
        script,
        rawLogPath: join(dir, "node-b.log"),
      }),
      armFile: join(dir, "arms", "journal.arm"),
    });

    expect(["node-a", "node-b"]).toContain(result.winnerNodeId);
    expect(result.loserNodeId).not.toBe(result.winnerNodeId);
    // The classification is by *how each process was terminated*, so the
    // returned winner must be the one the harness SIGKILLed at the journal
    // checkpoint and the returned loser must not be.
    expect(result.winner.attempts[0]?.signal).toBe("SIGKILL");
    expect(result.winner.attempts[0]?.outputTermination?.marker).toBe(
      JOURNAL_MARKER,
    );
    expect(result.loser.attempts[0]?.outputTermination?.marker).not.toBe(
      JOURNAL_MARKER,
    );
    // The returned logs belong to the processes they are attributed to.
    expect(result.winnerLog).toContain(JOURNAL_MARKER);
    expect(result.loserLog).not.toContain(JOURNAL_MARKER);
  });

  it("refuses a journal-contention result whose survivor never recorded lease contention", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "silent-survivor.mjs",
      armRaceScript(
        [
          `  console.log('${JOURNAL_MARKER} pid=' + process.pid);`,
          "  setInterval(() => {}, 1000);",
        ].join("\n"),
        [
          "  console.log('abandoning unsubmitted journal after lease expiry');",
          `  console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'bb'.repeat(32));`,
        ].join("\n"),
      ),
    );

    await expect(
      runPipelinedCommitLeaseContention({
        left: makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "node-a.log"),
        }),
        right: makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "node-b.log"),
        }),
        armFile: join(dir, "arms", "journal.arm"),
      }),
    ).rejects.toThrow(
      "Journal-kill survivor did not record state-queue lease contention",
    );
  });

  it("refuses a journal-contention result whose survivor never recovered the unsubmitted journal", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "no-recovery-survivor.mjs",
      armRaceScript(
        [
          `  console.log('${JOURNAL_MARKER} pid=' + process.pid);`,
          "  setInterval(() => {}, 1000);",
        ].join("\n"),
        [
          `  console.log('${LEASE_BUSY_LINE}');`,
          `  console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'bb'.repeat(32));`,
        ].join("\n"),
      ),
    );

    await expect(
      runPipelinedCommitLeaseContention({
        left: makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "node-a.log"),
        }),
        right: makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "node-b.log"),
        }),
        armFile: join(dir, "arms", "journal.arm"),
      }),
    ).rejects.toThrow(
      "Journal-kill survivor did not execute unsubmitted-journal recovery after lease expiry",
    );
  });

  it("classes one submitted winner and one T2-invalidated loser in normal contention", async () => {
    const dir = await makeTempDir();
    const lockFile = join(dir, "normal-contention.lock");
    const script = await writeScript(
      dir,
      "normal-contention-node.mjs",
      lockRaceScript(
        `  console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'aa'.repeat(32));`,
        [
          `  console.log('${LEASE_BUSY_LINE}');`,
          "  setTimeout(() => console.log('pipeline_trace phase=candidate_invalidated reason=T2 state=Invalidated'), 20);",
          "  setInterval(() => {}, 1000);",
        ].join("\n"),
      ),
    );
    const result = await runPipelinedCommitNormalLeaseContention({
      left: withSharedLock(
        makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "normal-node-a.log"),
        }),
        lockFile,
      ),
      right: withSharedLock(
        makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "normal-node-b.log"),
        }),
        lockFile,
      ),
    });

    expect(result.winnerNodeId).not.toBe(result.loserNodeId);
    expect(result.winner.attempts[0]?.outputTermination?.marker).toBe(
      SUBMITTED_MARKER,
    );
    expect(result.loser.attempts[0]?.outputTermination?.marker).toBe(
      "pipeline_trace phase=candidate_invalidated reason=T2",
    );
  });

  it("accepts the database single-active-journal refusal as the T7 loser's evidence", async () => {
    const dir = await makeTempDir();
    const lockFile = join(dir, "normal-journal-contention.lock");
    const script = await writeScript(
      dir,
      "normal-journal-contention-node.mjs",
      lockRaceScript(
        `  console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'cc'.repeat(32));`,
        [
          "  console.error('Refusing to prepare a new pending block while another active pending-finalization record exists');",
          "  setTimeout(() => console.log('pipeline_trace phase=candidate_invalidated reason=T7 state=Invalidated'), 20);",
          "  setInterval(() => {}, 1000);",
        ].join("\n"),
      ),
    );
    const result = await runPipelinedCommitNormalLeaseContention({
      left: withSharedLock(
        makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "journal-node-a.log"),
        }),
        lockFile,
      ),
      right: withSharedLock(
        makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "journal-node-b.log"),
        }),
        lockFile,
      ),
    });

    expect(result.winnerNodeId).not.toBe(result.loserNodeId);
    expect(result.winner.attempts[0]?.outputTermination?.marker).toBe(
      SUBMITTED_MARKER,
    );
    expect(result.loser.attempts[0]?.outputTermination?.marker).toBe(
      "pipeline_trace phase=candidate_invalidated reason=T7",
    );
  });

  it("refuses a normal-contention run in which nobody lost", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "both-submit.mjs",
      `console.log('${SUBMITTED_MARKER} submitted_header_hash=' + 'dd'.repeat(32));\nsetInterval(() => {}, 1000);\n`,
    );

    await expect(
      runPipelinedCommitNormalLeaseContention({
        left: makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "both-node-a.log"),
        }),
        right: makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "both-node-b.log"),
        }),
      }),
    ).rejects.toThrow(
      "Expected one submitted winner and one invalidated loser; observed submitted=2,invalidated=0",
    );
  });

  /**
   * The spec guard runs before any process is spawned: two nodes that do not
   * actually contend would make the gate above pass for the wrong reason.
   * Each row keeps every other requirement satisfied.
   */
  it.each([
    {
      name: "sharing an MPF store",
      mutate: (
        right: PipelinedCommitNodeProcessSpec,
        left: PipelinedCommitNodeProcessSpec,
      ) => ({ ...right, ledgerMpfDbPath: left.ledgerMpfDbPath }),
      message: "Lease-contention nodes must use distinct MPF store paths",
    },
    {
      name: "pointing at different Postgres instances",
      mutate: (right: PipelinedCommitNodeProcessSpec) => ({
        ...right,
        postgresIdentity: "other-test-postgres",
      }),
      message: "Lease-contention nodes must use the same Postgres identity",
    },
    {
      name: "leaving a supervised process unbounded",
      mutate: (right: PipelinedCommitNodeProcessSpec) => ({
        ...right,
        process: { ...right.process, timeoutMs: undefined },
      }),
      message:
        "Lease-contention node specs require bounded timeouts longer than the test lease TTL",
    },
    {
      name: "timing out before two lease TTLs have elapsed",
      mutate: (right: PipelinedCommitNodeProcessSpec) => ({
        ...right,
        process: {
          ...right.process,
          timeoutMs: right.stateQueueMutationLeaseTtlMs * 2,
        },
      }),
      message:
        "Lease-contention timeout for node-b must exceed twice its positive test lease TTL",
    },
  ])("rejects contention specs $name", async ({ mutate, message }) => {
    const dir = await makeTempDir();
    const script = await writeScript(dir, "idle.mjs", "process.exit(0);\n");
    const left = makeNodeSpec({
      nodeId: "node-a",
      script,
      rawLogPath: join(dir, "node-a.log"),
    });
    const rightBase = makeNodeSpec({
      nodeId: "node-b",
      script,
      rawLogPath: join(dir, "node-b.log"),
    });

    await expect(
      runPipelinedCommitLeaseContention({
        left,
        right: mutate(rightBase, left),
        armFile: join(dir, "arms", "invalid.arm"),
      }),
    ).rejects.toThrow(message);
  });
});
