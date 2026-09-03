import { access } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  type PipelinedCommitNodeProcessSpec,
  runPipelinedCommitCheckpointCrash,
  runPipelinedCommitLeaseContention,
  runPipelinedCommitNormalLeaseContention,
} from "../src/e2e/pipelined-commit-process-harness.js";
import {
  createTrackedTempDirFactory,
  writeScript,
} from "./helpers/temp-files.js";

const makeTempDir = createTrackedTempDirFactory(
  "midgard-pipelined-commit-process-",
);

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

describe("pipelined commit real-process harness", () => {
  it("uses a one-shot arm file and records an external checkpoint SIGKILL", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "checkpoint-node.mjs",
      [
        "import { unlinkSync } from 'node:fs';",
        "unlinkSync(process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE);",
        "console.log('pipeline_trace phase=e2e_crash_checkpoint checkpoint=speculative_mid_build');",
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

    expect(summary.attempts[0]?.signal).toBe("SIGKILL");
    expect(summary.attempts[0]?.outputTermination?.marker).toContain(
      "checkpoint=speculative_mid_build",
    );
    await expect(access(armFile)).rejects.toMatchObject({ code: "ENOENT" });
  });

  it("elects exactly one journal winner across two supervised processes", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "contention-node.mjs",
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
        "  console.log('pipeline_trace phase=e2e_crash_checkpoint checkpoint=journal_prepared_before_submit');",
        "  setInterval(() => {}, 1000);",
        "} else {",
        "  console.log('pipeline_trace phase=speculative_submission_deferred reason=state_queue_lease_busy');",
        "  console.log('abandoning unsubmitted journal after lease expiry');",
        "  console.log('pipeline_trace phase=candidate_submitted submitted_header_hash=' + 'bb'.repeat(32));",
        "}",
      ].join("\n"),
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
    expect(result.winnerLog).toContain(
      "checkpoint=journal_prepared_before_submit",
    );
    expect(result.loserLog).toContain("reason=state_queue_lease_busy");
    expect(result.loserLog).toContain("abandoning unsubmitted journal");
    expect(result.loserLog).toContain("phase=candidate_submitted");
  });

  it("records one submitted winner and one Busy-to-T2 loser", async () => {
    const dir = await makeTempDir();
    const lockFile = join(dir, "normal-contention.lock");
    const script = await writeScript(
      dir,
      "normal-contention-node.mjs",
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
        "  console.log('pipeline_trace phase=candidate_submitted submitted_header_hash=' + 'aa'.repeat(32));",
        "} else {",
        "  console.log('pipeline_trace phase=speculative_submission_deferred reason=state_queue_lease_busy');",
        "  setTimeout(() => console.log('pipeline_trace phase=candidate_invalidated reason=T2 state=Invalidated'), 20);",
        "  setInterval(() => {}, 1000);",
        "}",
      ].join("\n"),
    );
    const withSharedLock = (spec: PipelinedCommitNodeProcessSpec) => ({
      ...spec,
      process: {
        ...spec.process,
        env: { ...spec.process.env, SHARED_LOCK_FILE: lockFile },
      },
    });
    const result = await runPipelinedCommitNormalLeaseContention({
      left: withSharedLock(
        makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "normal-node-a.log"),
        }),
      ),
      right: withSharedLock(
        makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "normal-node-b.log"),
        }),
      ),
    });

    expect(result.winnerNodeId).not.toBe(result.loserNodeId);
    expect(result.winnerLog).toContain("phase=candidate_submitted");
    expect(result.loserLog).toContain("reason=state_queue_lease_busy");
    expect(result.loserLog).toContain("reason=T2");
  });

  it("accepts the database single-active-journal guard as the contention loser", async () => {
    const dir = await makeTempDir();
    const lockFile = join(dir, "normal-journal-contention.lock");
    const script = await writeScript(
      dir,
      "normal-journal-contention-node.mjs",
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
        "  console.log('pipeline_trace phase=candidate_submitted submitted_header_hash=' + 'cc'.repeat(32));",
        "} else {",
        "  console.error('Refusing to prepare a new pending block while another active pending-finalization record exists');",
        "  setTimeout(() => console.log('pipeline_trace phase=candidate_invalidated reason=T7 state=Invalidated'), 20);",
        "  setInterval(() => {}, 1000);",
        "}",
      ].join("\n"),
    );
    const withSharedLock = (spec: PipelinedCommitNodeProcessSpec) => ({
      ...spec,
      process: {
        ...spec.process,
        env: { ...spec.process.env, SHARED_LOCK_FILE: lockFile },
      },
    });
    const result = await runPipelinedCommitNormalLeaseContention({
      left: withSharedLock(
        makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "journal-node-a.log"),
        }),
      ),
      right: withSharedLock(
        makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "journal-node-b.log"),
        }),
      ),
    });

    expect(result.winnerNodeId).not.toBe(result.loserNodeId);
    expect(result.winnerLog).toContain("phase=candidate_submitted");
    expect(result.loserLog).toContain(
      "Refusing to prepare a new pending block while another active pending-finalization record exists",
    );
    expect(result.loserLog).toContain("reason=T7");
  });

  it("rejects contention specs that share an MPF store", async () => {
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
        right: { ...rightBase, ledgerMpfDbPath: left.ledgerMpfDbPath },
        armFile: join(dir, "arms", "invalid.arm"),
      }),
    ).rejects.toThrow("distinct MPF store paths");
  });
});
