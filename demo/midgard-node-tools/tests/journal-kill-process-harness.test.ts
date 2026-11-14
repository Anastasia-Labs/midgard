import "node:fs/promises";
import "node:path";
import "node:url";
import "@al-ft/midgard-test-support/temp-files";
import "vitest";
import "../src/e2e/journal-kill-process-harness.js";
import "./journal-kill-process-harness.make-temp-dir.js";

import { access, readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeScript } from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import {
  JOURNAL_KILL_CHECKPOINT_MARKER,
  JOURNAL_KILL_SURVIVOR_MARKERS,
  type JournalKillNodeProcessSpec,
  markersAppearInOrder,
  runJournalKillContention,
} from "../src/e2e/journal-kill-process-harness.js";
import {
  armRaceScript,
  logLines,
  makeNodeSpec,
  makeTempDir,
} from "./journal-kill-process-harness.make-temp-dir.js";

const [LEASE_BUSY, ABANDON_UNSUBMITTED, BLOCK_SUBMITTED] =
  JOURNAL_KILL_SURVIVOR_MARKERS;

const winnerLines = [
  `  console.log(${JSON.stringify(JOURNAL_KILL_CHECKPOINT_MARKER)} + ' pid=' + process.pid);`,
  "  setInterval(() => {}, 1000);",
].join("\n");

/** Survivor stubs stay alive until the harness writes their stop file. */
const survivorLines = (lines: readonly string[]): string =>
  [logLines(lines), "  setInterval(() => {}, 1000);"].join("\n");

const runPair = async (dir: string, script: string) =>
  runJournalKillContention({
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

const nodeSourceRoot = fileURLToPath(
  new URL("../../midgard-node/src", import.meta.url),
);

const readNodeSources = async (): Promise<string> => {
  const entries = await readdir(nodeSourceRoot, { recursive: true });
  const sources = await Promise.all(
    entries
      .filter((entry) => entry.endsWith(".ts"))
      .map((entry) => readFile(join(nodeSourceRoot, entry), "utf8")),
  );
  return sources.join("\n");
};

describe("journal-kill real-process harness", () => {
  it("classes the arm-file winner as the SIGKILLed journal holder and the other as the survivor", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "contention-node.mjs",
      armRaceScript(
        winnerLines,
        survivorLines([LEASE_BUSY, ABANDON_UNSUBMITTED, BLOCK_SUBMITTED]),
      ),
    );
    const result = await runPair(dir, script);

    expect(["node-a", "node-b"]).toContain(result.winnerNodeId);
    expect(result.loserNodeId).not.toBe(result.winnerNodeId);
    // The classification is by *how each process was terminated*, so the
    // returned winner must be the one the harness SIGKILLed at the journal
    // checkpoint, and the survivor must have been stopped by its stop file
    // after it logged the block submission.
    expect(result.winner.attempts[0]?.signal).toBe("SIGKILL");
    expect(result.winner.attempts[0]?.outputTermination?.marker).toBe(
      JOURNAL_KILL_CHECKPOINT_MARKER,
    );
    expect(result.loser.attempts[0]?.outputTermination).toBeNull();
    expect(result.loser.attempts[0]?.fileTermination?.signal).toBe("SIGTERM");
    // The returned logs belong to the processes they are attributed to.
    expect(result.winnerLog).toContain(JOURNAL_KILL_CHECKPOINT_MARKER);
    expect(result.loserLog).not.toContain(JOURNAL_KILL_CHECKPOINT_MARKER);
    expect(result.loserLog).toContain(BLOCK_SUBMITTED);
  });

  it.each([
    {
      name: "never found the lease busy",
      lines: [ABANDON_UNSUBMITTED, BLOCK_SUBMITTED],
      message: `Journal-kill survivor log is missing: ${LEASE_BUSY}`,
    },
    {
      name: "never recovered the unsubmitted journal",
      lines: [LEASE_BUSY, BLOCK_SUBMITTED],
      message: `Journal-kill survivor log is missing: ${ABANDON_UNSUBMITTED}`,
    },
    {
      name: "submitted before recovering the unsubmitted journal",
      lines: [LEASE_BUSY, BLOCK_SUBMITTED, ABANDON_UNSUBMITTED],
      message:
        "Journal-kill survivor logged lease-busy, unsubmitted-journal recovery and submission out of order",
    },
  ])("refuses a result whose survivor $name", async ({ lines, message }) => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "survivor.mjs",
      armRaceScript(winnerLines, survivorLines(lines)),
    );
    await expect(runPair(dir, script)).rejects.toThrow(message);
  });

  it("refuses a run in which no process reached the journal checkpoint", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "never-journals.mjs",
      survivorLines([LEASE_BUSY, ABANDON_UNSUBMITTED, BLOCK_SUBMITTED]),
    );
    await expect(runPair(dir, script)).rejects.toThrow(
      "Expected one journal winner to be SIGKILLed; observed 0",
    );
  });

  it("refuses to re-arm over an existing arm file", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(dir, "idle.mjs", "process.exit(0);\n");
    const armFile = join(dir, "arms", "journal.arm");
    await runJournalKillContention({
      left: makeNodeSpec({
        nodeId: "node-a",
        script,
        rawLogPath: join(dir, "first-a.log"),
      }),
      right: makeNodeSpec({
        nodeId: "node-b",
        script,
        rawLogPath: join(dir, "first-b.log"),
      }),
      armFile,
    }).catch(() => undefined);
    // Neither stub consumed the arm, so it is still there, and the next run
    // must refuse rather than reuse a crash a previous run already claimed.
    await access(armFile);
    await expect(
      runJournalKillContention({
        left: makeNodeSpec({
          nodeId: "node-a",
          script,
          rawLogPath: join(dir, "second-a.log"),
        }),
        right: makeNodeSpec({
          nodeId: "node-b",
          script,
          rawLogPath: join(dir, "second-b.log"),
        }),
        armFile,
      }),
    ).rejects.toMatchObject({ code: "EEXIST" });
  });

  it("checks survivor markers in order, not merely for presence", () => {
    expect(
      markersAppearInOrder(
        JOURNAL_KILL_SURVIVOR_MARKERS.join("\n"),
        JOURNAL_KILL_SURVIVOR_MARKERS,
      ),
    ).toBe(true);
    expect(
      markersAppearInOrder(
        [...JOURNAL_KILL_SURVIVOR_MARKERS].reverse().join("\n"),
        JOURNAL_KILL_SURVIVOR_MARKERS,
      ),
    ).toBe(false);
  });

  it("uses markers the node actually logs and the offline verifier also requires", async () => {
    const sources = await readNodeSources();
    for (const marker of JOURNAL_KILL_SURVIVOR_MARKERS) {
      expect(sources, marker).toContain(marker);
    }
    expect(sources).toContain('"journal_prepared_before_submit"');

    const verifierHelper = new URL(
      "../scripts/verify-phase4-journal-kill-recovery-summary.validate-cleanup.mjs",
      import.meta.url,
    ).href;
    const verifier = (await import(verifierHelper)) as {
      readonly JOURNAL_KILL_SURVIVOR_MARKERS: readonly string[];
      readonly JOURNAL_KILL_CHECKPOINT_MARKER: string;
    };
    expect(verifier.JOURNAL_KILL_SURVIVOR_MARKERS).toEqual([
      ...JOURNAL_KILL_SURVIVOR_MARKERS,
    ]);
    expect(verifier.JOURNAL_KILL_CHECKPOINT_MARKER).toBe(
      JOURNAL_KILL_CHECKPOINT_MARKER,
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
        right: JournalKillNodeProcessSpec,
        left: JournalKillNodeProcessSpec,
      ) => ({ ...right, ledgerMpfDbPath: left.ledgerMpfDbPath }),
      message: "Lease-contention nodes must use distinct MPF store paths",
    },
    {
      name: "pointing at different Postgres instances",
      mutate: (right: JournalKillNodeProcessSpec) => ({
        ...right,
        postgresIdentity: "other-test-postgres",
      }),
      message: "Lease-contention nodes must use the same Postgres identity",
    },
    {
      name: "leaving a supervised process unbounded",
      mutate: (right: JournalKillNodeProcessSpec) => ({
        ...right,
        process: { ...right.process, timeoutMs: undefined },
      }),
      message:
        "Lease-contention node specs require bounded timeouts longer than the test lease TTL",
    },
    {
      name: "timing out before two lease TTLs have elapsed",
      mutate: (right: JournalKillNodeProcessSpec) => ({
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
      runJournalKillContention({
        left,
        right: mutate(rightBase, left),
        armFile: join(dir, "arms", "invalid.arm"),
      }),
    ).rejects.toThrow(message);
  });
});
