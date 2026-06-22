import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  REQUIRED_FRESH_E2E_STEP_IDS,
  REQUIRED_FRESH_TRANSACTION_LABELS,
  requiredFreshEvidence,
} from "@/commands/e2e-finalize-summary.js";
import {
  E2E_STEP_SCHEMA_VERSION,
  type StepStatus,
  type StepSummary,
} from "@/e2e/runner.js";
import {
  createE2ERunSummary,
  E2E_SUMMARY_SCHEMA_VERSION,
  renderSummaryMarkdown,
  updateE2ERunSummary,
  writeSummaryJsonAtomic,
  writeSummaryMarkdownAtomic,
} from "@/e2e/summary.js";

let tempDirs: string[] = [];

const makeTempDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-e2e-summary-"));
  tempDirs.push(dir);
  return dir;
};

afterEach(async () => {
  await Promise.all(
    tempDirs.map((dir) => rm(dir, { recursive: true, force: true })),
  );
  tempDirs = [];
});

const step = ({
  id,
  status,
  txHashes = [],
}: {
  readonly id: string;
  readonly status: StepStatus;
  readonly txHashes?: readonly string[];
}): StepSummary => ({
  schemaVersion: E2E_STEP_SCHEMA_VERSION,
  id,
  status,
  command: {
    command: "node",
    args: ["dist/index.js"],
    cwd: "/tmp",
    envKeys: [],
  },
  pid: 123,
  startedAt: "2026-01-01T00:00:00.000Z",
  finishedAt: "2026-01-01T00:00:01.000Z",
  durationMs: 1000,
  exitCode: status === "success" ? 0 : 1,
  signal: null,
  timedOut: status === "timeout",
  rawLogPath: `logs/${id}.log`,
  observedTxHashes: txHashes,
  parsedJson: null,
  error: status === "success" ? null : "failed",
});

describe("e2e run summary", () => {
  it("classifies a fully satisfied run as complete", () => {
    const base = createE2ERunSummary({
      runId: "e2e-run-1",
      mode: "fresh",
      now: new Date("2026-01-01T00:00:00.000Z"),
    });
    const summary = updateE2ERunSummary(
      base,
      {
        steps: [step({ id: "submit-deposit", status: "success" })],
        transactions: [
          {
            label: "l2-transfer-a",
            txHash: "aa".repeat(32),
            status: "committed",
            source: "tx-status",
          },
        ],
        http: [
          {
            label: "readyz",
            method: "GET",
            url: "http://127.0.0.1:3000/readyz",
            statusCode: 200,
            semanticStatus: "satisfied",
            source: "runner",
          },
        ],
        db: [
          {
            label: "finalization_residue",
            status: "satisfied",
            source: "psql",
            details: {},
          },
        ],
      },
      new Date("2026-01-01T00:01:00.000Z"),
    );

    expect(summary.schemaVersion).toBe(E2E_SUMMARY_SCHEMA_VERSION);
    expect(summary.verdict).toBe("success");
    expect(summary.nextSafeAction).toBe("none_run_complete");
  });

  it("routes timeout after submitted tx hash to reconciliation", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-2" }),
      {
        steps: [
          step({
            id: "submit-deposit",
            status: "timeout",
            txHashes: ["bb".repeat(32)],
          }),
        ],
      },
    );

    expect(summary.verdict).toBe("blocked");
    expect(summary.nextSafeAction).toBe("reconcile_submitted_tx_before_rerun");
  });

  it("routes merge blockers to lease inspection", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-3" }),
      {
        steps: [step({ id: "merge", status: "success" })],
        http: [
          {
            label: "merge",
            method: "GET",
            url: "http://127.0.0.1:3000/merge",
            statusCode: 200,
            semanticStatus: "blocked",
            source: "runner",
          },
        ],
      },
    );

    expect(summary.verdict).toBe("blocked");
    expect(summary.nextSafeAction).toBe("inspect_state_queue_lease");
  });

  it("routes pending deposit projection to wait", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-4" }),
      {
        db: [
          {
            label: "deposit_projection",
            status: "pending",
            source: "psql",
            details: { inclusionTime: "2026-01-01T00:10:00.000Z" },
          },
        ],
      },
    );

    expect(summary.verdict).toBe("unknown");
    expect(summary.nextSafeAction).toBe("wait_until_deposit_projection_due");
  });

  it("fails the run when a required evidence gate fails", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-failed-gate" }),
      {
        steps: [step({ id: "readyz", status: "success" })],
        db: [
          {
            label: "required_fresh_steps",
            status: "failed",
            source: "e2e-run-step",
            details: { missing: "init-protocol" },
          },
        ],
      },
    );

    expect(summary.verdict).toBe("failed");
    expect(summary.nextSafeAction).toBe("investigate_unknown");
  });

  it("requires all fresh state-changing steps and tx evidence", () => {
    const missing = requiredFreshEvidence({
      mode: "fresh",
      steps: [step({ id: "collect-final-evidence", status: "success" })],
      transactions: [],
    });

    expect(missing).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_steps",
        status: "failed",
        details: expect.objectContaining({
          missing: REQUIRED_FRESH_E2E_STEP_IDS.join(","),
        }),
      }),
    );
    expect(missing).toContainEqual(
      expect.objectContaining({
        label: "required_transaction_evidence",
        status: "failed",
        details: expect.objectContaining({
          missing: REQUIRED_FRESH_TRANSACTION_LABELS.join(","),
        }),
      }),
    );

    const satisfied = requiredFreshEvidence({
      mode: "fresh",
      steps: REQUIRED_FRESH_E2E_STEP_IDS.map((id) =>
        step({ id, status: "success" }),
      ),
      transactions: REQUIRED_FRESH_TRANSACTION_LABELS.map((label) => ({
        label,
        txHash: "aa".repeat(32),
        status:
          label === "l2-transfer-a" || label === "l2-transfer-b"
            ? "committed"
            : "confirmed",
        source: "test",
      })),
    });

    expect(satisfied.every((entry) => entry.status === "satisfied")).toBe(true);
  });

  it("renders and writes summary artifacts atomically", async () => {
    const dir = await makeTempDir();
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-5", mode: "resume" }),
      {
        steps: [step({ id: "readyz", status: "success" })],
        rawEvidence: [{ label: "node-log", path: "logs/node.log" }],
      },
    );
    const jsonPath = join(dir, "summary.json");
    const markdownPath = join(dir, "summary.md");

    await writeSummaryJsonAtomic(jsonPath, summary);
    await writeSummaryMarkdownAtomic(markdownPath, summary);

    await expect(readFile(jsonPath, "utf8")).resolves.toContain(
      '"schemaVersion": "midgard-e2e-summary-v1"',
    );
    const markdown = await readFile(markdownPath, "utf8");
    expect(markdown).toContain("# Midgard E2E Run Summary");
    expect(markdown).toContain("| readyz | success |");
    expect(renderSummaryMarkdown(summary)).toBe(markdown);
  });
});
