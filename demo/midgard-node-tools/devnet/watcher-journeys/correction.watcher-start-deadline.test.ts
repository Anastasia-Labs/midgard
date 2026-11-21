import { describe, expect, it } from "vitest";

import { pollChainCorrection } from "./correction.chain-progress.test-support.js";
import type { JourneyWatcherStartStatus } from "./correction.watcher-start-deadline.js";

const headerHash = "34".repeat(28);
const category = "validationTraceDispute";
/** Nothing of ours has landed yet. */
const started: readonly string[] = [];
/** The workflow's first transaction has landed. */
const signed = ["ab".repeat(32)];

/** A stack watcher's queued job for the journey's header. */
const job = {
  mode: "run",
  category,
  headerHash,
  decisionDigest: "56".repeat(32),
  rollbackGeneration: "0",
  deadline: {
    headerHash,
    headerBlockTimeMs: "1794300000000",
    maturityAtMs: "1794300900000",
    latestSafeStartAtMs: "1794300450000",
  },
};
/** A stack watcher's `/v1/status` with these supervisor fields. */
const status = (
  supervisor: Partial<NonNullable<JourneyWatcherStartStatus["supervisor"]>>,
): JourneyWatcherStartStatus => ({
  liveness: "live",
  readiness: "not_ready",
  readinessReasons: ["deadline_unsafe"],
  supervisor: {
    phase: "accepting",
    recovered: true,
    queuedJobCount: 1,
    activeJob: null,
    earliestDeadlineJob: job,
    deadlineHealth: "unsafe",
    remainingSafeStartMs: "-1",
    journalDecisionMissing: [],
    ...supervisor,
  },
});
/** The watcher's own hold of the journey's objective past its safe start. */
const held = status({
  queuedJobCount: 0,
  earliestDeadlineJob: null,
  deadlineHealth: "safe",
  remainingSafeStartMs: null,
  journalDecisionMissing: [
    {
      kind: "objective",
      category,
      headerHash,
      decisionDigest: "56".repeat(32),
      detail: `${category}/${headerHash}`,
      readiness: "fault_proof_start_deadline_passed",
    },
  ],
});

/** The proof wait over these observations, each one poll; counts status reads. */
const proofWait = async (
  observations: readonly {
    landed: readonly string[];
    watcher: JourneyWatcherStartStatus;
  }[],
) =>
  (
    await pollChainCorrection(
      observations.map((observation, index) => ({ at: index, ...observation })),
    )
  ).statusReads;

const observationsEnded = /: observations ended$/u;

describe("the proof wait and the watcher's latest safe start", () => {
  it("fails at once, naming it, when the watcher holds the objective past its safe start", async () => {
    await expect(
      proofWait([{ landed: started, watcher: held }]),
    ).rejects.toThrow(
      `fault_proof_start_deadline_passed for ${category}/${headerHash}`,
    );
  });

  it("fails at once when the safe start passed with the job queued and nothing signed", async () => {
    await expect(
      proofWait([{ landed: started, watcher: status({}) }]),
    ).rejects.toThrow(/latest safe start.*remainingSafeStartMs -1/su);
  });

  it("keeps waiting on a job that is already running when its safe start passes", async () => {
    await expect(
      proofWait([
        { landed: started, watcher: status({ activeJob: job }) },
        { landed: started, watcher: status({ activeJob: job }) },
      ]),
    ).rejects.toThrow(observationsEnded);
  });

  it("keeps waiting once a transaction of ours has landed", async () => {
    await expect(
      proofWait([
        { landed: signed, watcher: held },
        { landed: signed, watcher: status({}) },
      ]),
    ).rejects.toThrow(observationsEnded);
  });

  it("keeps waiting while a restarted watcher answers its startup status", async () => {
    const startup = {
      liveness: "live",
      readiness: "not_ready",
      readinessReasons: ["startup_in_progress"],
      startup: { phase: "opening_journal" },
    } as unknown as JourneyWatcherStartStatus;
    await expect(
      proofWait([
        { landed: started, watcher: startup },
        { landed: started, watcher: startup },
      ]),
    ).rejects.toThrow(observationsEnded);
  });

  it("ignores another header's hold and a safe start that has not passed", async () => {
    const other = "9a".repeat(28);
    await expect(
      proofWait([
        {
          landed: started,
          watcher: status({
            earliestDeadlineJob: { ...job, headerHash: other },
            journalDecisionMissing: [
              {
                kind: "objective",
                category,
                headerHash: other,
                decisionDigest: "56".repeat(32),
                detail: `${category}/${other}`,
                readiness: "fault_proof_start_deadline_passed",
              },
            ],
          }),
        },
        {
          landed: started,
          watcher: status({
            deadlineHealth: "safe",
            remainingSafeStartMs: "1",
          }),
        },
      ]),
    ).rejects.toThrow(observationsEnded);
  });
});
