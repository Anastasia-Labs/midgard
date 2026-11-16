/**
 * A header this watcher found faulty merged into confirmed state before any
 * proof removed it: the invalid block is now confirmed state. The watcher
 * raises `unproven_fault_merged`, records the merge durably, and raises the
 * alert again after every restart, though the header has left the queue.
 */
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherPersistedFaultDecisionRecord } from "../../src/fault-proofs/fault-decision-journal.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherUnprovenFaultMerges } from "../../src/fault-proofs/watcher-unproven-fault-merges.js";
import { watcherUnprovenFaultMergedAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  decision,
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  MERGE_TX,
  operations,
  proof,
} from "./fault-decision-bridge.released-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await removeJournalDirectories();
});

const queue = (...suffixes: string[]) =>
  observation(
    suffixes.map((suffix) => headerFixture(suffix)),
    "Idle",
    suffixes.map(() => ATTESTED),
  );

const merges = (journalRoot: string) =>
  watcherUnprovenFaultMerges(() =>
    openWatcherJournalDatabase({
      journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    }),
  );

const unprovenAlerts = (observability: ReturnType<typeof operations>) =>
  observability.api
    .status()
    .activeAlerts.filter(({ code }) => code === "unproven_fault_merged")
    .map(({ subjectDigest }) => subjectDigest);

/** When, in seconds, `/metrics` says the newest unproven merge was recorded;
 * undefined while the watcher knows of none. */
const latestMergeSeconds = async (
  observability: ReturnType<typeof operations>,
) => {
  const response = await observability.handleHttpRequest(
    new Request("http://127.0.0.1/metrics"),
  );
  const sample = (await response.text())
    .split("\n")
    .find((line) =>
      line.startsWith(
        "midgard_watcher_latest_unproven_fault_merge_timestamp_seconds ",
      ),
    );
  return sample === undefined ? undefined : Number(sample.split(" ")[1]);
};

/** The error lines a callback writes. */
const errorLines = async (run: () => Promise<unknown>) => {
  const logged: string[] = [];
  const stderr = vi
    .spyOn(process.stderr, "write")
    .mockImplementation((line) => logged.push(String(line)) > 0);
  try {
    await run();
  } finally {
    stderr.mockRestore();
  }
  return logged.filter((line) => line.includes("unproven_fault_merged"));
};

describe("fault decision bridge when a detected fault merges unproven", () => {
  it("raises a durable alert for a header it found faulty, and raises it again after a restart once the header left the queue", async () => {
    const journalRoot = await journalDirectory("midgard-unproven-merge");
    const current = queue("01", "02");
    const [faulty, live] = current.finalizedHeaders;
    const mergedHeaders = vi
      .fn()
      .mockResolvedValueOnce(new Map())
      .mockResolvedValue(
        new Map([[faulty!.headerHash, proof(faulty!.headerHash)]]),
      );
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [faulty!.headerHash]: "doubleSpend",
        [live!.headerHash]: "transitionTrace",
      },
      classifyOverride: (fresh) =>
        fresh.headerHash === live!.headerHash
          ? { ...fresh, decision: "healthy" as const }
          : fresh,
      operationsSink: observability.sink,
      mergedHeaders,
      unprovenFaultMerges: () => merges(journalRoot),
    });
    // Found faulty while it waits in the queue: no alert yet.
    await h.bridge.reconcileAndDispatch(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(unprovenAlerts(observability)).toEqual([]);
    // It merges at release finality, unproven.
    const subject = watcherUnprovenFaultMergedAlertSubject(faulty!.headerHash);
    const lines = await errorLines(() =>
      h.bridge.reconcileAndDispatch(current),
    );
    expect(unprovenAlerts(observability)).toEqual([subject]);
    expect(merges(journalRoot).list()).toEqual([
      {
        headerHash: faulty!.headerHash,
        category: "doubleSpend",
        decisionDigest: decision(faulty!.headerHash, "doubleSpend")
          .decisionDigest,
        mergeTransactionHash: MERGE_TX,
        observedAtMs: expect.stringMatching(/^\d+$/u),
      },
    ]);
    expect(lines).toEqual([expect.stringContaining(faulty!.headerHash)]);
    // Informational: nothing acknowledges it, so it never holds readiness.
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    // A later pass over the same merge writes no second line.
    expect(
      await errorLines(() => h.bridge.reconcileAndDispatch(current)),
    ).toEqual([]);

    // A restart: the header has left the queue and the decision journal
    // holds nothing for it, yet the alert is raised again from the record.
    closeWatcherJournalDatabase(journalRoot);
    const later = queue("02");
    const restarted = operations();
    const again = harness({
      current: later,
      categoryByHeader: { [live!.headerHash]: "transitionTrace" },
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
      operationsSink: restarted.sink,
      mergedHeaders: async () => new Map(),
      unprovenFaultMerges: () => merges(journalRoot),
    });
    expect(unprovenAlerts(restarted)).toEqual([]);
    expect(
      await errorLines(() => again.bridge.reconcileAndDispatch(later)),
    ).toEqual([]);
    expect(unprovenAlerts(restarted)).toEqual([subject]);
  });

  it("raises it for a header found faulty before a restart that merged while the watcher was down", async () => {
    const journalRoot = await journalDirectory("midgard-unproven-merge");
    const current = queue("01");
    const [faulty] = current.finalizedHeaders;
    const found = decision(faulty!.headerHash, "doubleSpend");
    const records: WatcherPersistedFaultDecisionRecord[] = [
      Object.freeze({
        schemaVersion: "midgard-watcher-production-fault-decision-record-v1",
        revision: "0",
        priorRecordSha256: null,
        decision: found,
      }),
    ];
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      records,
      operationsSink: observability.sink,
      mergedHeaders: async () =>
        new Map([[faulty!.headerHash, proof(faulty!.headerHash)]]),
      unprovenFaultMerges: () => merges(journalRoot),
    });
    await errorLines(() => h.bridge.reconcileAndDispatch(current));
    expect(h.application.classifyHeader).not.toHaveBeenCalled();
    expect(unprovenAlerts(observability)).toEqual([
      watcherUnprovenFaultMergedAlertSubject(faulty!.headerHash),
    ]);
    expect(merges(journalRoot).list()).toMatchObject([
      {
        headerHash: faulty!.headerHash,
        decisionDigest: found.decisionDigest,
      },
    ]);
  });

  it("exports when the newest unproven merge was recorded, so each new one pages, and keeps that time across a restart", async () => {
    const journalRoot = await journalDirectory("midgard-unproven-merge");
    const current = queue("01", "02");
    const [first, second] = current.finalizedHeaders;
    const mergedHeaders = vi
      .fn()
      .mockResolvedValueOnce(new Map())
      .mockResolvedValueOnce(
        new Map([[first!.headerHash, proof(first!.headerHash)]]),
      )
      .mockResolvedValue(
        new Map([
          [first!.headerHash, proof(first!.headerHash)],
          [second!.headerHash, proof(second!.headerHash)],
        ]),
      );
    let clock = BigInt(Date.now());
    const nowMs = () => clock;
    const observability = operations();
    const h = harness({
      current,
      nowMs,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "doubleSpend",
      },
      operationsSink: observability.sink,
      mergedHeaders,
      unprovenFaultMerges: () => merges(journalRoot),
    });
    const seen: (number | undefined)[] = [];
    const passes: bigint[] = [];
    await errorLines(async () => {
      for (let pass = 0; pass < 4; pass += 1) {
        clock += 60_000n;
        passes.push(clock);
        await h.bridge.reconcileAndDispatch(current);
        seen.push(await latestMergeSeconds(observability));
      }
    });
    // Both alerts stay active; the time moves with each new incident and
    // not with a pass over a known one.
    const seconds = (ms: bigint) => Number(ms) / 1000;
    expect(seen).toEqual([
      undefined,
      seconds(passes[1]!),
      seconds(passes[2]!),
      seconds(passes[2]!),
    ]);
    expect(unprovenAlerts(observability)).toHaveLength(2);

    // A restart hours later does not move it: no new incident, so a rule on
    // its age stays quiet.
    closeWatcherJournalDatabase(journalRoot);
    clock += 2n * 3_600_000n;
    const later = queue("03");
    const restarted = operations();
    const again = harness({
      current: later,
      categoryByHeader: {
        [later.finalizedHeaders[0]!.headerHash]: "transitionTrace",
      },
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
      operationsSink: restarted.sink,
      nowMs,
      mergedHeaders: async () => new Map(),
      unprovenFaultMerges: () => merges(journalRoot),
    });
    // Unknown until read: no sample rather than a fresh time.
    expect(await latestMergeSeconds(restarted)).toBeUndefined();
    await again.bridge.reconcileAndDispatch(later);
    expect(await latestMergeSeconds(restarted)).toBe(seconds(passes[2]!));
    expect(unprovenAlerts(restarted)).toHaveLength(2);
  });

  it("raises nothing when a healthy or an unclassified header merges", async () => {
    const journalRoot = await journalDirectory("midgard-unproven-merge");
    const current = queue("01", "02");
    const [unclassified, healthy] = current.finalizedHeaders;
    const mergedHeaders = vi
      .fn()
      .mockResolvedValueOnce(
        new Map([[unclassified!.headerHash, proof(unclassified!.headerHash)]]),
      )
      .mockResolvedValue(
        new Map([
          [unclassified!.headerHash, proof(unclassified!.headerHash)],
          [healthy!.headerHash, proof(healthy!.headerHash)],
        ]),
      );
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [unclassified!.headerHash]: "doubleSpend",
        [healthy!.headerHash]: "transitionTrace",
      },
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
      operationsSink: observability.sink,
      mergedHeaders,
      unprovenFaultMerges: () => merges(journalRoot),
    });
    await h.bridge.reconcileAndDispatch(current);
    await h.bridge.reconcileAndDispatch(current);
    expect(mergedHeaders).toHaveBeenCalledTimes(2);
    expect(unprovenAlerts(observability)).toEqual([]);
    expect(merges(journalRoot).list()).toEqual([]);
  });
});
