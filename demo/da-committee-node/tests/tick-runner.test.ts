import { describe, expect, it } from "vitest";

import type {
  CommitteeL1View,
  CommitteeRetentionReadinessSnapshot,
  CommitteeTickResult,
} from "../src/committee-service.js";
import {
  createCommitteeTickRunner,
  L1_VIEW_UNAVAILABLE_EXIT_CODE,
  L1ViewUnavailableError,
} from "../src/tick-runner.js";

const FATAL_MS = 60_000;
const START = 1_000_000;

const emptyTick = (errors: readonly string[] = []): CommitteeTickResult => ({
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors,
});

const harness = (
  options: {
    /** Scan state queue, then never settle. */
    readonly hangAfterView?: boolean;
    /** Never settle, without reading L1. */
    readonly hangBeforeView?: boolean;
    readonly shutdown?: () => Promise<void>;
  } = {},
) => {
  let now = START;
  let view: CommitteeL1View | undefined;
  let l1Readable = true;
  let catchingUp = false;
  let progressAt: number | undefined;
  let hangs = false;
  const exits: number[] = [];
  const lines: string[] = [];
  const retentionRuns: number[] = [];
  let shutdowns = 0;
  let readiness: CommitteeRetentionReadinessSnapshot | undefined;
  const runner = createCommitteeTickRunner({
    tick: async () => {
      if (hangs && options.hangBeforeView === true)
        return new Promise<never>(() => undefined);
      if (catchingUp) {
        // Authenticated progress toward a view, but no view.
        progressAt = now;
        throw new Error("state-queue replay is catching up");
      }
      if (!l1Readable) throw new Error("ogmios connection refused");
      view = {
        observedAtMs: now,
        confirmedHeadHash: "aa".repeat(28),
        liveQueueHeaderHashes: new Set(["bb".repeat(28)]),
      };
      if (hangs && options.hangAfterView === true)
        return new Promise<never>(() => undefined);
      return emptyTick();
    },
    runAvailabilityResponse: async () => undefined,
    runRetention: async () => {
      retentionRuns.push(now);
      readiness = {
        status: "ok",
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
      };
    },
    latestL1View: () => view,
    latestL1ProgressAtMs: () => progressAt,
    setRetentionReadiness: (snapshot) => {
      readiness = snapshot;
    },
    l1ViewFatalMs: FATAL_MS,
    startedAtMs: START,
    nowMs: () => now,
    write: (_stream, line) => lines.push(line),
    shutdown: async () => {
      shutdowns += 1;
      await options.shutdown?.();
    },
    exit: (code) => exits.push(code),
    shutdownGraceMs: 10,
  });
  return {
    runner,
    advance: (ms: number) => {
      now += ms;
    },
    setL1Readable: (value: boolean) => {
      l1Readable = value;
    },
    setCatchingUp: (value: boolean) => {
      catchingUp = value;
    },
    startHanging: () => {
      hangs = true;
    },
    exits,
    lines,
    retentionRuns,
    shutdowns: () => shutdowns,
    readiness: () => readiness,
  };
};

describe("committee tick runner L1-view exit rule", () => {
  it("runs retention only against a view accepted this tick", async () => {
    const h = harness();
    await h.runner.runTick();
    expect(h.retentionRuns).toEqual([START]);
    expect(h.readiness()?.status).toBe("ok");
  });

  it("skips retention and reports l1_view_stale until the deadline, then exits 70 once", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);

    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.exits).toEqual([]);
    expect(h.retentionRuns).toEqual([START]);
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: FATAL_MS,
    });
    const skipped = h.lines
      .filter((line) => line.includes("retention_pass_skipped"))
      .map((line) => JSON.parse(line) as Record<string, unknown>);
    expect(skipped).toEqual([
      expect.objectContaining({
        l1ViewAgeMs: FATAL_MS,
        error: "ogmios connection refused",
      }),
    ]);

    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([L1_VIEW_UNAVAILABLE_EXIT_CODE]);
    expect(L1_VIEW_UNAVAILABLE_EXIT_CODE).toBe(70);
    expect(h.shutdowns()).toBe(1);

    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
    expect(h.shutdowns()).toBe(1);
  });

  it("recovers when a fresh view arrives before the deadline", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(FATAL_MS - 1);
    await h.runner.runTick();
    expect(h.readiness()?.status).toBe("l1_view_stale");
    h.setL1Readable(true);
    h.advance(FATAL_MS - 1);
    await h.runner.runTick();
    expect(h.readiness()?.status).toBe("ok");
    expect(h.exits).toEqual([]);
  });

  it("counts catch-up progress toward a view against the deadline, and exits once it stops", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setCatchingUp(true);
    // Catching up for several deadlines' worth of time, progressing on
    // every tick: no view is accepted, nothing is pruned, and no exit.
    for (let tick = 0; tick < 5; tick += 1) {
      h.advance(FATAL_MS);
      await h.runner.runTick();
      expect(h.exits).toEqual([]);
      expect(h.readiness()?.status).toBe("l1_view_stale");
    }
    expect(h.retentionRuns).toEqual([START]);
    // Progress stops: the deadline runs from the last progress.
    h.setCatchingUp(false);
    h.setL1Readable(false);
    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.exits).toEqual([]);
    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([L1_VIEW_UNAVAILABLE_EXIT_CODE]);
  });

  it("measures the deadline from startup when no view was ever accepted", async () => {
    const h = harness();
    h.setL1Readable(false);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: 0,
    });
    h.advance(FATAL_MS + 1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
  });

  it("applies the deadline while a hung tick that never read L1 is still running", async () => {
    const h = harness({ hangBeforeView: true });
    await h.runner.runTick();
    h.startHanging();
    h.advance(1_000);
    // Never awaited: this tick hangs forever.
    void h.runner.runTick();
    h.advance(FATAL_MS - 1_000);
    await h.runner.runTick();
    expect(h.exits).toEqual([]);
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: FATAL_MS,
    });
    expect(
      h.lines.some((line) => line.includes('"reason":"tick_in_flight"')),
    ).toBe(true);
    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
    expect(h.shutdowns()).toBe(1);
    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
  });

  it("does not report a slow tick that already read L1 as stale, but still exits past the deadline", async () => {
    const h = harness({ hangAfterView: true });
    await h.runner.runTick();
    h.startHanging();
    h.advance(1_000);
    void h.runner.runTick();
    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.exits).toEqual([]);
    expect(h.readiness()?.status).toBe("ok");
    h.advance(1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
  });

  it("exits even when the shutdown hangs", async () => {
    const h = harness({
      hangBeforeView: true,
      shutdown: () => new Promise<never>(() => undefined),
    });
    h.startHanging();
    void h.runner.runTick();
    h.advance(FATAL_MS + 1);
    await h.runner.runTick();
    expect(h.exits).toEqual([70]);
  });

  it("throws the typed error from the single-pass retention step", async () => {
    const h = harness();
    h.advance(FATAL_MS + 1);
    await expect(
      h.runner.runRetentionStep(START + FATAL_MS + 1),
    ).rejects.toBeInstanceOf(L1ViewUnavailableError);
  });
});

describe("slow committee tick log", () => {
  const slowTickRunner = (tickMs: () => number) => {
    let now = START;
    const lines: string[] = [];
    const runner = createCommitteeTickRunner({
      tick: async () => {
        now += tickMs();
        return emptyTick();
      },
      runAvailabilityResponse: async () => {
        now += 5;
      },
      runRetention: async () => {
        now += 7;
      },
      latestL1View: () => ({
        observedAtMs: now,
        confirmedHeadHash: "aa".repeat(28),
        liveQueueHeaderHashes: new Set(),
      }),
      latestL1ProgressAtMs: () => undefined,
      setRetentionReadiness: () => undefined,
      l1ViewFatalMs: FATAL_MS,
      startedAtMs: START,
      nowMs: () => now,
      write: (_stream, line) => lines.push(line),
      shutdown: async () => undefined,
      exit: () => undefined,
      slowTickMs: 15_000,
    });
    return { runner, lines };
  };

  it("logs nothing for a tick within the poll interval", async () => {
    const { runner, lines } = slowTickRunner(() => 14_000);
    await runner.runTick();
    expect(lines).toEqual([]);
  });

  it("logs one line with its duration and phases for a tick longer than the poll interval", async () => {
    const { runner, lines } = slowTickRunner(() => 20_000);
    await runner.runTick();
    expect(lines.map((line) => JSON.parse(line) as unknown)).toEqual([
      {
        event: "committee_tick_slow",
        durationMs: 20_012,
        slowTickMs: 15_000,
        tickMs: 20_000,
        availabilityResponseMs: 5,
        retentionMs: 7,
      },
    ]);
  });
});
