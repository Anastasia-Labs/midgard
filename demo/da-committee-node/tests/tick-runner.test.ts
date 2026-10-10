import {
  afterEach,
  beforeEach,
  describe,
  expect,
  it,
  type MockInstance,
  vi,
} from "vitest";

import type {
  CommitteeL1View,
  CommitteeRetentionReadinessSnapshot,
  CommitteeTickResult,
} from "../src/committee-service.js";
import {
  createCommitteeTickRunner,
  L1ViewUnavailableError,
  startCommitteeTickLoop,
} from "../src/tick-runner.js";

const FATAL_MS = 60_000;
const STALE_MS = 6_000;
const START = 1_000_000;
const PENDING_HEADER = "cc".repeat(28);

const emptyTick = (errors: readonly string[] = []): CommitteeTickResult => ({
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors,
});

/**
 * A runner over a stub service that, like the real one, acts only on a view
 * its own tick accepted: it attests each pending header once, on the first
 * tick that reads L1 after the header appeared.
 */
const harness = (
  options: {
    /** Scan state queue, then hold until released. */
    readonly hangAfterView?: boolean;
    /** Hold until released, without reading L1. */
    readonly hangBeforeView?: boolean;
    readonly readDaBondPool?: () => Promise<void>;
  } = {},
) => {
  let now = START;
  let view: CommitteeL1View | undefined;
  let l1Readable = true;
  let catchingUp = false;
  let progressAt: number | undefined;
  let hangs = false;
  let release: (() => void) | undefined;
  const pendingHeaders = new Set<string>();
  const attestations: string[] = [];
  const lines: string[] = [];
  const retentionRuns: number[] = [];
  let readiness: CommitteeRetentionReadinessSnapshot | undefined;
  const hold = (): Promise<void> =>
    new Promise<void>((resolve) => {
      release = () => {
        hangs = false;
        resolve();
      };
    });
  const runner = createCommitteeTickRunner({
    tick: async () => {
      if (hangs && options.hangBeforeView === true) await hold();
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
        finalBlockTimeMs: null,
      };
      if (hangs && options.hangAfterView === true) await hold();
      for (const header of pendingHeaders) {
        attestations.push(header);
        pendingHeaders.delete(header);
      }
      return emptyTick();
    },
    runAvailabilityResponse: async () => undefined,
    ...(options.readDaBondPool === undefined
      ? {}
      : { readDaBondPool: options.readDaBondPool }),
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
    setRetentionReadiness: (update) => {
      readiness = update(
        readiness ?? {
          status: "not_checked",
          scanned: 0,
          retained: 0,
          prunable: 0,
          alerting: 0,
        },
      );
    },
    l1ViewFatalMs: FATAL_MS,
    l1ViewStaleMs: STALE_MS,
    startedAtMs: START,
    nowMs: () => now,
    write: (_stream, line) => lines.push(line),
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
    release: () => release?.(),
    headerAppears: (header: string) => pendingHeaders.add(header),
    attestations,
    lines,
    events: (name: string) =>
      lines
        .filter((line) => line.includes(`"event":"${name}"`))
        .map((line) => JSON.parse(line) as Record<string, unknown>),
    retentionRuns,
    readiness: () => readiness,
  };
};

const flush = (): Promise<void> =>
  new Promise((resolve) => setImmediate(resolve));

describe("committee tick runner L1-view action gate", () => {
  let exitSpy: MockInstance<typeof process.exit>;
  beforeEach(() => {
    exitSpy = vi.spyOn(process, "exit").mockImplementation(() => {
      throw new Error("the tick runner must never exit the process");
    });
  });
  afterEach(() => {
    expect(exitSpy).not.toHaveBeenCalled();
    exitSpy.mockRestore();
  });

  it("runs retention only against a view accepted this tick", async () => {
    const h = harness();
    await h.runner.runTick();
    expect(h.retentionRuns).toEqual([START]);
    expect(h.readiness()?.status).toBe("ok");
    expect(h.runner.liveness()).toEqual({ consecutiveRetentionSkips: 0 });
  });

  it("stays up past the deadline, refuses every action while the view is stale, and resumes with exactly one attestation", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.headerAppears(PENDING_HEADER);

    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: FATAL_MS,
    });
    expect(h.events("retention_pass_skipped")).toEqual([
      expect.objectContaining({
        l1ViewAgeMs: FATAL_MS,
        consecutiveSkips: 1,
        error: "ogmios connection refused",
      }),
    ]);

    // Past the deadline for several more ticks: still ticking, nothing
    // attested or pruned, one entry event, readiness names the state.
    for (let tick = 0; tick < 4; tick += 1) {
      h.advance(FATAL_MS);
      await h.runner.runTick();
    }
    expect(h.attestations).toEqual([]);
    expect(h.retentionRuns).toEqual([START]);
    expect(h.readiness()?.status).toBe("l1_view_stale");
    expect(h.runner.liveness()).toEqual({
      l1ViewUnavailable: { l1ViewAgeMs: 5 * FATAL_MS, l1ViewFatalMs: FATAL_MS },
      consecutiveRetentionSkips: 5,
    });
    expect(h.events("l1_view_unavailable")).toEqual([
      {
        event: "l1_view_unavailable",
        l1ViewAgeMs: 2 * FATAL_MS,
        l1ViewFatalMs: FATAL_MS,
      },
    ]);

    h.setL1Readable(true);
    h.advance(1_000);
    await h.runner.runTick();
    h.advance(1_000);
    await h.runner.runTick();
    expect(h.attestations).toEqual([PENDING_HEADER]);
    expect(h.retentionRuns).toEqual([
      START,
      START + 5 * FATAL_MS + 1_000,
      START + 5 * FATAL_MS + 2_000,
    ]);
    expect(h.readiness()?.status).toBe("ok");
    expect(h.runner.liveness()).toEqual({ consecutiveRetentionSkips: 0 });
    expect(h.events("l1_view_recovered")).toEqual([
      expect.objectContaining({ unavailableForMs: 3 * FATAL_MS + 1_000 }),
    ]);
  });

  it("recovers before the deadline without ever entering the unavailable state", async () => {
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
    expect(h.events("l1_view_unavailable")).toEqual([]);
  });

  it("counts catch-up progress toward a view against the deadline, and reports unavailable once it stops", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setCatchingUp(true);
    // Catching up for several deadlines' worth of time, progressing on
    // every tick: no view is accepted, nothing is pruned, not unavailable.
    for (let tick = 0; tick < 5; tick += 1) {
      h.advance(FATAL_MS);
      await h.runner.runTick();
      expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
      expect(h.readiness()?.status).toBe("l1_view_stale");
    }
    expect(h.retentionRuns).toEqual([START]);
    // Progress stops: the deadline runs from the last progress.
    h.setCatchingUp(false);
    h.setL1Readable(false);
    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
    h.advance(1);
    await h.runner.runTick();
    expect(h.runner.liveness().l1ViewUnavailable).toEqual({
      l1ViewAgeMs: FATAL_MS + 1,
      l1ViewFatalMs: FATAL_MS,
    });
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
    expect(h.runner.liveness().l1ViewUnavailable?.l1ViewAgeMs).toBe(
      FATAL_MS + 1,
    );
    await h.runner.runTick();
    expect(h.events("l1_view_unavailable")).toHaveLength(1);
  });

  it("reports a hung tick that never read L1 without starting another, and recovers when it settles", async () => {
    const h = harness({ hangBeforeView: true });
    await h.runner.runTick();
    h.startHanging();
    h.advance(1_000);
    const hung = h.runner.runTick();
    h.advance(FATAL_MS - 1_000);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: FATAL_MS,
    });
    expect(h.events("committee_tick_overlap_prevented")).toHaveLength(1);
    expect(h.runner.liveness().tickHung).toBeUndefined();
    h.advance(1_001);
    await h.runner.runTick();
    expect(h.runner.liveness()).toMatchObject({
      l1ViewUnavailable: { l1ViewAgeMs: FATAL_MS + 1_001 },
      tickHung: { inFlightMs: FATAL_MS + 1 },
    });
    // The held tick settles and reads L1: the next state is live again.
    h.release();
    await hung;
    expect(h.runner.liveness()).toEqual({ consecutiveRetentionSkips: 0 });
    expect(h.events("l1_view_recovered")).toHaveLength(1);
  });

  it("does not report a slow tick that already read L1 as stale, and reports it hung past the deadline", async () => {
    const h = harness({ hangAfterView: true });
    await h.runner.runTick();
    h.startHanging();
    h.advance(1_000);
    const hung = h.runner.runTick();
    await flush();
    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.readiness()?.status).toBe("ok");
    expect(h.runner.liveness().tickHung).toBeUndefined();
    h.advance(1);
    expect(h.runner.liveness()).toMatchObject({
      l1ViewUnavailable: { l1ViewAgeMs: FATAL_MS + 1 },
      tickHung: { inFlightMs: FATAL_MS + 1 },
    });
    h.release();
    await hung;
    expect(h.runner.liveness().tickHung).toBeUndefined();
  });

  it("throws the typed error from the single-pass retention step", async () => {
    const h = harness();
    h.advance(FATAL_MS + 1);
    await expect(h.runner.runRetentionStep(undefined)).rejects.toBeInstanceOf(
      L1ViewUnavailableError,
    );
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
        finalBlockTimeMs: null,
      }),
      latestL1ProgressAtMs: () => undefined,
      setRetentionReadiness: () => undefined,
      l1ViewFatalMs: FATAL_MS,
      startedAtMs: START,
      nowMs: () => now,
      write: (_stream, line) => lines.push(line),
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

describe("committee tick runner pooled DA bond read", () => {
  it("reads the pool once per tick, including a tick that could not read the state queue", async () => {
    let reads = 0;
    const h = harness({
      readDaBondPool: async () => {
        reads += 1;
      },
    });
    await h.runner.runTick();
    expect(reads).toBe(1);
    h.setL1Readable(false);
    await h.runner.runTick();
    expect(reads).toBe(2);
  });

  it("logs a failing pool read and fails nothing else: retention still runs against the tick's view", async () => {
    const h = harness({
      readDaBondPool: async () => {
        throw new Error("pool reader broke");
      },
    });
    await h.runner.runTick();
    expect(h.lines).toEqual(["pool reader broke\n"]);
    expect(h.retentionRuns).toEqual([START]);
    expect(h.readiness()?.status).toBe("ok");
  });
});

describe("committee tick loop start", () => {
  /** A loop whose first tick waits until `finishFirstTick` is called. */
  const loopWithHeldFirstTick = () => {
    const handlers = new Map<string, () => void>();
    const calls: string[] = [];
    let finishFirstTick!: () => void;
    const firstTick = new Promise<void>((resolve) => {
      finishFirstTick = resolve;
    });
    const started = startCommitteeTickLoop({
      runTick: async () => {
        calls.push("tick");
        await firstTick;
      },
      pollIntervalMs: 60_000,
      shutdown: async () => {
        calls.push("shutdown");
      },
      exit: (code) => calls.push(`exit:${code.toString()}`),
      onSignal: (signal, handler) => handlers.set(signal, handler),
    });
    return { started, handlers, calls, finishFirstTick };
  };

  it.each(["SIGINT", "SIGTERM"] as const)(
    "shuts down and exits 0 on a %s during the first tick, and starts no interval",
    async (signal) => {
      const { started, handlers, calls, finishFirstTick } =
        loopWithHeldFirstTick();

      expect(calls).toEqual(["tick"]);
      handlers.get(signal)?.();
      await new Promise((resolve) => setImmediate(resolve));
      expect(calls).toEqual(["tick", "shutdown", "exit:0"]);

      finishFirstTick();
      await expect(started).resolves.toBeUndefined();
    },
  );

  it("starts the interval once the first tick finishes without a stop", async () => {
    const { started, handlers, calls, finishFirstTick } =
      loopWithHeldFirstTick();

    finishFirstTick();
    const interval = await started;
    clearInterval(interval);

    expect(interval).toBeDefined();
    expect([...handlers.keys()]).toEqual(["SIGINT", "SIGTERM"]);
    expect(calls).toEqual(["tick"]);
  });
});
