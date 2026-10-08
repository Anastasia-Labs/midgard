import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { AvailabilityResponderAwaitingScanError } from "../src/availability/awaiting-scan-error.js";
import {
  AvailabilityResponder,
  availabilityResponderReportLine,
} from "../src/availability/responder.js";
import {
  type CommitteeL1View,
  type CommitteeRetentionReadinessSnapshot,
  CommitteeService,
} from "../src/committee-service.js";
import { loadDaSigner } from "../src/signer.js";
import { type PostgresCommitteeStore } from "../src/store/postgres.js";
import type { RetentionL1View } from "../src/store/retention.js";
import {
  createCommitteeTickRunner,
  l1ViewStaleMs,
} from "../src/tick-runner.js";
import { minimalConfig, payloadSourceFromBytes, tempDir } from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";
import { fakeL1Source } from "./helpers/fake-l1-source.js";

const POLL_MS = 2_000;
const FATAL_MS = 600_000;
const STALE_MS = l1ViewStaleMs({
  pollIntervalMs: POLL_MS,
  l1ViewFatalMs: FATAL_MS,
});
const START = 1_000_000;

const harness = (
  options: {
    readonly responder?: AvailabilityResponder;
    /** Retention fails like the node's: readiness `failed`, then throw. */
    readonly retentionFails?: boolean;
    /** Wall-clock time each tick spends after its L1 read (actuation). */
    readonly tickMs?: number;
    /** Wall-clock time a tick whose L1 read fails takes; default `tickMs`. */
    readonly failedTickMs?: number;
  } = {},
) => {
  let now = START;
  let view: CommitteeL1View | undefined;
  let l1Readable = true;
  let hangs = false;
  let clockStepDuringTickMs = 0;
  const lines: { stream: "stdout" | "stderr"; line: string }[] = [];
  const retentionViews: RetentionL1View[] = [];
  const acceptView = () => {
    view = {
      observedAtMs: now,
      confirmedHeadHash: "aa".repeat(28),
      liveQueueHeaderHashes: new Set(["bb".repeat(28)]),
    };
  };
  let readiness: CommitteeRetentionReadinessSnapshot = {
    status: "not_checked",
    scanned: 0,
    retained: 0,
    prunable: 0,
    alerting: 0,
  };
  const runner = createCommitteeTickRunner({
    tick: async () => {
      if (hangs) return new Promise<never>(() => undefined);
      // The wall clock steps back while the tick waits on L1.
      now += clockStepDuringTickMs;
      clockStepDuringTickMs = 0;
      if (!l1Readable) {
        now += options.failedTickMs ?? options.tickMs ?? 0;
        throw new Error("ogmios connection refused");
      }
      acceptView();
      now += options.tickMs ?? 0;
      return {
        scannedHeaders: 0,
        signedHeaders: 0,
        reconciledHeaders: 0,
        skippedHeaders: 0,
        payloadFetches: [],
        errors: [],
      };
    },
    // The node's own glue: log the responder's report, throw what it throws.
    runAvailabilityResponse: async () => {
      if (options.responder === undefined) return;
      const logged = availabilityResponderReportLine(
        await options.responder.tick(),
      );
      if (logged !== undefined) lines.push(logged);
    },
    runRetention: async (retentionView) => {
      retentionViews.push(retentionView);
      readiness = {
        status: options.retentionFails === true ? "failed" : "ok",
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
        ...(options.retentionFails === true
          ? { error: "store read failed" }
          : {}),
      };
      if (options.retentionFails === true) throw new Error("store read failed");
    },
    latestL1View: () => view,
    latestL1ProgressAtMs: () => undefined,
    setRetentionReadiness: (update) => {
      readiness = update(readiness);
    },
    l1ViewFatalMs: FATAL_MS,
    l1ViewStaleMs: STALE_MS,
    startedAtMs: START,
    nowMs: () => now,
    write: (stream, line) => lines.push({ stream, line }),
  });
  return {
    runner,
    /** Moves the wall clock; a negative step is a backward clock step. */
    advance: (ms: number) => {
      now += ms;
    },
    stepClockDuringNextTick: (ms: number) => {
      clockStepDuringTickMs = ms;
    },
    setL1Readable: (value: boolean) => {
      l1Readable = value;
    },
    /** Later ticks never settle; they read L1 only via `acceptView`. */
    startHanging: () => {
      hangs = true;
    },
    /** The hanging in-flight tick accepts its own view and keeps running. */
    acceptView,
    view: () => view,
    lines,
    stderr: () =>
      lines.filter(({ stream }) => stream === "stderr").map(({ line }) => line),
    retentionViews,
    readiness: () => readiness,
  };
};

let dir: string;
let store: PostgresCommitteeStore;
let service: CommitteeService;

beforeAll(async () => {
  dir = await tempDir();
  const seed = "00".repeat(31) + "01";
  const signer = await loadDaSigner(`hex:${seed}`);
  store = await openTestCommitteeStore();
  service = new CommitteeService({
    config: minimalConfig({
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    }),
    store,
    l1: fakeL1Source({
      fetchStateQueueNodes: async () => [],
    }),
    payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
  });
  await service.initialize();
  await service.tick();
});

afterAll(async () => {
  await store.close();
});

/** The node's readiness verdict for a retention readiness snapshot. */
const verdict = async (retention: CommitteeRetentionReadinessSnapshot) => {
  const { ready, reasons } = await service.readinessSnapshot({ retention });
  return { ready, reasons };
};

describe("committee readiness judges the age of the last accepted L1 view", () => {
  it("derives the staleness bound from the poll interval, capped at the fatal deadline", () => {
    expect(STALE_MS).toBe(6_000);
    expect(
      l1ViewStaleMs({ pollIntervalMs: 15_000, l1ViewFatalMs: FATAL_MS }),
    ).toBe(45_000);
    expect(
      l1ViewStaleMs({ pollIntervalMs: 300_000, l1ViewFatalMs: FATAL_MS }),
    ).toBe(FATAL_MS);
  });

  it("stays ready when a pass skips on a view one second old, with the skip as detail", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(1_000);
    await h.runner.runTick();

    expect(h.retentionViews).toHaveLength(1);
    expect(h.readiness()).toMatchObject({
      status: "ok",
      skippedPass: { reason: "l1_view_unavailable", l1ViewAgeMs: 1_000 },
    });
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: true,
      reasons: [],
    });
  });

  it("is not ready once the last accepted view is older than the bound", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(STALE_MS);
    await h.runner.runTick();
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: true,
      reasons: [],
    });

    h.advance(1);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: STALE_MS + 1,
    });
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: false,
      reasons: [`l1_view_stale:${(STALE_MS + 1).toString()}`],
    });
    expect(h.retentionViews).toHaveLength(1);
  });

  it("is not ready when no view was ever accepted", async () => {
    const h = harness();
    h.setL1Readable(false);
    h.advance(1_000);
    await h.runner.runTick();
    expect(h.retentionViews).toEqual([]);
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: false,
      reasons: ["l1_view_stale:1000"],
    });
  });

  it("clears l1_view_stale once the last view is within the bound again", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(STALE_MS + 500);
    await h.runner.runTick();
    expect(h.readiness().status).toBe("l1_view_stale");
    // The clock steps back 720 ms: the same view is 5780 ms old.
    h.advance(-720);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "skipped",
      skippedPass: { l1ViewAgeMs: STALE_MS - 220 },
    });
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: true,
      reasons: [],
    });
  });

  it("keeps a failed retention check failed through a skip on a fresh view", async () => {
    const h = harness({ retentionFails: true });
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(1_000);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "failed",
      skippedPass: { reason: "l1_view_unavailable", l1ViewAgeMs: 1_000 },
    });
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: false,
      reasons: ["retention check failed: store read failed"],
    });
  });

  it("stays ready while a slow tick has not yet read L1, and is not ready past the bound", async () => {
    const h = harness();
    await h.runner.runTick();
    h.startHanging();
    h.advance(POLL_MS);
    void h.runner.runTick();
    h.advance(POLL_MS);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "ok",
      skippedPass: { reason: "tick_in_flight", l1ViewAgeMs: 2 * POLL_MS },
    });
    h.advance(STALE_MS);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: STALE_MS + 2 * POLL_MS,
    });
    expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
  });

  it("clears l1_view_stale once the in-flight tick accepts its own view, while it keeps actuating", async () => {
    const h = harness();
    await h.runner.runTick();
    h.startHanging();
    h.advance(POLL_MS);
    void h.runner.runTick();
    h.advance(STALE_MS);
    await h.runner.runTick();
    expect(h.readiness().status).toBe("l1_view_stale");

    h.acceptView();
    h.advance(POLL_MS);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "skipped",
      skippedPass: { reason: "tick_in_flight", l1ViewAgeMs: POLL_MS },
    });
    // A 60 s actuation keeps readiness ready; only the deadline applies.
    h.advance(60_000);
    await h.runner.runTick();
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: true,
      reasons: [],
    });
    expect(h.retentionViews).toHaveLength(1);
    expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
  });
});

describe("the staleness bound follows the tick cadence", () => {
  const skipLines = (h: ReturnType<typeof harness>) =>
    h
      .stderr()
      .filter((line) => line.includes("retention_pass_skipped"))
      .map((line) => JSON.parse(line) as Record<string, unknown>);

  it("rides out one failed tick at a cadence of ticks longer than the interval", async () => {
    // lc1: 2.2 s ticks on a 2 s poll start every 4 s; failed reads take 3 s.
    const h = harness({ tickMs: 2_200, failedTickMs: 3_000 });
    await h.runner.runTick();
    h.setL1Readable(false);
    const ages: number[] = [];
    let endedAtMs = 2_200;
    for (let tick = 1; tick <= 3; tick += 1) {
      h.advance(tick * 2 * POLL_MS - endedAtMs);
      await h.runner.runTick();
      endedAtMs = tick * 2 * POLL_MS + 3_000;
      ages.push(Number(skipLines(h).at(-1)?.l1ViewAgeMs));
      if (tick === 1) expect(h.readiness().status).toBe("ok");
    }
    expect(ages).toEqual([7_000, 11_000, 15_000]);
    // Failed ticks never widen the bound: it stays 3 x (2 s + 2.2 s).
    expect(skipLines(h).map((line) => line.l1ViewStaleMs)).toEqual([
      12_600, 12_600, 12_600,
    ]);
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: false,
      reasons: ["l1_view_stale:15000"],
    });
  });

  it("stays ready when the scan after a 98 s actuation tick outlasts its interval", async () => {
    const h = harness({ tickMs: 98_000 });
    await h.runner.runTick();
    h.startHanging();
    h.advance(POLL_MS);
    void h.runner.runTick();
    h.advance(POLL_MS);
    await h.runner.runTick();
    expect(h.readiness()).toMatchObject({
      status: "ok",
      skippedPass: { reason: "tick_in_flight", l1ViewAgeMs: 102_000 },
    });
    expect(skipLines(h)[0]?.l1ViewStaleMs).toBe(STALE_MS + 3 * 98_000);
  });

  it("never widens the bound past the fatal deadline", async () => {
    const h = harness({ tickMs: 250_000 });
    await h.runner.runTick();
    h.setL1Readable(false);
    await h.runner.runTick();
    expect(skipLines(h)[0]?.l1ViewStaleMs).toBe(FATAL_MS);
  });
});

describe("retention needs a view accepted in the current tick, whatever the clock does", () => {
  it("runs retention on a view accepted this tick although the clock stepped back during it", async () => {
    const h = harness();
    await h.runner.runTick();
    h.advance(POLL_MS);
    h.stepClockDuringNextTick(-720);
    await h.runner.runTick();

    expect(h.retentionViews).toHaveLength(2);
    expect(h.retentionViews[1]).toBe(h.view());
    expect(h.stderr()).toEqual([]);
    expect(h.readiness()).toEqual({
      status: "ok",
      scanned: 0,
      retained: 0,
      prunable: 0,
      alerting: 0,
    });
  });

  it("never runs retention on the previous tick's view after the clock steps back", async () => {
    const h = harness();
    await h.runner.runTick();
    const previous = h.view();
    h.setL1Readable(false);
    // The next tick starts 720 ms earlier on the wall clock than the
    // previous tick's view.
    h.advance(-720);
    await h.runner.runTick();

    expect(h.retentionViews).toEqual([previous]);
    expect(
      h.stderr().some((line) => line.includes("retention_pass_skipped")),
    ).toBe(true);
  });

  it("reports the view unavailable, and prunes nothing, once it is older than the fatal deadline", async () => {
    const h = harness();
    await h.runner.runTick();
    h.setL1Readable(false);
    h.advance(FATAL_MS);
    await h.runner.runTick();
    expect(h.runner.liveness().l1ViewUnavailable).toBeUndefined();
    expect(h.readiness()).toMatchObject({
      status: "l1_view_stale",
      l1ViewAgeMs: FATAL_MS,
    });
    h.advance(1);
    await h.runner.runTick();
    expect(h.runner.liveness().l1ViewUnavailable).toEqual({
      l1ViewAgeMs: FATAL_MS + 1,
      l1ViewFatalMs: FATAL_MS,
    });
    expect(h.retentionViews).toHaveLength(1);
  });
});

describe("committee tick with the availability responder", () => {
  const responder = (reconcile: () => Promise<"ready" | "pending">) =>
    new AvailabilityResponder({
      deploymentIdentity: "11".repeat(28),
      deploymentFingerprint: "22".repeat(32),
      store: { getDaPayload: async () => undefined },
      discover: async () => [],
      execute: async () => "confirmed",
      reconcile,
    });

  it("logs one compact line and no error while the responder awaits the next scan", async () => {
    const h = harness({
      responder: responder(async () => {
        throw new AvailabilityResponderAwaitingScanError();
      }),
    });
    await h.runner.runTick();

    expect(h.stderr()).toEqual([]);
    expect(h.lines).toEqual([
      {
        stream: "stdout",
        line: `${JSON.stringify({
          event: "availability_responder",
          challenges: 0,
          status: "awaiting_scan",
          detail: new AvailabilityResponderAwaitingScanError().message,
        })}\n`,
      },
    ]);
    expect(h.retentionViews).toHaveLength(1);
    await expect(verdict(h.readiness())).resolves.toEqual({
      ready: true,
      reasons: [],
    });
  });

  it("still logs a genuine responder error with its stack", async () => {
    const h = harness({
      responder: responder(async () => {
        throw new Error("kupmios read failed");
      }),
    });
    await h.runner.runTick();

    const [line, ...rest] = h.stderr();
    expect(rest).toEqual([]);
    expect(line).toMatch(/^Error: kupmios read failed\n\s+at /u);
  });
});
