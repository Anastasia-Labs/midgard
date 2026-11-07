import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { setImmediate as turn } from "node:timers/promises";

import { afterEach, describe, expect, it } from "vitest";

import { runAcceptanceSequence } from "../src/devnet-stack/acceptance.js";
import {
  AcceptanceJourney,
  type AcceptanceObservation,
  requireInjectedDrills,
} from "../src/devnet-stack/acceptance-journey.js";
import {
  acceptanceExec,
  assertAcceptanceActive,
} from "../src/devnet-stack/acceptance-process.js";
import {
  type ChaosDeps,
  drillCatalogue,
  runDrills,
  selectDrills,
} from "../src/devnet-stack/chaos.js";
import type { HubOracleOneShot } from "../src/devnet-stack/node-env.js";
import { SERVICE_MARKER_ENV } from "../src/devnet-stack/supervisor.js";
import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

afterEach(removeFakeContexts);

const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (error: unknown) => void;
  const promise = new Promise<T>((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
};

const fixture = (
  options: {
    preflight?: Promise<void>;
    wrongOwner?: boolean;
    triggerTimeoutMs?: number;
  } = {},
) => {
  const context = fakeContext();
  Object.assign(context.run, { ogmiosPort: 2337, postgresPort: 5544 });
  const runDir = context.layout.nodeRoot;
  const pidDir = join(runDir, "services");
  mkdirSync(pidDir);
  writeFileSync(join(pidDir, "node.json"), '{"pid":4242}');
  const abort = new AbortController();
  const injected = deferred<void>();
  const events: Record<string, unknown>[] = [];
  const observations: AcceptanceObservation[] = [];
  let clock = Date.parse("2026-10-02T00:00:00Z");
  let body: Record<string, unknown> = {};
  let preflights = 0;
  let signals = 0;
  let reads = 0;
  const deps: ChaosDeps = {
    now: () => clock,
    sleep: async (ms) => {
      clock += ms;
      await turn();
    },
    compose: async () => ({ code: 0, stderr: "" }),
    environ: () =>
      options.wrongOwner ? "foreign" : `${SERVICE_MARKER_ENV}=${runDir}#node`,
    fetchNode: async () => {
      reads += 1;
      return body;
    },
    signalGroup: () => {
      signals += 1;
      body = {};
      events.push({ event: "start", service: "node", pid: 4243 });
      injected.resolve();
    },
    supervisorEvents: () => events,
    readiness: async () => [
      { name: "node", alive: true, pid: 4243, ready: true },
    ],
    waitReady: async () => {
      preflights += 1;
      if (preflights === 1) await options.preflight;
      return { graced: false };
    },
  };
  const journey = new AcceptanceJourney(
    context,
    {} as HubOracleOneShot,
    [],
    {
      drills: drillCatalogue(["node"]),
      abort,
      drain: async () => {},
      observe: (receipt) => observations.push(receipt),
      chaos: {
        runDir,
        pidDir,
        drillsLog: join(runDir, "drills.ndjson"),
        deps,
        recoveryMs: 900_000,
        stableMs: 300_000,
        triggerTimeoutMs: options.triggerTimeoutMs ?? 1_200_000,
        pollMs: 10,
      },
    },
    {
      log: () => {},
      sleep: async () => {
        assertAcceptanceActive(abort.signal);
      },
    },
  );
  return {
    journey,
    abort,
    observations,
    injected,
    setBody: (next: Record<string, unknown>) => {
      body = next;
    },
    counters: () => ({ reads, signals, preflights }),
  };
};

describe("finite acceptance phase gates", () => {
  it("injects the exact twelve finite drills around the three genuinely gated phases", async () => {
    const f = fixture();
    const catalogue = drillCatalogue([
      "node",
      "da-committee-0",
      "da-committee-1",
      "public-retained-da",
      "watcher",
    ]);
    const seen: string[] = [];
    const records = await runAcceptanceSequence(
      f.journey,
      async (names) => {
        const drills = selectDrills(catalogue, names);
        for (const drill of drills)
          if (drill.kind === "kill")
            writeFileSync(
              join(f.journey.gate.chaos.pidDir, `${drill.service}.json`),
              '{"pid":4242}',
            );
        const events: Record<string, unknown>[] = [];
        const killedServices = drills
          .filter((drill) => drill.kind === "kill")
          .map((drill) => drill.service);
        const summary = await runDrills({
          ...f.journey.gate.chaos,
          drills,
          rounds: 1,
          deps: {
            ...f.journey.gate.chaos.deps,
            environ: () =>
              names
                .map((name) => catalogue.find((d) => d.name === name)?.service)
                .map(
                  (service) =>
                    `${SERVICE_MARKER_ENV}=${f.journey.gate.chaos.runDir}#${service}`,
                )
                .join("\0"),
            signalGroup: () => {
              events.push({
                event: "start",
                service: killedServices.shift(),
                pid: 4243,
              });
            },
            supervisorEvents: () => events,
          },
        });
        seen.push(...names);
        return summary.records;
      },
      async () => {
        expect(seen).toEqual([
          "restart-cardano-node",
          "stop-kupo",
          "stop-ogmios",
          "kill-public-retained-da",
        ]);
        for (const [phase, body] of [
          ["transfers-concurrent", { durableAdmission: { backlog: "1" } }],
          [
            "transfers-sequential",
            { stateQueue: { unconfirmedSubmittedBlockTxHash: "ab" } },
          ],
          [
            "withdrawals-concurrent",
            {
              settlement: {
                state: "waiting",
                detail:
                  "settlement transaction abababababababababababababababababababababababababababababababab journaled; S6 sends its exact bytes until it lands",
              },
            },
          ],
        ] as const)
          await f.journey.phase(phase, async () => {
            f.setBody(body);
          });
      },
    );
    expect(records.map((record) => record.drill)).toEqual([
      "restart-cardano-node",
      "stop-kupo",
      "stop-ogmios",
      "kill-public-retained-da",
      "kill-node-on-admission",
      "kill-node-on-block-submitted",
      "kill-node-on-settlement",
      "kill-node",
      "kill-da-member-0",
      "kill-da-member-1",
      "kill-watcher",
      "pause-postgres",
    ]);
    requireInjectedDrills(
      selectDrills(
        catalogue,
        records.map((record) => record.drill),
      ),
      records,
    );
  });

  it("never releases a workload when preflight fails before trigger polling", async () => {
    const preflight = deferred<void>();
    const f = fixture({ preflight: preflight.promise });
    let ran = false;
    const task = f.journey.phase("transfers-concurrent", async () => {
      ran = true;
    });
    preflight.reject(new Error("actual preflight refused"));
    await expect(task).rejects.toThrow(
      /not injected.*actual preflight refused/,
    );
    expect(ran).toBe(false);
    expect(f.counters().reads).toBe(0);
    expect(f.observations).toEqual([]);
  });

  it.each([
    [
      "transfers-concurrent",
      "kill-node-on-admission",
      { durableAdmission: { backlog: "1" } },
    ],
    [
      "transfers-sequential",
      "kill-node-on-block-submitted",
      { stateQueue: { unconfirmedSubmittedBlockTxHash: "ab".repeat(32) } },
    ],
    [
      "withdrawals-concurrent",
      "kill-node-on-settlement",
      {
        settlement: {
          state: "waiting",
          detail:
            "settlement transaction abababababababababababababababababababababababababababababababab journaled; S6 sends its exact bytes until it lands",
        },
      },
    ],
  ])(
    "arms %s only after preflight, captures its real short trigger and joins recovery",
    async (phase, drill, body) => {
      const preflight = deferred<void>();
      const f = fixture({ preflight: preflight.promise });
      let phaseRan = false;
      const task = f.journey.phase(phase, async () => {
        phaseRan = true;
        expect(f.counters().reads).toBeGreaterThan(0);
        expect(f.observations[0]?.kind).toBe("armed");
        f.setBody(body);
        await f.injected.promise;
      });
      await turn();
      expect(phaseRan).toBe(false);
      expect(f.counters()).toEqual({ reads: 0, signals: 0, preflights: 1 });
      preflight.resolve();
      await task;
      expect(f.counters().signals).toBe(1);
      expect(f.counters().preflights).toBe(2);
      expect(f.journey.drillRecords).toMatchObject([{ drill, ok: true }]);
      expect(f.observations).toMatchObject([
        { phase, drill, kind: "armed" },
        { phase, drill, kind: "injection", body, pid: 4242 },
      ]);
      expect(f.observations[0]?.nonce).toBe(f.observations[1]?.nonce);
      expect(f.journey.journal.get(`phase:${phase}`)).toBe("done");
    },
  );

  it("fails a missed trigger rather than promoting the runner's skipped success", async () => {
    const f = fixture({ triggerTimeoutMs: 30 });
    await expect(
      f.journey.phase("transfers-concurrent", async () => {}),
    ).rejects.toThrow(/incomplete/);
    expect(f.counters().signals).toBe(0);
    expect(f.abort.signal.aborted).toBe(true);
    expect(f.journey.drillRecords).toEqual([]);
  });

  it("refuses a foreign PID without recording a successful targeted kill", async () => {
    const f = fixture({ wrongOwner: true });
    await expect(
      f.journey.phase("transfers-concurrent", async () => {
        f.setBody({ durableAdmission: { backlog: "1" } });
      }),
    ).rejects.toThrow(/not this run/);
    expect(f.counters().signals).toBe(0);
  });

  it("joins the drill after callback failure and leaves the failed phase uncompleted", async () => {
    const f = fixture();
    await expect(
      f.journey.phase("transfers-concurrent", async () => {
        throw new Error("genuine phase failed");
      }),
    ).rejects.toThrow("genuine phase failed");
    expect(f.abort.signal.aborted).toBe(true);
    expect(f.journey.journal.get("phase:transfers-concurrent")).toBeUndefined();
    const atExit = f.counters();
    await turn();
    expect(f.counters()).toEqual(atExit);
    expect(f.counters().signals).toBe(0);
  });

  it("delegates an already completed phase without fabricating a fresh receipt", async () => {
    const f = fixture();
    f.journey.journal.set("phase:transfers-concurrent", "done");
    await f.journey.phase("transfers-concurrent", async () => {
      throw new Error("must not run");
    });
    expect(f.counters()).toEqual({ reads: 0, signals: 0, preflights: 0 });
    expect(f.journey.drillRecords).toEqual([]);
    expect(() =>
      requireInjectedDrills(
        selectDrills(drillCatalogue(["node"]), ["kill-node-on-admission"]),
        f.journey.drillRecords,
      ),
    ).toThrow(/expected 1 drills/);
  });

  it("does not publish phase done when cancellation happens before its callback resolves", async () => {
    const f = fixture();
    await expect(
      f.journey.phase("ordinary", async () => {
        f.abort.abort();
      }),
    ).rejects.toThrow(/cancelled/);
    expect(f.journey.journal.get("phase:ordinary")).toBeUndefined();
  });

  it("waits for a cancelled phase's physical cleanup after the drill misses its trigger", async () => {
    const f = fixture({ triggerTimeoutMs: 30 });
    const cleanup = deferred<void>();
    const sawAbort = deferred<void>();
    let returned = false;
    const task = f.journey.phase("transfers-concurrent", async () => {
      f.abort.signal.addEventListener("abort", () => sawAbort.resolve(), {
        once: true,
      });
      await sawAbort.promise;
      await cleanup.promise;
    });
    void task.then(
      () => {
        returned = true;
      },
      () => {
        returned = true;
      },
    );
    await sawAbort.promise;
    await turn();
    expect(returned).toBe(false);
    cleanup.resolve();
    await expect(task).rejects.toThrow(/incomplete/);
    expect(returned).toBe(true);
    expect(f.journey.journal.get("phase:transfers-concurrent")).toBeUndefined();
  });

  it.each([
    "missing",
    "skipped",
    "failed",
    "wrong target",
    "null injection",
    "null recovery",
  ])("rejects %s coverage", (invalid) => {
    const expected = selectDrills(drillCatalogue(["node"]), ["kill-node"]);
    const record = {
      drill: "kill-node",
      target: "node",
      injectedAt: "2026-10-02T00:00:00Z",
      recoveredAt: "2026-10-02T00:01:00Z",
      ok: true,
      detail: "fixture",
    };
    const overrides = {
      skipped: { skipped: true },
      failed: { ok: false },
      "wrong target": { target: "foreign" },
      "null injection": { injectedAt: null },
      "null recovery": { recoveredAt: null },
    };
    expect(() =>
      requireInjectedDrills(
        expected,
        invalid === "missing"
          ? []
          : [{ ...record, ...overrides[invalid as keyof typeof overrides] }],
      ),
    ).toThrow(/acceptance/);
  });
});

describe("acceptance command ownership", () => {
  it("physically joins its cancelled child and persists the actual exit transcript", async () => {
    const context = fakeContext();
    const abort = new AbortController();
    const readyPath = join(context.layout.nodeRoot, "child-ready");
    // The child ignores SIGTERM before it announces its pid, so the abort
    // can never land before the handler and end it with SIGTERM.
    const task = acceptanceExec(
      process.execPath,
      [
        "-e",
        `process.on('SIGTERM',()=>{});require('fs').writeFileSync(process.argv[1],String(process.pid));setInterval(()=>{},1000)`,
        readyPath,
      ],
      {
        cwd: context.layout.nodeRoot,
        env: {},
        logDir: join(context.layout.nodeRoot, "logs"),
        label: "owned",
        signal: abort.signal,
        timeoutMs: 10_000,
        killGraceMs: 30,
      },
    );
    let pid: number | undefined;
    const deadline = Date.now() + 5000;
    while (pid === undefined) {
      try {
        const observed = Number(readFileSync(readyPath, "utf8"));
        if (Number.isSafeInteger(observed) && observed > 0) pid = observed;
      } catch {
        await turn();
      }
      if (Date.now() > deadline) throw new Error("child did not start");
    }
    abort.abort();
    const result = await task;
    expect(result.signal).toBe("SIGKILL");
    expect(() => process.kill(pid, 0)).toThrow();
    expect(readFileSync(result.log, "utf8")).toContain("signal=SIGKILL");
  });
});
