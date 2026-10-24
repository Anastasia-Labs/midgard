import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  type ChaosDeps,
  drillCatalogue,
  runDrills,
  selectDrills,
} from "../src/devnet-stack/chaos.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-chaos-outage-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

/**
 * Fakes with a virtual clock: sleep advances it instantly. Compose calls are
 * recorded, never run.
 */
const fakes = (overrides: Partial<ChaosDeps> = {}) => {
  let clock = Date.parse("2026-09-30T00:00:00Z");
  const signals: { pid: number; signal: string }[] = [];
  const composeCalls: string[] = [];
  const events: Record<string, unknown>[] = [];
  const deps: ChaosDeps = {
    signalGroup: (pid, signal) => {
      signals.push({ pid, signal });
    },
    environ: () => undefined,
    compose: async (args) => {
      composeCalls.push(args.join(" "));
      return { code: 0, stderr: "" };
    },
    fetchNode: async () => ({}),
    now: () => clock,
    sleep: async (ms) => {
      clock += ms;
    },
    waitReady: async () => ({ graced: false }),
    supervisorEvents: () => events,
    readiness: async () => [],
    ...overrides,
  };
  return {
    deps,
    signals,
    composeCalls,
    events,
    advance: (ms: number) => {
      clock += ms;
    },
  };
};

describe("runDrills outages", () => {
  const outage = (name: string) =>
    selectDrills(drillCatalogue([], { outageMs: 60_000, pauseMs: 30_000 }), [
      name,
    ]);

  it("restores the dependency even when the recovery wait throws", async () => {
    const dir = scratch();
    let calls = 0;
    const fake = fakes({
      waitReady: async () => {
        calls += 1;
        if (calls > 1) throw new Error("kupo still down");
        return { graced: false };
      },
    });
    const drillsLog = join(dir, "drills.ndjson");
    const summary = await runDrills({
      drills: outage("stop-kupo"),
      runDir: dir,
      pidDir: dir,
      drillsLog,
      deps: fake.deps,
    });
    expect(fake.composeCalls).toEqual(["stop kupo", "start kupo"]);
    expect(summary.failures).toHaveLength(1);
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(false);
  });

  it("restores the dependency when interrupted mid-outage", async () => {
    const dir = scratch();
    const abort = new AbortController();
    const fake = fakes();
    fake.deps = {
      ...fake.deps,
      sleep: async (ms) => {
        // The outage wait is where the operator presses Ctrl-C.
        if (ms === 30_000) abort.abort();
        fake.advance(ms);
      },
    };
    await runDrills({
      drills: outage("pause-postgres"),
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
      signal: abort.signal,
    });
    expect(fake.composeCalls).toEqual(["pause postgres", "unpause postgres"]);
  });

  it("restores the dependency when the injection itself fails and retries a failing restore", async () => {
    const dir = scratch();
    let starts = 0;
    const fake = fakes({
      compose: async (args) => {
        fake.composeCalls.push(args.join(" "));
        if (args[0] === "stop") return { code: 1, stderr: "docker hiccup" };
        starts += 1;
        return starts < 3
          ? { code: 1, stderr: "daemon busy" }
          : { code: 0, stderr: "" };
      },
    });
    const summary = await runDrills({
      drills: outage("stop-ogmios"),
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
    });
    expect(fake.composeCalls).toEqual([
      "stop ogmios",
      "start ogmios",
      "start ogmios",
      "start ogmios",
    ]);
    expect(summary.failures[0]?.detail).toMatch(
      /injection failed.*docker hiccup/,
    );
    expect(summary.failures[0]?.detail).not.toMatch(/RESTORE FAILED/);
  });

  it("halts, keeping the owed restore on disk, when a restore keeps failing", async () => {
    const dir = scratch();
    const drillsLog = join(dir, "drills.ndjson");
    const fake = fakes({
      compose: async (args) => {
        fake.composeCalls.push(args.join(" "));
        return args[0] === "start"
          ? { code: 1, stderr: "daemon gone" }
          : { code: 0, stderr: "" };
      },
    });
    const summary = await runDrills({
      drills: selectDrills(drillCatalogue([]), [
        "stop-kupo",
        "restart-cardano-node",
      ]),
      runDir: dir,
      pidDir: dir,
      drillsLog,
      deps: fake.deps,
    });
    expect(fake.composeCalls).toEqual([
      "stop kupo",
      ...Array<string>(5).fill("start kupo"),
    ]);
    expect(summary.failures[0]?.detail).toMatch(/RESTORE FAILED.*daemon gone/);
    expect(
      JSON.parse(readFileSync(`${drillsLog}.restore.json`, "utf8")),
    ).toMatchObject({
      service: "kupo",
      undo: "start",
    });
  });

  it("finishes a restore a killed previous run left owed before any drill", async () => {
    const dir = scratch();
    const drillsLog = join(dir, "drills.ndjson");
    writeFileSync(
      `${drillsLog}.restore.json`,
      JSON.stringify({
        drill: "pause-postgres",
        service: "postgres",
        undo: "unpause",
      }),
    );
    const fake = fakes();
    const summary = await runDrills({
      drills: [],
      runDir: dir,
      pidDir: dir,
      drillsLog,
      deps: fake.deps,
    });
    expect(fake.composeCalls).toEqual(["unpause postgres"]);
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(false);
    expect(summary.records).toMatchObject([
      { drill: "pause-postgres", ok: true },
    ]);
  });

  it("only issues lifecycle verbs on this project's L1 services", async () => {
    const dir = scratch();
    const fake = fakes();
    const summary = await runDrills({
      drills: [
        {
          kind: "restart",
          name: "restart-cardano-node",
          service: "cardano-node",
        },
        {
          kind: "restart",
          name: "evil",
          service: "someone-elses-db" as "postgres",
        },
      ],
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
      gapMs: 1_000,
    });
    expect(fake.composeCalls).toEqual([
      "restart cardano-node",
      "start cardano-node",
    ]);
    expect(summary.failures[0]?.detail).toMatch(
      /refusing docker compose restart someone-elses-db/,
    );
    expect(existsSync(join(dir, "drills.ndjson.restore.json"))).toBe(false);
  });

  it("refuses a run that could not inject anything", async () => {
    const dir = scratch();
    const fake = fakes();
    const base = {
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
    };
    const kupo = selectDrills(drillCatalogue([], { outageMs: Number("60s") }), [
      "stop-kupo",
    ]);
    await expect(runDrills({ ...base, drills: kupo })).rejects.toThrow(
      /above 0/,
    );
    const restart = selectDrills(drillCatalogue([]), ["restart-cardano-node"]);
    await expect(
      runDrills({ ...base, drills: restart, rounds: Number("3x") }),
    ).rejects.toThrow(/above 0/);
    await expect(
      runDrills({ ...base, drills: restart, recoveryMs: Number.NaN }),
    ).rejects.toThrow(/above 0/);
    expect(fake.composeCalls).toEqual([]);
  });

  it("stops between drills once the stop condition holds", async () => {
    const dir = scratch();
    const fake = fakes();
    let done = false;
    fake.deps = {
      ...fake.deps,
      compose: async (args) => {
        fake.composeCalls.push(args.join(" "));
        done = true;
        return { code: 0, stderr: "" };
      },
    };
    const summary = await runDrills({
      drills: selectDrills(drillCatalogue([]), ["restart-cardano-node"]),
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
      rounds: Number.POSITIVE_INFINITY,
      shouldStop: () => done,
    });
    expect(fake.composeCalls).toEqual([
      "restart cardano-node",
      "start cardano-node",
    ]);
    expect(summary.records).toHaveLength(1);
  });
});
