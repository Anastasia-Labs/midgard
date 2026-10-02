import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import {
  type ChaosDeps,
  drillCatalogue,
  productionChaosDeps,
  runDrills,
  selectDrills,
} from "../src/devnet-stack/chaos.js";
import type { DeployContext } from "../src/devnet-stack/deploy.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { SERVICE_MARKER_ENV } from "../src/devnet-stack/supervisor.js";

type WaitOptions = { honourStartGrace?: boolean; onGrace?: () => void };

const waits = vi.hoisted(() => ({
  services: [] as { timeoutMs: number; options: WaitOptions }[],
  /** How long the fake waitForServices takes. */
  takesMs: 0,
  /** Whether a start grace holds the fake wait open past its bound. */
  graced: false,
}));

vi.mock("../src/devnet-stack/chain.js", () => ({
  waitL1Ready: async () => {},
}));
vi.mock("../src/devnet-stack/stack.js", () => ({
  waitForServices: async (
    _context: unknown,
    _oneShot: unknown,
    timeoutMs: number,
    options: WaitOptions,
  ) => {
    waits.services.push({ timeoutMs, options });
    await new Promise((resolve) => setTimeout(resolve, waits.takesMs));
    if (waits.graced) options.onGrace?.();
    return [];
  },
}));

const dirs: string[] = [];
afterEach(() => {
  waits.services.splice(0);
  waits.takesMs = 0;
  waits.graced = false;
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-chaos-grace-"));
  dirs.push(dir);
  return dir;
};

describe("productionChaosDeps.waitReady", () => {
  const deps = () => {
    const run = {
      runId: "t",
      composeProject: "p",
      portOffset: 20_000,
    } as RunEnv;
    const context = { layout: makeLayout(scratch()), run } as DeployContext;
    return productionChaosDeps(context, {
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
  };

  it("honours the start grace a service's spec grants, as the supervisor does", async () => {
    await expect(deps().waitReady(60_000)).resolves.toEqual({ graced: false });
    expect(waits.services).toHaveLength(1);
    expect(waits.services[0]!.options.honourStartGrace).toBe(true);
    expect(waits.services[0]!.timeoutMs).toBeLessThanOrEqual(60_000);
  });

  it("reports a wait a start grace held open past its bound", async () => {
    waits.graced = true;
    await expect(deps().waitReady(60_000)).resolves.toEqual({ graced: true });
  });

  it("does not credit a grace for a wait that merely ended late", async () => {
    // Its last probe round began inside the bound and finished past it.
    waits.takesMs = 500;
    await expect(deps().waitReady(10)).resolves.toEqual({ graced: false });
  });
});

describe("runDrills inside a start grace", () => {
  /** A kill of `service`, which the fake supervisor restarts at once. */
  const killRun = (service: string, waitReady: ChaosDeps["waitReady"]) => {
    const runDir = scratch();
    const pidDir = join(runDir, "services");
    mkdirSync(pidDir);
    writeFileSync(
      join(pidDir, `${service}.json`),
      JSON.stringify({ pid: 4242 }),
    );
    let clock = Date.parse("2026-09-30T00:00:00Z");
    const events: Record<string, unknown>[] = [];
    const deps: ChaosDeps = {
      compose: async () => ({ code: 0, stderr: "" }),
      sleep: async (ms) => {
        clock += ms;
      },
      signalGroup: (pid) => {
        events.push({ event: "start", service, pid: pid + 1 });
      },
      environ: () => `${SERVICE_MARKER_ENV}=${runDir}#${service}`,
      fetchNode: async () => ({}),
      now: () => clock,
      waitReady: (timeoutMs) => waitReady(timeoutMs),
      supervisorEvents: () => events,
      readiness: async () => [],
    };
    const drillsLog = join(runDir, "drills.ndjson");
    return {
      drillsLog,
      advance: (ms: number) => {
        clock += ms;
      },
      run: (drills: string[]) =>
        runDrills({
          drills: selectDrills(drillCatalogue([service]), drills),
          runDir,
          pidDir,
          drillsLog,
          deps,
          recoveryMs: 15 * 60_000,
        }),
    };
  };

  it("counts a watcher back only after its long catch-up as recovered", async () => {
    let calls = 0;
    const drill = killRun("watcher", async (timeoutMs) => {
      calls += 1;
      if (calls === 1) return { graced: false };
      // The restarted watcher catches up for 40 min, inside its 60 min grace.
      drill.advance(timeoutMs + 25 * 60_000);
      return { graced: true };
    });
    const summary = await drill.run(["kill-watcher"]);
    expect(summary.counts).toEqual({ recovered: 1, skipped: 0, failed: 0 });
    expect(summary.records[0]!.detail).toMatch(
      /past the 900 s bound inside a service's start grace/,
    );
  });

  it("still injects the next drill while the previous kill's catch-up runs", async () => {
    let calls = 0;
    const drill = killRun("watcher", async (timeoutMs) => {
      calls += 1;
      // The second drill's preflight meets the watcher still catching up.
      if (calls !== 3) return { graced: false };
      drill.advance(timeoutMs + 60_000);
      return { graced: true };
    });
    const summary = await drill.run(["kill-watcher", "kill-watcher"]);
    expect(summary.counts).toEqual({ recovered: 2, skipped: 0, failed: 0 });
    const log = readFileSync(drill.drillsLog, "utf8").trim().split("\n");
    expect(log).toHaveLength(2);
  });
});
