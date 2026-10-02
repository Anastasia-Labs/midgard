import { appendFileSync, mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { waitStablyReady } from "../src/devnet-stack/chaos-stable.js";
import type { DeployContext } from "../src/devnet-stack/deploy.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";

type WaitOptions = { honourStartGrace?: boolean; supervisorPid?: number };

const probes = vi.hoisted(() => ({
  waits: [] as WaitOptions[],
  /** Runs on every readiness probe of the watcher. */
  onWatcherProbe: undefined as (() => void) | undefined,
}));

vi.mock("../src/devnet-stack/stack.js", () => ({
  waitForServices: async (
    _context: unknown,
    _oneShot: unknown,
    _timeoutMs: number,
    options: WaitOptions,
  ) => {
    probes.waits.push(options);
    return [];
  },
  serviceReport: async (_layout: unknown, spec: { name: string }) => {
    if (spec.name === "watcher") probes.onWatcherProbe?.();
    return { name: spec.name, pid: 1, alive: true, ready: true };
  },
  runningSupervisor: () => undefined,
}));
vi.mock("../src/devnet-stack/services.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/devnet-stack/services.js")>()),
  serviceSpecs: () => [{ name: "node" }, { name: "watcher" }],
}));

const dirs: string[] = [];
afterEach(() => {
  probes.waits.splice(0);
  probes.onWatcherProbe = undefined;
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const context = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-chaos-stable-up-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  mkdirSync(dirname(layout.supervisorEvents), { recursive: true });
  const run = { runId: "t", composeProject: "p", portOffset: 20_000 } as RunEnv;
  return { layout, run } as DeployContext;
};
const oneShot = { txHash: "00".repeat(32), outputIndex: 0 };

describe("up's readiness wait", () => {
  it("reports ready once every service has stayed ready with no restart", async () => {
    await expect(
      waitStablyReady(context(), oneShot, 2_000, {
        supervisorPid: process.pid,
        windowMs: 100,
      }),
    ).resolves.toBeUndefined();
    expect(probes.waits).toEqual([
      expect.objectContaining({
        honourStartGrace: true,
        supervisorPid: process.pid,
      }),
    ]);
  });

  it("refuses a stack whose watcher restarts between ready answers", async () => {
    const ctx = context();
    // The supervisor restarts the watcher between every two probes.
    let pid = 100;
    probes.onWatcherProbe = () => {
      const at = new Date().toISOString();
      appendFileSync(
        ctx.layout.supervisorEvents,
        `${JSON.stringify({ at, event: "exit", service: "watcher", pid, code: 70, signal: null })}\n` +
          `${JSON.stringify({ at, event: "start", service: "watcher", pid: (pid += 1) })}\n`,
      );
    };
    await expect(
      waitStablyReady(ctx, oneShot, 400, {
        supervisorPid: process.pid,
        windowMs: 100,
      }),
    ).rejects.toThrow(
      /^services: not stably ready within 0\.4 s.*watcher: \d+ exits \(code 70 x\d+\), \d+ restarts/,
    );
    // After the first disruption no start grace holds the wait open.
    expect(probes.waits.length).toBeGreaterThan(1);
    expect(
      probes.waits.slice(1).every((wait) => wait.honourStartGrace === false),
    ).toBe(true);
  });

  it("refuses a stack whose supervisor is gone", async () => {
    await expect(
      waitStablyReady(context(), oneShot, 300, {
        supervisorPid: 0x7ffffff0,
        windowMs: 100,
      }),
    ).rejects.toThrow(/supervisor is not running/);
  });
});
