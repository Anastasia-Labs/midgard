import { existsSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  type ChaosDeps,
  drillCatalogue,
  runDrills,
  selectDrills,
} from "../src/devnet-stack/chaos.js";

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});
const setup = () => {
  const dir = mkdtempSync(join(tmpdir(), "acceptance-chaos-cancel-"));
  dirs.push(dir);
  const abort = new AbortController();
  let now = 0;
  const calls: string[] = [];
  const deps: { -readonly [K in keyof ChaosDeps]: ChaosDeps[K] } = {
    now: () => now,
    sleep: async (ms) => {
      now += ms;
    },
    readiness: async () => [],
    waitReady: async () => ({ graced: false }),
    supervisorEvents: () => [],
    fetchNode: async () => ({}),
    environ: () => undefined,
    signalGroup: () => {
      calls.push("kill");
    },
    compose: async (args) => {
      calls.push(args.join(" "));
      return { code: 0, stderr: "" };
    },
  };
  return {
    abort,
    calls,
    deps,
    options: {
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      recoveryMs: 900_000,
      stableMs: 300_000,
      signal: abort.signal,
    },
  };
};
it("restores a previous owed outage even when cancelled before entry", async () => {
  const test = setup();
  test.abort.abort();
  writeFileSync(
    test.options.drillsLog + ".restore.json",
    JSON.stringify({ drill: "stop-kupo", service: "kupo", undo: "start" }),
  );
  await runDrills({
    ...test.options,
    deps: test.deps,
    drills: selectDrills(drillCatalogue([]), ["restart-cardano-node"]),
  });
  expect(test.calls).toEqual(["start kupo"]);
  expect(existsSync(test.options.drillsLog + ".restore.json")).toBe(false);
});
it("retries owed restoration after abort during injection and records the failed drill", async () => {
  const test = setup();
  let restores = 0;
  test.deps.compose = async (args) => {
    test.calls.push(args.join(" "));
    if (args[0] === "stop") test.abort.abort();
    else if (++restores === 1) return { code: 1, stderr: "retry fixture" };
    return { code: 0, stderr: "" };
  };
  const result = await runDrills({
    ...test.options,
    deps: test.deps,
    drills: selectDrills(drillCatalogue([], { outageMs: 60_000 }), [
      "stop-kupo",
    ]),
  });
  expect(test.calls).toEqual(["stop kupo", "start kupo", "start kupo"]);
  expect(result.failures).toHaveLength(1);
  expect(existsSync(test.options.drillsLog + ".restore.json")).toBe(false);
});
it("does not signal after cancellation inside the final actual trigger response", async () => {
  const test = setup();
  writeFileSync(
    join(test.options.pidDir, "node.json"),
    JSON.stringify({ pid: 44 }),
  );
  test.deps.environ = () =>
    `MIDGARD_DEVNET_SERVICE=${test.options.runDir}#node`;
  test.deps.fetchNode = async () => {
    test.abort.abort();
    return { ready: true };
  };
  await runDrills({
    ...test.options,
    deps: test.deps,
    drills: [
      {
        kind: "kill",
        name: "kill-node",
        service: "node",
        trigger: {
          endpoint: "/readyz",
          description: "fixture",
          holds: () => true,
        },
      },
    ],
  });
  expect(test.calls).toEqual([]);
});
it("rechecks a signal aborted by an asynchronous stop callback after successful preflight", async () => {
  const test = setup();
  let count = 0;
  await runDrills({
    ...test.options,
    deps: test.deps,
    shouldStop: async () => {
      if (++count === 2) test.abort.abort();
      return false;
    },
    drills: selectDrills(drillCatalogue([]), ["restart-cardano-node"]),
  });
  expect(test.calls).toEqual([]);
});

it("refuses a kill if PID ownership lookup itself requests cancellation", async () => {
  const test = setup();
  writeFileSync(
    join(test.options.pidDir, "node.json"),
    JSON.stringify({ pid: 44 }),
  );
  test.deps.environ = () => {
    test.abort.abort();
    return `MIDGARD_DEVNET_SERVICE=${test.options.runDir}#node`;
  };
  const result = await runDrills({
    ...test.options,
    deps: test.deps,
    drills: [{ kind: "kill", name: "kill-node", service: "node" }],
  });
  expect(test.calls).toEqual([]);
  expect(result.failures).toHaveLength(1);
});
it("allows a live owned kill and genuine recorded restart without cancellation", async () => {
  const test = setup();
  writeFileSync(
    join(test.options.pidDir, "node.json"),
    JSON.stringify({ pid: 44 }),
  );
  test.deps.environ = () =>
    `MIDGARD_DEVNET_SERVICE=${test.options.runDir}#node`;
  test.deps.supervisorEvents = () =>
    test.calls.length === 0
      ? []
      : [{ event: "start", service: "node", pid: 45 }];
  const result = await runDrills({
    ...test.options,
    deps: test.deps,
    drills: [
      {
        kind: "kill",
        name: "kill-node",
        service: "node",
        trigger: {
          endpoint: "/readyz",
          description: "fixture",
          holds: () => true,
        },
      },
    ],
  });
  expect(test.calls).toEqual(["kill"]);
  expect(result.failures).toEqual([]);
  expect(result.records[0]?.injectedAt).not.toBeNull();
});
