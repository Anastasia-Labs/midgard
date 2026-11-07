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
import {
  type ComposeDeps,
  finishOwedRestore,
} from "../src/devnet-stack/chaos-restore.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-chaos-restore-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

/** Compose calls are recorded; `fail` decides a call's exit. */
const composeFake = (
  fail: (args: readonly string[]) => boolean = () => false,
) => {
  const calls: string[] = [];
  const deps: ComposeDeps = {
    compose: async (args) => {
      calls.push(args.join(" "));
      return fail(args)
        ? { code: 1, stderr: "daemon gone" }
        : { code: 0, stderr: "" };
    },
    sleep: async () => {},
  };
  return { calls, deps };
};

describe("finishOwedRestore", () => {
  it("does nothing when no restore is owed", async () => {
    const dir = scratch();
    const fake = composeFake();
    expect(
      await finishOwedRestore(fake.deps, join(dir, "drills.ndjson")),
    ).toBeUndefined();
    expect(fake.calls).toEqual([]);
  });

  it("brings the dependency back and clears the record", async () => {
    const drillsLog = join(scratch(), "drills.ndjson");
    const owed = { drill: "stop-kupo", service: "kupo", undo: "start" };
    writeFileSync(`${drillsLog}.restore.json`, JSON.stringify(owed));
    const fake = composeFake();
    expect(await finishOwedRestore(fake.deps, drillsLog)).toEqual(owed);
    expect(fake.calls).toEqual(["start kupo"]);
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(false);
  });

  it("keeps the record when the dependency cannot be brought back", async () => {
    const drillsLog = join(scratch(), "drills.ndjson");
    writeFileSync(
      `${drillsLog}.restore.json`,
      JSON.stringify({
        drill: "stop-ogmios",
        service: "ogmios",
        undo: "start",
      }),
    );
    const fake = composeFake(() => true);
    await expect(finishOwedRestore(fake.deps, drillsLog)).rejects.toThrow(
      /daemon gone/,
    );
    expect(fake.calls).toEqual(Array<string>(5).fill("start ogmios"));
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(true);
  });

  it("counts a container that is no longer paused as restored", async () => {
    const drillsLog = join(scratch(), "drills.ndjson");
    writeFileSync(
      `${drillsLog}.restore.json`,
      JSON.stringify({
        drill: "pause-postgres",
        service: "postgres",
        undo: "unpause",
      }),
    );
    const fake = composeFake();
    fake.deps = {
      ...fake.deps,
      compose: async (args) => {
        fake.calls.push(args.join(" "));
        return { code: 1, stderr: "Container postgres is not paused" };
      },
    };
    await finishOwedRestore(fake.deps, drillsLog);
    expect(fake.calls).toEqual(["unpause postgres"]);
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(false);
  });
});

describe("restart-cardano-node", () => {
  it("owes a start while the restart is in flight, so a killed run never leaves the node stopped", async () => {
    const dir = scratch();
    const drillsLog = join(dir, "drills.ndjson");
    const owedDuringRestart: unknown[] = [];
    const calls: string[] = [];
    let clock = 0;
    const deps: ChaosDeps = {
      compose: async (args) => {
        calls.push(args.join(" "));
        if (args[0] === "restart")
          owedDuringRestart.push(
            JSON.parse(readFileSync(`${drillsLog}.restore.json`, "utf8")),
          );
        return { code: 0, stderr: "" };
      },
      sleep: async (ms) => {
        clock += ms;
      },
      signalGroup: () => {},
      environ: () => undefined,
      fetchNode: async () => ({}),
      now: () => clock,
      waitReady: async () => ({ graced: false }),
      supervisorEvents: () => [],
      readiness: async () => [],
    };
    const summary = await runDrills({
      drills: selectDrills(drillCatalogue([]), ["restart-cardano-node"]),
      runDir: dir,
      pidDir: dir,
      drillsLog,
      deps,
    });
    expect(owedDuringRestart).toEqual([
      { drill: "restart-cardano-node", service: "cardano-node", undo: "start" },
    ]);
    expect(calls).toEqual(["restart cardano-node", "start cardano-node"]);
    expect(existsSync(`${drillsLog}.restore.json`)).toBe(false);
    expect(summary.failures).toEqual([]);
  });
});
