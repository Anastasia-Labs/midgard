import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  createOperatorPause,
  OPERATOR_NODE_SERVICE,
  type OperatorNodeControl,
} from "./operator-pause.js";

const directories: string[] = [];
const holders: number[] = [];
afterEach(() => {
  for (const pid of holders.splice(0))
    try {
      process.kill(pid);
    } catch {
      // Already stopped.
    }
});
afterAll(() =>
  Promise.all(
    directories.map((directory) =>
      rm(directory, { recursive: true, force: true }),
    ),
  ),
);

/** A process standing in for the journey's test process. */
const holderProcess = () => {
  const holder = spawn(
    process.execPath,
    ["-e", "setInterval(() => {}, 1000)"],
    {
      stdio: "ignore",
    },
  );
  holders.push(holder.pid!);
  return holder;
};

/**
 * Operator-2's node as a Compose service that `stop` and `start` flip, whose
 * out-of-process start command writes `started` to a file.
 */
const startedNode = async () => {
  const directory = await mkdtemp(join(tmpdir(), "operator-release-"));
  directories.push(directory);
  const started = join(directory, "started");
  let running = true;
  const control: OperatorNodeControl = {
    name: "drill-op2/midgard-node",
    stop: () => {
      running = false;
      return Promise.resolve();
    },
    start: () => {
      running = true;
      return Promise.resolve();
    },
    startCommand: [
      process.execPath,
      "-e",
      `require("node:fs").writeFileSync(${JSON.stringify(started)}, "started")`,
    ],
    runningServices: () =>
      Promise.resolve(running ? ["kupo", OPERATOR_NODE_SERVICE] : ["kupo"]),
    readReadiness: () =>
      Promise.resolve(
        running
          ? {
              reachable: true,
              httpStatus: 200,
              body: { ready: true, reasons: [] },
            }
          : { reachable: false },
      ),
  };
  return { started, control };
};

const killed = (holder: ReturnType<typeof holderProcess>) =>
  new Promise<void>((resolve) => {
    holder.once("exit", () => resolve());
    holder.kill("SIGKILL");
  });

const appears = async (path: string, withinMs: number) => {
  const deadline = performance.now() + withinMs;
  while (!existsSync(path) && performance.now() < deadline) await pause(50);
  return existsSync(path);
};

describe("a run killed while it holds operator-2's node", () => {
  it("starts the node again when the holding process is killed", async () => {
    const holder = holderProcess();
    const node = await startedNode();
    const held = createOperatorPause(node.control, {
      resumeTimeoutMs: 1000,
      holderPid: holder.pid!,
    });
    await held.hold();
    await killed(holder);
    expect(await appears(node.started, 10_000)).toBe(true);
  });

  it("leaves the node alone once the run resumed or released it", async () => {
    const holder = holderProcess();
    const node = await startedNode();
    const held = createOperatorPause(node.control, {
      resumeTimeoutMs: 1000,
      holderPid: holder.pid!,
    });
    await held.hold();
    await held.release();
    await killed(holder);
    expect(await appears(node.started, 3000)).toBe(false);
  });
});
