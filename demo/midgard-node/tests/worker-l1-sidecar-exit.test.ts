/**
 * A node worker thread that opened the L1 node transport leaves no sidecar
 * behind when its run ends. A sidecar is a child of the thread that spawned
 * it, and only that thread collects its exit: one still running when the
 * thread ends stays a defunct child of the node until the node exits, one per
 * run (425 after 65 minutes of a devnet's commit ticks).
 */
import { execFileSync } from "node:child_process";
import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";
import { Worker } from "node:worker_threads";

import {
  LEDGER_HANDLER,
  writeFakeSidecar,
} from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { Effect } from "effect";
import { describe, expect, it, onTestFinished } from "vitest";

import { runCommitWorkerInThread } from "../src/fibers/block-commitment.run-commit-worker-in-thread.js";

const RUNS = 5;
const MAGIC = 42;
const WORKER = new URL(
  "./helpers/l1-transport-worker-run.mjs",
  import.meta.url,
);
const compiled = (file: string): string =>
  new URL(`../dist/${file}`, import.meta.url).href;

type RunEnd = "after-ready" | "at-once";

/** The fake sidecar a run's transport spawns, and the worker data naming it. */
const workerDataFor = async (worker: "commit" | "settlement", ends: RunEnd) => {
  const directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-worker-sidecar-")),
  );
  onTestFinished(() => rm(directory, { recursive: true, force: true }));
  return {
    worker,
    ends,
    compiled: compiled(
      worker === "commit"
        ? "commit-block-header.post-commit-worker-output.js"
        : "settlement.run-settlement-worker.js",
    ),
    node: {
      binaryPath: await writeFakeSidecar({
        path: join(directory, "midgard-l1-node-transport"),
        handlerModule: LEDGER_HANDLER,
        options: { magic: MAGIC },
      }),
      socketPath: join(directory, "node.socket"),
      networkMagic: MAGIC,
    },
  };
};

/** This process's children as `ps` states them (`Z` is defunct), by pid. */
const children = (): Map<string, string> =>
  new Map(
    execFileSync("ps", ["-A", "-o", "ppid=,pid=,stat=,comm="], {
      encoding: "utf8",
    })
      .split("\n")
      .map((line) => line.trim().split(/\s+/))
      .filter(
        ([ppid, , , command]) =>
          ppid === process.pid.toString() && command !== "ps",
      )
      .map(([, pid, state, command]) => [pid!, `${state} ${command}`]),
  );

/**
 * The children started since `before` that are left defunct, once every one
 * of them has stopped running.
 */
const defunctChildrenSince = async (
  before: ReadonlyMap<string, string>,
): Promise<string[]> => {
  for (;;) {
    const started = [...children()]
      .filter(([pid]) => !before.has(pid))
      .map(([, state]) => state);
    if (started.every((state) => state.startsWith("Z"))) return started;
    await delay(20);
  }
};

describe.each<RunEnd>(["after-ready", "at-once"])(
  "a worker run that ends %s",
  (ends) => {
    it("leaves no defunct sidecar when the commit worker's parent terminates it on its output", async () => {
      const workerData = await workerDataFor("commit", ends);
      const before = children();
      for (let run = 0; run < RUNS; run += 1) {
        const output = await Effect.runPromise(
          runCommitWorkerInThread<{ readonly type: string }, string>({
            workerEntry: WORKER,
            // The compiled modules resolve their imports as production
            // does, not through Vitest's source condition.
            workerOptions: { workerData, execArgv: [] },
            takeOutput: (message) => message.type,
            releaseLedgerLease: () => Promise.resolve(),
          }),
        );
        expect(output).toBe("NothingToCommitOutput");
      }
      expect(await defunctChildrenSince(before)).toStrictEqual([]);
    });

    it("leaves no defunct sidecar when the settlement worker's program fails", async () => {
      const workerData = await workerDataFor("settlement", ends);
      const before = children();
      for (let run = 0; run < RUNS; run += 1) {
        const worker = new Worker(WORKER, { workerData, execArgv: [] });
        const reports: Array<{ readonly state: string }> = [];
        worker.on("message", (report) => reports.push(report));
        const exitCode = await new Promise<number>((resolve, reject) => {
          worker.once("error", reject);
          worker.once("exit", resolve);
        });
        expect([exitCode, reports.map((report) => report.state)]).toStrictEqual(
          [1, ["error"]],
        );
      }
      expect(await defunctChildrenSince(before)).toStrictEqual([]);
    });
  },
);
