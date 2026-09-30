import "./da-bond-pool-committee-process.the-committee-node-2.js";

import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  createDaBondPoolCommitteeObserver,
  daBondPoolCommitteeExpectedView,
  type DaBondPoolCommitteeProcess,
  spawnDaBondPoolCommitteeNode,
} from "./da-bond-pool-committee-process.js";
import { answer } from "./da-bond-pool-committee-process.the-journey-committee-node.js";

describe("the real committee node process's output capture (P27(2))", () => {
  it("reads the responder's stderr reports and its stdout action reports from a spawned child", async () => {
    const logDirectory = await mkdtemp(join(tmpdir(), "da-bond-pool-spawn-"));
    const unavailable = (headerHash: string) =>
      JSON.stringify({
        event: "availability_responder",
        headerHash,
        status: "unavailable",
      });
    const acted = (headerHash: string) =>
      JSON.stringify({
        event: "availability_responder",
        headerHash,
        action: "publish",
        status: "pending",
      });
    // A JSON event on stdout that is not a responder report, as the node's
    // chain sync writes; it must never reach the responder lines.
    const chainSync = JSON.stringify({ event: "l1_chain_sync_caught_up" });
    const flag = join(logDirectory, "second-round");
    // As the node does: failed/unavailable to stderr, actions to stdout.
    // stdin is ignored, so the second round waits on a flag file.
    const script = [
      'const { existsSync } = require("node:fs");',
      'process.on("SIGTERM", () => process.exit(0));',
      `process.stdout.write(${JSON.stringify(`${chainSync}\n`)});`,
      `process.stderr.write(${JSON.stringify(`${unavailable("b1")}\n`)});`,
      `process.stdout.write(${JSON.stringify(`${acted("b1")}\n`)});`,
      "let second = false;",
      "setInterval(() => {",
      `  if (second || !existsSync(${JSON.stringify(flag)})) return;`,
      "  second = true;",
      `  process.stderr.write(${JSON.stringify(`${unavailable("b2")}\n`)});`,
      `  process.stdout.write(${JSON.stringify(`${acted("b2")}\n`)});`,
      "}, 10);",
    ].join("\n");
    let spawned: DaBondPoolCommitteeProcess | undefined;
    const observer = createDaBondPoolCommitteeObserver({
      spawn: async () => {
        const node = await spawnDaBondPoolCommitteeNode({
          argv: [process.execPath, "-e", script],
          env: { PATH: process.env.PATH ?? "" },
          cwd: logDirectory,
          logDirectory,
          apiUrl: "http://127.0.0.1:1",
        });
        spawned = node;
        // No HTTP API in the child; the capture is what is under test.
        return { ...node, readyz: async () => answer([], Date.now()) };
      },
      checkDaemons: () => {},
      submitterUtxos: async () => ["fund#0"],
      expectedView: async () =>
        daBondPoolCommitteeExpectedView(
          { state: "bonded", backing: 100n },
          100n,
        ),
      env: {},
      syncTimeoutMs: 1_000,
      startTimeoutMs: 5_000,
      pollMs: 10,
      stopBoundMs: 5_000,
      // A 20 ms settle window.
      nodeCadence: {
        pollIntervalMs: 10,
        confirmationDepth: 1,
        slotLengthMs: 5,
        activeSlotsCoeff: 1,
      },
    });
    try {
      await observer.start();
      // Wait on the log files, which the capture does not feed, so a lost
      // in-memory line shows in the observation, not as a timeout here.
      const logged = (stream: "stdout" | "stderr") =>
        readFile(
          join(
            logDirectory,
            `committee-${spawned!.pid.toString()}.${stream}.log`,
          ),
          "utf8",
        ).catch(() => "");
      const awaitLogged = async (stdout: string, stderr: string) => {
        const deadline = Date.now() + 10_000;
        while (
          (await logged("stdout")) !== stdout ||
          (await logged("stderr")) !== stderr
        ) {
          if (Date.now() >= deadline)
            throw new Error("The child never logged its lines");
          await new Promise((resolveWait) => setTimeout(resolveWait, 10));
        }
      };
      await awaitLogged(
        `${chainSync}\n${acted("b1")}\n`,
        `${unavailable("b1")}\n`,
      );
      const observation = await observer.observe();
      expect(observation.process.pid).toBe(spawned!.pid);
      expect(observation.process.availabilityResponder).toEqual([
        unavailable("b1"),
        acted("b1"),
      ]);
      // Output after an observation reaches the next one, and only it.
      await writeFile(flag, "");
      await awaitLogged(
        `${chainSync}\n${acted("b1")}\n${acted("b2")}\n`,
        `${unavailable("b1")}\n${unavailable("b2")}\n`,
      );
      const second = await observer.observe();
      expect(second.process.availabilityResponder).toEqual([
        unavailable("b2"),
        acted("b2"),
      ]);
      await expect(observer.stop()).resolves.toMatchObject({
        exitCode: 0,
        killed: false,
      });
    } finally {
      await observer.teardown();
      await rm(logDirectory, { recursive: true, force: true });
    }
  });
});
