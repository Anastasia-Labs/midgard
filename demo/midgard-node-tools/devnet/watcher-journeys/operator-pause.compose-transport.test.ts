import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import {
  chmod,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { createServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  composeOperatorNodeControl,
  createOperatorPause,
} from "./operator-pause.js";

const directories: string[] = [];
const holders: number[] = [];
afterEach(() => {
  vi.unstubAllEnvs();
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

/** A loopback port nothing listens on. */
const freePort = () =>
  new Promise<number>((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      server.close(() =>
        typeof address === "object" && address !== null
          ? resolve(address.port)
          : reject(new Error("no port")),
      );
    });
  });

/**
 * A process standing in for the journey's test process, whose end is what
 * operator-2's release guard watches for.
 */
const holderProcess = () => {
  const holder = spawn(
    process.execPath,
    ["-e", "setInterval(() => {}, 1000)"],
    { stdio: "ignore" },
  );
  holders.push(holder.pid!);
  return holder;
};

const killed = (holder: ReturnType<typeof holderProcess>) =>
  new Promise<void>((resolve) => {
    holder.once("exit", () => resolve());
    holder.kill("SIGKILL");
  });

describe("operator-2 node over Compose, when Docker gives no answer", () => {
  it("reads Compose's running services again when its CLI exits without an answer, and still refuses a running node, leaving no release guard armed", async () => {
    const root = await mkdtemp(join(tmpdir(), "operator-pause-"));
    directories.push(root);
    const bin = join(root, "bin");
    await mkdir(bin);
    const port = await freePort();
    const psCount = join(root, "ps-count");
    const running = join(root, "running");
    const starts = join(root, "starts");
    // A Docker CLI whose daemon fails the first `ps` it is asked, then
    // lists the project's running services; nothing listens on `port`.
    await writeFile(
      join(bin, "docker"),
      `#!${process.execPath}
const { existsSync, readFileSync, writeFileSync } = require("node:fs");
const [compose, p, project, verb] = process.argv.slice(2);
if (compose !== "compose" || p !== "-p" || project !== "drill-op2") process.exit(125);
if (verb === "stop") process.exit(0);
if (verb === "start") {
  require("node:fs").appendFileSync(${JSON.stringify(starts)}, "start\\n");
  process.exit(0);
}
if (verb !== "ps") process.exit(125);
const count = existsSync(${JSON.stringify(psCount)}) ? Number(readFileSync(${JSON.stringify(psCount)}, "utf8")) : 0;
writeFileSync(${JSON.stringify(psCount)}, String(count + 1));
if (count === 0) {
  console.error("Error response from daemon: i/o timeout");
  process.exit(1);
}
console.log("kupo");
if (existsSync(${JSON.stringify(running)})) console.log("midgard-node");
`,
    );
    await chmod(join(bin, "docker"), 0o755);
    vi.stubEnv("PATH", `${bin}:${process.env.PATH}`);
    const holder = holderProcess();
    const startCount = () =>
      existsSync(starts)
        ? readFile(starts, "utf8").then((log) => log.split("\n").length - 1)
        : Promise.resolve(0);
    const pause = createOperatorPause(
      composeOperatorNodeControl({
        composeProject: "drill-op2",
        endpoint: `http://127.0.0.1:${port.toString()}`,
      }),
      { resumeTimeoutMs: 1000, pollIntervalMs: 20, holderPid: holder.pid! },
    );
    await expect(pause.hold()).resolves.toMatchObject({
      runningServices: ["kupo"],
      readyz: "connection_refused",
    });
    expect(Number(await readFile(psCount, "utf8"))).toBe(2);
    // A daemon that answers that the node runs is a refusal, read once.
    await writeFile(running, "");
    await expect(pause.assertStopped()).rejects.toThrow(
      /still running; refusing to sign as operator-2/u,
    );
    expect(Number(await readFile(psCount, "utf8"))).toBe(3);
    // The test ends as a run does, starting the node it held, which disarms
    // the hold's release guard: the holder's end then starts nothing more.
    await pause.release();
    const started = await startCount();
    expect(started).toBe(1);
    await killed(holder);
    // The guard reads its holder once a second.
    await sleep(3000);
    expect(await startCount()).toBe(started);
  });
});
