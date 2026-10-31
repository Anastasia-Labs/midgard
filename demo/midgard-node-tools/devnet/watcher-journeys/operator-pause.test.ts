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

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  composeOperatorNodeControl,
  createOperatorPause,
  OPERATOR_NODE_SERVICE,
  type OperatorNodeControl,
  type OperatorNodeReadiness,
} from "./operator-pause.js";

const directories: string[] = [];
const servers: number[] = [];
afterEach(() => {
  vi.unstubAllEnvs();
  for (const pid of servers.splice(0))
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

/**
 * Operator-2's node as a scripted Compose service: `stop` and `start` flip
 * it, and every read answers from the script for its current state.
 */
const scriptedNode = ({
  stopLeaves = "nothing",
  catchUp = [],
}: {
  /** What still runs after `stop`: a Compose service, or a stray listener. */
  stopLeaves?: "nothing" | "service" | "listener";
  /**
   * `/readyz` answers after `start`, in order; the last one repeats. An
   * error is a read that fails other than by a refused connection.
   */
  catchUp?: readonly (OperatorNodeReadiness | Error)[];
}) => {
  const calls: string[] = [];
  let state: "running" | "stopped" = "running";
  let answered = 0;
  const control: OperatorNodeControl = {
    name: "drill-op2/midgard-node",
    stop: () => {
      calls.push("stop");
      state = "stopped";
      return Promise.resolve();
    },
    start: () => {
      calls.push("start");
      state = "running";
      return Promise.resolve();
    },
    runningServices: () =>
      Promise.resolve(
        state === "running" || stopLeaves === "service"
          ? ["kupo", OPERATOR_NODE_SERVICE]
          : ["kupo"],
      ),
    readReadiness: () => {
      if (state === "stopped")
        return Promise.resolve(
          stopLeaves === "listener"
            ? { reachable: true, httpStatus: 503, body: {} }
            : { reachable: false },
        );
      const answer = catchUp[Math.min(answered, catchUp.length - 1)] ?? ready();
      answered += 1;
      return answer instanceof Error
        ? Promise.reject(answer)
        : Promise.resolve(answer);
    },
  };
  return { control, calls, state: () => state };
};

const ready = (): OperatorNodeReadiness => ({
  reachable: true,
  httpStatus: 200,
  body: { ready: true, reasons: [] },
});
const holding = (...reasons: string[]): OperatorNodeReadiness => ({
  reachable: true,
  httpStatus: 503,
  body: { ready: false, reasons },
});
/** A node whose operator the drill's convictions left out of the active set. */
const outOfSet = (
  follower: "caught_up" | "catching_up",
  ...reasons: string[]
): OperatorNodeReadiness => ({
  reachable: true,
  httpStatus: 503,
  body: {
    ready: false,
    reasons:
      follower === "caught_up"
        ? reasons
        : [...reasons, "l1_follower_catching_up"],
    l1Follower: { atTip: follower === "caught_up" },
  },
});
const reset = Object.assign(new Error("socket hang up"), {
  code: "ECONNRESET",
});

describe("operator-2 pause", () => {
  it("stops the node and proves it cannot sign before every signature", async () => {
    const node = scriptedNode({});
    const pause = createOperatorPause(node.control, { resumeTimeoutMs: 1000 });
    await expect(pause.assertStopped()).rejects.toThrow(
      /not held stopped by this drill/u,
    );
    const stopped = await pause.hold();
    expect(stopped).toMatchObject({
      node: "drill-op2/midgard-node",
      runningServices: ["kupo"],
      readyz: "connection_refused",
    });
    expect(pause.held()).toBe(true);
    await expect(pause.assertStopped()).resolves.toMatchObject({
      readyz: "connection_refused",
    });
    expect(node.calls).toEqual(["stop"]);
  });

  it("refuses when Compose still runs the node or its /readyz still answers", async () => {
    await expect(
      createOperatorPause(scriptedNode({ stopLeaves: "service" }).control, {
        resumeTimeoutMs: 1000,
      }).hold(),
    ).rejects.toThrow(/still running; refusing to sign as operator-2/u);
    await expect(
      createOperatorPause(scriptedNode({ stopLeaves: "listener" }).control, {
        resumeTimeoutMs: 1000,
      }).hold(),
    ).rejects.toThrow(/\/readyz still answers \(HTTP 503\)/u);
  });

  it("refuses to sign once something restarted the held node", async () => {
    const node = scriptedNode({});
    const pause = createOperatorPause(node.control, { resumeTimeoutMs: 1000 });
    await pause.hold();
    await node.control.start();
    await expect(pause.assertStopped()).rejects.toThrow(/still running/u);
  });

  it("resumes the node it held and waits until it is ready", async () => {
    const node = scriptedNode({
      catchUp: [
        { reachable: false },
        holding("landed_block_invalid"),
        holding("l1_follower_catching_up"),
        ready(),
      ],
    });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 5000,
      pollIntervalMs: 1,
    });
    await pause.hold();
    const resumed = await pause.resume();
    expect(resumed?.observedReasons).toEqual([
      ["landed_block_invalid"],
      ["l1_follower_catching_up"],
      [],
    ]);
    expect(node.state()).toBe("running");
    expect(pause.held()).toBe(false);
    // Resuming is once: a second call reports the same proof, and no
    // signature is allowed afterwards.
    await expect(pause.resume()).resolves.toBe(resumed);
    await expect(pause.assertStopped()).rejects.toThrow(/not held stopped/u);
    await expect(pause.hold()).rejects.toThrow(/already resumed/u);
    expect(node.calls).toEqual(["stop", "start"]);
  });

  it("resumes a node whose operator the convictions removed, once it caught up with no hold", async () => {
    const node = scriptedNode({
      catchUp: [
        outOfSet("catching_up", "operator_removed"),
        outOfSet("caught_up", "operator_removed", "landed_block_invalid"),
        outOfSet("caught_up", "operator_removed"),
      ],
    });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 5000,
      pollIntervalMs: 1,
    });
    await pause.hold();
    await expect(pause.resume()).resolves.toMatchObject({
      terminal: "operator_removed",
      observedReasons: [
        ["operator_removed", "l1_follower_catching_up"],
        ["operator_removed", "landed_block_invalid"],
        ["operator_removed"],
      ],
    });
  });

  it("resumes a node re-registered but not yet active, once it caught up", async () => {
    const node = scriptedNode({
      catchUp: [outOfSet("caught_up", "operator_not_yet_active")],
    });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 5000,
      pollIntervalMs: 1,
    });
    await pause.hold();
    await expect(pause.resume()).resolves.toMatchObject({
      terminal: "awaiting_activation",
    });
  });

  it("never takes a removed node that still holds the fault, or has not caught up, for resumed", async () => {
    for (const answer of [
      outOfSet("caught_up", "operator_removed", "landed_block_invalid"),
      outOfSet("catching_up", "operator_removed"),
    ]) {
      const node = scriptedNode({ catchUp: [answer] });
      const pause = createOperatorPause(node.control, {
        resumeTimeoutMs: 50,
        pollIntervalMs: 5,
      });
      await pause.hold();
      await expect(pause.resume()).rejects.toThrow(/not ready within 50 ms/u);
    }
  });

  it("keeps waiting through a reset or aborted /readyz read while the node restarts", async () => {
    const node = scriptedNode({
      catchUp: [reset, new DOMException("timed out", "TimeoutError"), ready()],
    });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 5000,
      pollIntervalMs: 1,
    });
    await pause.hold();
    await expect(pause.resume()).resolves.toMatchObject({
      terminal: "ready",
      observedReasons: [[]],
    });
  });

  it("never starts a node this drill did not stop", async () => {
    const node = scriptedNode({});
    await expect(
      createOperatorPause(node.control, { resumeTimeoutMs: 1000 }).resume(),
    ).resolves.toBeUndefined();
    expect(node.calls).toEqual([]);
  });

  it("releases a held node unproven when the run ends early, and only one it held", async () => {
    const idle = scriptedNode({});
    await createOperatorPause(idle.control).release();
    expect(idle.calls).toEqual([]);
    const node = scriptedNode({
      catchUp: [holding("landed_block_invalid")],
    });
    const pause = createOperatorPause(node.control);
    await pause.hold();
    await pause.release();
    await pause.release();
    expect(node.state()).toBe("running");
    expect(pause.held()).toBe(false);
    await expect(pause.assertStopped()).rejects.toThrow(/not held stopped/u);
    expect(node.calls).toEqual(["stop", "start"]);
  });

  it("refuses a resumed node that took a drill block for its own", async () => {
    const node = scriptedNode({
      catchUp: [holding("landed_block_own_journal_mismatch")],
    });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 5000,
      pollIntervalMs: 1,
    });
    await pause.hold();
    await expect(pause.resume()).rejects.toThrow(
      /landed_block_own_journal_mismatch: it did not replay the drill's blocks as foreign/u,
    );
  });

  it("times out on a resumed node that never becomes ready", async () => {
    const node = scriptedNode({ catchUp: [holding("landed_block_invalid")] });
    const pause = createOperatorPause(node.control, {
      resumeTimeoutMs: 50,
      pollIntervalMs: 5,
    });
    await pause.hold();
    await expect(pause.resume()).rejects.toThrow(
      /not ready within 50 ms \(reasons seen: \[\["landed_block_invalid"\]\]\)/u,
    );
  });

  it("rejects a resume timeout that is not positive", () => {
    for (const resumeTimeoutMs of [0, -1, Number.POSITIVE_INFINITY])
      expect(() =>
        createOperatorPause(scriptedNode({}).control, { resumeTimeoutMs }),
      ).toThrow(/finite and positive/u);
  });
});

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

describe("operator-2 node over Compose", () => {
  it("stops and starts only the node's service, by project name alone", async () => {
    const root = await mkdtemp(join(tmpdir(), "operator-pause-"));
    directories.push(root);
    const bin = join(root, "bin");
    await mkdir(bin);
    const port = await freePort();
    const log = join(root, "docker.log");
    const pidFile = join(root, "node.pid");
    // A Docker CLI that runs one project's `midgard-node` as a real HTTP
    // listener on `port`, logs every invocation, and refuses anything that
    // could remove the node or its volumes.
    await writeFile(
      join(bin, "docker"),
      `#!${process.execPath}
const { spawn } = require("node:child_process");
const { appendFileSync, existsSync, readFileSync, rmSync, writeFileSync } = require("node:fs");
const args = process.argv.slice(2);
appendFileSync(${JSON.stringify(log)}, JSON.stringify(args) + "\\n");
if (Object.keys(process.env).some((name) => name.startsWith("COMPOSE_")) || process.cwd() !== "/") {
  console.error("compose would load another project's files");
  process.exit(125);
}
const [compose, p, project, verb, ...rest] = args;
if (compose !== "compose" || p !== "-p") process.exit(125);
if (project !== "drill-op2") {
  console.error("no such project " + project);
  process.exit(1);
}
const alive = () => {
  if (!existsSync(${JSON.stringify(pidFile)})) return false;
  try { process.kill(Number(readFileSync(${JSON.stringify(pidFile)}, "utf8")), 0); return true; } catch { return false; }
};
if (verb === "ps" && rest.join(" ") === "--status running --services") {
  console.log("kupo");
  if (alive()) console.log("midgard-node");
} else if (verb === "stop" && rest.join(" ") === "midgard-node") {
  if (alive()) {
    const pid = Number(readFileSync(${JSON.stringify(pidFile)}, "utf8"));
    process.kill(pid);
    for (let i = 0; i < 200; i += 1) {
      try { process.kill(pid, 0); } catch { break; }
      Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 10);
    }
  }
  rmSync(${JSON.stringify(pidFile)}, { force: true });
} else if (verb === "start" && rest.join(" ") === "midgard-node") {
  if (!alive()) {
    const child = spawn(process.execPath, ["-e", ${JSON.stringify(
      `require("node:http").createServer((request, response) => { response.writeHead(request.url === "/readyz" ? 200 : 404, { "content-type": "application/json" }); response.end(JSON.stringify({ ready: true, reasons: [] })); }).listen(${port.toString()}, "127.0.0.1", () => process.stdout.write("up"));`,
    )}], { detached: true, stdio: ["ignore", "pipe", "ignore"] });
    child.stdout.once("data", () => {
      writeFileSync(${JSON.stringify(pidFile)}, String(child.pid));
      child.unref();
      process.exit(0);
    });
    return;
  }
} else {
  console.error("unsupported compose verb " + JSON.stringify(rest.length === 0 ? verb : [verb, ...rest]));
  process.exit(125);
}
`,
    );
    await chmod(join(bin, "docker"), 0o755);
    vi.stubEnv("PATH", `${bin}:${process.env.PATH}`);
    vi.stubEnv("COMPOSE_FILE", join(root, "compose.yaml"));
    vi.stubEnv("COMPOSE_PROJECT_NAME", "another-stack");

    const control = composeOperatorNodeControl({
      composeProject: "drill-op2",
      endpoint: `http://127.0.0.1:${port.toString()}`,
    });
    await control.start();
    servers.push(Number(await readFile(pidFile, "utf8")));
    const pause = createOperatorPause(control, {
      resumeTimeoutMs: 10_000,
      pollIntervalMs: 20,
    });
    // A running node is never taken for stopped.
    expect(await control.runningServices()).toEqual(["kupo", "midgard-node"]);
    await expect(control.readReadiness()).resolves.toEqual({
      reachable: true,
      httpStatus: 200,
      body: { ready: true, reasons: [] },
    });

    await expect(pause.hold()).resolves.toMatchObject({
      runningServices: ["kupo"],
      readyz: "connection_refused",
    });
    await expect(pause.resume()).resolves.toMatchObject({
      node: "drill-op2/midgard-node",
      observedReasons: [[]],
    });
    servers.push(Number(await readFile(pidFile, "utf8")));

    const verbs = (await readFile(log, "utf8"))
      .trim()
      .split("\n")
      .map((line) => (JSON.parse(line) as string[]).slice(3).join(" "));
    expect(new Set(verbs)).toEqual(
      new Set([
        "start midgard-node",
        "stop midgard-node",
        "ps --status running --services",
      ]),
    );

    await expect(
      composeOperatorNodeControl({
        composeProject: "another-stack",
        endpoint: `http://127.0.0.1:${port.toString()}`,
      }).stop(),
    ).rejects.toThrow(
      /docker compose -p another-stack stop midgard-node failed: no such project another-stack/u,
    );
  });
});
