import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { existsSync, linkSync, readFileSync } from "node:fs";
import { createServer } from "node:net";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath } from "node:url";

import { ensureArtifacts } from "../../src/devnet-stack/deploy.js";
import {
  codeStamp,
  runtimeDistTargets,
} from "../../src/devnet-stack/dist-freshness.js";
import { historyChildClient } from "../../src/devnet-stack/history-child-client.js";
import { createHistoryRoleRegistry } from "../../src/devnet-stack/history-role-registry.js";
import { loadIdentities } from "../../src/devnet-stack/identities.js";
import { makeLayout, readRunEnv } from "../../src/devnet-stack/layout.js";
import { watcherServiceSpecs } from "../../src/devnet-stack/watcher.js";

const runDir = process.argv[2];
const mode = process.argv[3];
if (runDir === undefined || (mode !== "pending" && mode !== "backoff"))
  throw Error("owned cancellation fixture arguments absent");
const layout = makeLayout(runDir);
const run = readRunEnv(layout);
const config = JSON.parse(readFileSync(layout.watcherRuntimeConfig, "utf8"));
const server = createServer();
const sockets = new Set<import("node:net").Socket>();
let connected = false;
server.on("connection", (socket) => {
  connected = true;
  sockets.add(socket);
  socket.once("close", () => sockets.delete(socket));
  // An actual node-to-client connection with no handshake answer.
  socket.on("data", () => undefined);
});
const socketPath = config.l1.source.chainSync.socketPath;
if (mode === "pending")
  await new Promise<void>((resolve) => server.listen(socketPath, resolve));
else {
  // Preserve an actual socket inode but remove its listener: native validation
  // succeeds, and the actual Unix dial reports node_handshake_failed.
  const privatePath = `${socketPath}.bind`;
  await new Promise<void>((resolve) => server.listen(privatePath, resolve));
  linkSync(privatePath, socketPath);
  await new Promise<void>((resolve) => server.close(() => resolve()));
}
const context = {
  layout,
  run,
  identities: loadIdentities(layout),
  artifacts: ensureArtifacts(layout),
};
const specs = watcherServiceSpecs(context, {
  txHash: "00".repeat(32),
  outputIndex: 0,
}).filter((s) => s.historyReadiness !== undefined);
const recorder = specs.find(
  (s) => s.historyReadiness?.role === "history-recorder",
);
if (recorder === undefined || recorder.historyReadiness === undefined)
  throw Error("actual recorder specification absent");
const registry = createHistoryRoleRegistry({
  runDir,
  runtimeCodeStamp: () => codeStamp(runtimeDistTargets(layout)),
  serviceSpecs: specs,
  pidDir: join(layout.state, "services"),
  events: layout.supervisorEvents,
  serviceLog: layout.serviceLog,
});
const attempt = registry.prepare(recorder);
if (attempt === null) throw Error("actual recorder attempt absent");
const child = spawn(recorder.command, recorder.args, {
  cwd: fileURLToPath(new URL("../../", import.meta.url)),
  env: { ...process.env, ...attempt.env },
  stdio: ["ignore", "pipe", "pipe", "pipe"],
});
if (child.pid === undefined) throw Error("owned recorder PID absent");
const recorderPid = child.pid;
const pipe = child.stdio[3];
if (!(pipe instanceof Duplex)) throw Error("owned fd3 absent");
let output = "";
for (const stream of [child.stdout, child.stderr])
  stream?.on("data", (bytes) => {
    output = (output + bytes.toString()).slice(-16384);
  });
let joined = false;
let pipeClosed = false;
pipe.once("close", () => {
  pipeClosed = true;
});
const done = new Promise<{
  code: number | null;
  signal: NodeJS.Signals | null;
}>((resolve, reject) => {
  child.once("error", reject);
  child.once("close", (code, signal) => {
    joined = true;
    resolve({ code, signal });
  });
});
const client = historyChildClient({
  actor: {
    role: "history-recorder",
    runId: run.runId,
    deploymentFingerprint: recorder.historyReadiness.deploymentFingerprint,
    ...attempt.scope,
    attemptId: attempt.attemptId,
    childPid: recorderPid,
  },
  pipe,
});
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));
const children = () => {
  const path = `/proc/${recorderPid}/task/${recorderPid}/children`;
  if (!existsSync(path)) return [];
  try {
    return readFileSync(path, "utf8")
      .trim()
      .split(/\s+/u)
      .filter(Boolean)
      .map(Number);
  } catch (error) {
    if (
      error !== null &&
      typeof error === "object" &&
      "code" in error &&
      error.code === "ENOENT"
    )
      return [];
    throw error;
  }
};
const helpers = new Set<number>();
let cleanupKill = false;
try {
  const until = performance.now() + 20000;
  while (
    mode === "pending"
      ? !connected
      : !output.includes('"event":"native_node_unavailable"')
  ) {
    if (joined || performance.now() >= until)
      throw Error(`native ${mode} stage not reached: ${output}`);
    for (const pid of children()) helpers.add(pid);
    await sleep(10);
  }
  for (const pid of children()) helpers.add(pid);
  if (mode === "pending")
    assert.ok(
      helpers.size > 0,
      "actual native helper spawned before pending shutdown",
    );
  const reply = await client.request("seal", null, 500);
  assert.notEqual(
    reply,
    null,
    "early actual recorder fd3 responds before native readiness",
  );
  assert.equal(reply?.offer, null, "unready recorder cannot offer readiness");
  const stoppedAt = performance.now();
  process.kill(recorderPid, "SIGTERM"); // Only the recorder, never its helper/process group.
  const knownAtSignal = new Set(helpers);
  while (!joined && performance.now() - stoppedAt < 3000) {
    for (const pid of children()) helpers.add(pid);
    await sleep(5);
  }
  const result = joined ? await done : null;
  assert.equal(
    [...helpers].some((pid) => !knownAtSignal.has(pid)),
    false,
    "no later helper observed during 5ms shutdown sampling",
  );
  assert.notEqual(
    result,
    null,
    `recorder ${mode} shutdown must physically join without waiting 60s startup or test kill`,
  );
  assert.equal(
    result?.code,
    0,
    `owned drained cancellation is clean: ${output}`,
  );
  assert.equal(
    result?.signal,
    null,
    "command exits voluntarily after native/FD3 drain",
  );
  assert.equal(pipeClosed, true, "fd3 physically closed before command join");
  for (const pid of helpers)
    assert.throws(
      () => process.kill(pid, 0),
      /ESRCH/u,
      "every observed native helper is physically gone",
    );
  console.log("PASS actual compiled recorder cancellation", {
    mode,
    elapsedMs: Math.round(performance.now() - stoppedAt),
    helpers: helpers.size,
    cleanupKill,
  });
} finally {
  client.close();
  registry.close();
  if (!joined) {
    cleanupKill = true;
    for (const pid of children()) helpers.add(pid);
    for (const pid of helpers) {
      try {
        process.kill(pid, "SIGKILL");
      } catch {
        /* Owned helper already gone. */
      }
    }
    child.kill("SIGKILL");
  }
  await done;
  for (const socket of sockets) socket.destroy();
  if (mode === "pending")
    await new Promise<void>((resolve) => server.close(() => resolve()));
}
