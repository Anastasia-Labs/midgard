import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";

import { startWatcherNativeChainSync } from "midgard-watcher/native-chain-sync";
import { expect, it } from "vitest";

import {
  config,
  INTERSECTION,
  readIdentityFixture,
  spawnFixture,
  waitFor,
} from "./native-chain-sync.config.js";

const readyScript = `
const {createHash}=require('node:crypto');
require('node:readline').createInterface({input:process.stdin}).once('line', line=>{
 const s=JSON.parse(line);process.stdout.write(JSON.stringify({authorityNodeId:s.authorityNodeId,currentTip:{kind:'point',blockHash:'bb'.repeat(32),blockNo:'10',slot:'101'},genesisIdentitySha256:s.genesisIdentitySha256,kind:'ready',network:s.network,networkMagic:s.networkMagic,operation:s.operation,schemaVersion:s.schemaVersion,selectedIntersection:s.intersection,socketPath:s.socketPath,startupDigest:createHash('sha256').update(line).digest('hex')},(_k,v)=>v&&typeof v==='object'&&!Array.isArray(v)?Object.fromEntries(Object.entries(v).sort()):v)+'\\n');
});
process.on('SIGTERM',()=>{});setInterval(()=>{},1000);
`;

it.each(["forged_ready", "missing_operation"])(
  "joins rejected %s startup before exposing its original error",
  async (mode) => {
    let child!: ChildProcessWithoutNullStreams;
    let closed = false;
    await expect(
      startWatcherNativeChainSync({
        binaryPath: "/test/native",
        watcherConfig: config(),
        intersection: INTERSECTION,
        startupTimeoutMs: 2000,
        onEvent: async () => {},
        unsafeReadIdentityFileForTest: readIdentityFixture,
        unsafeSpawnForTest: () => {
          child = spawnFixture(mode)();
          child.once("close", () => {
            closed = true;
          });
          return child;
        },
      }),
    ).rejects.toThrow(mode === "missing_operation" ? "missing" : "identity");
    expect(closed).toBe(true);
    expect(child.signalCode).toBe("SIGKILL");
  },
);

it("joins an aborted startup and preserves cancellation without an orphan", async () => {
  const controller = new AbortController();
  let child!: ChildProcessWithoutNullStreams;
  let closed = false;
  const started = startWatcherNativeChainSync({
    binaryPath: "/test/native",
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    signal: controller.signal,
    onEvent: async () => {},
    unsafeReadIdentityFileForTest: readIdentityFixture,
    unsafeSpawnForTest: () => {
      child = spawnFixture("no_ready")();
      child.once("close", () => {
        closed = true;
      });
      return child;
    },
  });
  const reason = new Error("owned startup retired");
  const rejected = expect(started).rejects.toBe(reason);
  await waitFor(() => child !== undefined);
  controller.abort(reason);
  await rejected;
  expect(closed).toBe(true);
  expect(child.signalCode).not.toBeNull();
});

it("preserves an actual revocation failure over concurrent startup cancellation after drainage", async () => {
  const controller = new AbortController();
  const failure = new Error("owned revocation failure");
  let child!: ChildProcessWithoutNullStreams;
  let closed = false;
  const started = startWatcherNativeChainSync({
    binaryPath: "/test/native",
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    signal: controller.signal,
    onEvent: async () => {},
    onAuthorityRevoked: () => {
      throw failure;
    },
    unsafeReadIdentityFileForTest: readIdentityFixture,
    unsafeSpawnForTest: () => {
      child = spawnFixture("no_ready")();
      child.once("close", () => {
        closed = true;
      });
      return child;
    },
  });
  const rejected = expect(started).rejects.toMatchObject({
    message: "native read lifetime revocation failed",
    cause: failure,
  });
  await waitFor(() => child !== undefined);
  controller.abort(new Error("owned stop"));
  await rejected;
  expect(closed).toBe(true);
});

it("waits for a SIGTERM-resistant helper's physical close after force kill", async () => {
  let child!: ChildProcessWithoutNullStreams;
  let closed = false;
  const runtime = await startWatcherNativeChainSync({
    binaryPath: "/test/native",
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    onEvent: async () => {},
    unsafeReadIdentityFileForTest: readIdentityFixture,
    unsafeSpawnForTest: () => {
      child = spawn(process.execPath, ["-e", readyScript], {
        stdio: ["pipe", "pipe", "pipe"],
      });
      child.once("close", () => {
        closed = true;
      });
      return child;
    },
  });
  await runtime.close();
  await runtime.done;
  expect(closed).toBe(true);
  expect(child.signalCode).toBe("SIGKILL");
}, 10000);
