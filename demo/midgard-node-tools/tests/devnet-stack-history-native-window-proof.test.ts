import { randomUUID } from "node:crypto";
import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncEvent,
} from "midgard-watcher/native-chain-sync";
import { afterEach, expect, it } from "vitest";

import {
  createHistoryWindowSealer,
  HistoryWindowRefusal,
} from "../src/devnet-stack/history-native-window-proof.js";
import { windowFixture } from "./helpers/history-native-window-fixture.js";

const fixtures: ReturnType<typeof windowFixture>[] = [];
afterEach(async () => {
  await Promise.all(fixtures.splice(0).map((f) => f.close()));
});
const setup = async (
  last: number,
  onForward?: (event: WatcherNativeChainSyncEvent) => Promise<void>,
) => {
  const f = windowFixture(last);
  fixtures.push(f);
  const sealer = createHistoryWindowSealer({
    actor: {
      role: "history-recorder",
      runId: randomUUID(),
      deploymentFingerprint: "11".repeat(32),
      codeStamp: "22".repeat(32),
      serviceSpecsDigest: "33".repeat(32),
      attemptId: randomUUID(),
    },
    directories: f.directories,
    watcherConfig: f.watcherConfig,
    binaryPath: f.binaryPath,
  });
  let opening: WatcherNativeChainSyncEvent | undefined;
  const native = await f.startMain(async (event) => {
    if (opening === undefined) opening = event;
    else await onForward?.(event);
  });
  await expect.poll(() => opening, { timeout: 10000 }).toBeDefined();
  if (opening === undefined) throw Error("synthetic main intersection absent");
  const target = f.points[last];
  if (target === undefined) throw Error("synthetic target absent");
  return { f, sealer, native, opening, target };
};
it("captures every actual raw row of the configured2160 window and seals the same coherent physical range", async () => {
  const { f, sealer, opening, target } = await setup(2161);
  const before = f.directories.map((d) =>
    readFileSync(join(d, "canonical", "100.json"), "utf8"),
  );
  await sealer.capture(opening, target, 20000);
  const seal = sealer.seal(10000);
  expect(seal).not.toBeNull();
  expect(seal).toMatchObject({
    recoveryWindow: 2160,
    rowCount: 2160,
    first: f.points[2],
    last: target,
  });
  if (seal === null) throw Error("full seal absent");
  expect(sealer.revalidate(seal.sealId, seal.generation)).toEqual(seal);
  expect(
    f.directories.map((d) =>
      readFileSync(join(d, "canonical", "100.json"), "utf8"),
    ),
  ).toEqual(before);
  expect(readFileSync(f.startsPath, "utf8").trim().split("\n")).toHaveLength(2);
});
it("refuses an earlier raw block contradiction even when the latest point and retained rows agree", async () => {
  const { f, sealer, opening, target } = await setup(2161);
  const old = f.events[100];
  if (old === undefined) throw Error("old source row absent");
  const changed = [...f.events];
  changed[100] = { ...old, rawBlockCbor: old.rawBlockCbor.slice(0, -2) + "81" };
  writeFileSync(f.eventsPath, JSON.stringify(changed));
  await expect(sealer.capture(opening, target, 20000)).rejects.toBeInstanceOf(
    HistoryWindowRefusal,
  );
  expect(sealer.seal(10000)).toBeNull();
});
it("refuses a missing required predecessor before another native child or retained write", async () => {
  const { f, sealer, opening, target } = await setup(2161);
  rmSync(join(f.directories[0], "canonical", "1.json"));
  const before = f.directories.map((d) =>
    readFileSync(join(d, "canonical", "2.json"), "utf8"),
  );
  await expect(sealer.capture(opening, target, 20000)).rejects.toMatchObject({
    reason: "missing retained evidence",
  });
  expect(readFileSync(f.startsPath, "utf8").trim().split("\n")).toHaveLength(1);
  expect(
    f.directories.map((d) =>
      readFileSync(join(d, "canonical", "2.json"), "utf8"),
    ),
  ).toEqual(before);
  expect(sealer.seal(10000)).toBeNull();
});
it("requires auxiliary source identity to match the actual main authority", async () => {
  const { f, opening, target } = await setup(5);
  const other = createHistoryWindowSealer({
    actor: {
      role: "history-recorder",
      runId: randomUUID(),
      deploymentFingerprint: "11".repeat(32),
      codeStamp: "22".repeat(32),
      serviceSpecsDigest: "33".repeat(32),
      attemptId: randomUUID(),
    },
    directories: f.directories,
    watcherConfig: {
      ...f.watcherConfig,
      l1: {
        ...f.watcherConfig.l1,
        source: {
          ...f.watcherConfig.l1.source,
          authorityNodeId: "another-node",
        },
      },
    },
    binaryPath: f.binaryPath,
  });
  await expect(other.capture(opening, target, 10000)).rejects.toMatchObject({
    reason: "native capture authority mismatch",
  });
  expect(other.seal(10000)).toBeNull();
});
it("proves a genesis-short window using bounded readonly Origin rather than a writer fallback", async () => {
  const { f, sealer, opening, target } = await setup(5);
  await sealer.capture(opening, target, 10000);
  expect(sealer.seal(10000)).toMatchObject({
    rowCount: 6,
    recoveryWindow: 2160,
    first: f.points[0],
    last: target,
  });
  expect(
    readFileSync(f.startsPath, "utf8")
      .trim()
      .split("\n")
      .map((s) => JSON.parse(s)),
  ).toEqual([
    { kind: "point", blockHash: target.blockHash, slot: target.slot },
    { kind: "origin" },
  ]);
});
it("preserves a pinned full range while actual admitted appends promote the live window", async () => {
  let consume: (
    event: WatcherNativeChainSyncEvent,
  ) => Promise<void> = async () => {};
  const { f, sealer, opening, target } = await setup(2161, (event) =>
    consume(event),
  );
  await sealer.capture(opening, target, 20000);
  const old = sealer.seal(10000);
  if (old === null) throw Error("old full range absent");
  let updated = false;
  consume = async (event) => {
    if (event.kind !== "roll_forward")
      throw Error("unexpected synthetic rollback");
    sealer.prepareAppend(event);
    expect(sealer.revalidate(old.sealId, old.generation)).toBeNull();
    f.retain(event);
    sealer.completeAppend();
    updated = true;
  };
  f.send(f.appendBlock(false));
  await expect.poll(() => updated, { timeout: 10000 }).toBe(true);
  expect(sealer.revalidate(old.sealId, old.generation)).toEqual(old);
  const next = sealer.seal(10000);
  if (next === null) throw Error("promoted full range absent");
  expect(next).toMatchObject({
    first: f.points[3],
    last: f.points[2162],
    rowCount: 2160,
    sourceEpoch: old.sourceEpoch,
  });
  expect(next.promotionDigest).not.toBe(old.promotionDigest);
  expect(sealer.revalidate(old.sealId, old.generation)).toBeNull();
  rmSync(join(f.directories[0], "canonical", "3.json"));
  expect(sealer.revalidate(next.sealId, next.generation)).toBeNull();
});
it("revokes a seal on an actual native rollback before an asynchronous callback completes", async () => {
  let release = () => {};
  let callbackActive = false;
  const blocked = new Promise<void>((resolve) => {
    release = resolve;
  });
  const { f, sealer, opening, target } = await setup(5, async () => {
    callbackActive = true;
    await blocked;
  });
  try {
    await sealer.capture(opening, target, 10000);
    const seal = sealer.seal(10000);
    if (seal === null) throw Error("rollback fixture seal absent");
    f.send({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_backward",
      point: { kind: "origin" },
      tip: { kind: "origin" },
    });
    await expect.poll(() => callbackActive, { timeout: 10000 }).toBe(true);
    expect(sealer.revalidate(seal.sealId, seal.generation)).toBeNull();
  } finally {
    release();
  }
});
it("holds stale/expired pins and actual main process loss without a durable snapshot escape", async () => {
  const { sealer, native, opening, target } = await setup(5);
  await sealer.capture(opening, target, 10000);
  const first = sealer.seal(10000);
  if (first === null) throw Error("first pin absent");
  const second = sealer.seal(1);
  if (second === null) throw Error("second pin absent");
  expect(sealer.revalidate(first.sealId, first.generation)).toBeNull();
  expect(sealer.revalidate(second.sealId, "ff".repeat(32))).toBeNull();
  await new Promise((resolve) => setTimeout(resolve, 10));
  expect(sealer.revalidate(second.sealId, second.generation)).toBeNull();
  const current = sealer.seal(10000);
  if (current === null) throw Error("current pin absent");
  await native.close();
  expect(sealer.revalidate(current.sealId, current.generation)).toBeNull();
  expect(sealer.seal(10000)).toBeNull();
});

it("bounds a stalled actual source capture and keeps it transient and unknown", async () => {
  const { f, sealer, opening, target } = await setup(5);
  f.stall();
  let error: unknown;
  try {
    await sealer.capture(opening, target, 1000);
  } catch (failure) {
    error = failure;
  }
  expect(error).toBeInstanceOf(Error);
  expect(error).not.toBeInstanceOf(HistoryWindowRefusal);
  expect(sealer.seal(10000)).toBeNull();
});
