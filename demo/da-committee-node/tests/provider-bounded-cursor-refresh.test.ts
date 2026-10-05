import { join } from "node:path";

import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { afterEach, expect, it } from "vitest";

import { readAvailabilityCursor } from "../src/l1/availability-cursor.js";
import { FileChainSyncConsumerCursorStore } from "../src/l1/provider.file-chain-sync-consumer-cursor-store.js";
import { FileChainSyncCursorStore } from "../src/l1/provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "../src/l1/provider.local-node-chain-authority.js";
import { LocalNodeStateQueueProvider } from "../src/l1/provider.local-node-state-queue-provider.js";
import { OgmiosChainSyncEventSource } from "../src/l1/provider.parse-fixture-chain-sync-events.js";
import type {
  ChainSyncCursor,
  ChainSyncCursorStore,
} from "../src/l1/provider.parse-persisted-chain-sync-state.js";
import { createCommitteeTickRunner } from "../src/tick-runner.js";
import { tempDir } from "./helpers.js";
import {
  chainPoint,
  cursorServer,
} from "./provider-bounded-cursor-refresh.server.js";

const limits = {
  requestRefusalMs: 10_000,
  httpResponseBytes: 100_000,
  webSocketMessageBytes: 100_000,
  rawUtxos: 32,
};
const cleanup: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of cleanup.splice(0).reverse()) await close();
});
const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((r) => {
    resolve = r;
  });
  return { promise, resolve };
};
const scoped = (signal?: AbortSignal) => {
  const scope = createDaAvailabilityReadScope({
    attemptTimeoutMs: 10_000,
    signal,
  });
  cleanup.push(() => scope.close());
  return { scope, limits, maxEvents: 32 };
};
const fixture = async (
  wrap?: (store: ChainSyncCursorStore) => ChainSyncCursorStore,
) => {
  const server = await cursorServer();
  cleanup.push(server.close);
  const dir = await tempDir();
  const store = new FileChainSyncCursorStore(
    join(dir, "cursor.json"),
    "ab".repeat(32),
  );
  const consumed = new FileChainSyncConsumerCursorStore(
    join(dir, "consumed.json"),
    "ab".repeat(32),
  );
  const point = {
    network: "Custom",
    slot: 10,
    blockHash: chainPoint(10).id,
    providerSource: "chain-sync:local",
    observedAt: new Date(0).toISOString(),
  };
  const seed: ChainSyncCursor = { sequence: 0, rollbackGeneration: 0, point };
  await store.append({ direction: "roll_forward", point }, seed);
  await consumed.save(seed);
  const authority = new LocalNodeChainAuthority(
    "local",
    "Custom",
    new OgmiosChainSyncEventSource(
      server.url,
      "Custom",
      "local",
      undefined,
      424242,
    ),
    wrap?.(store) ?? store,
  );
  const provider = new LocalNodeStateQueueProvider(
    authority,
    [
      {
        currentChainPoint: async () => point,
        fetchStateQueueNodes: async () => [],
      },
    ],
    ["local"],
    consumed,
  );
  return { server, authority, provider, store, consumed, seed };
};

it("refreshes the actual durable cursor while the committee tick stays blocked, and gates an unconsumed rollback", async () => {
  const f = await fixture();
  const blocked = deferred();
  const entered = deferred();
  cleanup.push(() => blocked.resolve());
  const runner = createCommitteeTickRunner({
    tick: async () => {
      entered.resolve();
      await blocked.promise;
      return {
        scannedHeaders: 0,
        signedHeaders: 0,
        reconciledHeaders: 0,
        skippedHeaders: 0,
        payloadFetches: [],
        errors: [],
      };
    },
    runAvailabilityResponse: async () => {},
    runRetention: async () => {},
    latestL1View: () => undefined,
    latestL1ProgressAtMs: () => undefined,
    setRetentionReadiness: () => {},
    l1ViewFatalMs: 60000,
    startedAtMs: Date.now(),
    nowMs: Date.now,
    write: () => {},
  });
  const tick = runner.runTick();
  await entered.promise;
  f.server.setChain([chainPoint(10), chainPoint(11)]);
  const budget = scoped();
  const forward = await f.provider.refreshAvailabilityCursor(budget);
  expect(forward.point.slot).toBe(11);
  expect((await f.store.load())?.point.slot).toBe(11);
  expect((await f.consumed.load())?.point.slot).toBe(10);
  f.server.setChain([chainPoint(10), chainPoint(11, 1)]);
  await expect(f.provider.refreshAvailabilityCursor(budget)).rejects.toThrow(
    "durable consumption of its rollback generation",
  );
  const rolled = await f.provider.currentChainSyncCursor();
  expect(rolled.rollbackGeneration).toBe(1);
  expect((await f.consumed.load())?.rollbackGeneration).toBe(0);
  await expect(
    readAvailabilityCursor(f.provider, budget.scope),
  ).rejects.toThrow("durable consumption");
  // Only the existing consumer acknowledgement clears the fence.
  await f.provider.acknowledgeChainSyncCursor(rolled);
  expect(await readAvailabilityCursor(f.provider, budget.scope)).toEqual(
    rolled,
  );
  blocked.resolve();
  await tick;
});

it("caps total events without claiming the still-unreached tip", async () => {
  const f = await fixture();
  f.server.setChain([10, 11, 12, 13].map((n) => chainPoint(n)));
  const budget = { ...scoped(), maxEvents: 2 };
  await expect(f.provider.refreshAvailabilityCursor(budget)).rejects.toThrow(
    "event limit",
  );
  expect((await f.store.load())?.point.slot).toBe(12);
  expect(
    (await f.provider.refreshAvailabilityCursor({ ...budget, maxEvents: 1 }))
      .point.slot,
  ).toBe(13);
});

it("cancels the actual pending WebSocket request and appends no late cursor", async () => {
  const f = await fixture();
  f.server.setChain([chainPoint(10), chainPoint(11)]);
  const held = f.server.hold("nextBlock");
  const closed = f.server.nextClose();
  const controller = new AbortController();
  const budget = scoped(controller.signal);
  const refresh = f.provider.refreshAvailabilityCursor(budget);
  const rejected = expect(refresh).rejects.toThrow("test cursor cancellation");
  await held;
  controller.abort(new Error("test cursor cancellation"));
  await rejected;
  await closed;
  expect(await f.store.load()).toEqual(f.seed);
});

it("expires queued work without any later append", async () => {
  const appendStarted = deferred();
  const appendRelease = deferred();
  const f = await fixture((store) => ({
    ...store,
    load: store.load.bind(store),
    replay: store.replay.bind(store),
    cursorAt: store.cursorAt.bind(store),
    intersectionPoints: store.intersectionPoints?.bind(store),
    append: async (event, cursor) => {
      appendStarted.resolve();
      await appendRelease.promise;
      await store.append(event, cursor);
    },
  }));
  cleanup.push(appendRelease.resolve);
  f.server.setChain([chainPoint(10), chainPoint(11)]);
  const first = f.authority.refreshToTip(scoped());
  await appendStarted.promise;
  const controller = new AbortController();
  const queued = f.authority.refreshToTip(scoped(controller.signal));
  const refused = expect(queued).rejects.toThrow("queued expired");
  controller.abort(new Error("queued expired"));
  await refused;
  const reads = f.server.requests();
  appendRelease.resolve();
  await first;
  // Joining the existing writer ensures the expired queued callback ran.
  expect((await f.authority.refreshToTip(scoped())).point.slot).toBe(11);
  expect((await f.store.load())?.sequence).toBe(1);
  expect(f.server.requests()).toBeGreaterThan(reads);
});

it("awaits a started durable append even when its scope expires", async () => {
  const appendStarted = deferred();
  const appendRelease = deferred();
  const f = await fixture((store) => ({
    ...store,
    load: store.load.bind(store),
    replay: store.replay.bind(store),
    cursorAt: store.cursorAt.bind(store),
    intersectionPoints: store.intersectionPoints?.bind(store),
    append: async (event, cursor) => {
      appendStarted.resolve();
      await appendRelease.promise;
      await store.append(event, cursor);
    },
  }));
  cleanup.push(appendRelease.resolve);
  f.server.setChain([chainPoint(10), chainPoint(11)]);
  const controller = new AbortController();
  let settled = false;
  const refresh = f.authority.refreshToTip(scoped(controller.signal));
  void refresh.then(
    () => {
      settled = true;
    },
    () => {
      settled = true;
    },
  );
  const rejected = expect(refresh).rejects.toThrow("append scope expired");
  await appendStarted.promise;
  controller.abort(new Error("append scope expired"));
  await Promise.resolve();
  await Promise.resolve();
  expect(settled).toBe(false);
  appendRelease.resolve();
  await rejected;
  expect((await f.store.load())?.point.slot).toBe(11);
});
