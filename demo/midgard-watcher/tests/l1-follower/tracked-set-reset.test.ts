/**
 * The watcher over a tracked-set reset (lane SI, rulings SI-R2 and SI-R3):
 * the start that resets the store tells the decision driver as a rewind to
 * the origin; the tx-input sweep waits for the replay to reach the tip, so
 * the inputs stored at ingest (class C) survive it; and the driver makes no
 * pass until the follower in this process has finished its store start.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type {
  ChainSyncEvent,
  ChainSyncStream,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import {
  decodeBlock,
  type FactStore,
  openSqliteFactStore,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import {
  encodeUtxoAnswer,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFollowerRuntime,
  watcherSecurityParameter,
} from "../../src/l1-follower/follower-runtime.js";
import type { WatcherObservationAuthority } from "../../src/l1-follower/observation.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import { WATCHER_TX_INPUTS_TABLE } from "../../src/l1-follower/tables.js";
import {
  createWatcherDecisionDriver,
  WATCHER_FOLLOWER_NOT_STARTED,
  watcherFollowerStarted,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import { okValue } from "../support/l1-follower-raw-reads-fixture.js";
import {
  commitTx,
  initTx,
  queueState,
  SIM_HUB_ORACLE_ONE_SHOT,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";
import {
  closeRemovedHeaders,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

const D = SIM_WATCHER_DEPLOYMENT;
const RECOVERY_DEPTH = 4;
const K = watcherSecurityParameter(RECOVERY_DEPTH);
const RELEASE_DEPTH = 2;
const SOURCE_ID = "tracked-set-reset";
const ONE_SHOT: SimUtxo = {
  outRef: SIM_HUB_ORACLE_ONE_SHOT,
  output: { address: simUniverse().untrackedAddress, lovelace: 5_000_000n },
};
const AUTHORITY: WatcherObservationAuthority = {
  authorityDigest: "a1".repeat(32),
  deploymentFingerprint: "a2".repeat(32),
  protocolScriptHashes: {
    hubOracleMint: D.hubOracleMint,
    stateQueueSpend: D.stateQueueSpend,
    stateQueueMint: D.stateQueueMint,
    correctionLockSpend: D.correctionLockSpend,
    fraudProofSpend: D.fraudProofSpend,
    fraudProofMint: D.fraudProofMint,
    referenceScriptAuthMint: "b1".repeat(28),
    availabilityChallengeSpend: D.availabilityChallengeSpend,
    availabilityChallengeMint: D.availabilityChallengeMint,
    daBondPoolSpend: D.daBondPoolSpend,
    daBondPoolMint: "b2".repeat(28),
    daAttestationMint: D.daAttestationMint,
    availabilityChallengeOpenWithdraw: "b3".repeat(28),
    availabilityChallengeSettleWithdraw: "b4".repeat(28),
    availabilityChallengeCloseWithdraw: "b5".repeat(28),
    availabilityChallengeTimeoutWithdraw: "b6".repeat(28),
  },
};

const scratch = mkdtempSync(join(tmpdir(), "watcher-tracked-set-reset-"));
const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
  await closeRemovedHeaders();
});
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

const until = async (what: string, holds: () => boolean, ms = 20_000) => {
  const deadline = Date.now() + ms;
  while (!holds()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};

/** A store at the start that drops its tracked-set record: the next start resets it. */
const dropRecord = (store: FactStore) =>
  store.transaction("write", (tx) =>
    tx.query("DELETE FROM l1_follower_tracked_set"),
  );

/** The protocol init, then `blocks` blocks, a header commit in the first. */
const chainEvents = (blocks: number) => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const events: ChainSyncEvent[] = [chain.forward([initTx(D)]).event];
  for (let i = 0; i < blocks; i += 1) {
    const state = i === 0 ? queueState(chain, D) : null;
    events.push(
      chain.forward(state === null ? [] : [commitTx(state, D)]).event,
    );
  }
  return events;
};

const applyAll = async (
  store: FactStore,
  events: readonly ChainSyncEvent[],
) => {
  for (const event of events) {
    if (event.kind !== "roll_forward") throw new Error("forward events only");
    expect((await store.applyBlock(decodeBlock(event.block))).kind).toBe(
      "applied",
    );
  }
};

/** Stub bridge, availability and user-event history counting what the driver asks of them. */
const collaborators = () => {
  const seen = {
    bridgeInvalidations: 0,
    availabilityInvalidations: 0,
    recoveryPreparations: 0,
    dispatched: [] as string[],
    historyRollbacks: [] as unknown[],
  };
  let head = {
    blockHash: SIM_ORIGIN.point.hash.toString("hex"),
    slot: String(SIM_ORIGIN.point.slot),
    blockNo: String(SIM_ORIGIN.height),
    pointId: "",
  };
  const history = {
    read: () => ({
      status: "ready" as const,
      currentPoint: head,
      headCursor: head,
      generation: 0,
    }),
    advanceThrough: (point: typeof head) => {
      head = point;
      return Promise.resolve();
    },
    handleRollback: (point: { slot: string; blockHash: string }) => {
      seen.historyRollbacks.push(point);
      head = { ...head, slot: point.slot, blockHash: point.blockHash };
      return Promise.resolve();
    },
  };
  const bridge = {
    prepareForRecovery: (observation: { observationDigest: string }) => {
      seen.recoveryPreparations += 1;
      return Promise.resolve({
        observationDigest: observation.observationDigest,
        decisionDigests: [],
        target: null,
      });
    },
    recoverExisting: () => Promise.resolve(0),
    reconcileAndDispatch: (observation: { observationDigest: string }) => {
      seen.dispatched.push(observation.observationDigest);
      return Promise.resolve({
        observationDigest: observation.observationDigest,
        decisionDigests: [],
        target: null,
      });
    },
    invalidateForRollback: () => {
      seen.bridgeInvalidations += 1;
    },
    beforeHistoryAdvance: () => undefined,
  };
  const availability = {
    reconcile: () => Promise.resolve(),
    invalidateForRollback: () => {
      seen.availabilityInvalidations += 1;
    },
  };
  return { seen, history, bridge, availability, head: () => head };
};

describe("the decision driver over a tracked-set reset", () => {
  it("hears the reset as a rewind to the origin: invalidates, re-arms recovery and rolls the history back to O", async () => {
    const path = join(scratch, "driver-reset.db");
    const store = openSqliteFactStore({
      ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
      path,
    });
    const c = collaborators();
    const driver = createWatcherDecisionDriver(
      {
        store,
        onFollowerChange: () => () => undefined,
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        history: c.history as never,
        retryDelayMs: 10,
      },
      { atTip: () => true, started: () => true },
    );
    opened.push(async () => {
      await driver.close();
      await store.close();
    });
    const events = chainEvents(6);
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events);
    driver.wake();
    await until("the first decision", () => driver.readiness().length === 0);
    await driver.idle();
    const before = driver.current().observationDigest;
    expect(c.seen.recoveryPreparations).toBe(1);
    expect(BigInt(c.head().slot)).toBeGreaterThan(
      BigInt(SIM_ORIGIN.point.slot),
    );

    // A start that finds the store unrecorded resets it.
    await dropRecord(store);
    expect(await store.start()).toMatchObject({
      kind: "ready",
      cursor: null,
      trackedSet: { kind: "reset", cause: "unrecorded" },
      replaying: true,
    });
    expect(c.seen.bridgeInvalidations).toBe(1);
    expect(c.seen.availabilityInvalidations).toBe(1);
    expect(driver.status().rewinds).toBe(1);
    expect(driver.inclusion()).toBeNull();

    // The replay from the origin: the next pass re-prepares recovery, the
    // history went back to O, and the decision is the one before.
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events);
    driver.wake();
    await until(
      "the decision after the replay",
      () => c.seen.recoveryPreparations === 2,
    );
    await driver.idle();
    expect(c.seen.historyRollbacks).toEqual([
      {
        kind: "point",
        blockHash: SIM_ORIGIN.point.hash.toString("hex"),
        slot: String(SIM_ORIGIN.point.slot),
      },
    ]);
    expect(driver.readiness()).toEqual([]);
    expect(driver.current().observationDigest).toBe(before);
    expect(c.seen.dispatched.at(-1)).toBe(before);
  });

  it("makes no pass while the follower's start is held on a store with a cursor, and passes once it completes", async () => {
    const path = join(scratch, "driver-locked.db");
    const events = chainEvents(6);
    const held = 4;
    // Another process holds the writer lease of a store that has a cursor.
    const holder = openSqliteFactStore({
      ...projectionStoreOptions(
        [watcherProjection(D)],
        {
          securityParameter: K,
          trackedSet: {
            addresses: new Set(),
            paymentCredentials: new Set(),
            policies: new Set(),
          },
        },
        "sqlite",
      ),
      path,
    });
    let holderOpen = true;
    opened.push(async () => {
      if (holderOpen) await holder.close();
    });
    expect((await holder.start()).kind).toBe("ready");
    expect((await holder.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(holder, events.slice(0, held));

    const state = { acked: held };
    const transport = {
      openChainSync: (): ChainSyncStream => {
        let position = state.acked;
        let closed = false;
        return {
          opened: Promise.resolve(),
          next: async () => {
            for (;;) {
              if (closed) return undefined;
              if (position < events.length) return events[(position += 1) - 1];
              await new Promise((resolve) => setTimeout(resolve, 2));
            }
          },
          ack: (seq: bigint) => {
            const index = events.findIndex((event) => event.seq === seq);
            if (index >= 0) state.acked = Math.max(state.acked, index + 1);
          },
          close: () => {
            closed = true;
            return Promise.resolve();
          },
        } as unknown as ChainSyncStream;
      },
      withLedgerState: (
        _at: unknown,
        use: (session: { query: () => Promise<Uint8Array> }) => unknown,
      ) => use({ query: () => Promise.resolve(encodeUtxoAnswer([ONE_SHOT])) }),
      // The scripted node is always reachable: no readiness change to report.
      readiness: { ready: true, nodeToClientVersion: 32784 },
      onReadiness: (): (() => void) => () => undefined,
      close: () => Promise.resolve(),
    } as unknown as L1NodeTransport;
    const follower = openWatcherFollowerRuntime({
      deployment: D,
      storePath: path,
      automaticRecoveryMaxDepth: RECOVERY_DEPTH,
      origin: {
        origin: SIM_ORIGIN.point,
        hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
      },
      node: { binaryPath: "unused", socketPath: "unused", networkMagic: 42 },
      walletAddresses: [],
      unsafeTransportForTest: transport,
    });
    const c = collaborators();
    const driver = createWatcherDecisionDriver(
      {
        store: follower.store,
        onFollowerChange: (listener) => follower.onChange(() => listener()),
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        retryDelayMs: 10,
      },
      {
        atTip: () => follower.status()?.atTip === true,
        started: () => watcherFollowerStarted(follower.status()),
      },
    );
    opened.push(async () => {
      await driver.close();
      await follower.close();
    });

    await until("the follower to wait out the held lease", () =>
      JSON.stringify(follower.status() ?? {}).includes("store_locked"),
    );
    driver.wake();
    await driver.idle();
    // The store has a cursor, but this process's follower has not started.
    expect((await follower.store.cursor())?.point.slot).toBeGreaterThan(
      SIM_ORIGIN.point.slot,
    );
    expect(driver.readiness().map(({ reason }) => reason)).toEqual([
      WATCHER_FOLLOWER_NOT_STARTED,
    ]);
    expect(c.seen.dispatched).toEqual([]);
    expect(c.seen.recoveryPreparations).toBe(0);

    await holder.close();
    holderOpen = false;
    await until(
      "a pass once the follower started",
      () => c.seen.dispatched.length > 0 && driver.readiness().length === 0,
    );
    expect(watcherFollowerStarted(follower.status())).toBe(true);
    expect(c.seen.recoveryPreparations).toBe(1);
  });
});

describe("tx inputs over a tracked-set reset", () => {
  it("keeps the inputs stored at ingest through the replay with no ledger read, and sweeps again once the replay reached the tip", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    const store = r.h.store;
    const inputs = (txHash: string) =>
      r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", txHash);
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
    // A row of a tx no fact names: what the sweep is for.
    const stray = "ef".repeat(32);
    await store.transaction("write", (tx) =>
      tx.query(
        `INSERT INTO ${WATCHER_TX_INPUTS_TABLE} (tx_hash, out_tx_hash, out_index, output_cbor) VALUES (?, ?, ?, ?)`,
        [Buffer.from(stray, "hex"), Buffer.alloc(32, 0x01), 0, Buffer.of(0xa0)],
      ),
    );
    const blocks = r.h.chain.rawBlocks();

    await dropRecord(store);
    expect(await store.start()).toMatchObject({
      trackedSet: { kind: "reset" },
      replaying: true,
    });
    // The node can serve none of the deep parents: any read would fail.
    r.ledger.down(true);
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    // Mid-replay, before the commit is back in l1_txs.
    expect((await store.applyBlock(decodeBlock(blocks[0]!))).kind).toBe(
      "applied",
    );
    expect(await store.txByHash(Buffer.from(r.commitHash, "hex"))).toBeNull();
    await r.resolver.step();
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
    expect(await inputs(stray)).toBe(1);
    for (const raw of blocks.slice(1))
      expect((await store.applyBlock(decodeBlock(raw))).kind).toBe("applied");
    const unresolved = await r.resolver.step();
    expect(unresolved.map(({ txHash }) => txHash)).not.toContain(r.commitHash);
    expect(await inputs(stray)).toBe(1);
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs).toEqual([]);
    expect(
      raw.transaction.resolvedInputs.map(({ outRef }) => outRef),
    ).toContain(r.operatorUtxo);

    // The first report at the tip ends the replay: the sweep runs again.
    expect(await store.endTrackedSetReplay()).toBe(true);
    await r.resolver.step();
    expect(await inputs(stray)).toBe(0);
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
  });
});
