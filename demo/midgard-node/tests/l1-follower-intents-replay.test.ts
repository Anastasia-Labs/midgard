/**
 * The node intent stage while the follower store replays a tracked-set
 * reset (plan §8.3 S6 over §5's reset), on a SQLite and a Postgres store:
 *
 * - a reset rewinds the facts to the origin and replays them; until the
 *   replay reaches the node tip the facts are a prefix of the chain, and an
 *   intent recorded before the reset may look unwanted there. The stage
 *   takes no decision from that view: no resend and no abandon, one hold
 *   under `tracked_set_changed`;
 * - a pass that a reset overtook (the store reported no replay at the
 *   pass's start) reads the flag again before each predicate read and waits
 *   under the same reason;
 * - once the replay reached the node tip and the flag cleared, the next
 *   pass decides as usual: the wanted intent is resent, the unwanted one
 *   abandoned.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import {
  applyChainSyncEvent,
  currentViewIn,
  decodeTransaction,
  type FactStore,
  FOLLOWER_TRACKED_SET_CHANGED,
  intentJournalProjection,
  openPostgresFactStore,
  openSqliteFactStore,
  projectionStoreOptions,
  readIntentEventsIn,
  recordIntentIn,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  encodeSimTx,
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../src/l1-state-queue/index.js";
import { nodeFamilyPredicate } from "../src/services/l1-follower.intent-predicates.js";
import { createNodeIntentStage } from "../src/services/l1-follower.intents.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  nodeDatum,
  QUEUE_ADDRESS,
  queueOutput,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "node-intents-replay-"));
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
}, DROP_ALL_TIMEOUT_MS);

const K = 6;
const GENESIS = "00".repeat(28);
const EMPTY: TrackedSet = {
  addresses: new Set(),
  paymentCredentials: new Set(),
  policies: new Set(),
};
/** An address the first store did not track: reopening with it resets. */
const GAINED = `61${"ab".repeat(28)}`;
const PROJECTIONS = [
  stateQueueProjection(SIM_QUEUE_CONFIG),
  intentJournalProjection,
];

type Opener = (trackedSet: TrackedSet) => Promise<FactStore>;

/** Opens and starts a store on one database, under `trackedSet`. */
const opener = async (dialect: "sqlite" | "postgres"): Promise<Opener> => {
  const where =
    dialect === "sqlite"
      ? join(scratch, `${String(Math.random()).slice(2)}.db`)
      : await databases.create();
  return async (trackedSet) => {
    const options = projectionStoreOptions(
      PROJECTIONS,
      { securityParameter: K, trackedSet },
      dialect,
    );
    const store =
      dialect === "sqlite"
        ? openSqliteFactStore({ ...options, path: where })
        : openPostgresFactStore({
            ...options,
            connection: { connectionString: where },
          });
    opened.push(store);
    return store;
  };
};

const started = async (store: FactStore) => {
  const result = await store.start();
  if (result.kind !== "ready")
    throw new Error(`store start: ${JSON.stringify(result)}`);
  return result;
};

const apply = async (store: FactStore, event: ChainSyncEvent) => {
  const result = await applyChainSyncEvent(store, event);
  if (result.result.kind !== "applied")
    throw new Error(
      `apply: ${JSON.stringify(result.result, (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v))}`,
    );
};

const closeOne = async (store: FactStore) => {
  opened.splice(opened.indexOf(store), 1);
  await store.close();
};

const transport = () => {
  const sent: Buffer[] = [];
  return {
    sent,
    hasTx: () => Promise.resolve(false),
    submit: (bytes: Uint8Array) => {
      sent.push(Buffer.from(bytes));
      return Promise.resolve({ accepted: true } as const);
    },
    withLedgerState: () => Promise.reject(new Error("no ledger state")),
  };
};

const stageOn = (store: FactStore, sink: ReturnType<typeof transport>) =>
  createNodeIntentStage({
    store,
    transport: sink,
    securityParameter: K,
    seededAddresses: [],
    wanted: nodeFamilyPredicate({
      store,
      stateQueue: SIM_QUEUE_CONFIG,
      operatorSet: null,
      slotToPosixMs: (slot) => slot * 1000,
      horizonLagBlocks: 0,
    }),
    log: () => {},
  });

/**
 * A queue's root with spares (block 1), then its first node (block 2);
 * an attestation of that node's header (wanted once block 2 is a fact)
 * and a merge of a header the queue never holds (never wanted), both
 * recorded at the tip and never decided before the reset.
 */
const chainWithIntents = async (store: FactStore) => {
  const chain = new SimChain(
    simUniverse(),
    SIM_ORIGIN,
    stateQueueTrackedSet(SIM_QUEUE_CONFIG),
  );
  const init = await store.initialize(SIM_ORIGIN);
  if (init.kind !== "initialized") throw new Error(`initialize: ${init.kind}`);
  const plain = { address: QUEUE_ADDRESS, lovelace: 3_000_000n };
  const events: ChainSyncEvent[] = [];
  const forward = async (txs: readonly SimTx[]) => {
    const { event, encoded } = chain.forward(txs);
    events.push(event);
    await apply(store, event);
    return encoded.txHashes;
  };
  const [rootTx] = await forward([
    {
      inputs: [chain.outsideInput()],
      outputs: [
        queueOutput(SDK.STATE_QUEUE_ROOT_ASSET_NAME, rootDatum(GENESIS, null)),
        plain,
        plain,
      ],
      nonce: chain.nonce(),
    },
  ]);
  const first = simHeader(1, GENESIS);
  const firstHash = SDK.stateQueueHeaderHash(first);
  await forward([
    {
      inputs: [{ txHash: rootTx!, index: 0 }],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(GENESIS, firstHash),
        ),
        queueOutput(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + firstHash,
          nodeDatum(first, "Unattested", null),
        ),
      ],
      nonce: chain.nonce(),
    },
  ]);
  const record = async (family: string, index: number, contentRef: string) => {
    const txCbor = encodeSimTx({
      inputs: [{ txHash: rootTx!, index }],
      outputs: [plain],
      nonce: chain.nonce(),
    });
    const result = await store.transaction("write", async (tx) =>
      recordIntentIn(tx, store.dialect, {
        family,
        workflowKey: `${family}:test`,
        txCbor,
        isOwnOutput: () => false,
        builtAt: (await currentViewIn(tx, store.dialect))!,
        contentRef: Buffer.from(contentRef, "hex"),
      }),
    );
    expect(result.kind).toBe("recorded");
    return txCbor;
  };
  const attest = await record("attest", 1, firstHash);
  const merge = await record("merge", 2, "ee".repeat(28));
  return { events, attest, merge };
};

const eventKinds = async (store: FactStore, txCbor: Buffer) =>
  (
    await store.transaction("read", (tx) =>
      readIntentEventsIn(tx, decodeTransaction(txCbor).hash),
    )
  ).map((event) => event.kind);

describe.each(["sqlite", "postgres"] as const)(
  "the node intent stage across a tracked-set reset (%s)",
  (dialect) => {
    it("decides nothing while the store replays below the node tip, and decides as usual once the replay clears", async () => {
      const open = await opener(dialect);
      const before = await open(EMPTY);
      await started(before);
      const { events, attest, merge } = await chainWithIntents(before);
      await closeOne(before);

      // The tracked set gained an address: the start resets and replays.
      const store = await open({ ...EMPTY, addresses: new Set([GAINED]) });
      const start = await started(store);
      expect(start.trackedSet.kind).toBe("reset");
      expect(start.replaying).toBe(true);
      const init = await store.initialize(SIM_ORIGIN);
      expect(init.kind).toBe("initialized");
      // The replay re-applies block 1 only: the queue holds no node yet, so
      // the attestation's header is not a fact at this view.
      await apply(store, events[0]!);

      const sink = transport();
      const stage = stageOn(store, sink);
      const held = await stage.run();
      expect(held.map((hold) => hold.reason)).toEqual([
        FOLLOWER_TRACKED_SET_CHANGED,
      ]);
      expect(sink.sent).toEqual([]);
      expect(await eventKinds(store, attest)).toEqual(["signed"]);
      expect(await eventKinds(store, merge)).toEqual(["signed"]);

      // A pass that a reset overtook: the store reported no replay at its
      // start, and replays at the predicate reads.
      let reads = 0;
      const overtaken = stageOn(
        {
          ...store,
          trackedSetRecord: async () => {
            const record = await store.trackedSetRecord();
            reads += 1;
            return reads === 1 && record !== null
              ? { ...record, replaying: false }
              : record;
          },
        },
        sink,
      );
      const waited = await overtaken.run();
      expect(waited.map((hold) => hold.reason)).toEqual([
        FOLLOWER_TRACKED_SET_CHANGED,
        FOLLOWER_TRACKED_SET_CHANGED,
      ]);
      expect(sink.sent).toEqual([]);
      expect(await eventKinds(store, attest)).toEqual(["signed"]);
      expect(await eventKinds(store, merge)).toEqual(["signed"]);
      overtaken.close();

      // The replay reaches the node tip; the follow loop clears the flag.
      await apply(store, events[1]!);
      expect(await store.endTrackedSetReplay()).toBe("ended");
      expect(await stage.run()).toEqual([]);
      expect(sink.sent).toEqual([attest]);
      expect(await eventKinds(store, attest)).toEqual([
        "signed",
        "submit_attempt",
      ]);
      expect(await eventKinds(store, merge)).toEqual(["signed", "abandoned"]);
      stage.close();
    });
  },
);
