/**
 * A watcher taking in a ledger from a node over real libp2p: the node's
 * retained-payload listener started from its environment, serving
 * `ledger-snapshot` from a memory ledger store at the deployment manifest's
 * 1 MiB page bound, and the watcher's own libp2p transport, peer sources and
 * bootstrap over a SQLite ledger store. The only addition between them is a
 * recorder around the watcher's transport. Expected entries are the node
 * store's own `entries()`, never a page.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { openSqliteLedgerStorage } from "@al-ft/midgard-fault-proofs/ledger-store/sqlite";
import {
  SIZED_ENTRIES_FROM,
  testLedgerEntry,
} from "@al-ft/midgard-fault-proofs/test-support/ledger-store-conformance";
import { startDaLibp2pRetainedPayloadServerFromEnv } from "midgard-node/da/libp2p-producer.create-da-libp2p-producer-transport";
import type { DaLedgerSnapshotLookup } from "midgard-node/da/libp2p-producer.parse-committee-peers";
import {
  closeServer,
  DEPLOYMENT,
  listenOnLoopback,
  runtimeManifestFixture,
  serverPort,
} from "midgard-node/tests/da-payload-libp2p-producer.manifest-fixture";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import { createWatcherLedgerSnapshotBootstrap } from "../../src/fault-proofs/ledger-snapshot-bootstrap.js";
import { watcherLedgerSnapshotPeers } from "../../src/storage/ledger-snapshot-fetch.js";
import type { WatcherPublicDaLibp2pTransportV1 } from "../../src/storage/public-da-client.js";
import {
  createWatcherPublicDaLibp2pTransport,
  type WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";

const PAGE_BOUND = DA_TRANSPORT_LIMITS.maxInlineResponseBytes;

/** Sized entries from `SIZED_ENTRIES_FROM` on, as many as it takes for
 * their bytes to pass three page bounds, so the ledger spans at least three
 * pages whatever each page's framing. */
const entriesPastThreePages = () => {
  const entries = [];
  let bytes = 0;
  for (let n = SIZED_ENTRIES_FROM; bytes <= 3 * PAGE_BOUND; n += 1) {
    const entry = testLedgerEntry(n);
    entries.push(entry);
    bytes += entry.outRef.length + entry.output.length;
  }
  return { entries, bytes };
};

/** `store` keeps the ledger of `entries`; that ledger. */
const keep = async (
  store: LedgerStore,
  entries: readonly { outRef: Buffer; output: Buffer }[],
): Promise<Ledger> => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(entries.map((entry) => ({ type: "insert" as const, ...entry })));
  await store.persist(ledger);
  return ledger;
};

const listed = async (ledger: Ledger): Promise<string[]> => {
  const rows: string[] = [];
  for await (const { outRef, output } of ledger.entries())
    rows.push(`${outRef.toString("hex")}:${output.toString("hex")}`);
  return rows;
};

const cleanups: (() => Promise<void>)[] = [];

/** A node listener on a free loopback port with the identity of `key`,
 * serving `ledger-snapshot` only when `ledgerEntries` is given. */
const startNode = async (
  identity: string,
  key: string,
  ledgerEntries?: DaLedgerSnapshotLookup,
) => {
  const probe = await listenOnLoopback();
  const port = serverPort(probe);
  await closeServer(probe);
  const { peerId } = await loadDaLibp2pIdentity(key);
  const directory = await mkdtemp(join(tmpdir(), "ledger-snapshot-node-"));
  cleanups.push(() => rm(directory, { recursive: true, force: true }));
  const manifestPath = join(directory, "manifest.json");
  await writeFile(
    manifestPath,
    JSON.stringify(runtimeManifestFixture(peerId, port)),
  );
  const server = await startDaLibp2pRetainedPayloadServerFromEnv({
    retrieveByHeaderHash: async () => undefined,
    ...(ledgerEntries === undefined ? {} : { ledgerEntries }),
    env: {
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      DA_LIBP2P_PRIVATE_KEY_SOURCE: key,
    },
  });
  cleanups.push(async () => {
    await server.close?.();
  });
  expect(server.configured).toBe(true);
  return {
    identity,
    peerId,
    multiaddr: `/dns4/localhost/tcp/${port.toString()}/p2p/${peerId}`,
  };
};

/** Each lookup the serving node made: `after` as hex, `null` when absent. */
const lookups: (string | null)[] = [];
/** Each request the watcher made, with its response's size in bytes. */
const requests: { peer: string; responseBytes?: number }[] = [];

const nodeStore = createLedgerStore(createMemoryLedgerStorage());
let large: Ledger;
let largeBytes: number;
let small: Ledger;
let transport: WatcherPublicDaLibp2pTransport;
let recorded: WatcherPublicDaLibp2pTransportV1;
let committee: Awaited<ReturnType<typeof startNode>>;
let serving: Awaited<ReturnType<typeof startNode>>;

beforeAll(async () => {
  const { entries, bytes } = entriesPastThreePages();
  largeBytes = bytes;
  large = await keep(nodeStore, entries);
  small = await keep(nodeStore, entries.slice(0, 40));
  const lookup: DaLedgerSnapshotLookup = async (utxosRoot, after) => {
    lookups.push(after === undefined ? null : after.toString("hex"));
    return (await nodeStore.at(utxosRoot)).entries(
      after === undefined ? {} : { after },
    );
  };
  committee = await startNode("committee", `seed:${"00".repeat(31)}02`);
  serving = await startNode("node", `seed:${"00".repeat(31)}01`, lookup);
  transport = await createWatcherPublicDaLibp2pTransport();
  cleanups.push(() => transport.stop());
  recorded = {
    request: async (request) => {
      const entry: (typeof requests)[number] = { peer: request.peerIdentity };
      requests.push(entry);
      const response = await transport.request(request);
      entry.responseBytes = response.length;
      return response;
    },
  };
}, 120_000);

afterAll(async () => {
  for (const cleanup of cleanups.reverse()) await cleanup();
});

beforeEach(() => {
  lookups.length = 0;
  requests.length = 0;
});

/** A watcher over a fresh SQLite store whose peers are `peers`, in order. */
const watcher = async (
  peers: readonly Awaited<ReturnType<typeof startNode>>[],
) => {
  const directory = await mkdtemp(join(tmpdir(), "ledger-snapshot-watcher-"));
  const storage = openSqliteLedgerStorage(join(directory, "ledger.sqlite"));
  cleanups.push(async () => {
    storage.close();
    await rm(directory, { recursive: true, force: true });
  });
  const store = createLedgerStore(storage);
  const bootstrap = createWatcherLedgerSnapshotBootstrap({
    store,
    log: () => undefined,
  });
  const sources = watcherLedgerSnapshotPeers({
    transport: recorded,
    peers,
    deploymentFingerprint: DEPLOYMENT,
    timeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    signal: new AbortController().signal,
  });
  return {
    store,
    counts: storage.counts.bind(storage),
    take: (root: string) => bootstrap(sources, root),
  };
};

describe("a watcher taking in a ledger from a node over libp2p", () => {
  it("holds the node's ledger, page by page at the full page bound", async () => {
    const scene = await watcher([serving]);

    await expect(scene.take(large.root)).resolves.toBe(true);

    expect(await listed(await scene.store.at(large.root))).toEqual(
      await listed(large),
    );
    expect(largeBytes).toBeGreaterThan(3 * PAGE_BOUND);
    expect(lookups.length).toBeGreaterThanOrEqual(3);
    expect(lookups[0]).toBeNull();
    for (const after of lookups.slice(1)) expect(after).not.toBeNull();
    const largest = Math.max(
      ...requests.map(({ responseBytes }) => responseBytes ?? 0),
    );
    // A full page ends within one entry of the bound.
    expect(largest).toBeLessThanOrEqual(PAGE_BOUND);
    expect(PAGE_BOUND - largest).toBeLessThan(1_024);
  }, 120_000);

  it("takes the ledger past a peer that does not speak the protocol, and asks it again on a later call", async () => {
    const scene = await watcher([committee, serving]);

    await expect(scene.take(small.root)).resolves.toBe(true);
    expect(requests.map(({ peer }) => peer)).toEqual(["committee", "node"]);
    expect(requests[0]?.responseBytes).toBeUndefined();

    requests.length = 0;
    await scene.store.retain(new Set());
    await expect(scene.take(small.root)).resolves.toBe(true);
    expect(requests.map(({ peer }) => peer)).toEqual(["committee", "node"]);
  }, 60_000);

  it("holds nothing new once the store retains again after a root the node does not hold", async () => {
    const scene = await watcher([serving]);
    const before = await scene.counts();

    await expect(scene.take("11".repeat(32))).resolves.toBe(false);
    await scene.store.retain(new Set());

    expect(await scene.counts()).toEqual(before);
    expect(lookups).toEqual([null]);
  }, 60_000);
});
