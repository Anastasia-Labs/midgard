/**
 * A watcher with no verified head taking in a ledger from its DA peers over
 * `ledger-snapshot`. Each peer is a small in-test server over a real memory
 * ledger store, answering pages with the protocol's own page helper and
 * codec; the watcher side is a real SQLite ledger store. Expected roots come
 * from the trie library's oracle, and expected entries are the ones the
 * servers were built from.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  daLedgerSnapshotPage,
  type DaLedgerSnapshotRequest,
  type DaLedgerSnapshotResponse,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaLedgerSnapshotRequestCbor,
  encodeDaLedgerSnapshotResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import type { LedgerEntry } from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  LedgerNotRetained,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  openSqliteLedgerStorage,
  type SqliteLedgerStorage,
} from "@al-ft/midgard-fault-proofs/ledger-store/sqlite";
import { oracleLedgerRoot } from "@al-ft/midgard-fault-proofs/test-support/ledger-root-oracle";
import { testLedgerEntries } from "@al-ft/midgard-fault-proofs/test-support/ledger-store-conformance";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  createWatcherLedgerSnapshotBootstrap,
  WATCHER_LEDGER_SNAPSHOT_PEER_EXCLUDED,
} from "../../src/fault-proofs/ledger-snapshot-bootstrap.js";
import { watcherLedgerSnapshotPeers } from "../../src/storage/ledger-snapshot-fetch.js";
import type {
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequest,
} from "../../src/storage/public-da-client.js";

const FINGERPRINT = "ab".repeat(28);
const PROTOCOL_ID = daRequestResponseProtocolId(
  FINGERPRINT,
  DaRequestResponseProtocol.ledgerSnapshot,
);
/** A server's page bound, small enough that a few hundred entries span
 * several pages. */
const PAGE_BYTES = 4_096;

/** One server's answer to one decoded request; `ask` counts from 0. */
type Handler = (
  request: DaLedgerSnapshotRequest,
  ask: number,
  wire: WatcherPublicDaRequest,
) => Promise<Uint8Array>;

const encode = (response: DaLedgerSnapshotResponse): Uint8Array =>
  encodeDaLedgerSnapshotResponseCbor(response);

/** The page after `request.after` of whichever of `ledgers` it names. */
const honestPage = async (
  ledgers: readonly Ledger[],
  request: DaLedgerSnapshotRequest,
): Promise<DaLedgerSnapshotResponse> => {
  if (!request.deploymentFingerprint.equals(Buffer.from(FINGERPRINT, "hex")))
    return { status: "rejected" };
  const ledger = ledgers.find(
    ({ root }) => root === request.utxosRoot.toString("hex"),
  );
  if (ledger === undefined) return { status: "not_held" };
  const page = await daLedgerSnapshotPage(
    ledger.entries(request.after === null ? {} : { after: request.after }),
    PAGE_BYTES,
  );
  return { status: "ok", ...page };
};

const honest =
  (ledgers: readonly Ledger[]): Handler =>
  async (request) =>
    encode(await honestPage(ledgers, request));

/** Each request to a peer, in order, with its decoded body. */
type Asked = { peer: string; request: DaLedgerSnapshotRequest };

/** A transport whose peers are `servers`; a peer is refused the
 * connection unless the request names the deployment's protocol. */
const transportOf = (servers: Readonly<Record<string, Handler>>) => {
  const asked: Asked[] = [];
  const transport: WatcherPublicDaLibp2pTransportV1 = {
    request: async (wire) => {
      const request = decodeDaLedgerSnapshotRequestCbor(wire.requestCbor);
      const ask = asked.filter(({ peer }) => peer === wire.peerIdentity).length;
      asked.push({ peer: wire.peerIdentity, request });
      const server = servers[wire.peerIdentity];
      if (server === undefined || wire.protocolId !== PROTOCOL_ID)
        throw new Error(`${wire.peerIdentity} refused the protocol`);
      return await server(request, ask, wire);
    },
  };
  return { transport, asked };
};

let directory: string;
let storage: SqliteLedgerStorage | undefined;
afterEach(() => {
  storage?.close();
  storage = undefined;
  rmSync(directory, { recursive: true, force: true });
});

const ENTRIES = testLedgerEntries(0, 300);
const OTHER_ENTRIES = testLedgerEntries(400, 40);
/** More entries than the store writes in one batch. */
const LARGE_ENTRIES = testLedgerEntries(10_000, 2_500);
let served: Ledger;
let other: Ledger;
let large: Ledger;
beforeAll(async () => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  served = await store.admit(ENTRIES, await oracleLedgerRoot(ENTRIES));
  other = await store.admit(
    OTHER_ENTRIES,
    await oracleLedgerRoot(OTHER_ENTRIES),
  );
  large = await store.admit(
    LARGE_ENTRIES,
    await oracleLedgerRoot(LARGE_ENTRIES),
  );
});

/** A watcher over a fresh SQLite store, whose peers are `servers` in their
 * listing order. */
const watcher = (
  servers: Readonly<Record<string, Handler>>,
  options: { readonly timeoutMs?: number; readonly signal?: AbortSignal } = {},
) => {
  directory = mkdtempSync(join(tmpdir(), "ledger-snapshot-bootstrap-"));
  storage = openSqliteLedgerStorage(join(directory, "ledger.sqlite"));
  const store: LedgerStore = createLedgerStore(storage);
  const { transport, asked } = transportOf(servers);
  const peers = watcherLedgerSnapshotPeers({
    transport,
    peers: Object.keys(servers).map((identity) => ({
      identity,
      peerId: `peer-${identity}`,
      multiaddr: `/dns4/${identity}.example/tcp/4001`,
    })),
    deploymentFingerprint: FINGERPRINT,
    timeoutMs: options.timeoutMs ?? 5_000,
    signal: options.signal ?? new AbortController().signal,
  });
  const logged: Record<string, unknown>[] = [];
  const bootstrap = createWatcherLedgerSnapshotBootstrap({
    store,
    log: (line) => logged.push(JSON.parse(line) as Record<string, unknown>),
  });
  const counts = storage.counts.bind(storage);
  return {
    store,
    asked,
    logged,
    counts,
    /** The peers asked from here on. */
    askedFrom: () => {
      const from = asked.length;
      return () => asked.slice(from).map(({ peer }) => peer);
    },
    take: (root: string) => bootstrap(peers, root),
  };
};

const listed = async (ledger: Ledger): Promise<string[]> => {
  const rows: string[] = [];
  for await (const { outRef, output } of ledger.entries())
    rows.push(`${outRef.toString("hex")}:${output.toString("hex")}`);
  return rows.sort();
};

const rowsOf = (entries: readonly LedgerEntry[]): string[] =>
  entries
    .map(
      ({ outRef, output }) =>
        `${outRef.toString("hex")}:${output.toString("hex")}`,
    )
    .sort();

const excludedPeers = (logged: readonly Record<string, unknown>[]) =>
  logged
    .filter(({ reason }) => reason === WATCHER_LEDGER_SNAPSHOT_PEER_EXCLUDED)
    .map(({ peerIdentity }) => peerIdentity);

/** `handler`, with the output of the first entry of page `page` replaced by
 * another entry's output. */
const altering =
  (ledgers: readonly Ledger[], page = 1): Handler =>
  async (request, ask) => {
    const response = await honestPage(ledgers, request);
    if (response.status !== "ok" || ask !== page) return encode(response);
    const [first, ...rest] = response.entries;
    return encode({
      ...response,
      entries: [{ outRef: first!.outRef, output: ENTRIES[7]!.output }, ...rest],
    });
  };

describe("a watcher taking in a ledger from its DA peers", () => {
  it("holds the ledger an honest peer serves, asking for each page once", async () => {
    const scene = watcher({ honest: honest([served]) });

    await expect(scene.take(served.root)).resolves.toBe(true);

    expect(await listed(await scene.store.at(served.root))).toEqual(
      rowsOf(ENTRIES),
    );
    const afters = scene.asked.map(({ request }) =>
      request.after === null ? null : request.after.toString("hex"),
    );
    expect(afters[0]).toBeNull();
    expect(afters.length).toBeGreaterThan(2);
    expect(new Set(afters).size).toBe(afters.length);
  });

  it("takes the ledger from the next peer after one serves an altered output, and asks the first no more", async () => {
    const scene = watcher({
      liar: altering([served, other]),
      honest: honest([served, other]),
    });

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(excludedPeers(scene.logged)).toEqual(["liar"]);

    const asked = scene.askedFrom();
    await expect(scene.take(other.root)).resolves.toBe(true);
    expect(new Set(asked())).toEqual(new Set(["honest"]));
    expect(await listed(await scene.store.at(other.root))).toEqual(
      rowsOf(OTHER_ENTRIES),
    );
  });

  it("holds nothing more once the store retains again after every peer served a wrong ledger", async () => {
    const scene = watcher({
      liar: altering([large], 0),
      laterLiar: altering([large], 50),
    });
    await scene.store.admit(OTHER_ENTRIES, other.root);
    await scene.store.retain(new Set([other.root]));
    const before = await scene.counts();

    await expect(scene.take(large.root)).resolves.toBe(false);
    // The later liar's transfer went past a full batch, which was written.
    expect(await scene.counts()).not.toEqual(before);
    await scene.store.retain(new Set([other.root]));

    expect(await scene.counts()).toEqual(before);
    await expect(scene.store.at(large.root)).rejects.toBeInstanceOf(
      LedgerNotRetained,
    );
    expect(excludedPeers(scene.logged)).toEqual(["liar", "laterLiar"]);
  });

  it("takes the ledger from the next peer when a request fails mid-transfer, and asks the first again later", async () => {
    const flaky: Handler = async (request, ask, wire) => {
      if (ask === 2) throw new Error("stream reset");
      return await honest([served, other])(request, ask, wire);
    };
    const scene = watcher({ flaky, honest: honest([served, other]) });

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(scene.asked.filter(({ peer }) => peer === "flaky")).toHaveLength(3);

    const asked = scene.askedFrom();
    await expect(scene.take(other.root)).resolves.toBe(true);
    expect(new Set(asked())).toEqual(new Set(["flaky"]));
    expect(scene.logged).toEqual([]);
  });

  it("takes the ledger from the next peer when a request times out", async () => {
    const silent: Handler = (_request, _ask, wire) =>
      new Promise((_resolve, reject) =>
        wire.signal.addEventListener("abort", () => reject(wire.signal.reason)),
      );
    const scene = watcher(
      { silent, honest: honest([served]) },
      { timeoutMs: 50 },
    );

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(scene.logged).toEqual([]);
  });

  it("asks a peer that did not hold the ledger, or refused the deployment, again on a later call", async () => {
    const foreign: Handler = async () => encode({ status: "rejected" });
    const scene = watcher({
      lagging: honest([other]),
      foreign,
      honest: honest([served, other]),
    });

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(scene.logged).toEqual([]);

    const asked = scene.askedFrom();
    await expect(scene.take(other.root)).resolves.toBe(true);
    expect(new Set(asked())).toEqual(new Set(["lagging"]));

    const unheld = await oracleLedgerRoot(testLedgerEntries(500, 3));
    const askedUnheld = scene.askedFrom();
    await expect(scene.take(unheld)).resolves.toBe(false);
    await expect(scene.take(unheld)).resolves.toBe(false);
    expect(askedUnheld()).toEqual([
      ...["lagging", "foreign", "honest"],
      ...["lagging", "foreign", "honest"],
    ]);
  });

  it("asks no more of a peer that answers an empty page short of the end", async () => {
    const stuck: Handler = async () =>
      encode({ status: "ok", entries: [], done: false });
    const scene = watcher({ stuck });

    await expect(scene.take(served.root)).resolves.toBe(false);
    expect(scene.asked).toHaveLength(1);
    expect(excludedPeers(scene.logged)).toEqual(["stuck"]);
    // Named as the peer's answer, not as a ledger short of the root.
    expect(scene.logged[0]?.cause).toMatch(/page with no entry/);

    await expect(scene.take(served.root)).resolves.toBe(false);
    expect(scene.asked).toHaveLength(1);
  });

  it("asks no more of a peer whose answer does not decode", async () => {
    const garbled: Handler = async () => Buffer.from("ff00", "hex");
    const scene = watcher({ garbled, honest: honest([served]) });

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(excludedPeers(scene.logged)).toEqual(["garbled"]);

    const asked = scene.askedFrom();
    await scene.store.retain(new Set());
    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(asked()).not.toContain("garbled");
  });

  it("asks no peer for a ledger the store already holds", async () => {
    const scene = watcher({ honest: honest([served]) });
    await scene.store.admit(ENTRIES, served.root);

    await expect(scene.take(served.root)).resolves.toBe(true);
    expect(scene.asked).toEqual([]);
  });

  it("ends the transfer with the runtime's close, asking no further peer", async () => {
    const lifetime = new AbortController();
    const closing: Handler = async () => {
      lifetime.abort(new Error("runtime closed"));
      throw new Error("connection closed");
    };
    const scene = watcher(
      { closing, honest: honest([served]) },
      { signal: lifetime.signal },
    );

    await expect(scene.take(served.root)).rejects.toThrow("runtime closed");
    expect(scene.asked.map(({ peer }) => peer)).toEqual(["closing"]);
  });
});
