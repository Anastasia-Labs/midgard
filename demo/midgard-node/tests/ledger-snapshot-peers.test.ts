/**
 * `LEDGER_SNAPSHOT_PEERS` as the node reads it, and a node without a ledger
 * taking in a peer's ledger over real libp2p: the peer is a node's
 * retained-payload listener started from its environment, serving a ledger
 * store filled directly; the node dials it from its own DA host
 * (`configuredLedgerSnapshotPeers`) and installs what it took.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { SqlClient } from "@effect/sql";
import { ConfigProvider, Effect, Either } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

import { startDaLibp2pRetainedPayloadServerFromEnv } from "../src/da/libp2p-producer.create-da-libp2p-producer-transport.js";
import { closeDaLibp2pPublicationTransport } from "../src/da/libp2p-producer.publication-transport.js";
import { ledgerSnapshotPeersConfig } from "../src/services/config.ledger-snapshot-peers.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { configuredLedgerSnapshotPeers } from "../src/services/ledger-snapshot-peers.js";
import { takeInLedger } from "../src/services/ledger-snapshot-start.js";
import {
  closeServer,
  listenOnLoopback,
  PEER_A,
  PEER_B,
  PRODUCER_PRIVATE_KEY_SOURCE,
  runtimeManifestFixture,
  serverPort,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const read = (value: string | undefined) =>
  Effect.runPromise(
    Effect.either(
      ledgerSnapshotPeersConfig.pipe(
        Effect.withConfigProvider(
          ConfigProvider.fromMap(
            new Map(
              value === undefined ? [] : [["LEDGER_SNAPSHOT_PEERS", value]],
            ),
          ),
        ),
      ),
    ),
  );

const A = `/dns4/node-a.example/tcp/4001/p2p/${PEER_A}`;
const B = `/ip4/10.0.0.2/tcp/4001/p2p/${PEER_B}`;

describe("LEDGER_SNAPSHOT_PEERS", () => {
  it.each([
    ["unset", undefined],
    ["empty", ""],
    ["blank", "  "],
  ])("names no peer when %s", async (_name, value) => {
    expect(await read(value)).toEqual(Either.right([]));
  });

  it("names its peers in order, each by peer id and multiaddr", async () => {
    expect(await read(` ${B} , ${A}`)).toEqual(
      Either.right([
        { peerId: PEER_B, multiaddr: B },
        { peerId: PEER_A, multiaddr: A },
      ]),
    );
  });

  it.each([
    ["a value that is not a multiaddr", "node-a.example:4001"],
    ["a multiaddr without a peer id", "/ip4/10.0.0.2/tcp/4001"],
    ["a peer id that is not last", `/p2p/${PEER_A}/tcp/4001`],
    ["an invalid peer id", "/ip4/10.0.0.2/tcp/4001/p2p/QmNotAPeerId"],
    ["a peer named twice", `${A},/ip4/10.0.0.3/tcp/4001/p2p/${PEER_A}`],
    ["an empty entry", `${A},,${B}`],
  ])("refuses %s", async (_name, value) => {
    expect(Either.isLeft(await read(value))).toBe(true);
  });
});

type Entry = { readonly outRef: Buffer; readonly output: Buffer };

const entriesOf = (count: number): Entry[] =>
  Array.from({ length: count }, (_, n) => ({
    outRef: makeOutRefCbor(simDigest(`libp2p-take-in:${n.toString()}`), n % 3),
    output: makeOutput(BigInt(2_000_000 + n)),
  }));

const keep = async (store: LedgerStore, entries: readonly Entry[]) => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(entries.map((entry) => ({ type: "insert" as const, ...entry })));
  await store.persist(ledger);
  return ledger.root;
};

const freePort = async () => {
  const probe = await listenOnLoopback();
  const port = serverPort(probe);
  await closeServer(probe);
  return port;
};

const pairs = (rows: readonly Entry[]) =>
  rows
    .map((row) => [row.outRef.toString("hex"), row.output.toString("hex")])
    .sort(([left], [right]) => (left! < right! ? -1 : 1));

describe("a node takes in a peer's ledger over libp2p", () => {
  it("dials the configured peers in order from its DA host and installs the first one's ledger that serves it", async () => {
    const entries = entriesOf(300);
    const store = createLedgerStore(createMemoryLedgerStorage());
    const root = await keep(store, entries);
    const directory = await mkdtemp(join(tmpdir(), "ledger-take-in-libp2p-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));

    // The serving node.
    const port = await freePort();
    const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
    const servingManifest = join(directory, "serving.json");
    await writeFile(
      servingManifest,
      JSON.stringify(runtimeManifestFixture(identity.peerId, port)),
    );
    const server = await startDaLibp2pRetainedPayloadServerFromEnv({
      retrieveByHeaderHash: async () => undefined,
      ledgerEntries: async (utxosRoot, after) =>
        (await store.at(utxosRoot)).entries(
          after === undefined ? {} : { after },
        ),
      env: {
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: servingManifest,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
      },
    });
    onTestFinished(() => server.close?.());
    expect(server.configured).toBe(true);

    // The node without a ledger, under its own DA identity; its first peer
    // listens nowhere.
    const nodeKey = `seed:${"00".repeat(31)}09`;
    const nodeIdentity = await loadDaLibp2pIdentity(nodeKey);
    const nodeManifest = join(directory, "node.json");
    await writeFile(
      nodeManifest,
      JSON.stringify(
        runtimeManifestFixture(nodeIdentity.peerId, await freePort()),
      ),
    );
    vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", nodeManifest);
    vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", nodeKey);
    onTestFinished(async () => {
      vi.unstubAllEnvs();
      await closeDaLibp2pPublicationTransport();
    });
    const down = await freePort();
    const configured = await read(
      [
        `/ip4/127.0.0.1/tcp/${down.toString()}/p2p/${PEER_A}`,
        `/ip4/127.0.0.1/tcp/${port.toString()}/p2p/${identity.peerId}`,
      ].join(","),
    );
    if (Either.isLeft(configured)) throw configured.left;

    const anchor = {
      headerHash: "c2".repeat(28),
      utxosRoot: root,
      endTimeMs: 9_000n,
    };
    const installed = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          yield* takeInLedger({
            store: createLedgerStore(createMemoryLedgerStorage()),
            anchor,
            peers: configuredLedgerSnapshotPeers({
              LEDGER_SNAPSHOT_PEERS: configured.right,
            }),
            write: withFollowerWrite,
          });
          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<{
            outref: Buffer;
            output: Buffer;
          }>`SELECT outref, output FROM confirmed_ledger`;
          const [stamp] = yield* sql<{ root_hex: string | null }>`
            SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger'`;
          return {
            confirmed: pairs(
              rows.map((row) => ({ outRef: row.outref, output: row.output })),
            ),
            stamp: stamp?.root_hex,
          };
        }),
      ),
    );
    expect(installed).toEqual({ confirmed: pairs(entries), stamp: root });
  }, 60_000);
});
