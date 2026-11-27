/**
 * The node's `ledger-snapshot` server: the handler the retained-payload
 * listener registers when it is given a ledger lookup, driven through a fake
 * stream over a memory ledger store, and the same handler on a real libp2p
 * listener started from the node's environment, asked by a dialing
 * transport. Expected entries come from a plain `entries()` of the ledger
 * and from the entries the ledger was built from, never from a page.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaLedgerSnapshotResponseCbor,
  encodeDaLedgerSnapshotRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStorage,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it, onTestFinished } from "vitest";

import { createDaLibp2pProducerTransport } from "../src/da/libp2p-producer.create-da-libp2p-producer-transport.js";
import { startDaLibp2pRetainedPayloadServerFromEnv } from "../src/da/libp2p-producer.create-da-libp2p-producer-transport.js";
import { createDaLibp2pRetainedPayloadRequestHandlers } from "../src/da/libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import type { DaLedgerSnapshotLookup } from "../src/da/libp2p-producer.parse-committee-peers.js";
import { parseDaProducerPublicationManifest } from "../src/da/libp2p-producer.parse-da-producer-publication-manifest.js";
import {
  callRetainedPayloadHandler,
  closeServer,
  DEPLOYMENT,
  listenOnLoopback,
  parseManifestFixture,
  PRODUCER_PRIVATE_KEY_SOURCE,
  runtimeManifestFixture,
  serverPort,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

type Entry = { readonly outRef: Buffer; readonly output: Buffer };
type Listed = [outRef: string, output: string][];

/** Entry `n`: its output holds `n % policies` native-asset policies, each
 * with one asset whose name is `n % 4` times 8 bytes, so outputs differ in
 * size. */
const entryOf = (n: number, policies: number): Entry => ({
  outRef: makeOutRefCbor(simDigest(`ledger-snapshot:${n.toString()}`), n % 3),
  output: makeOutput(
    BigInt(2_000_000 + n),
    undefined,
    new Map(
      Array.from({ length: n % policies }, (_, policy) => [
        Buffer.alloc(28, 0x10 + policy).toString("hex"),
        new Map([[Buffer.alloc((n % 4) * 8, 0x61).toString("hex"), BigInt(n)]]),
      ]),
    ),
  ),
});

const entriesOf = (count: number, policies: number): Entry[] =>
  Array.from({ length: count }, (_, n) => entryOf(n, policies));

/** `store` keeps the ledger of `entries`; that ledger. */
const keep = async (store: LedgerStore, entries: readonly Entry[]) => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(entries.map((entry) => ({ type: "insert" as const, ...entry })));
  await store.persist(ledger);
  return ledger;
};

const hexPairs = (
  entries: Iterable<{ outRef: Uint8Array; output: Uint8Array }>,
): Listed =>
  [...entries].map(({ outRef, output }) => [
    Buffer.from(outRef).toString("hex"),
    Buffer.from(output).toString("hex"),
  ]);

const listed = async (ledger: Ledger): Promise<Listed> => {
  const result: Entry[] = [];
  for await (const entry of ledger.entries()) result.push(entry);
  return hexPairs(result);
};

const sorted = (pairs: Listed): Listed =>
  [...pairs].sort(([left], [right]) => (left < right ? -1 : 1));

/** The lookup the node injects, over `store`. */
const storeLookup =
  (store: LedgerStore): DaLedgerSnapshotLookup =>
  async (utxosRoot, after) =>
    (await store.at(utxosRoot)).entries(after === undefined ? {} : { after });

const snapshotRequest = (
  utxosRoot: string,
  after: Uint8Array | null,
  fingerprint = DEPLOYMENT,
) =>
  encodeDaLedgerSnapshotRequestCbor({
    deploymentFingerprint: Buffer.from(fingerprint, "hex"),
    utxosRoot: Buffer.from(utxosRoot, "hex"),
    after: after === null ? null : Buffer.from(after),
  });

const handlersWith = (
  ledgerEntries: DaLedgerSnapshotLookup | undefined,
  maxInlineResponseBytes: number = DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
) =>
  createDaLibp2pRetainedPayloadRequestHandlers({
    manifest: { ...parseManifestFixture(), maxInlineResponseBytes },
    retrieveByHeaderHash: async () => undefined,
    ...(ledgerEntries === undefined ? {} : { ledgerEntries }),
  });

const ask = (
  handlers: ReturnType<typeof handlersWith>,
  request: Uint8Array,
): Promise<Buffer> =>
  callRetainedPayloadHandler({
    handlers,
    protocol: DaRequestResponseProtocol.ledgerSnapshot,
    request,
  });

const snapshotProtocolId = daRequestResponseProtocolId(
  DEPLOYMENT,
  DaRequestResponseProtocol.ledgerSnapshot,
);

describe("ledger-snapshot handler", () => {
  it("pages a ledger in trie order within the inline bound, each page resuming after the last", async () => {
    const entries = entriesOf(300, 6);
    const store = createLedgerStore(createMemoryLedgerStorage());
    const ledger = await keep(store, entries);
    const bound = 2_000;
    const handlers = handlersWith(storeLookup(store), bound);

    const pages: Listed[] = [];
    let after: Uint8Array | null = null;
    for (let turn = 0; turn < entries.length; turn += 1) {
      const encoded = await ask(handlers, snapshotRequest(ledger.root, after));
      expect(encoded.length).toBeLessThanOrEqual(bound);
      const response = decodeDaLedgerSnapshotResponseCbor(encoded);
      if (response.status !== "ok") throw new Error(response.status);
      pages.push(hexPairs(response.entries));
      if (response.done) break;
      after = response.entries.at(-1)!.outRef;
    }

    expect(pages.length).toBeGreaterThan(10);
    const served = pages.flat();
    expect(served).toEqual(await listed(ledger));
    expect(sorted(served)).toEqual(sorted(hexPairs(entries)));
  });

  it("answers not_held for a root the store does not hold, and before the node can open ledgers", async () => {
    const store = createLedgerStore(createMemoryLedgerStorage());
    await keep(store, entriesOf(20, 6));
    const unknownRoot = "ee".repeat(32);

    expect(
      decodeDaLedgerSnapshotResponseCbor(
        await ask(
          handlersWith(storeLookup(store)),
          snapshotRequest(unknownRoot, null),
        ),
      ),
    ).toEqual({ status: "not_held" });
    expect(
      decodeDaLedgerSnapshotResponseCbor(
        await ask(
          handlersWith(async () => undefined),
          snapshotRequest(unknownRoot, null),
        ),
      ),
    ).toEqual({ status: "not_held" });
  });

  it("answers not_held when a read of the ledger misses an output part-way", async () => {
    const storage = createMemoryLedgerStorage();
    const ledger = await keep(createLedgerStore(storage), entriesOf(300, 6));
    const missing = (await listed(ledger))[150]![0];
    const losing: LedgerStorage = {
      ...storage,
      readOutputs: async (outRefs) =>
        (await storage.readOutputs(outRefs)).map((output, index) =>
          outRefs[index] === missing ? undefined : output,
        ),
    };

    const response = decodeDaLedgerSnapshotResponseCbor(
      await ask(
        handlersWith(storeLookup(createLedgerStore(losing))),
        snapshotRequest(ledger.root, null),
      ),
    );

    expect(response).toEqual({ status: "not_held" });
  });

  it("answers not_held when the ledger's nodes go missing after it is opened", async () => {
    const storage = createMemoryLedgerStorage();
    const ledger = await keep(createLedgerStore(storage), entriesOf(300, 6));
    let dropped = false;
    const dropping: LedgerStorage = {
      ...storage,
      readNodes: async (hashes) =>
        dropped ? hashes.map(() => undefined) : await storage.readNodes(hashes),
    };
    const opened = storeLookup(createLedgerStore(dropping));
    const openThenDrop: DaLedgerSnapshotLookup = async (utxosRoot, after) => {
      const entries = await opened(utxosRoot, after);
      dropped = true;
      return entries;
    };

    const response = decodeDaLedgerSnapshotResponseCbor(
      await ask(handlersWith(openThenDrop), snapshotRequest(ledger.root, null)),
    );

    expect(response).toEqual({ status: "not_held" });
  });

  it("rejects another deployment's fingerprint without reading the ledger", async () => {
    let lookups = 0;
    const handlers = handlersWith(async () => {
      lookups += 1;
      return undefined;
    });

    const response = decodeDaLedgerSnapshotResponseCbor(
      await ask(
        handlers,
        snapshotRequest(EMPTY_LEDGER_ROOT, null, "cd".repeat(28)),
      ),
    );

    expect(response).toEqual({ status: "rejected" });
    expect(lookups).toBe(0);
  });

  it("fails the stream of a request that does not decode, and serves the next request", async () => {
    const store = createLedgerStore(createMemoryLedgerStorage());
    const ledger = await keep(store, entriesOf(5, 6));
    const handlers = handlersWith(storeLookup(store));

    await expect(ask(handlers, Buffer.from("00", "hex"))).rejects.toThrow(
      "invalid ledger-snapshot request",
    );

    const response = decodeDaLedgerSnapshotResponseCbor(
      await ask(handlers, snapshotRequest(ledger.root, null)),
    );
    expect(response).toMatchObject({ status: "ok", done: true });
    if (response.status === "ok")
      expect(hexPairs(response.entries)).toEqual(await listed(ledger));
  });

  it("is not registered without a ledger lookup", () => {
    const handlers = handlersWith(undefined);

    expect(handlers.has(snapshotProtocolId)).toBe(false);
    expect(
      handlers.has(
        daRequestResponseProtocolId(
          DEPLOYMENT,
          DaRequestResponseProtocol.payloadByHeader,
        ),
      ),
    ).toBe(true);
  });
});

describe("ledger-snapshot on the node's libp2p listener", () => {
  it("serves two consecutive pages to a dialing peer", async () => {
    // Outputs of up to 49 policies: about 1.3 MB in all, so two pages of
    // the 1 MiB inline bound the deployment manifest fixes.
    const store = createLedgerStore(createMemoryLedgerStorage());
    const ledger = await keep(store, entriesOf(1_000, 50));

    const probe = await listenOnLoopback();
    const port = serverPort(probe);
    await closeServer(probe);
    const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
    const directory = await mkdtemp(join(tmpdir(), "ledger-snapshot-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const manifestPath = join(directory, "manifest.json");
    await writeFile(
      manifestPath,
      JSON.stringify(runtimeManifestFixture(identity.peerId, port)),
    );
    const server = await startDaLibp2pRetainedPayloadServerFromEnv({
      retrieveByHeaderHash: async () => undefined,
      ledgerEntries: storeLookup(store),
      env: {
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE,
      },
    });
    onTestFinished(() => server.close?.());
    expect(server.configured).toBe(true);

    const dialerKey = `seed:${"00".repeat(31)}07`;
    const dialerIdentity = await loadDaLibp2pIdentity(dialerKey);
    const dialer = await createDaLibp2pProducerTransport(
      parseDaProducerPublicationManifest(
        runtimeManifestFixture(dialerIdentity.peerId, port),
        { DA_LIBP2P_PRIVATE_KEY_SOURCE: dialerKey },
      ),
      { mode: "dial-only" },
    );
    onTestFinished(() => dialer.close?.());
    const node = {
      signerIndex: 0,
      daVkey: "00".repeat(32),
      peerId: identity.peerId,
      multiaddrs: [
        `/ip4/127.0.0.1/tcp/${port.toString()}/p2p/${identity.peerId}`,
      ],
      roles: [],
    };
    const page = async (after: Uint8Array | null) =>
      decodeDaLedgerSnapshotResponseCbor(
        await dialer.request(
          node,
          snapshotProtocolId,
          snapshotRequest(ledger.root, after),
          DA_TRANSPORT_LIMITS.requestTimeoutMs,
        ),
      );

    const first = await page(null);
    if (first.status !== "ok") throw new Error(first.status);
    expect(first.done).toBe(false);
    const second = await page(first.entries.at(-1)!.outRef);
    if (second.status !== "ok") throw new Error(second.status);
    expect(second.done).toBe(true);

    expect([...hexPairs(first.entries), ...hexPairs(second.entries)]).toEqual(
      await listed(ledger),
    );
  }, 60_000);
});
