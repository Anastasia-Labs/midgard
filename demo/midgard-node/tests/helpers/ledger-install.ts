/**
 * Installs a node's ledger the way a node without a ledger stamp does
 * (`takeInLedger`), for tests that drive event ingestion or landed
 * processing without starting the node: until the install, the node gives
 * no L1 event a row and runs no landed processing. A ledger other than the
 * empty one is taken in from a snapshot peer that answers through the
 * node's own `ledger-snapshot` handler over a ledger store the test filled.
 */
import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";

import { createDaLibp2pRetainedPayloadRequestHandlers } from "../../src/da/libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import type { DaLedgerSnapshotLookup } from "../../src/da/libp2p-producer.parse-committee-peers.js";
import type * as Ledger from "../../src/database/utils/ledger.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";
import { takeInLedger } from "../../src/services/ledger-snapshot-start.js";
import {
  callRetainedPayloadHandler,
  DEPLOYMENT,
  parseManifestFixture,
} from "../da-payload-libp2p-producer.manifest-fixture.js";

export const memoryStore = () => createLedgerStore(createMemoryLedgerStorage());

/** The node's `ledger-snapshot` lookup over `store`. */
export const storeLookup =
  (store: LedgerStore): DaLedgerSnapshotLookup =>
  async (utxosRoot, after) =>
    (await store.at(utxosRoot)).entries(after === undefined ? {} : { after });

/** A peer that answers through the node's `ledger-snapshot` handler over
 * `lookup`, counting the requests it is sent. */
export const peerOver = (identity: string, lookup: DaLedgerSnapshotLookup) => {
  const handlers = createDaLibp2pRetainedPayloadRequestHandlers({
    manifest: parseManifestFixture(),
    retrieveByHeaderHash: async () => undefined,
    ledgerEntries: lookup,
  });
  const peer = {
    identity,
    requests: 0,
    request: async (requestCbor: Buffer) => {
      peer.requests += 1;
      return callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.ledgerSnapshot,
        request: requestCbor,
      });
    },
  };
  return peer;
};

/**
 * Installs the ledger of `entries` at the header `headerHash` ending at
 * `endTimeMs`, into the node's ledger store `node` (a fresh memory store
 * when omitted): the empty ledger asks no peer; any other is taken in from
 * one snapshot peer serving it. A run in `provideDatabaseLayers`, which
 * grants the follower write fixture.
 */
export const installLedger = (input: {
  readonly headerHash: string;
  readonly endTimeMs: bigint;
  readonly entries: readonly Ledger.MinimalEntry[];
  readonly node?: LedgerStore;
}) =>
  Effect.gen(function* () {
    const served = memoryStore();
    const ledger = yield* Effect.promise(async () => {
      const built = await (
        await served.at(EMPTY_LEDGER_ROOT)
      ).apply(
        input.entries.map((entry) => ({
          type: "insert" as const,
          outRef: entry.outref,
          output: entry.output,
        })),
      );
      await served.persist(built);
      return built;
    });
    yield* takeInLedger({
      store: input.node ?? memoryStore(),
      anchor: {
        headerHash: input.headerHash,
        utxosRoot: ledger.root,
        endTimeMs: input.endTimeMs,
      },
      peers: Effect.succeed({
        deploymentFingerprint: DEPLOYMENT,
        peers: [peerOver("fixture-peer", storeLookup(served))],
        excluded: new Set<string>(),
      }),
      write: withFollowerWrite,
    });
  });

/**
 * Installs the empty ledger at the genesis header, given the end time
 * `endTimeMs` (genesis's own, 0, unless a test moves it): the node gives no
 * row to an event included at or before it, and its frontier is at the
 * genesis root of a deployment with no genesis outputs.
 */
export const installEmptyLedger = (endTimeMs = 0n) =>
  installLedger({
    headerHash: SDK.GENESIS_HEADER_HASH,
    endTimeMs,
    entries: [],
  });
