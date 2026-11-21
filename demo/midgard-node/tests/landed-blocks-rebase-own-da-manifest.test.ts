/**
 * A bad DA manifest never holds the landed-block rebase (#926). With the
 * node's DA manifest missing, and separately corrupt, the rebase applies a
 * foreign landed block and this node's own landed block on it: the own
 * block's transaction reaches `immutable` and `blocks`, its payload is
 * stored and its journal derives applied. The DA publication reconciler,
 * the one path that reads the manifest, delivers nothing and names
 * `da_publication_manifest_unavailable`: it leaves its seeding cursor where
 * it was, writes no outbox row for the payload and counts it as its backlog.
 * Once the manifest is restored, its next pass clears the reason and
 * delivers the held payload exactly once to each committee peer, with one
 * announcement; a later pass delivers nothing again.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeDaPayloadSubmitRequestCbor,
  encodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import * as Publication from "../src/da/libp2p-producer.publication-transport.js";
import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import {
  DA_PUBLICATION_MANIFEST_UNAVAILABLE,
  type DaPublicationSeedState,
  reconcileDaPublicationsOnce,
} from "../src/fibers/da-publication-reconciler.js";
import { insertRow } from "../src/landed-blocks/store.js";
import type { Globals } from "../src/services/globals.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import {
  manifestFixture,
  PEER_A,
  PEER_B,
  PRODUCER_PRIVATE_KEY_SOURCE,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { ownJournal } from "./landed-blocks-own-journal.fixture.js";
import {
  attempt,
  BLOCK,
  E1,
  freshNative,
  hex,
  processOf,
  R1,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

const Journals = PendingBlockFinalizationsDB;
let journal: PendingBlockFinalizationsDB.PrepareInput;
let txId: Buffer;
let manifestDirectory: string;
let manifestPath: string;
/** The committee peers each payload-submit reached, in order. */
const submitted: string[] = [];
let announcements = 0;

const writeManifest = (content: string) => writeFile(manifestPath, content);
const VALID = () => JSON.stringify(manifestFixture());

beforeAll(async () => {
  ({
    journal,
    tx: { id: txId },
  } = await ownJournal());
  manifestDirectory = await mkdtemp(join(tmpdir(), "landed-own-da-manifest-"));
  manifestPath = join(manifestDirectory, "manifest.json");
  vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", manifestPath);
  vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", PRODUCER_PRIVATE_KEY_SOURCE);
  // The committee: every peer accepts what it is sent, and the gossip
  // announcement reaches one peer.
  vi.spyOn(Publication, "getPublicationTransport").mockResolvedValue({
    localPeerId: () => "producer-peer",
    sign: () => Buffer.alloc(64, 0x0b),
    request: async (peer, _protocol, bytes) => {
      submitted.push(peer.peerId);
      const request = decodeDaPayloadSubmitRequestCbor(bytes);
      return encodeDaPayloadSubmitResponseCbor({
        status: "accepted",
        headerHash: request.headerHash,
        payloadHash: request.payloadHash,
        reasonCode: null,
        retryAfterMs: null,
      });
    },
    publish: async () => {
      announcements += 1;
      return { recipients: [PEER_A] };
    },
  });
});
afterAll(async () => {
  vi.restoreAllMocks();
  vi.unstubAllEnvs();
  await rm(manifestDirectory, { recursive: true, force: true });
});

/**
 * The fixture's processed foreign block, then this node's processed own
 * block on it (no ledger delta of its own) with its journal.
 */
const landForeignThenOwn = async () => {
  const globals = await processOf(freshNative());
  await seed(globals);
  await run(
    globals,
    testWrite(
      Effect.gen(function* () {
        yield* insertRow({
          headerHash: journal.headerHash.toString("hex"),
          parentHeaderHash: BLOCK,
          parentUtxosRoot: R1,
          utxosRoot: R1,
          kind: "own",
          state: "processed",
          applied: false,
          spent: [],
          produced: [],
          depositIds: [],
          withdrawals: [],
          forcedIds: [],
          txIds: [txId],
        });
        yield* prepareUnderTestLease(journal);
      }),
    ),
  );
  return globals;
};

/** The own block as stored, and what DA publication holds for it. */
const shownOwn = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const header = journal.headerHash;
      const sql = yield* SqlClient.SqlClient;
      const count = (query: Effect.Effect<readonly { n: number }[], unknown>) =>
        Effect.map(query, ([row]) => row?.n ?? 0);
      const peers = yield* sql<{ peer_id: string; status: string }>`
        SELECT peer_id, status FROM da_payload_publications
        WHERE header_hash = ${header} ORDER BY peer_id`;
      return {
        status: yield* Journals.retrieveStatusByHeaderHash(header),
        immutable: yield* count(sql<{ n: number }>`
          SELECT COUNT(*)::int AS n FROM immutable WHERE tx_id = ${txId}`),
        blocks: yield* count(sql<{ n: number }>`
          SELECT COUNT(*)::int AS n FROM blocks WHERE header_hash = ${header}`),
        da: Option.isSome(yield* DaPayloadsDB.retrieveByHeaderHash(header)),
        peers: peers.map((row) => [row.peer_id, row.status]),
        announced: yield* count(sql<{ n: number }>`
          SELECT COUNT(*)::int AS n FROM da_payload_announcements
          WHERE header_hash = ${header} AND status = 'published'`),
      };
    }),
  );

/** Every outbox row for the own block, whatever its status. */
const outboxRows = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const header = journal.headerHash;
      const sql = yield* SqlClient.SqlClient;
      const [peers] = yield* sql<{ n: number }>`
        SELECT COUNT(*)::int AS n FROM da_payload_publications
        WHERE header_hash = ${header}`;
      const [announcements] = yield* sql<{ n: number }>`
        SELECT COUNT(*)::int AS n FROM da_payload_announcements
        WHERE header_hash = ${header}`;
      return { peers: peers?.n, announcements: announcements?.n };
    }),
  );

const applied = {
  status: Journals.OwnBlockStatus.Applied,
  immutable: 1,
  blocks: 1,
  da: true,
};

describe.each([
  ["missing", () => rm(manifestPath, { force: true })],
  ["corrupt", () => writeManifest('{"schemaVersion": ')],
] as const)(
  "a %s DA manifest",
  { concurrent: false },
  (_label, breakManifest) => {
    it("holds DA delivery, never the rebase, and delivers once when restored", async () => {
      submitted.length = 0;
      announcements = 0;
      const globals = await landForeignThenOwn();
      const reconcile = (state: DaPublicationSeedState) =>
        run(globals, reconcileDaPublicationsOnce({ seed: state }));
      const reasons = () => Effect.runPromise(currentLivenessReasons(globals));
      // The reconciler has run before, under a valid manifest.
      const state: DaPublicationSeedState = {};
      await writeManifest(VALID());
      expect((await reconcile(state)).manifestLoaded).toBe(true);
      const cursor = { ...state };
      expect(cursor.seededFrom).toBeInstanceOf(Date);
      await breakManifest();

      const shown = await attempt(globals);
      expect(shown.hold).toBeUndefined();
      expect(shown.due).toBe("none");
      expect(shown.applied).toEqual([true, true]);
      expect(shown.working).toContain(hex(E1.outref));
      expect(await shownOwn(globals)).toEqual({
        ...applied,
        peers: [],
        announced: 0,
      });

      // The held pass: no seed, so the cursor stays where it was and the
      // payload has no outbox row of any status; it is the backlog.
      expect(await reconcile(state)).toMatchObject({
        manifestLoaded: false,
        attempted: 0,
        backlog: 1,
      });
      expect(await reasons()).toContain(DA_PUBLICATION_MANIFEST_UNAVAILABLE);
      expect(state).toEqual(cursor);
      expect(await outboxRows(globals)).toEqual({ peers: 0, announcements: 0 });
      expect(submitted).toEqual([]);

      await writeManifest(VALID());
      expect(await reconcile(state)).toMatchObject({
        manifestLoaded: true,
        attempted: 2,
        backlog: 0,
      });
      expect(await reasons()).not.toContain(
        DA_PUBLICATION_MANIFEST_UNAVAILABLE,
      );
      const delivered = {
        ...applied,
        peers: [PEER_A, PEER_B].sort().map((peer) => [peer, "accepted"]),
        announced: 1,
      };
      expect(await shownOwn(globals)).toEqual(delivered);
      expect([...submitted].sort()).toEqual([PEER_A, PEER_B].sort());
      expect(announcements).toBe(1);

      // Nothing is seeded or delivered twice.
      expect((await reconcile(state)).attempted).toBe(0);
      expect(await shownOwn(globals)).toEqual(delivered);
      expect(submitted).toHaveLength(2);
      expect(announcements).toBe(1);
    });
  },
);
