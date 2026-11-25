/**
 * A node without a ledger takes in the ledger at its confirmed header H
 * (`takeInLedger`) from snapshot peers, each served by the node's own
 * `ledger-snapshot` handler over a ledger store filled directly, and
 * installs it in one transaction. Expected rows come from the entries the
 * peer's ledger was built from.
 */
import {
  EMPTY_LEDGER_ROOT,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as SnapshotAnchor from "../src/database/snapshot-anchor.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  type LedgerSnapshotPeer,
  type LedgerSnapshotPeerSource,
  LedgerSnapshotPending,
  takeInLedger,
} from "../src/services/ledger-snapshot-start.js";
import { DEPLOYMENT } from "./da-payload-libp2p-producer.manifest-fixture.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  memoryStore,
  peerOver,
  storeLookup,
} from "./helpers/ledger-install.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

type Entry = { readonly outRef: Buffer; readonly output: Buffer };
type Pairs = [outRef: string, output: string][];

const entryOf = (label: string, n: number): Entry => ({
  outRef: makeOutRefCbor(simDigest(`take-in:${label}:${n.toString()}`), n % 3),
  output: makeOutput(BigInt(2_000_000 + n)),
});

const entriesOf = (label: string, count: number): Entry[] =>
  Array.from({ length: count }, (_, n) => entryOf(label, n));

/** A ledger store that keeps the ledger of `entries`; its root. */
const keep = async (store: LedgerStore, entries: readonly Entry[]) => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(entries.map((entry) => ({ type: "insert" as const, ...entry })));
  await store.persist(ledger);
  return ledger.root;
};

/** A peer that serves another ledger whatever root it is asked for. */
const wrongPeer = async (identity: string) => {
  const store = memoryStore();
  const root = await keep(store, entriesOf("wrong", 40));
  return peerOver(identity, async (_utxosRoot, after) =>
    (await store.at(root)).entries(after === undefined ? {} : { after }),
  );
};

const sourceOf = (
  peers: readonly LedgerSnapshotPeer[],
  excluded = new Set<string>(),
): LedgerSnapshotPeerSource => ({
  deploymentFingerprint: DEPLOYMENT,
  peers,
  excluded,
});

const anchorAt = (utxosRoot: string): SnapshotAnchor.SnapshotAnchor => ({
  headerHash: "c1".repeat(28),
  utxosRoot,
  endTimeMs: 7_000n,
});

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const takeIn = (
  store: LedgerStore,
  anchor: SnapshotAnchor.SnapshotAnchor,
  source: LedgerSnapshotPeerSource,
) =>
  run(
    Effect.either(
      takeInLedger({
        store,
        anchor,
        peers: Effect.succeed(source),
        write: withFollowerWrite,
      }),
    ),
  );

const pairs = (rows: readonly Entry[]): Pairs =>
  rows
    .map((row): [string, string] => [
      row.outRef.toString("hex"),
      row.output.toString("hex"),
    ])
    .sort(([left], [right]) => (left < right ? -1 : 1));

/** What the install wrote, read back. */
const installed = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const ledgerOf = (table: string) =>
        sql<{
          outref: Buffer;
          output: Buffer;
        }>`SELECT outref, output FROM ${sql(table)}`.pipe(
          Effect.map((rows) =>
            pairs(
              rows.map((row) => ({ outRef: row.outref, output: row.output })),
            ),
          ),
        );
      const [stamp] = yield* sql<{ root_hex: string | null }>`
        SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger'`;
      const [staged] = yield* sql<{ count: string }>`
        SELECT count(*)::text AS count FROM node_ledger_snapshot_staging`;
      return {
        confirmed: yield* ledgerOf("confirmed_ledger"),
        mempool: yield* ledgerOf("mempool_ledger"),
        frontier: yield* Frontier.retrieve,
        stamp: stamp?.root_hex ?? null,
        anchor: yield* SnapshotAnchor.retrieve,
        staged: Number(staged?.count),
      };
    }),
  );

const nothingInstalled = {
  confirmed: [],
  mempool: [],
  frontier: undefined,
  stamp: null,
  anchor: undefined,
  staged: 0,
};

beforeEach(async () => {
  await run(resetApplicationTables);
});

describe("a node takes in its ledger from a snapshot peer", () => {
  it("installs a served ledger in one transaction: both ledgers hold its entries, the frontier and stamp are at (H, R) and the anchor is recorded", async () => {
    const entries = entriesOf("served", 2_500);
    const peerStore = memoryStore();
    const root = await keep(peerStore, entries);
    const anchor = anchorAt(root);
    const result = await takeIn(
      memoryStore(),
      anchor,
      sourceOf([peerOver("peer-a", storeLookup(peerStore))]),
    );
    expect(Either.isRight(result)).toBe(true);
    expect(await installed()).toEqual({
      confirmed: pairs(entries),
      mempool: pairs(entries),
      frontier: { headerHash: anchor.headerHash, utxosRoot: root },
      stamp: root,
      anchor,
      staged: 0,
    });
  });

  it("does not ask again a peer that served a wrong ledger, and installs the next peer's", async () => {
    const entries = entriesOf("served", 30);
    const peerStore = memoryStore();
    const root = await keep(peerStore, entries);
    const wrong = await wrongPeer("peer-wrong");
    const holder = memoryStore();
    const notYet = peerOver("peer-late", storeLookup(holder));
    const excluded = new Set<string>();
    const first = await takeIn(
      memoryStore(),
      anchorAt(root),
      sourceOf([wrong, notYet], excluded),
    );
    expect(Either.isLeft(first) && first.left).toBeInstanceOf(
      LedgerSnapshotPending,
    );
    expect(await installed()).toEqual(nothingInstalled);
    const asked = wrong.requests;
    expect(asked).toBeGreaterThan(0);
    const right = peerOver("peer-right", storeLookup(peerStore));
    const second = await takeIn(
      memoryStore(),
      anchorAt(root),
      sourceOf([wrong, right], excluded),
    );
    expect(Either.isRight(second)).toBe(true);
    expect(wrong.requests).toBe(asked);
    expect((await installed()).confirmed).toEqual(pairs(entries));
  });

  it("holds ledger_snapshot_pending while no peer serves the ledger, and installs it once one does", async () => {
    const entries = entriesOf("served", 30);
    const holder = memoryStore();
    const root = await keep(memoryStore(), entries);
    const peer = peerOver("peer-late", storeLookup(holder));
    const node = memoryStore();
    const none = await takeIn(node, anchorAt(root), sourceOf([]));
    const unserved = await takeIn(node, anchorAt(root), sourceOf([peer]));
    for (const result of [none, unserved])
      expect(Either.isLeft(result) && result.left).toBeInstanceOf(
        LedgerSnapshotPending,
      );
    expect(
      Either.isLeft(unserved) &&
        unserved.left instanceof LedgerSnapshotPending &&
        unserved.left.detail,
    ).toContain("peer-late");
    expect(await installed()).toEqual(nothingInstalled);
    await keep(holder, entries);
    const served = await takeIn(node, anchorAt(root), sourceOf([peer]));
    expect(Either.isRight(served)).toBe(true);
    expect((await installed()).confirmed).toEqual(pairs(entries));
  });

  it("finishes on the next start a take-in that crashed between the store's admit and the install", async () => {
    const entries = entriesOf("served", 30);
    const peerStore = memoryStore();
    const root = await keep(peerStore, entries);
    const node = memoryStore();
    // The crashed start: the node's store took the ledger in and staging
    // holds rows, but nothing was installed.
    await node.admit(entries, root, { current: true });
    const stale = entryOf("stale", 1);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO node_ledger_snapshot_staging (outref, tx_id, output, address)
          VALUES (${stale.outRef}, ${Buffer.alloc(32)}, ${stale.output}, 'stale')`;
      }),
    );
    const result = await takeIn(
      node,
      anchorAt(root),
      sourceOf([peerOver("peer-a", storeLookup(peerStore))]),
    );
    expect(Either.isRight(result)).toBe(true);
    const after = await installed();
    expect(after.confirmed).toEqual(pairs(entries));
    expect(after.stamp).toBe(root);
  });
});
