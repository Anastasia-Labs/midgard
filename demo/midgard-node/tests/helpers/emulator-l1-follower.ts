/**
 * The L1 follower and its change driver, standing in for an emulator chain
 * (N1). The emulator has no chain-sync, so this writes what the follower
 * would hold for the node's ingestion (its cursor at the emulator's tip, the
 * tip's block, the event key set) and builds the event projection's plan
 * from the live list outputs, each opened by the follower's own derivation
 * (`openOrder`). The plan then goes through the production write paths: the
 * follower-change driver (`makeEmulatorDriver` in
 * `emulator-l1-follower.driver.ts`: its sink, recompute and
 * landed-block hook), or `reconcileFollowerEvents` under the test-only
 * fixture capability (`ingestEmulatorEventsUnowned`), before any driver has
 * applied a view.
 *
 * A rollback (a restored emulator snapshot or a fork) is the follower's
 * rewind and replay, net (`rewindToEmulatorChain`): the keys whose admitting
 * transactions the emulator no longer holds go, the generation advances.
 *
 * Forced orders are not followed: a test that places a forced row writes
 * it with the order row the node ingested it from
 * (`insertForcedEntriesWithOrders` in `emulator-l1-follower.forced-orders.ts`),
 * as the forced-order ingestion only writes a row for an order the follower
 * projects.
 *
 * `mirrorEmulatorEvents` writes what a lookup by event id reads: the list
 * and retention outputs and one live event row per live Order.
 *
 * Heights count the synced tips: a new tip slot is one block above the
 * highest kept one, so the block d below the covered tip (the horizon lag)
 * is the tip synced d syncs earlier. A test that lags syncs every block.
 *
 * Differences from a followed chain, none of which these tests rely on:
 * synthetic block hashes, a key's first admission recorded where
 * the first sync saw its Order, a rollback seen only through its orphaned
 * keys or a lower tip, and a plan without the events retired within k (they
 * are absent, as for a node that ingested them before).
 */
import { encodeOutRef } from "@al-ft/midgard-l1-follower";
import {
  openOrder,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Emulator,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { reconcileFollowerEvents } from "../../src/database/follower-events.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import type { IngestionPlan } from "../../src/l1-events/driver.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../../src/services/follower-write-gate.js";
import {
  Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../../src/services/index.js";
import { runningFollower } from "../readiness-l1-follower.fixture.js";
import {
  listContracts,
  liveOrders,
  type OpenedOrder,
} from "./emulator-l1-follower.live-orders.js";
import {
  FOLLOWER_GENERATION,
  followerBlockHash,
  writeFollowerTip,
} from "./follower-view.js";
import {
  mirrorEmulatorStateQueue,
  writeAddressFacts,
} from "./landed-state-queue.js";

const failed = (message: string, cause?: unknown) =>
  new DatabaseError({ table: "l1_follower_cursor", message, cause });

/** The 34-byte admission outref back to an outref. */
const decodeOutRef = (bytes: Buffer) => ({
  txHash: Buffer.from(bytes.subarray(0, 32)),
  index: bytes.readUInt16BE(32),
});

/** The emulator's confirmed transactions: the chain the follower follows. */
const emulatorChain = (lucid: LucidEvolution) =>
  Effect.gen(function* () {
    const provider: unknown = lucid.config().provider;
    if (!(provider instanceof Emulator))
      return yield* failed("The follower stand-in follows an emulator only");
    return (txHash: string) =>
      provider.transactionHistory[txHash]?.status === "confirmed";
  });

/**
 * The follower's rewind and replay, net, onto the emulator's chain at
 * `slot`: a key whose admitting transaction the chain no longer confirms
 * goes, as do the blocks past the rewind target, and the generation
 * advances. A key still canonical stays at its first admission, as the
 * replay re-admits it there. Returns the generation to write the tip at.
 */
const rewindToEmulatorChain = (
  slot: number,
  confirmed: (txHash: string) => boolean,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const cursor = yield* sql<{
      slot: number | string;
      generation: number | string;
    }>`SELECT slot, generation FROM l1_follower_cursor`;
    if (cursor[0] === undefined) return FOLLOWER_GENERATION;
    const generation = Number(cursor[0].generation);
    const keys = yield* sql<{
      kind: string;
      key: Buffer;
      origin_outref: Buffer;
      first_canonical_slot: number | string;
    }>`SELECT kind, key, origin_outref, first_canonical_slot FROM l1_event_keys`;
    const orphaned = keys.filter(
      (row) => !confirmed(row.origin_outref.subarray(0, 32).toString("hex")),
    );
    if (orphaned.length === 0 && slot >= Number(cursor[0].slot))
      return generation;
    const target =
      Math.min(
        slot,
        ...orphaned.map((row) => Number(row.first_canonical_slot)),
      ) - 1;
    for (const row of orphaned)
      yield* sql`DELETE FROM l1_event_keys WHERE kind = ${row.kind} AND key = ${row.key}`;
    yield* sql`DELETE FROM l1_blocks WHERE slot > ${target}`;
    return generation + 1;
  });

/**
 * Writes the follower's cursor at `slot` (after any rewind onto the chain),
 * the tip's block and the keys of `orders` (a known key keeps its first
 * admission), and returns the plan.
 */
const writeFollowerView = (
  slot: number,
  orders: readonly OpenedOrder[],
  confirmed: (txHash: string) => boolean,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const generation = yield* rewindToEmulatorChain(slot, confirmed);
    const heights = yield* sql<{ height: string }>`SELECT COALESCE(
        (SELECT height FROM l1_blocks WHERE slot = ${slot}),
        (SELECT max(height) + 1 FROM l1_blocks), 0)::text AS height`;
    const view = yield* writeFollowerTip(
      slot,
      generation,
      Number(heights[0]!.height),
    );
    const hash = view.point.hash;
    const events: ProjectedEvent[] = [];
    for (const { kind, utxo, opened } of orders) {
      const { retained: _retained, ...content } = opened;
      const key = Buffer.from(opened.key, "hex");
      const location = {
        txHash: Buffer.from(utxo.txHash, "hex"),
        index: utxo.outputIndex,
      };
      yield* sql`INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot)
        VALUES (${kind}, ${key}, ${encodeOutRef(location)}, ${slot}) ON CONFLICT DO NOTHING`;
      const known = yield* sql<{ origin_outref: Buffer }>`SELECT origin_outref
        FROM l1_event_keys WHERE kind = ${kind} AND key = ${key}`;
      const admission = decodeOutRef(known[0]!.origin_outref);
      events.push({
        kind,
        ...content,
        admission: {
          blockHash: hash.toString("hex"),
          slot,
          height: slot,
          txHash: admission.txHash.toString("hex"),
          txIndex: 0,
          outRef: admission,
        },
        retirement: null,
        location,
      });
    }
    return { view, events } satisfies IngestionPlan;
  });

/**
 * The facts a lookup by event id reads (NC13), from the emulator's live
 * outputs: the list and retention outputs as seed rows at the emulator's
 * slot, and one live `node_l1_events` row per live Order, opened by the
 * follower's own derivation with the retention output it read. A retired
 * event has no live row, as on a followed chain. An event keeps the block
 * of the first sync that admitted it while that block is kept, so its depth
 * below the view grows as on a followed chain.
 */
export const mirrorEmulatorEvents = (fixture: EmulatorFollowerFixture) =>
  Effect.gen(function* () {
    const lucid = fixture.operatorLucid;
    const lists = listContracts(
      fixture.contracts,
      lucid.config().network === "Mainnet" ? 1 : 0,
    );
    const slot = lucid.currentSlot();
    for (const list of lists)
      for (const address of [list.listAddress, list.retentionAddress])
        yield* writeAddressFacts(
          address,
          yield* Effect.promise(() => lucid.utxosAt(address)),
          slot,
        );
    const orders = yield* Effect.tryPromise({
      try: () => liveOrders(lucid, lists),
      catch: (cause) => failed("Emulator list outputs are unreadable", cause),
    });
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const [tip] = yield* sql<{
          slot: string;
          hash: Buffer;
          height: string;
        }>`SELECT slot::text AS slot, hash, height::text AS height FROM l1_follower_cursor`;
        if (tip === undefined)
          return yield* failed("The emulator follower has no cursor");
        const kept = new Map(
          (yield* sql<{
            kind: string;
            event_key: Buffer;
            hash: Buffer;
            height: string;
            slot: string;
          }>`SELECT e.kind, e.event_key, e.admitted_block_hash AS hash,
              e.admitted_height::text AS height, e.admitted_slot::text AS slot
            FROM node_l1_events e JOIN l1_blocks b
              ON b.hash = e.admitted_block_hash
            WHERE e.retired_slot IS NULL`).map((row) => [
            `${row.kind}:${row.event_key.toString("hex")}`,
            row,
          ]),
        );
        yield* sql`DELETE FROM node_l1_events WHERE retired_slot IS NULL`;
        for (const { kind, utxo, opened } of orders) {
          const admitted = kept.get(`${kind}:${opened.key}`) ?? tip;
          yield* sql`INSERT INTO node_l1_events (kind, event_key, event_id,
              inclusion_time, facts_cbor, payload_cbor, original_assets_cbor,
              admission_tx_hash, admission_output_index, admission_tx_index,
              admitted_block_hash, admitted_height, admitted_slot, retired_slot,
              retained_tx_hash, retained_output_index)
            VALUES (${kind}, ${Buffer.from(opened.key, "hex")},
              ${Buffer.from(opened.idCbor, "hex")}, ${opened.inclusionTime.toString()},
              ${Buffer.from(opened.factsCbor, "hex")}, ${Buffer.from(opened.payloadCbor, "hex")},
              ${Buffer.from(opened.originalAssetsCbor, "hex")},
              ${Buffer.from(utxo.txHash, "hex")}, ${utxo.outputIndex}, 0,
              ${admitted.hash}, ${admitted.height}, ${admitted.slot}, NULL,
              ${opened.retained === null ? null : Buffer.from(opened.retained.txHash, "hex")},
              ${opened.retained?.index ?? null})`;
        }
      }),
    );
  });

export type EmulatorFollowerFixture = {
  readonly operatorLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
};

/**
 * Brings the follower tables to the emulator's tip (the event keys and the
 * state queue's outputs, P1's facts) and returns the plan there. With `globals`, the node's follower state becomes a caught-up
 * follower whose current plan is this one.
 */
export const syncEmulatorFollower = (
  fixture: EmulatorFollowerFixture,
  globals?: Globals,
) =>
  Effect.gen(function* () {
    const lucid = fixture.operatorLucid;
    const network = lucid.config().network;
    const lists = listContracts(
      fixture.contracts,
      network === "Mainnet" ? 1 : 0,
    );
    const orders = yield* Effect.tryPromise({
      try: () => liveOrders(lucid, lists),
      catch: (cause) => failed("Emulator list outputs are unreadable", cause),
    });
    const plan = yield* writeFollowerView(
      lucid.currentSlot(),
      orders,
      yield* emulatorChain(lucid),
    );
    yield* mirrorEmulatorStateQueue(lucid, fixture.contracts.stateQueue);
    yield* mirrorEmulatorEvents(fixture);
    if (globals !== undefined)
      yield* Ref.set(globals.L1_FOLLOWER, {
        ...runningFollower(),
        planCurrent: () => Promise.resolve({ kind: "ok", plan }),
      });
    return plan;
  });

/**
 * The driver's ingestion without a driver, under the test-only fixture
 * capability (refused once a driver has applied a view). Deposits due by `projectThroughMs` move into the mempool
 * ledger (hidden) and, with `globals`, the cache delta is published; the
 * default projects nothing, as the old commit barrier did.
 */
export const ingestEmulatorEventsUnowned = (
  fixture: EmulatorFollowerFixture,
  options: Readonly<{ globals?: Globals; projectThroughMs?: number }> = {},
) =>
  Effect.gen(function* () {
    const plan = yield* syncEmulatorFollower(fixture, options.globals);
    const lucid = fixture.operatorLucid;
    const network = lucid.config().network;
    if (network === undefined) return yield* failed("Emulator has no network");
    const outcome = yield* withFollowerWrite(
      reconcileFollowerEvents(plan, {
        network,
        slotToUnixTime: (slot) => lucid.slotToUnixTime(slot),
        cutoffMs: options.projectThroughMs ?? 0,
      }),
    ).pipe(Effect.provideService(FollowerWriteFixture, true));
    if (outcome.kind === "stale")
      return yield* failed("The emulator follower view moved");
    const globals = options.globals;
    const { projected, spendableUpserts } = outcome.ingestion;
    if (globals !== undefined && (projected > 0 || spendableUpserts.length > 0))
      yield* publishMempoolLedgerDelta(
        globals,
        {
          full: false,
          // Newly projected deposits stay hidden until a header is assigned;
          // restored header-assigned rows are spendable at once.
          upserts: spendableUpserts.map((entry) => [
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
            entry[MempoolLedgerDB.Columns.OUTPUT],
          ]),
          deletes: [],
        },
        (yield* NodeConfig).VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
    return outcome.ingestion;
  });

/**
 * `utxo`, an Order of the list minting under `policyId`, as the follower
 * opens and projects it: admitted and live at its own output.
 */
export const projectOrderAsFollower = (
  utxo: UTxO,
  kind: ProjectedEvent["kind"],
  policyId: string,
  retained: readonly UTxO[] = [],
): ProjectedEvent => {
  const opened = openOrder(
    utxo,
    {
      kind,
      policyId,
      listAddress: "",
      retentionAddress: "",
      retirementScriptHash: "",
    },
    retained,
  );
  if (opened === "not_an_order") throw new Error("expected an Order");
  const { retained: _retained, ...content } = opened;
  const location = {
    txHash: Buffer.from(utxo.txHash, "hex"),
    index: utxo.outputIndex,
  };
  return {
    kind,
    ...content,
    admission: {
      blockHash: followerBlockHash(0).toString("hex"),
      slot: 0,
      height: 0,
      txHash: utxo.txHash,
      txIndex: 0,
      outRef: location,
    },
    retirement: null,
    location,
  };
};
