/**
 * The L1 follower and its change driver, standing in for an emulator chain
 * (N1). The emulator has no chain-sync, so this writes what the follower
 * would hold for the node's ingestion (its cursor at the emulator's tip, the
 * tip's block, the event key set) and builds the event projection's plan
 * from the live list outputs, each opened by the follower's own derivation
 * (`openOrder`). The plan then goes through the production write paths: the
 * driver's sink (`readyProducerSink`) under a Ready history owner, or
 * `reconcileFollowerEvents` under the unowned-history fixture gate.
 *
 * A rollback (a restored emulator snapshot or a fork) is the follower's
 * rewind and replay, net (`rewindToEmulatorChain`): the keys whose admitting
 * transactions the emulator no longer holds go, the generation advances.
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
import type { IngestionPlan, SinkResult } from "../../src/l1-events/driver.js";
import {
  EVENT_KINDS,
  type EventListConfig,
  eventProjectionConfigFromContracts,
  openOrder,
  type ProjectedEvent,
} from "../../src/l1-events/index.js";
import {
  UnownedHistoryFixture,
  withHistoryIngestion,
} from "../../src/services/event-history-producer.js";
import {
  type Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../../src/services/index.js";
import { readyProducerSink } from "../../src/services/l1-follower.js";
import { runningFollower } from "../readiness-l1-follower.fixture.js";
import {
  FOLLOWER_GENERATION,
  followerBlockHash,
  writeFollowerTip,
} from "./follower-view.js";

const failed = (message: string, cause?: unknown) =>
  new DatabaseError({ table: "l1_follower_cursor", message, cause });

/** The 34-byte admission outref back to an outref. */
const decodeOutRef = (bytes: Buffer) => ({
  txHash: Buffer.from(bytes.subarray(0, 32)),
  index: bytes.readUInt16BE(32),
});

type ListContracts = {
  readonly config: EventListConfig;
  readonly listAddress: string;
  readonly retentionAddress: string;
};

const listContracts = (
  contracts: SDK.MidgardValidators,
  networkId: 0 | 1,
): readonly ListContracts[] => {
  const pair = SDK.requireEventHistoryContracts(contracts);
  const projection = eventProjectionConfigFromContracts(pair, networkId);
  return EVENT_KINDS.map((kind) => ({
    config: projection.lists.find((list) => list.kind === kind)!,
    listAddress: pair[kind].list.spendingScriptAddress,
    retentionAddress: pair[kind].retention.spendingScriptAddress,
  }));
};

type OpenedOrder = {
  readonly kind: ProjectedEvent["kind"];
  readonly utxo: UTxO;
  readonly opened: Exclude<ReturnType<typeof openOrder>, "not_an_order">;
};

/** The live Orders the follower's derivation admits, per list. */
const liveOrders = async (
  lucid: LucidEvolution,
  lists: readonly ListContracts[],
): Promise<OpenedOrder[]> => {
  const orders: OpenedOrder[] = [];
  for (const list of lists) {
    const retained = await lucid.utxosAt(list.retentionAddress);
    for (const utxo of await lucid.utxosAt(list.listAddress)) {
      const names = Object.keys(utxo.assets)
        .filter((unit) => unit.startsWith(list.config.policyId))
        .map((unit) => unit.slice(list.config.policyId.length));
      if (!names.some((name) => name.length === 64)) continue;
      let opened: ReturnType<typeof openOrder>;
      try {
        opened = openOrder(utxo, list.config, retained);
      } catch {
        continue; // the follower refuses it as malformed
      }
      if (opened !== "not_an_order")
        orders.push({ kind: list.config.kind, utxo, opened });
    }
  }
  return orders;
};

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
        ...opened,
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

export type EmulatorFollowerFixture = {
  readonly operatorLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
};

/**
 * Brings the follower tables to the emulator's tip and returns the plan
 * there. With `globals`, the node's follower state becomes a caught-up
 * follower whose current plan is this one, so a history owner's recovery
 * ingests it (`ingestAtFollowerView`).
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
    if (globals !== undefined)
      yield* Ref.set(globals.L1_FOLLOWER, {
        ...runningFollower(),
        planCurrent: () => Promise.resolve({ kind: "ok", plan }),
      });
    return plan;
  });

/**
 * The driver's ingestion without a history owner, under the unowned-history
 * fixture gate. Deposits due by `projectThroughMs` move into the mempool
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
    const outcome = yield* withHistoryIngestion(
      reconcileFollowerEvents(plan, {
        network,
        slotToUnixTime: (slot) => lucid.slotToUnixTime(slot),
        cutoffMs: options.projectThroughMs ?? 0,
      }),
    ).pipe(Effect.provideService(UnownedHistoryFixture, true));
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
 * One driver run under a Ready history owner: the production sink applies
 * the emulator's plan. A held result carries the hold (`/readyz` reason)
 * the node would report.
 */
export const driveEmulatorFollower = (
  fixture: EmulatorFollowerFixture,
  globals: Globals,
) =>
  Effect.gen(function* () {
    const plan = yield* syncEmulatorFollower(fixture, globals);
    const sink = yield* readyProducerSink;
    return yield* Effect.promise(
      (): Promise<SinkResult> =>
        sink.apply({ kind: "unchanged", view: plan.view }, plan),
    );
  });

/**
 * `awaitReady` (the history owner ready at the tip), then driver runs until
 * one applies; returns the owner's coverage. A run held for recovery
 * (orphans, a cache reload) has asked the owner to reconcile, and the
 * recovery ingests at the same view, so the run after it must apply.
 */
export const readyWithEmulatorFollower = <C, E, R>(
  awaitReady: Effect.Effect<C, E, R>,
  fixture: EmulatorFollowerFixture,
  globals: Globals,
) =>
  Effect.gen(function* () {
    let coverage = yield* awaitReady;
    for (let run = 0; ; run++) {
      const result = yield* driveEmulatorFollower(fixture, globals);
      if (result.kind === "applied") return coverage;
      if (result.kind !== "held" || run > 0)
        return yield* Effect.die(
          new Error(
            `The emulator driver run did not apply: ${JSON.stringify(result)}`,
          ),
        );
      coverage = yield* awaitReady;
    }
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
  const location = {
    txHash: Buffer.from(utxo.txHash, "hex"),
    index: utxo.outputIndex,
  };
  return {
    kind,
    ...opened,
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
