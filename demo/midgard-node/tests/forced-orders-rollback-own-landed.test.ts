/**
 * A forced order near its inclusion and a rollback one block deeper than
 * the commit event depth d, on a simulated chain with the follower store in
 * the node database:
 *
 * - the order lands again identically: its row stays, included in the
 *   landed own block that carries it; nothing is reported;
 * - the order never lands again and no block carries it: its row is
 *   deleted (excluded), with no hold;
 * - the landed own block that carries it stays while the order never lands
 *   again: the node follows the block. Nothing holds (no hook hold, no
 *   commit refusal, no pending write gate); the hook logs one warning
 *   naming the header and the order, and `/readyz` reports the degradation
 *   `l1_own_block_forced_order_orphaned:<count>` until the header leaves
 *   the landed queue.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import { ownLandedForcedOrphanDetail } from "../src/commands/listen-router.get-readiness-handler.inputs.js";
import { countOrphansAwaitingRecovery } from "../src/database/follower-orphan-repair.js";
import { ForcedTransactionsDB } from "../src/database/index.js";
import { refuseCommitForOrphanedOwnBlockEvent } from "../src/fibers/block-commitment.own-block-event-orphaned.js";
import {
  forcedOrderIngestionHook,
  type RunDatabase,
} from "../src/forced-orders/index.js";
import { readOwnLandedForcedOrphans } from "../src/forced-orders/own-landed-orphans.js";
import { insertRow, setState } from "../src/landed-blocks/store.js";
import { Globals } from "../src/services/globals.globals.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import {
  honest,
  inlineOrder,
  label,
  nodeFollowerLifecycle,
  rows,
} from "./helpers/forced-orders-node-chain.js";
import {
  db,
  runDatabase,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { landedRow } from "./landed-blocks-rebase.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const follow = nodeFollowerLifecycle();

/** The modelled commit event depth: the order is d deep when committed. */
const DEPTH = 2;
/** The landed own block that carries the order. */
const OWN = "0c".repeat(28);

/** The forced-order hook, with its log lines and its warnings captured. */
const hookOf = (
  store: Parameters<typeof forcedOrderIngestionHook>[0]["store"],
) => {
  const logs: string[] = [];
  const warnings: string[] = [];
  const capture = Logger.replace(
    Logger.defaultLogger,
    Logger.make(({ logLevel, message }) => {
      if (logLevel._tag === "Warning")
        warnings.push(
          (Array.isArray(message) ? message : [message]).map(String).join(" "),
        );
    }),
  );
  const run: RunDatabase = (effect) =>
    runDatabase(effect.pipe(Effect.provide(capture)));
  const hook = forcedOrderIngestionHook({
    store,
    config: FORCED_CONFIG,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    contentSourcesConfigured: true,
    run,
    log: (line) => logs.push(line),
  });
  return { hook, logs, warnings };
};

/** The order is ingested, then d blocks land on it: it is d deep. */
const ingestDeep = async () => {
  const { store, chain } = await follow();
  const order = inlineOrder(chain.chain, honest());
  await chain.forward([order]);
  for (let block = 0; block < DEPTH; block += 1) await chain.forward([]);
  const captured = hookOf(store);
  expect(await captured.hook(UNCHANGED)).toBeUndefined();
  expect(await rows()).toHaveLength(1);
  return { chain, order, ...captured };
};

/** Landed own block `OWN` carries the forced row, finalized locally. */
const carry = async () => {
  const [row] = await rows();
  const txOrderId = Buffer.from(row!.tx_order_id);
  await db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* insertRow(
        landedRow({ headerHash: OWN, kind: "own", forcedIds: [txOrderId] }),
      );
      yield* sql`UPDATE ${sql(ForcedTransactionsDB.tableName)}
        SET status = 'finalized', projected_header_hash = ${Buffer.from(OWN, "hex")}
        WHERE tx_order_id = ${txOrderId}`;
    }),
  );
};

/** What the node reports and holds for the forced rows of landed own blocks. */
const reported = () =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const ownLanded = yield* readOwnLandedForcedOrphans;
        return {
          ownLanded,
          detail: ownLandedForcedOrphanDetail(ownLanded.length),
          commitRefused: yield* refuseCommitForOrphanedOwnBlockEvent,
          awaitingRecovery: yield* countOrphansAwaitingRecovery,
        };
      }),
    ).pipe(Effect.provide(Globals.Default)),
  );

describe("a forced order near inclusion and a rollback of d + 1", () => {
  it("an order that lands again identically stays included in its landed own block, with nothing reported", async () => {
    const { chain, order, hook, warnings } = await ingestDeep();
    await carry();
    const before = await rows();
    await chain.backward(DEPTH + 1);
    await chain.forward([order]);
    for (let block = 0; block < DEPTH; block += 1) await chain.forward([]);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
    expect(before[0]!.projected_header_hash).toEqual(Buffer.from(OWN, "hex"));
    expect(await reported()).toEqual({
      ownLanded: [],
      detail: undefined,
      commitRefused: false,
      awaitingRecovery: 0,
    });
    expect(warnings).toEqual([]);
  });

  it("an order that never lands again and no block carries is excluded, with no hold", async () => {
    const { chain, order, hook, logs, warnings } = await ingestDeep();
    await chain.backward(DEPTH + 1);
    await chain.forward([]);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
    expect(logs.join("\n")).toContain(
      `deleted 1 forced row(s) whose order left the chain: ${label(order)}`,
    );
    expect(await reported()).toEqual({
      ownLanded: [],
      detail: undefined,
      commitRefused: false,
      awaitingRecovery: 0,
    });
    expect(warnings).toEqual([]);
  });

  it("a landed own block whose forced order never lands again is followed: no hold, one warning, a readiness degradation until it leaves the landed queue", async () => {
    const { chain, order, hook, warnings } = await ingestDeep();
    await carry();
    await chain.backward(DEPTH + 1);
    await chain.forward([]);
    for (let attempt = 0; attempt < 2; attempt += 1)
      expect(await hook(UNCHANGED)).toBeUndefined();
    // The row stays the block's: it is neither deleted nor released.
    expect(await rows()).toMatchObject([
      { status: "finalized", projected_header_hash: Buffer.from(OWN, "hex") },
    ]);
    expect(await reported()).toEqual({
      ownLanded: [{ headerHash: OWN, order: label(order) }],
      detail: "l1_own_block_forced_order_orphaned:1",
      commitRefused: false,
      awaitingRecovery: 0,
    });
    expect(warnings).toHaveLength(1);
    expect(warnings[0]).toContain(OWN);
    expect(warnings[0]).toContain(label(order));

    // The header leaves the landed queue: the degradation clears.
    await db(setState([OWN], "removed"));
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await reported()).toMatchObject({
      ownLanded: [],
      detail: undefined,
      commitRefused: false,
    });
    expect(warnings).toHaveLength(1);
  });
});
