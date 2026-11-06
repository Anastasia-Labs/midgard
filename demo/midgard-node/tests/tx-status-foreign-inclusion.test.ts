import { HttpServerRequest } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  getTxStatusHandler,
  postTxStatusBatchHandler,
} from "../src/commands/http/transaction-status.js";
import { retentionSweepAction } from "../src/fibers/retention-sweeper.js";
import {
  foldMerge,
  pruneMerges,
} from "../src/landed-blocks/confirmed-merges.js";
import { processRow, rollBackRows } from "../src/landed-blocks/settlements.js";
import { Frontier, type LandedBlockRow } from "../src/landed-blocks/store.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { canonicalManifest } from "./deployment-manifest.canonical-identity.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * A tx this node accepted that another operator's block carried: the node
 * the user submitted to answers its status from the landed block, through
 * the real handlers over a migrated database.
 */

const transfer = Buffer.alloc(32, 0xa1);
const unrelated = Buffer.alloc(32, 0xa2);
/** This node's own committed tx, as old as any retention could reach. */
const own = Buffer.alloc(32, 0xa3);
const parent = "c0".repeat(28);
const parentRoot = "11".repeat(32);
/** Another operator's block, landed on L1, including `transfer`. */
const foreign: LandedBlockRow = {
  headerHash: "f2".repeat(28),
  parentHeaderHash: parent,
  parentUtxosRoot: parentRoot,
  utxosRoot: parentRoot,
  kind: "foreign",
  state: "processed",
  applied: true,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [transfer],
};
const TIP_HEIGHT = 1_000;

type Stage = "landed" | "merged" | "merged-shallow" | "rolled-back" | "final";
const MERGE_SLOT = 4_000;
const DAY_MS = 86_400_000;

const seed = (stage: Stage) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* resetApplicationTables;
    yield* sql`INSERT INTO l1_follower_cursor (slot, hash, height, generation,
      origin_slot, origin_hash, pruned_through_slot)
      VALUES (5000, ${Buffer.alloc(32, 1)}, ${TIP_HEIGHT}, 1, 0, ${Buffer.alloc(32, 2)}, 0)`;
    yield* sql`UPDATE node_follower_write_gate SET applied_generation = 1,
      applied_slot = 5000, applied_hash = ${Buffer.alloc(32, 1)},
      applied_height = ${TIP_HEIGHT}`;
    // This node accepted both; neither is in its own blocks.
    yield* sql`INSERT INTO mempool (tx_id, tx) VALUES
      (${transfer}, ${Buffer.from([0x84])}), (${unrelated}, ${Buffer.from([0x84])})`;
    yield* processRow(foreign);
    if (stage === "rolled-back") yield* rollBackRows([foreign], []);
    if (stage !== "landed" && stage !== "rolled-back") {
      yield* Frontier.upsert({ headerHash: parent, utxosRoot: parentRoot });
      yield* foldMerge(foreign, {
        slot: MERGE_SLOT,
        outRef: `${"e1".repeat(32)}#0`,
      });
      // `safe`: at least cd deep at the tip (depth = tip - height + 1).
      const safe =
        TIP_HEIGHT - canonicalManifest().l1Finality.confirmationDepth + 1;
      yield* sql`INSERT INTO node_l1_queue_terminals (header_hash,
        transaction_hash, terminal_outcome, tx_index, block_hash, height, slot)
        VALUES (${Buffer.from(foreign.headerHash, "hex")}, ${Buffer.alloc(32, 0xe1)},
          'merged', 0, ${Buffer.alloc(32, 3)},
          ${stage === "merged-shallow" ? safe + 1 : safe}, ${MERGE_SLOT})`;
    }
    if (stage === "final") {
      yield* sql`INSERT INTO immutable (tx_id, tx, time_stamp_tz)
        VALUES (${own}, ${Buffer.from([0x84])}, NOW() - interval '3650 days')`;
      // The follower's prune boundary passes the merge slot: the fold is
      // final. Then every retention rule runs, far in the future, with the
      // merge's height final.
      yield* pruneMerges(MERGE_SLOT);
      yield* retentionSweepAction(
        {
          confirmedHeadHash: Buffer.from(foreign.headerHash, "hex"),
          liveQueueHeaderHashes: [],
          finalThroughHeight: TIP_HEIGHT,
        },
        new Date(Date.now() + 3650 * DAY_MS),
      );
    }
  });

const identity = () => {
  const manifest = canonicalManifest();
  return ContractDeploymentIdentity.make({
    kind: "manifest",
    manifestId: manifest.manifestId,
    consensusProfile: manifest.consensusProfile,
    manifest,
  });
};

const query = (route: "GET" | "batch", stage: Stage, txId: Buffer) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* seed(stage);
        const handler =
          route === "GET"
            ? getTxStatusHandler.pipe(
                Effect.provideService(ParsedSearchParams, {
                  tx_hash: txId.toString("hex"),
                }),
              )
            : postTxStatusBatchHandler.pipe(
                Effect.provideService(
                  HttpServerRequest.HttpServerRequest,
                  HttpServerRequest.fromWeb(
                    new Request("http://midgard.test/tx-status", {
                      method: "POST",
                      body: JSON.stringify({
                        txHashes: [txId.toString("hex")],
                      }),
                    }),
                  ),
                ),
              );
        const response = yield* handler;
        if (response.body._tag !== "Uint8Array")
          throw new Error("expected JSON body");
        const body = JSON.parse(new TextDecoder().decode(response.body.body));
        return route === "GET" ? body : body.results[0];
      }).pipe(
        Effect.provideService(ContractDeploymentIdentity, identity()),
        Effect.scoped,
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<Record<string, unknown>, unknown, never>,
  );

for (const route of ["GET", "batch"] as const)
  describe(`${route} /tx-status for a tx another operator's block included`, () => {
    it("is committed in that block once it lands", async () =>
      expect(await query(route, "landed", transfer)).toMatchObject({
        status: "committed",
        committedMeaning: "landed_block_inclusion_not_confirmed_ledger_merge",
        headerHash: foreign.headerHash,
        mergeStatus: "not_finalized",
        confirmedLedgerFinalized: false,
      }));
    it("is finalized once that block is folded and its merge is safe", async () => {
      expect(await query(route, "merged", transfer)).toMatchObject({
        status: "committed",
        headerHash: foreign.headerHash,
        mergeStatus: "finalized",
        confirmedLedgerFinalized: true,
      });
      expect(await query(route, "merged-shallow", transfer)).toMatchObject({
        status: "committed",
        confirmedLedgerFinalized: false,
      });
    });
    it("stays committed and finalized once that block's fold is final, through every retention rule", async () => {
      expect(await query(route, "final", transfer)).toMatchObject({
        status: "committed",
        committedMeaning: "landed_block_inclusion_not_confirmed_ledger_merge",
        headerHash: foreign.headerHash,
        mergeStatus: "finalized",
        confirmedLedgerFinalized: true,
      });
      // The same retention keeps an own committed tx of any age.
      expect(await query(route, "final", own)).toMatchObject({
        status: "committed",
        committedMeaning: "immutable_db_inclusion_not_confirmed_ledger_merge",
      });
    });
    it("is pending again when a rollback takes the block off the landed chain", async () =>
      expect(await query(route, "rolled-back", transfer)).toEqual({
        txId: transfer.toString("hex"),
        status: "accepted",
      }));
    it("leaves a tx the block does not include as it was", async () => {
      for (const stage of ["landed", "merged", "final"] as const)
        expect(await query(route, stage, unrelated)).toEqual({
          txId: unrelated.toString("hex"),
          status: "accepted",
        });
    });
  });
