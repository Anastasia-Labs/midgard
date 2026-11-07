import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import * as MigrationRunner from "midgard-node/database/migrations/runner";
import {
  foldMerge,
  pruneMerges,
} from "midgard-node/landed-blocks/confirmed-merges";
import {
  processRow,
  rollBackRows,
} from "midgard-node/landed-blocks/settlements";
import {
  Frontier,
  type LandedBlockRow,
} from "midgard-node/landed-blocks/store";
import {
  insertOwnJournal,
  JournalStatus,
  setJournalStatus,
} from "midgard-node/tests/helpers/landed-blocks-sim.own";
import {
  provideDatabaseLayers,
  resetApplicationTables,
} from "midgard-node/tests/utils";
import { describe, expect, it } from "vitest";

import { collectDbCounts } from "../src/commands/e2e-finalize-summary.db-counts.js";

const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

/**
 * The finalizer's counts on the node a user submitted to, in a run where
 * another operator won a block race: this node's block holding `lost` was
 * abandoned and the other operator's block included `lost` instead.
 */
const ROOT = "11".repeat(32);
const won = Buffer.alloc(32, 0xb1);
const lost = Buffer.alloc(32, 0xb2);
const pending = Buffer.alloc(32, 0xb3);
const ownWon = "a1".repeat(28);
const ownLost = "a2".repeat(28);
const foreign: LandedBlockRow = {
  headerHash: "f2".repeat(28),
  parentHeaderHash: ownWon,
  parentUtxosRoot: ROOT,
  utxosRoot: ROOT,
  kind: "foreign",
  state: "processed",
  applied: true,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [lost],
};

const journal = (headerHash: string, txId: Buffer) =>
  insertOwnJournal({
    headerHash,
    baseHeaderHash: "c0".repeat(28),
    baseUtxosRoot: ROOT,
    expectedUtxosRoot: ROOT,
    spent: [],
    produced: [],
    txIds: [txId],
    at: new Date(1_000),
  });

type Variant = "race-lost" | "final" | "rolled-back" | "unfinished";

const counts = (variant: Variant) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "e2e-finalize-db-counts.test",
        });
        yield* resetApplicationTables;
        yield* journal(ownWon, won);
        yield* setJournalStatus(ownWon, JournalStatus.LocallyApplied);
        yield* sql`INSERT INTO immutable (tx_id, tx) VALUES (${won}, ${Buffer.from([0x84])})`;
        yield* journal(ownLost, lost);
        yield* setJournalStatus(
          ownLost,
          variant === "unfinished"
            ? JournalStatus.SubmittedUnconfirmed
            : JournalStatus.Abandoned,
        );
        yield* sql`INSERT INTO mempool (tx_id, tx) VALUES
          (${lost}, ${Buffer.from([0x84])}), (${pending}, ${Buffer.from([0x84])})`;
        yield* processRow(foreign);
        if (variant === "rolled-back") yield* rollBackRows([foreign], []);
        if (variant === "final") {
          // The other operator's block folds and its fold becomes final.
          yield* Frontier.upsert({ headerHash: ownWon, utxosRoot: ROOT });
          yield* foldMerge(foreign, {
            slot: 4_000,
            outRef: `${"e1".repeat(32)}#0`,
          });
          yield* pruneMerges(4_000);
        }
        const found = yield* collectDbCounts();
        yield* resetApplicationTables;
        return Object.fromEntries(
          [
            "pending_finalization_tx_headers",
            "pending_finalizations_finalized_tx_headers",
            "pending_finalizations_unfinished",
            "committed_l2_txs",
          ].map((label) => [label, found.get(label)]),
        );
      }),
    ) as Effect.Effect<Record<string, bigint | undefined>, unknown, never>,
  );

describe.skipIf(!dbEnabled)(
  "finalizer counts on a node that lost a block race",
  () => {
    it("counts only the journals that can finish, and every tx a landed block committed", async () =>
      expect(await counts("race-lost")).toEqual({
        pending_finalization_tx_headers: 1n,
        pending_finalizations_finalized_tx_headers: 1n,
        pending_finalizations_unfinished: 0n,
        committed_l2_txs: 2n,
      }));
    it("still counts that tx once the landed block's fold is final", async () =>
      expect((await counts("final")).committed_l2_txs).toBe(2n));
    it("does not count a tx whose landed block was rolled back", async () =>
      expect((await counts("rolled-back")).committed_l2_txs).toBe(1n));
    it("still counts a journal that has not finished", async () =>
      expect(await counts("unfinished")).toMatchObject({
        pending_finalization_tx_headers: 2n,
        pending_finalizations_finalized_tx_headers: 1n,
        pending_finalizations_unfinished: 1n,
      }));
  },
);
