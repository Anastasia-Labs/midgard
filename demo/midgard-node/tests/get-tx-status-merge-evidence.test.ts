import { HttpServerRequest } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

vi.mock("../src/database/index.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  const actual =
    await importOriginal<typeof import("../src/database/index.js")>();
  return {
    ...actual,
    ImmutableDB: {
      ...actual.ImmutableDB,
      retrieveTxCborsByHashes: () =>
        Effect.succeed([{ tx_cbor: Buffer.from([1]) }]),
    },
    MempoolDB: {
      ...actual.MempoolDB,
      retrieveTxCborsByHashes: () => Effect.succeed([]),
    },
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      retrieveTxCborsByHashes: () => Effect.succeed([]),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      getByTxId: () => Effect.succeed({ status: "accepted" }),
    },
    TxRejectionsDB: {
      ...actual.TxRejectionsDB,
      retrieveByTxId: () => Effect.succeed([]),
    },
  };
});

import {
  getTxStatusHandler,
  postTxStatusBatchHandler,
} from "../src/commands/http/transaction-status.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { canonicalManifest } from "./deployment-manifest.canonical-identity.js";
import { provideDatabaseLayers } from "./utils.js";

const txId = Buffer.alloc(32, 0xaa);
const header = Buffer.alloc(28, 0xbb);
const otherHeader = Buffer.alloc(28, 0xcc);
const base = Buffer.alloc(28, 0x0b);
/** The follower's covered tip height in every case. */
const TIP_HEIGHT = 1_000;
/** The derived status of a journal holding the tx: folded (its retained
 * fold), applied or landed (its landed row), or dead (unlanded, with no
 * commit anchor). */
type JournalStatus = "folded" | "applied" | "landed" | "dead";
type Options = {
  /** The status of the tx's journal; `applied` by default. */
  status?: JournalStatus;
  outcome?: string;
  /** The merge tx is one block short of `safe` (cd deep) at the tip. */
  shallow?: boolean;
  /** Another applied journal holds the tx too. */
  ambiguous?: boolean;
  /** A dead journal of another header holds the tx too. */
  deadPredecessor?: boolean;
  revoked?: boolean;
  /** The follower write gate: open at an applied view (the default), a
   * driver recompute pending, or no view applied yet. */
  gate?: "open" | "pending" | "unapplied";
};
const query = (route: "GET" | "batch", options: Options = {}) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const manifest = canonicalManifest();
        const sql = yield* SqlClient.SqlClient;
        return yield* sql.withTransaction(
          Effect.gen(function* () {
            // Connection-local fabricated rows exercise actual joins, never deployed state.
            yield* sql`CREATE TEMP TABLE pending_block_finalizations (header_hash bytea PRIMARY KEY, base_tail_header_hash bytea, intended_tx_hash bytea, commit_anchor_hash bytea, commit_anchor_height bigint, commit_anchor_slot bigint, created_at timestamptz DEFAULT now()) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE pending_block_finalization_txs (header_hash bytea, member_id bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_confirmed_ledger_frontier (header_hash bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_confirmed_merges (header_hash bytea, parent_header_hash bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_landed_blocks (header_hash bytea, parent_header_hash bytea, state text, applied boolean) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_foreign_tx_inclusions (tx_id bytea, header_hash bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_l1_queue_terminals (header_hash bytea, terminal_outcome text, height bigint, tx_index integer) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE l1_blocks (hash bytea, slot bigint, height bigint) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE l1_intents (tx_hash bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE l1_follower_cursor (height bigint, pruned_through_slot bigint) ON COMMIT DROP`;
            yield* sql`INSERT INTO l1_follower_cursor VALUES (${TIP_HEIGHT}, 0)`;
            yield* sql`CREATE TEMP TABLE node_follower_write_gate (singleton boolean, applied_generation bigint, pending_reason text) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE immutable (tx_id bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE processed_mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE tx_rejections (tx_id bytea, reject_code text, reject_detail text, created_at timestamptz) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE tx_admissions (tx_id bytea, status text) ON COMMIT DROP`;
            yield* sql`INSERT INTO immutable VALUES (${txId})`;
            /** A journal of `headerHash` holding the tx, with `status`'s facts. */
            const journal = (headerHash: Buffer, status: JournalStatus) =>
              Effect.gen(function* () {
                yield* sql`INSERT INTO pending_block_finalizations (header_hash, base_tail_header_hash)
                  VALUES (${headerHash}, ${base})`;
                yield* sql`INSERT INTO pending_block_finalization_txs VALUES (${headerHash}, ${txId})`;
                if (status === "folded")
                  yield* sql`INSERT INTO node_confirmed_merges VALUES (${headerHash}, ${base})`;
                if (status === "applied" || status === "landed")
                  yield* sql`INSERT INTO node_landed_blocks VALUES (${headerHash}, ${base}, 'processed', ${status === "applied"})`;
              });
            yield* journal(header, options.status ?? "applied");
            if (options.ambiguous) yield* journal(otherHeader, "applied");
            if (options.deadPredecessor) yield* journal(otherHeader, "dead");
            if (options.outcome) {
              // `safe`: at least cd deep at the tip (depth = tip - height + 1).
              const safeHeight =
                TIP_HEIGHT - manifest.l1Finality.confirmationDepth + 1;
              yield* sql`INSERT INTO node_l1_queue_terminals VALUES (${header}, ${options.outcome}, ${options.shallow ? safeHeight + 1 : safeHeight}, 0)`;
            }
            const gate = options.gate ?? "open";
            yield* sql`INSERT INTO node_follower_write_gate VALUES (true,
              ${gate === "unapplied" ? null : 7},
              ${gate === "pending" ? "l1_driver_recompute_pending" : null})`;
            if (options.revoked)
              yield* sql`DELETE FROM node_l1_queue_terminals`;
            const handler =
              route === "GET"
                ? getTxStatusHandler.pipe(
                    Effect.provideService(ParsedSearchParams, {
                      tx_hash: txId.toString("hex"),
                    }),
                  )
                : postTxStatusBatchHandler;
            const response = yield* handler.pipe(
              Effect.provideService(ParsedSearchParams, {
                tx_hash: txId.toString("hex"),
              }),
              Effect.provideService(
                HttpServerRequest.HttpServerRequest,
                HttpServerRequest.fromWeb(
                  new Request("http://midgard.test/tx-status", {
                    method: "POST",
                    body: JSON.stringify({ txHashes: [txId.toString("hex")] }),
                  }),
                ),
              ),
              Effect.provideService(
                ContractDeploymentIdentity,
                ContractDeploymentIdentity.make({
                  kind: "manifest",
                  manifestId: manifest.manifestId,
                  consensusProfile: manifest.consensusProfile,
                  manifest,
                }),
              ),
            );
            if (response.body._tag !== "Uint8Array")
              throw new Error("expected JSON body");
            const body = JSON.parse(
              new TextDecoder().decode(response.body.body),
            );
            return {
              status: response.status,
              body: route === "GET" ? body : body.results[0],
            };
          }),
        );
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    ),
  );

for (const route of ["GET", "batch"] as const)
  describe(`${route} canonical merge evidence`, () => {
    it("does not call an L1-confirmed commitment an actual merge", async () => {
      expect((await query(route)).body.confirmedLedgerFinalized).toBe(false);
    });
    it("reports the merge of a folded header", async () => {
      expect(
        (await query(route, { status: "folded", outcome: "merged" })).body,
      ).toMatchObject({
        status: "committed",
        headerHash: header.toString("hex"),
        confirmedLedgerFinalized: true,
        mergeStatus: "finalized",
      });
    });
    it.each<Options>([
      { status: "applied", outcome: "merged" },
      { status: "landed", outcome: "merged" },
      { status: "folded" },
      { status: "dead" },
      { status: "folded", outcome: "removed" },
      { status: "folded", outcome: "merged", shallow: true },
      { status: "folded", outcome: "merged", ambiguous: true },
      { status: "folded", outcome: "merged", revoked: true },
      { status: "folded", outcome: "merged", gate: "pending" },
      { status: "folded", outcome: "merged", gate: "unapplied" },
    ])(
      "refuses missing, revoked or unfolded merge evidence, a dead journal, or a gate a driver recompute holds: %j",
      async (options) => {
        const result = await query(route, options);
        expect(result.status).toBe(200);
        expect(result.body.confirmedLedgerFinalized).toBe(false);
      },
    );
    it("uses the current merged incarnation while retaining a dead predecessor", async () => {
      expect(
        (
          await query(route, {
            status: "folded",
            outcome: "merged",
            deadPredecessor: true,
          })
        ).body.confirmedLedgerFinalized,
      ).toBe(true);
    });
  });
