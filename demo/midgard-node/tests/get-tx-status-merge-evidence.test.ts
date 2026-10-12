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

import { getTxStatusHandler } from "../src/commands/listen-router.get-tx-status-handler.js";
import { postTxStatusBatchHandler } from "../src/commands/listen-router.post-tx-status-batch-handler.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { canonicalManifest } from "./deployment-manifest.canonical-identity.js";
import { provideDatabaseLayers } from "./utils.js";

const txId = Buffer.alloc(32, 0xaa);
const header = Buffer.alloc(28, 0xbb);
const otherHeader = Buffer.alloc(28, 0xcc);
const root = "11".repeat(32);
/** The follower's covered tip height in every case. */
const TIP_HEIGHT = 1_000;
type Options = {
  status?: string;
  job?: string;
  outcome?: string;
  /** The merge tx is one block short of `safe` (cd deep) at the tip. */
  shallow?: boolean;
  wrongRoot?: boolean;
  ambiguous?: boolean;
  oldAbandoned?: boolean;
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
            yield* sql`CREATE TEMP TABLE pending_block_finalizations (header_hash bytea PRIMARY KEY, status text, expected_utxos_root text) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE pending_block_finalization_txs (header_hash bytea, member_id bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE local_mutation_jobs (job_id text, kind text, status text, payload jsonb) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE node_l1_queue_terminals (header_hash bytea, terminal_outcome text, height bigint) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE l1_follower_cursor (height bigint) ON COMMIT DROP`;
            yield* sql`INSERT INTO l1_follower_cursor VALUES (${TIP_HEIGHT})`;
            yield* sql`CREATE TEMP TABLE node_follower_write_gate (singleton boolean, applied_generation bigint, pending_reason text) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE immutable (tx_id bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE processed_mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE tx_rejections (tx_id bytea, reject_code text, reject_detail text, created_at timestamptz) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE tx_admissions (tx_id bytea, status text) ON COMMIT DROP`;
            yield* sql`INSERT INTO immutable VALUES (${txId})`;
            yield* sql`INSERT INTO pending_block_finalizations VALUES (${header}, ${options.status ?? "locally_applied"}, ${root})`;
            yield* sql`INSERT INTO pending_block_finalization_txs VALUES (${header}, ${txId})`;
            if (options.oldAbandoned || options.ambiguous) {
              yield* sql`INSERT INTO pending_block_finalizations VALUES (${otherHeader}, ${options.oldAbandoned ? "abandoned" : "locally_applied"}, ${root})`;
              yield* sql`INSERT INTO pending_block_finalization_txs VALUES (${otherHeader}, ${txId})`;
            }
            if (options.job) {
              yield* sql`INSERT INTO local_mutation_jobs VALUES (${"confirmed_merge_finalization:" + header.toString("hex")}, 'confirmed_merge_finalization', ${options.job}, ${JSON.stringify({ headerHash: header.toString("hex"), confirmedLedgerSnapshotRoot: options.wrongRoot ? "55".repeat(32) : root })}::jsonb)`;
            }
            if (options.outcome) {
              // `safe`: at least cd deep at the tip (depth = tip - height + 1).
              const safeHeight =
                TIP_HEIGHT - manifest.l1Finality.confirmationDepth + 1;
              yield* sql`INSERT INTO node_l1_queue_terminals VALUES (${header}, ${options.outcome}, ${options.shallow ? safeHeight + 1 : safeHeight})`;
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
    it("reports a completed local merge of the exact current header", async () => {
      expect(
        (await query(route, { job: "completed", outcome: "merged" })).body,
      ).toMatchObject({
        status: "committed",
        headerHash: header.toString("hex"),
        confirmedLedgerFinalized: true,
        mergeStatus: "finalized",
      });
    });
    it.each<Options>([
      { job: "running", outcome: "merged" },
      { job: "failed", outcome: "merged" },
      { job: "completed" },
      { outcome: "merged" },
      { job: "completed", outcome: "removed" },
      { job: "completed", outcome: "merged", shallow: true },
      { job: "completed", outcome: "merged", wrongRoot: true },
      { job: "completed", outcome: "merged", ambiguous: true },
      { job: "completed", outcome: "merged", status: "abandoned" },
      { job: "completed", outcome: "merged", revoked: true },
      { job: "completed", outcome: "merged", gate: "pending" },
      { job: "completed", outcome: "merged", gate: "unapplied" },
    ])(
      "refuses missing, revoked or mismatched merge evidence, or a gate a driver recompute holds: %j",
      async (options) => {
        const result = await query(route, options);
        expect(result.status).toBe(200);
        expect(result.body.confirmedLedgerFinalized).toBe(false);
      },
    );
    it("uses the current merged incarnation while retaining an abandoned predecessor", async () => {
      expect(
        (
          await query(route, {
            job: "completed",
            outcome: "merged",
            oldAbandoned: true,
          })
        ).body.confirmedLedgerFinalized,
      ).toBe(true);
    });
  });
