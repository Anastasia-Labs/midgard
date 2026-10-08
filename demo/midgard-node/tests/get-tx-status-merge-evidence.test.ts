import { HttpServerRequest } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
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
import type {
  EventHistoryOwner,
  HistoryOwnerCoverage,
} from "../src/services/event-history-owner.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { canonicalManifest } from "./deployment-manifest.canonical-identity.js";
import { provideDatabaseLayers } from "./utils.js";

const txId = Buffer.alloc(32, 0xaa);
const header = Buffer.alloc(28, 0xbb);
const otherHeader = Buffer.alloc(28, 0xcc);
const root = "11".repeat(32);
const ownerToken = "00000000-0000-4000-8000-000000000001";
const unavailable = new HistoryRecoverySuperseded({ message: "test rollback" });
const coverage: HistoryOwnerCoverage = {
  bindingDigest: "22".repeat(32),
  checkpointRevision: "1",
  point: { id: "33".repeat(32), slot: 1 },
  snapshotDigest: "44".repeat(32),
  includedThroughMs: 1,
};
type Options = {
  status?: string;
  job?: string;
  outcome?: string;
  foreign?: boolean;
  wrongPolicy?: boolean;
  wrongRoot?: boolean;
  ambiguous?: boolean;
  oldAbandoned?: boolean;
  revoked?: boolean;
  authority?: string;
  expired?: boolean;
  missingOwner?: boolean;
  superseded?: boolean;
};
const fixtureOwner = (
  manifestId: string,
  superseded: boolean,
): EventHistoryOwner => ({
  close: Effect.void,
  requestReconciliation: () => Effect.void,
  reconciliationStatus: Effect.succeed(undefined),
  sourceStatus: Effect.succeed({
    state: "following",
    reason: null,
    since: null,
    escalated: false,
    attempts: 0,
    lastError: null,
  }),
  retentionHold: Effect.succeed(undefined),
  frontier: Effect.succeed({
    ready: true,
    headHeight: 1,
    tipHeight: 1,
    lagBlocks: 0,
    maximumLagBlocks: 5,
  }),
  awaitReady: Effect.void,
  awaitReadyAt: () => Effect.succeed(coverage),
  awaitStopped: Effect.void,
  runProducer: (work) =>
    work(
      { deploymentIdentity: manifestId, ownerToken, generation: "1" },
      Effect.void,
      coverage,
    ).pipe(
      Effect.tap(() => (superseded ? Effect.fail(unavailable) : Effect.void)),
    ),
});

const query = (route: "GET" | "batch", options: Options = {}) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const manifest = canonicalManifest();
        const globals = yield* Globals;
        yield* Ref.set(
          globals.EVENT_HISTORY_OWNER,
          options.missingOwner
            ? undefined
            : fixtureOwner(manifest.manifestId, options.superseded ?? false),
        );
        const sql = yield* SqlClient.SqlClient;
        return yield* sql.withTransaction(
          Effect.gen(function* () {
            // Connection-local fabricated rows exercise actual joins, never deployed state.
            yield* sql`CREATE TEMP TABLE pending_block_finalizations (header_hash bytea PRIMARY KEY, status text, expected_utxos_root text) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE pending_block_finalization_txs (header_hash bytea, member_id bytea) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE local_mutation_jobs (job_id text, kind text, status text, payload jsonb) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE da_payload_terminal_outcomes (header_hash bytea, terminal_outcome text, transition_kind text, deployment_identity_digest bytea, state_queue_policy_id bytea, finality_depth bigint) ON COMMIT DROP`;
            yield* sql`CREATE TEMP TABLE event_history_authority (singleton boolean, deployment_identity bytea, owner_token uuid, generation bigint, state text, lease_until timestamptz) ON COMMIT DROP`;
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
              yield* sql`INSERT INTO da_payload_terminal_outcomes VALUES (${header}, ${options.outcome}, ${options.outcome === "merged" ? "merge" : "fraud_removal"}, ${Buffer.from(options.foreign ? "66".repeat(32) : manifest.manifestId, "hex")}, ${Buffer.from(options.wrongPolicy ? "77".repeat(28) : manifest.contracts.stateQueueMint.scriptHash, "hex")}, ${manifest.l1Finality.confirmationDepth}::bigint)`;
            }
            yield* sql`INSERT INTO event_history_authority VALUES (true, ${Buffer.from(manifest.manifestId, "hex")}, ${ownerToken}::uuid, 1, ${options.authority ?? "ready"}, clock_timestamp() + (${options.expired ? -1 : 60000} * interval '1 millisecond'))`;
            if (options.revoked)
              yield* sql`DELETE FROM da_payload_terminal_outcomes`;
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
    it("reports a completed local merge of the exact current authenticated header", async () => {
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
      { job: "completed", outcome: "merged", foreign: true },
      { job: "completed", outcome: "merged", wrongPolicy: true },
      { job: "completed", outcome: "merged", wrongRoot: true },
      { job: "completed", outcome: "merged", ambiguous: true },
      { job: "completed", outcome: "merged", status: "abandoned" },
      { job: "completed", outcome: "merged", revoked: true },
      { job: "completed", outcome: "merged", authority: "recovering" },
      { job: "completed", outcome: "merged", expired: true },
      { job: "completed", outcome: "merged", missingOwner: true },
      { job: "completed", outcome: "merged", superseded: true },
    ])(
      "refuses missing, revoked or mismatched merge authority: %j",
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
