import "./utils.js";

import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpRouter } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

vi.mock("@al-ft/midgard-core/deployment-profile", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("@al-ft/midgard-core/deployment-profile")
    >();
  return {
    ...actual,
    SELECTED_DEPLOYMENT_PROFILE: {
      ...actual.SELECTED_DEPLOYMENT_PROFILE,
      name: "preprod-public",
    },
  };
});

import { withHttpReadSql } from "../src/commands/listen-http-read.js";
import { getPipelineStatusHandler } from "../src/commands/listen-router.get-pipeline-status-handler.js";
import {
  COMMIT_ANCHOR_SOURCE,
  COMMIT_ANCHOR_UNAVAILABLE,
} from "../src/fibers/block-commitment.commit-anchor-readiness.js";
import {
  L1_OWN_BLOCK_EVENT_ORPHANED,
  OWN_BLOCK_EVENT_ORPHANED_SOURCE,
} from "../src/fibers/block-commitment.own-block-event-orphaned.js";
import {
  applyCommitWorkerReadiness,
  COMMIT_BASE_PENDING,
  COMMIT_BASE_SOURCE,
  COMMIT_WINDOW_PENDING,
  COMMIT_WINDOW_SOURCE,
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_SOURCE,
} from "../src/fibers/block-commitment.worker-readiness.js";
import {
  NATIVE_MPF_OWNER_RECOVERY_PENDING,
  NATIVE_MPF_OWNER_STUCK,
  NATIVE_MPF_OWNER_SUPERVISOR_SOURCE,
  NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
} from "../src/fibers/native-mpf-owner-supervisor.js";
import {
  OPERATOR_WATCHDOG_MANIFEST_MISMATCH,
  OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
} from "../src/fibers/operator-watchdog.manifest-gate.js";
import { RETENTION_L1_VIEW_STALE } from "../src/fibers/retention-sweeper.js";
import { Globals } from "../src/services/globals.js";
import {
  L1_NODE_CONFIG_FAILED,
  L1_NODE_CONFIG_UNREADABLE,
} from "../src/services/l1-follower.readiness.js";
import {
  COMMIT_DA_FRAME_EVENTS_OVERFLOW,
  COMMIT_DA_FRAME_SOURCE,
  MPF_CLOSURE_MISSING,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const PRIVATE_PROVIDER = "untrusted-provider/opaque-base64-password==";
const PRIVATE_NATIVE = "native diagnostic opaque/SECRET/==";
const PRIVATE_LEASE = "private-lease-token:token/SECRET/==";
const PRIVATE_JOB = "upstream payout error password=opaque/base64==";
const DEPLOYMENT = "a1".repeat(32);
const PENDING_BLOCK_TX_HASH = "b2".repeat(32);
const EVENT_ID = `d8799f5820${"a2".repeat(32)}00ff`;

const seedLease = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`INSERT INTO state_queue_mutation_leases (token, scope, holder, status, expires_at, last_error)
    VALUES (${PRIVATE_LEASE}, 'state_queue', 'commit', 'active', NOW() + interval '1 minute', ${PRIVATE_JOB})`;
});

// The profile injection selects presentation policy only. These are actual
// HTTP handlers over real SQL, not a compiled-public release/image proof.
describe("public status HTTP presentation", () => {
  it.each([
    ["waiting", L1_NODE_CONFIG_UNREADABLE],
    ["failed", L1_NODE_CONFIG_FAILED],
  ] as const)(
    "retains actual readyz %s node configuration refusal",
    async (kind, reason) => {
      const result = await readyz({
        useHttpTransport: true,
        l1Follower:
          kind === "waiting"
            ? {
                kind,
                reason: L1_NODE_CONFIG_UNREADABLE,
                detail: PRIVATE_PROVIDER,
              }
            : { kind, reason: L1_NODE_CONFIG_FAILED, detail: PRIVATE_PROVIDER },
      });
      expect(result.status).toBe(503);
      expect(result.ready).toBe(false);
      expect(result.reasons).toContain(reason);
      expect(result.l1Follower).toMatchObject({
        state: kind,
        readiness: [{ reason, detail: "E_READINESS_DIAGNOSTIC_REDACTED" }],
      });
      expect(JSON.stringify(result)).not.toContain(PRIVATE_PROVIDER);
    },
  );

  const commitOutputs: readonly [string, string, WorkerOutput][] = [
    [
      COMMIT_WORKER_SOURCE,
      COMMIT_WORKER_FAILED,
      { type: "FailureOutput", error: PRIVATE_JOB },
    ],
    [
      COMMIT_BASE_SOURCE,
      COMMIT_BASE_PENDING,
      {
        type: "AwaitingCommitBaseOutput",
        baseHeaderHash: "ab".repeat(28),
        detail: PRIVATE_JOB,
      },
    ],
    [
      COMMIT_WINDOW_SOURCE,
      COMMIT_WINDOW_PENDING,
      {
        type: "AwaitingNextCommitWindowOutput",
        heldHeaderHash: "ab".repeat(28),
        detail: PRIVATE_JOB,
      },
    ],
  ];
  it.each(commitOutputs)(
    "retains actual readyz worker producer %s/%s",
    async (source, reason, output) => {
      const result = await readyz({
        useHttpTransport: true,
        globalsSetup: (globals) => applyCommitWorkerReadiness(globals, output),
      });
      expect(result.status).toBe(503);
      expect(result.ready).toBe(false);
      expect(result.reasons).toContain(reason);
      expect(result).toMatchObject({
        livenessReasons: [
          {
            source,
            reason,
            ageMs: expect.any(Number),
            detail: "E_READINESS_DIAGNOSTIC_REDACTED",
          },
        ],
      });
      expect(JSON.stringify(result)).not.toContain(PRIVATE_JOB);
    },
  );

  // Exact exported reason constants pass through the same incident publisher
  // used by these producers. This tests the HTTP presentation, not each fiber.
  it.each([
    [COMMIT_ANCHOR_SOURCE, COMMIT_ANCHOR_UNAVAILABLE],
    ["retention_sweeper", RETENTION_L1_VIEW_STALE],
    [OWN_BLOCK_EVENT_ORPHANED_SOURCE, L1_OWN_BLOCK_EVENT_ORPHANED],
    ["operator_watchdog_manifest", OPERATOR_WATCHDOG_MANIFEST_MISMATCH],
    ["operator_watchdog_manifest", OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED],
    [NATIVE_MPF_OWNER_SUPERVISOR_SOURCE, NATIVE_MPF_OWNER_STUCK],
    [NATIVE_MPF_OWNER_SUPERVISOR_SOURCE, NATIVE_MPF_OWNER_RECOVERY_PENDING],
    [
      NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
      NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
    ],
    ["landed_block_rebase", MPF_CLOSURE_MISSING],
    ["landed_block_rebase", NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED],
    ["landed_block_rebase", NATIVE_MPF_RESTORE_READ_TRANSIENT],
    [COMMIT_DA_FRAME_SOURCE, COMMIT_DA_FRAME_EVENTS_OVERFLOW],
  ])(
    "retains actual readyz incident publication %s/%s",
    async (source, reason) => {
      const result = await readyz({
        useHttpTransport: true,
        globalsSetup: (globals) =>
          raiseLivenessIncident(globals, source, reason, PRIVATE_JOB),
      });
      expect(result.status).toBe(503);
      expect(result.ready).toBe(false);
      expect(result.reasons).toContain(reason);
      expect(result).toMatchObject({
        livenessReasons: [
          {
            source,
            reason,
            ageMs: expect.any(Number),
            detail: "E_READINESS_DIAGNOSTIC_REDACTED",
          },
        ],
      });
      expect(JSON.stringify(result)).not.toContain(PRIVATE_JOB);
    },
  );

  it("suppresses raw readyz provider/native diagnostics and lease tokens while keeping refusals and observations", async () => {
    const result = await readyz({
      useHttpTransport: true,
      provider: [
        { healthy: true, agoMs: 1_000 },
        { healthy: false, agoMs: 0 },
      ],
      providerError: PRIVATE_PROVIDER,
      nativeDiagnosticsError: PRIVATE_NATIVE,
      journal: seedLease,
    });
    const wire = JSON.stringify(result);
    for (const secret of [
      PRIVATE_PROVIDER,
      PRIVATE_NATIVE,
      PRIVATE_LEASE,
      PRIVATE_JOB,
    ])
      expect(wire).not.toContain(secret);
    expect(result.status).toBe(503);
    expect(result.ready).toBe(false);
    expect(result.reasons).toContain("native_mpf_owner_unhealthy");
    expect(result).toMatchObject({
      providerQueryHealthy: false,
      blockCommitmentCoordination: {
        commitWorkerActive: false,
        commitPipelinePhase: "idle",
      },
      providerQueryError: "E_READINESS_L1_PROVIDER_UNAVAILABLE",
      nativeMpfOwner: {
        healthy: false,
        error: "E_READINESS_NATIVE_OWNER_UNAVAILABLE",
      },
      stateQueueMutationLease: {
        status: "busy",
        activeLease: { holder: "commit", status: "active", expired: false },
      },
    });
    expect(result.details[0]).toMatch(
      /^provider_query_degraded:l1-provider:\d+$/u,
    );
  });

  it("suppresses diagnostics on the early readyz database failure without changing503 or db_unhealthy", async () => {
    const result = await readyz({
      useHttpTransport: true,
      databaseDown: true,
      settlementDetail: PRIVATE_JOB,
    });
    expect(JSON.stringify(result)).not.toContain(PRIVATE_JOB);
    expect(result.status).toBe(503);
    expect(result.ready).toBe(false);
    expect(result.reasons).toEqual(["db_unhealthy"]);
    expect(result).toMatchObject({
      settlement: { state: "starting", observedAt: expect.any(Number) },
    });
  });

  it("suppresses pipeline lease/job diagnostics while preserving the real status, counts, IDs and due dates", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          yield* seedLease;
          const globals = yield* Globals;
          yield* Ref.set(
            globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
            PENDING_BLOCK_TX_HASH,
          );
          yield* Ref.set(
            globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
            Date.now() - 1_000,
          );
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase, last_error, failures)
        VALUES (${DEPLOYMENT}, 'withdrawal', ${EVENT_ID}, 'fund', ${PRIVATE_JOB}, 3)`;
          const handler = yield* NodeHttpServer.makeHandler(
            HttpRouter.empty.pipe(
              HttpRouter.get(
                "/pipeline-status",
                withHttpReadSql(getPipelineStatusHandler),
              ),
            ),
          );
          const server = createServer(handler);
          server.listen(0, "127.0.0.1");
          yield* Effect.promise(() => once(server, "listening"));
          yield* Effect.addFinalizer(() =>
            Effect.promise(async () => {
              server.closeAllConnections();
              await new Promise<void>((resolve) =>
                server.close(() => resolve()),
              );
            }),
          );
          const response = yield* Effect.promise(() =>
            fetch(
              `http://127.0.0.1:${(server.address() as AddressInfo).port}/pipeline-status`,
              { signal: AbortSignal.timeout(5_000) },
            ),
          );
          const text = yield* Effect.promise(() => response.text());
          expect(text).not.toContain(PRIVATE_LEASE);
          expect(text).not.toContain(PRIVATE_JOB);
          expect(response.status).toBe(200);
          const body: unknown = JSON.parse(text);
          expect(body).toMatchObject({
            status: "ok",
            stateQueue: {
              unconfirmedSubmittedBlockTxHash: PENDING_BLOCK_TX_HASH,
              unconfirmedSubmittedBlockAgeMs: expect.any(Number),
            },
            durableAdmission: { backlog: "0" },
            localResidue: { mempoolTxCount: "0", processedMempoolTxCount: "0" },
            localMutationJobs: { unfinished: "0" },
            stateQueueMutationLease: {
              status: "busy",
              activeLease: { holder: "commit", status: "active" },
            },
            settlement: {
              unfinishedJobs: "1",
              failingJobs: [
                {
                  kind: "withdrawal",
                  eventId: EVENT_ID,
                  phase: "fund",
                  failures: 3,
                  dueAt: expect.any(String),
                },
              ],
            },
          });
        }).pipe(
          Effect.provide(Globals.Default),
          Effect.provideService(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make({
              kind: "manifest",
              manifestId: DEPLOYMENT,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            }),
          ),
          Effect.scoped,
        ),
      ),
    );
  });
});
