import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { ForeignTipReconciliationsDB } from "../src/database/index.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import {
  nonEmptyWindowHeader,
  recordForeignTip,
} from "./foreign-tip-gate.fixtures.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { seedVerifiedForeignBase } from "./readiness-verified-foreign-base.fixture.js";
import { withFailingStatements } from "./sql-fault-injection.js";
import { provideDatabaseLayers } from "./utils.js";

// Stable valid headers are recorded through the public DB API, not raw inserts.
const seedAwaitingForeignTips = Effect.forEach(["31", "32"], (byte) =>
  Effect.gen(function* () {
    const id = yield* recordForeignTip(
      nonEmptyWindowHeader({
        prevHeaderHash: byte.repeat(28),
        startTime: 1n,
        endTime: 2n,
      }),
    );
    const retained =
      yield* ForeignTipReconciliationsDB.retrieveAwaitingByForeignHeaderHash(
        id,
      );
    expect(Option.isSome(retained)).toBe(true);
    const row = Option.getOrThrow(retained);
    const status = row[ForeignTipReconciliationsDB.Columns.STATUS];
    expect(status).toBe(ForeignTipReconciliationsDB.Status.Awaiting);
    return {
      id,
      status,
      evidenceKind: row[ForeignTipReconciliationsDB.Columns.EVIDENCE_KIND],
    };
  }),
);
const inspectForeignTips = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly database: string }>`
    SELECT current_database() AS database
  `;
  return {
    database: rows[0]!.database,
    awaiting: yield* ForeignTipReconciliationsDB.countAwaiting,
  };
});

// Only the settings /readyz reads. Provider evidence is always published
// before the request, so the handler never probes a real provider.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

const responsiveOwner = {
  diagnostics: () =>
    Promise.resolve({
      ownerEpoch: Buffer.alloc(16, 1),
      durableRoot: "ab".repeat(32),
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    }),
} as unknown as NativeMpfOwnerService;

type ProviderObservation = {
  readonly healthy: boolean;
  readonly agoMs: number;
};

type Readyz = {
  readonly status: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly details: readonly string[];
  readonly dbError?: string;
  readonly settlement?: unknown;
  readonly pendingFinalizationAgeMs?: number | null;
  readonly signedIntentUnresolvedAgeMs?: number | null;
  readonly providerQueryHealthy?: boolean;
};

const SUCCESS_NOW: readonly ProviderObservation[] = [
  { healthy: true, agoMs: 0 },
];

const readyz = ({
  provider = SUCCESS_NOW,
  databaseDown = false,
  journal = Effect.void,
  ogmiosTipMaxAgeMs,
  forceProviderProbe = false,
  foreignBaseVerified = true,
}: {
  readonly provider?: readonly ProviderObservation[];
  readonly databaseDown?: boolean;
  readonly journal?: Effect.Effect<void, unknown, never>;
  readonly ogmiosTipMaxAgeMs?: number;
  readonly forceProviderProbe?: boolean;
  /** False models a node whose commitment tick has not yet checked the
   * canonical base of its current history authority. */
  readonly foreignBaseVerified?: boolean;
} = {}): Promise<Readyz> =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Cleared before and after: the handler reads journal ages from the
        // real tables, so a row left here would leak into the next file.
        const clear = sql`TRUNCATE TABLE pending_block_finalizations,
          state_queue_mutation_leases, event_history_authority, foreign_tip_reconciliations
          RESTART IDENTITY CASCADE`;
        yield* clear;
        return yield* Effect.gen(function* () {
          yield* journal;
          const globals = yield* Globals;
          if (foreignBaseVerified) yield* seedVerifiedForeignBase(globals);
          for (const observation of provider)
            yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
              nextL1ProviderHealthEvidence({
                current,
                healthy: observation.healthy,
                ...(observation.healthy
                  ? {}
                  : { error: "HubOracle query failed: fetch failed" }),
                observedAtMs: Date.now() - observation.agoMs,
                successKind: "exact",
              }),
            );
          yield* Ref.set(globals.NATIVE_MPF_OWNER, responsiveOwner);
          // A serving node has authenticated its own active membership.
          yield* Ref.set(globals.OPERATOR_MEMBERSHIP, "active");
          const request = buildListenRouter().pipe(
            Effect.provideService(
              HttpServerRequest.HttpServerRequest,
              HttpServerRequest.fromWeb(
                new Request("http://midgard.test/readyz"),
              ),
            ),
          );
          const response = (yield* databaseDown
            ? request.pipe(
                Effect.provideService(
                  SqlClient.SqlClient,
                  withFailingStatements(sql, () => true),
                ),
              )
            : request) as HttpServerResponse.HttpServerResponse;
          const web = HttpServerResponse.toWeb(response);
          const body = (yield* Effect.promise(() => web.json())) as Omit<
            Readyz,
            "status"
          >;
          return { status: web.status, ...body };
        }).pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(NodeConfig, {
          ...nodeConfig,
          ...(forceProviderProbe
            ? {
                READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 1,
                L1_PROVIDER: "Kupmios",
                L1_OGMIOS_KEY: "http://ogmios.readyz.test",
                L1_KUPO_KEY: "http://kupo.readyz.test",
                NETWORK: "Custom",
                L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: 60_000,
              }
            : {}),
        }),
        Effect.provideService(ValidationPool, {
          poolSize: 1,
          stats: Effect.succeed({
            oldestInFlightAgeMs: 0,
            liveWorkers: 1,
            restartingWorkers: 0,
          }),
        } as unknown as ValidationPool["Type"]),
        Effect.provideService(Lucid, { ogmiosTipMaxAgeMs } as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as unknown as Effect.Effect<Readyz, unknown, never>,
    ),
  );

const signIntent = (headerHash: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txHash = Buffer.alloc(32, 9);
    yield* sql`UPDATE pending_block_finalizations
      SET prepared_tx_hash = ${txHash}, intended_tx_hash = ${txHash},
        signed_tx_cbor = ${Buffer.from("84a0a0f5f6", "hex")}
      WHERE header_hash = ${headerHash}`;
  });

const backdateJournals = (ms: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET created_at = NOW() - make_interval(secs => ${ms / 1000})`;
  });

const asJournalSetup = <E, R>(effect: Effect.Effect<void, E, R>) =>
  effect as unknown as Effect.Effect<void, unknown, never>;

describe("GET /readyz under internal transients", () => {
  it("isolates incoming Awaiting foreign-tip rows before and after a healthy request", async () => {
    const trace = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* ForeignTipReconciliationsDB.clear;
          const seeds = yield* seedAwaitingForeignTips;
          const before = yield* inspectForeignTips;
          let entry: typeof before | undefined;
          const response = yield* Effect.promise(() =>
            readyz({
              journal: asJournalSetup(
                Effect.gen(function* () {
                  entry = yield* inspectForeignTips;
                }),
              ),
            }),
          );
          return {
            seeds,
            before,
            entry,
            response,
            after: yield* inspectForeignTips,
          };
        }).pipe(
          Effect.ensuring(Effect.orDie(ForeignTipReconciliationsDB.clear)),
        ),
      ),
    );
    console.info("readiness honest SQL isolation trace", JSON.stringify(trace));
    expect(new Set(trace.seeds.map((seed) => seed.id)).size).toBe(2);
    expect(trace.before.awaiting).toBe(2);
    expect(trace.response.status).toBe(200);
    expect(trace.response.ready).toBe(true);
    expect(trace.response.reasons).toEqual([]);
    expect(trace.entry).toEqual({
      database: trace.before.database,
      awaiting: 0,
    });
    expect(trace.after).toEqual({
      database: trace.before.database,
      awaiting: 0,
    });
  });

  it("refuses active Awaiting foreign-tip rows seeded inside the readiness fixture", async () => {
    const trace = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* ForeignTipReconciliationsDB.clear;
          let active:
            | Effect.Effect.Success<typeof inspectForeignTips>
            | undefined;
          let seeds:
            | Effect.Effect.Success<typeof seedAwaitingForeignTips>
            | undefined;
          const response = yield* Effect.promise(() =>
            readyz({
              journal: asJournalSetup(
                Effect.gen(function* () {
                  seeds = yield* seedAwaitingForeignTips;
                  active = yield* inspectForeignTips;
                }),
              ),
            }),
          );
          return { seeds, active, response };
        }).pipe(
          Effect.ensuring(Effect.orDie(ForeignTipReconciliationsDB.clear)),
        ),
      ),
    );
    console.info("readiness active foreign-tip trace", JSON.stringify(trace));
    expect(trace.active?.awaiting).toBe(2);
    expect(trace.response.status).toBe(503);
    expect(trace.response.ready).toBe(false);
    expect(trace.response.reasons).toEqual([
      "foreign_tip_reconciliation_awaiting:2",
    ]);
  });

  it("holds readiness until the current authority's foreign base is verified", async () => {
    const response = await readyz({ foreignBaseVerified: false });
    expect(response.status).toBe(503);
    expect(response.ready).toBe(false);
    expect(response.reasons).toEqual(["foreign_base_verification_unobserved"]);
  });

  it.each([
    { boundMs: 200_000, ageMs: 150_000, healthy: true },
    { boundMs: 10_000, ageMs: 15_000, healthy: false },
    { boundMs: 600_000, ageMs: 350_000, healthy: true },
  ])(
    "uses the runtime $boundMs ms tip bound in the raw probe for a $ageMs ms gap",
    async ({ boundMs, ageMs, healthy }) => {
      const calls: string[] = [];
      vi.stubGlobal("fetch", async (url: string) => {
        calls.push(url);
        if (url === "http://kupo.readyz.test/health") return new Response("ok");
        return new Response(
          JSON.stringify(
            url.endsWith("/health")
              ? {
                  connectionStatus: "connected",
                  networkSynchronization: 1,
                  lastKnownTip: { slot: 41 },
                  lastTipUpdate: new Date(Date.now() - ageMs).toISOString(),
                }
              : { jsonrpc: "2.0", result: { slot: 41 } },
          ),
        );
      });
      try {
        const response = await readyz({
          provider: [{ healthy: true, agoMs: 1_000 }],
          ogmiosTipMaxAgeMs: boundMs,
          forceProviderProbe: true,
        });
        expect(calls).toContain("http://ogmios.readyz.test/health");
        expect(response.providerQueryHealthy).toBe(healthy);
        // Failed probes remain within the existing readiness grace after a
        // recent exact success; the tip policy is distinct from that grace.
        expect(response.status).toBe(200);
        expect(response.details).toEqual(
          healthy
            ? []
            : [expect.stringMatching(/^provider_query_degraded:l1-provider:/u)],
        );
      } finally {
        vi.unstubAllGlobals();
      }
    },
  );

  it("answers a database that does not serve with 503 db_unhealthy, not a server error", async () => {
    const down = await readyz({ databaseDown: true });
    expect(down.status).toBe(503);
    expect(down.ready).toBe(false);
    expect(down.reasons).toEqual(["db_unhealthy"]);
    expect(down.details).toEqual([]);
    expect(down.dbError).toContain("Injected statement failure");
    expect(down.settlement).toBeDefined();

    const up = await readyz();
    expect(up.status).toBe(200);
    expect(up.ready).toBe(true);
    expect(up.reasons).toEqual([]);
    expect(up.dbError).toBeUndefined();
  });

  it("keeps a provider failure shortly after a success as a detail and stays ready", async () => {
    const degraded = await readyz({
      provider: [
        { healthy: true, agoMs: 60_000 },
        { healthy: false, agoMs: 0 },
      ],
    });
    expect(degraded.status).toBe(200);
    expect(degraded.ready).toBe(true);
    expect(degraded.reasons).toEqual([]);
    expect(degraded.details).toHaveLength(1);
    expect(degraded.details[0]).toMatch(
      /^provider_query_degraded:l1-provider:\d+$/u,
    );
  });

  it("goes unready on a provider failing past the bound, or one that never succeeded", async () => {
    for (const provider of [
      [
        { healthy: true, agoMs: 6 * 60_000 },
        { healthy: false, agoMs: 0 },
      ],
      [{ healthy: false, agoMs: 0 }],
    ]) {
      const unhealthy = await readyz({ provider });
      expect(unhealthy.status).toBe(503);
      expect(unhealthy.reasons).toEqual([
        "provider_query_unhealthy:l1-provider",
      ]);
      expect(unhealthy.details).toEqual([]);
    }
  });

  it("holds a provider failure as a detail for as long as the derived L1 tip bound allows", async () => {
    const provider = [
      { healthy: true, agoMs: 6 * 60_000 },
      { healthy: false, agoMs: 0 },
    ];
    const insideDerived = await readyz({
      provider,
      ogmiosTipMaxAgeMs: 10 * 60_000,
    });
    expect(insideDerived.status).toBe(200);
    expect(insideDerived.reasons).toEqual([]);
    expect(insideDerived.details[0]).toMatch(
      /^provider_query_degraded:l1-provider:\d+$/u,
    );
    const pastDerived = await readyz({
      provider,
      ogmiosTipMaxAgeMs: 5 * 60_000,
    });
    expect(pastDerived.status).toBe(503);
    expect(pastDerived.reasons).toEqual([
      "provider_query_unhealthy:l1-provider",
    ]);
  });

  it("reports how long journals have held back the next commit", async () => {
    const none = await readyz();
    expect(none.pendingFinalizationAgeMs).toBeNull();
    expect(none.signedIntentUnresolvedAgeMs).toBeNull();

    const unsigned = await readyz({
      journal: asJournalSetup(
        PendingBlockFinalizationsDB.preparePendingSubmission(
          journalFixture(header("readiness-unsigned")),
        ),
      ),
    });
    expect(unsigned.pendingFinalizationAgeMs).toEqual(expect.any(Number));
    expect(unsigned.signedIntentUnresolvedAgeMs).toBeNull();
    expect(unsigned.details).toEqual([]);
    expect(unsigned.ready).toBe(true);

    const signedHeader = header("readiness-signed");
    const signedPastBound = await readyz({
      journal: asJournalSetup(
        Effect.gen(function* () {
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(signedHeader),
          );
          yield* signIntent(signedHeader);
          yield* backdateJournals(16 * 60_000);
        }),
      ),
    });
    expect(signedPastBound.pendingFinalizationAgeMs).toBeGreaterThanOrEqual(
      16 * 60_000,
    );
    expect(signedPastBound.signedIntentUnresolvedAgeMs).toBeGreaterThanOrEqual(
      16 * 60_000,
    );
    expect(signedPastBound.details).toHaveLength(1);
    expect(signedPastBound.details[0]).toMatch(
      /^pending_finalization_age:\d+:900000$/u,
    );
    // Admission does not wait on the journal: still ready.
    expect(signedPastBound.reasons).toEqual([]);
    expect(signedPastBound.status).toBe(200);
  });
});
