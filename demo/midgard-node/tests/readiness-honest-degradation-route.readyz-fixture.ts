/**
 * The /readyz and /healthz request the readiness route tests make: the real
 * listen router over the test database, with the settings, provider
 * evidence, L1 follower state and L1 access each test names.
 */
import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import type { L1FollowerState } from "../src/services/l1-follower.readiness.js";
import { ledgerSubmitSlotSnapshot } from "../src/services/l1-provider.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { seedCaughtUpL1Follower } from "./readiness-l1-follower.fixture.js";
import { withFailingStatements } from "./sql-fault-injection.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

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

export type ProviderObservation = {
  readonly healthy: boolean;
  readonly agoMs: number;
};

export type Readyz = {
  readonly status: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly details: readonly string[];
  readonly dbError?: string;
  readonly settlement?: unknown;
  readonly pendingFinalizationAgeMs?: number | null;
  readonly signedIntentUnresolvedAgeMs?: number | null;
  readonly providerQueryHealthy?: boolean;
  readonly l1Follower?: { readonly state: string; readonly node?: unknown };
};

const SUCCESS_NOW: readonly ProviderObservation[] = [
  { healthy: true, agoMs: 0 },
];

const SLOT_CONFIG: SlotConfig = { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 };

/** The node's L1 access as the Lucid service carries it. */
export type L1AccessStub = {
  readonly transport: TransportReadiness;
  /** How far the ledger tip trails wall time, read by the probe. */
  readonly ledgerLagMs?: number;
  readonly onRead?: () => void;
};

const lucidWith = (
  access: L1AccessStub | undefined,
  nodeBehindMaxMs: number,
): Lucid =>
  (access === undefined
    ? {}
    : {
        l1Endpoint: "/run/cardano/node.socket",
        l1TransportReadiness: () => access.transport,
        readSubmitSlotSnapshotOnce: () =>
          Effect.try({
            try: () => {
              access.onRead?.();
              const nowMs = Date.now();
              return ledgerSubmitSlotSnapshot({
                slotConfig: SLOT_CONFIG,
                ledgerTipSlot: Math.floor(
                  (nowMs - (access.ledgerLagMs ?? 0)) / 1_000,
                ),
                nowMs,
                boundMs: nodeBehindMaxMs,
              });
            },
            catch: (cause) =>
              cause instanceof Error ? cause : new Error(String(cause)),
          }),
      }) as unknown as Lucid;

export const readyz = ({
  provider = SUCCESS_NOW,
  databaseDown = false,
  journal = Effect.void,
  nodeBehindMaxMs,
  l1Access,
  forceProviderProbe = false,
  l1Follower,
  path = "readyz",
}: {
  readonly provider?: readonly ProviderObservation[];
  readonly databaseDown?: boolean;
  readonly journal?: Effect.Effect<void, unknown, never>;
  /** `L1_NODE_BEHIND_MAX_MS`; unset reads as the flat readiness bound. */
  readonly nodeBehindMaxMs?: number;
  /** The Lucid service's L1 access; none reads as an access not yet open. */
  readonly l1Access?: L1AccessStub;
  readonly forceProviderProbe?: boolean;
  /** The L1 follower state; a caught-up follower by default. */
  readonly l1Follower?: L1FollowerState;
  readonly path?: "readyz" | "healthz";
} = {}): Promise<Readyz> =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Reset before and after: the handler reads journal ages, jobs and
        // leases from the real tables, so a row another file left would turn
        // a ready answer unready, and one left here would leak into the next.
        const clear = resetApplicationTables;
        yield* clear;
        return yield* Effect.gen(function* () {
          yield* journal;
          const globals = yield* Globals;
          yield* seedCaughtUpL1Follower(globals, l1Follower);
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
                new Request(`http://midgard.test/${path}`),
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
          return { ...body, status: web.status };
        }).pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(NodeConfig, {
          ...nodeConfig,
          ...(nodeBehindMaxMs === undefined
            ? {}
            : { L1_NODE_BEHIND_MAX_MS: nodeBehindMaxMs }),
          ...(forceProviderProbe
            ? {
                READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 1,
                NETWORK: "Custom",
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
        Effect.provideService(
          Lucid,
          lucidWith(l1Access, nodeBehindMaxMs ?? 200_000),
        ),
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
