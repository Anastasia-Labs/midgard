import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
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
import type {
  NativeMpfFullIndexHealth,
  NativeMpfOwnerDiagnostics,
  NativeMpfOwnerService,
} from "../src/services/mpf-native-owner/protocol.js";
import {
  NATIVE_MPF_FULL_INDEX_NEAR_CAP,
  NATIVE_MPF_FULL_INDEX_WARNING_FRACTION,
} from "../src/services/mpf-native-owner/service.full-index-accounting.js";
import {
  FULL_INDEX_MAX_BYTES,
  FULL_INDEX_MAX_RECORDS,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

// Only the settings /readyz reads. Fresh exact provider evidence keeps the
// handler off Lucid and the contracts, so those are never dereferenced.
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

type ReadyzNativeOwner = {
  readonly status: number;
  readonly native: readonly string[];
  readonly nearCap: readonly string[];
  readonly nativeMpfOwner: {
    readonly healthy: boolean;
    readonly error?: string;
    readonly durableRoot?: string;
  } | null;
};

// The handler never reaches the services this leaves unprovided, hence the
// widening cast on the composed effect.
const readyz = (
  diagnostics: () => Promise<NativeMpfOwnerDiagnostics>,
  fullIndexHealth?: () => NativeMpfFullIndexHealth,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
          nextL1ProviderHealthEvidence({
            current,
            healthy: true,
            observedAtMs: Date.now(),
            successKind: "exact",
          }),
        );
        yield* Ref.set(globals.NATIVE_MPF_OWNER, {
          diagnostics,
          ...(fullIndexHealth === undefined ? {} : { fullIndexHealth }),
        } as unknown as NativeMpfOwnerService);
        const response = (yield* buildListenRouter().pipe(
          Effect.provideService(
            HttpServerRequest.HttpServerRequest,
            HttpServerRequest.fromWeb(
              new Request("http://midgard.test/readyz"),
            ),
          ),
        )) as HttpServerResponse.HttpServerResponse;
        const web = HttpServerResponse.toWeb(response);
        const body = (yield* Effect.promise(() => web.json())) as {
          readonly reasons: readonly string[];
          readonly details: readonly string[];
          readonly nativeMpfOwner: ReadyzNativeOwner["nativeMpfOwner"];
        };
        return {
          status: web.status,
          native: body.reasons.filter((reason) =>
            reason.startsWith("native_mpf"),
          ),
          nearCap: body.details.filter((detail) =>
            detail.startsWith(NATIVE_MPF_FULL_INDEX_NEAR_CAP),
          ),
          nativeMpfOwner: body.nativeMpfOwner,
        };
      }).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ValidationPool, {
          poolSize: 1,
          stats: Effect.succeed({
            oldestInFlightAgeMs: 0,
            liveWorkers: 1,
            restartingWorkers: 0,
          }),
        } as unknown as ValidationPool["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        // /readyz scopes DA removal outcomes by the verified deployment.
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as unknown as Effect.Effect<ReadyzNativeOwner, unknown, never>,
    ),
  );

describe("GET /readyz native MPF owner", () => {
  it("reports an owner whose diagnostics reject as unhealthy with its error, not a bare 500", async () => {
    const result = await readyz(() =>
      Promise.reject(
        new Error(
          "Native MPF owner restart limit exhausted: 3 restart(s) within 3600000 ms",
        ),
      ),
    );
    expect(result.status).toBe(503);
    expect(result.native).toEqual(["native_mpf_owner_unhealthy"]);
    expect(result.nativeMpfOwner).toEqual({
      healthy: false,
      error:
        "Error: Native MPF owner restart limit exhausted: 3 restart(s) within 3600000 ms",
    });
  });

  const durableRoot = "ab".repeat(32);
  const responsive = () =>
    Promise.resolve({
      ownerEpoch: Buffer.alloc(16, 1),
      durableRoot,
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    });

  it("reports a responsive owner's diagnostics without a native reason", async () => {
    const result = await readyz(responsive);
    expect(result.native).toEqual([]);
    expect(result.nearCap).toEqual([]);
    expect(result.nativeMpfOwner).toMatchObject({
      healthy: true,
      durableRoot,
    });
  });

  it("warns, without a reason, for each full-index cap the live root is past the warning fraction of", async () => {
    const past = (limit: number) =>
      Math.floor(limit * NATIVE_MPF_FULL_INDEX_WARNING_FRACTION) + 1;
    const below = (limit: number) =>
      Math.floor(limit * NATIVE_MPF_FULL_INDEX_WARNING_FRACTION);
    const quiet = await readyz(responsive, () => ({
      bytes: below(FULL_INDEX_MAX_BYTES),
      records: below(FULL_INDEX_MAX_RECORDS),
      promotionRefusal: undefined,
    }));
    expect(quiet.nearCap).toEqual([]);
    const bytes = past(FULL_INDEX_MAX_BYTES);
    const records = past(FULL_INDEX_MAX_RECORDS);
    const warned = await readyz(responsive, () => ({
      bytes,
      records,
      promotionRefusal: undefined,
    }));
    expect(warned.native).toEqual([]);
    expect(warned.nearCap).toEqual([
      `${NATIVE_MPF_FULL_INDEX_NEAR_CAP}:FULL_INDEX_MAX_RECORDS:${records.toString()}:${FULL_INDEX_MAX_RECORDS.toString()}`,
      `${NATIVE_MPF_FULL_INDEX_NEAR_CAP}:FULL_INDEX_MAX_BYTES:${bytes.toString()}:${FULL_INDEX_MAX_BYTES.toString()}`,
    ]);
  });
});
