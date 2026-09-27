import "./utils.js";

import { mkdtemp, rm } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { Level } from "level";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { readNativeRoot } from "../src/commands/state-reconciliation.js";
import { ROOT_KEY } from "../src/mpf/store-primitives.js";
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
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/index.js";
import type { NativeMpfOwnerDiagnostics } from "../src/services/mpf-native-owner/protocol.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * The reconciler's Architecture-G native-root source against the node's real
 * router served over HTTP: it must read the owner from `GET /readyz`, report an
 * owner the node calls unhealthy, and copy the LevelDB only when no node URL
 * was given and the node gave no owner answer.
 */

const DURABLE_ROOT = "5d".repeat(32);
/** The root a copy of LEDGER_MPF_DB_PATH would report instead. */
const COPY_ROOT = "c0".repeat(32);

const diagnostics: NativeMpfOwnerDiagnostics = {
  ownerEpoch: new Uint8Array(16).fill(7),
  durableRoot: DURABLE_ROOT,
  residentNodes: 3,
  residentEdges: 2,
  residentBytes: 4_096,
  activeGenerations: 0,
  generatedNodes: 0,
  generatedBytes: 0,
  rssBytes: 1_048_576,
  peakRssBytes: 2_097_152,
  childRestarts: 0,
};

// Only the settings /readyz reads. Fresh exact provider evidence keeps the
// handler off Lucid and the contracts, so those are never dereferenced.
const routerConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  WAIT_BETWEEN_MERGE_TXS: 10_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} satisfies Partial<NodeConfig["Type"]> as unknown as NodeConfig["Type"];

/** The owner the served node holds; `diagnostics` rejects when unhealthy. */
let ownerHealthy = true;

/** Runs one request through the node's real router. */
const route = (path: string): Promise<Response> =>
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
          diagnostics: () =>
            ownerHealthy
              ? Promise.resolve(diagnostics)
              : Promise.reject(new Error("native MPF owner child exited")),
        } as unknown as NativeMpfOwnerService);
        return HttpServerResponse.toWeb(
          (yield* buildListenRouter().pipe(
            Effect.provideService(
              HttpServerRequest.HttpServerRequest,
              HttpServerRequest.fromWeb(
                new Request(`http://midgard.test${path}`),
              ),
            ),
          )) as HttpServerResponse.HttpServerResponse,
        );
      }).pipe(
        Effect.provideService(NodeConfig, routerConfig),
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
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as unknown as Effect.Effect<Response, unknown, never>,
    ),
  );

const listen = (server: Server): Promise<number> =>
  new Promise((resolve) => {
    server.listen(0, "127.0.0.1", () =>
      resolve((server.address() as AddressInfo).port),
    );
  });

const close = (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    server.close((error) => (error === undefined ? resolve() : reject(error)));
  });

let node: Server;
let nodePort: number;
/** A port nothing listens on. */
let closedPort: number;
let levelPath: string;

beforeAll(async () => {
  node = createServer((request, response) => {
    void route(request.url ?? "/").then(
      async (answer) => {
        response.writeHead(answer.status, {
          "content-type": answer.headers.get("content-type") ?? "",
        });
        response.end(Buffer.from(await answer.arrayBuffer()));
      },
      (error: unknown) => {
        response.destroy(error as Error);
      },
    );
  });
  nodePort = await listen(node);
  const probe = createServer();
  closedPort = await listen(probe);
  await close(probe);
  levelPath = await mkdtemp(join(tmpdir(), "midgard-native-root-route-"));
  const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
  await db.open();
  await db.put(ROOT_KEY, COPY_ROOT);
  await db.close();
});

afterAll(async () => {
  await close(node);
  await rm(levelPath, { recursive: true, force: true });
});

const observe = (port: number, nodeUrl?: string) =>
  Effect.runPromise(
    readNativeRoot(nodeUrl === undefined ? {} : { nodeUrl }).pipe(
      Effect.provideService(NodeConfig, {
        PORT: port,
        LEDGER_MPF_DB_PATH: levelPath,
      } satisfies Partial<NodeConfig["Type"]> as unknown as NodeConfig["Type"]),
    ),
  );

describe("reconcile-state native root from the node's /readyz", () => {
  it("reads the owner's durable root from the real readiness route at --node-url", async () => {
    ownerHealthy = true;
    expect(await observe(closedPort, `http://127.0.0.1:${nodePort}`)).toEqual({
      kind: "observed",
      root: DURABLE_ROOT,
      source: "node-readiness",
    });
  });

  it("reports an owner the node calls unhealthy, and does not read the LevelDB copy instead", async () => {
    ownerHealthy = false;
    expect(await observe(nodePort)).toEqual({
      kind: "unhealthy",
      reason:
        "node reports its native MPF owner unhealthy (Error: native MPF owner child exited)",
    });
  });

  it("copies the LevelDB only when no --node-url was given and the node gave no answer", async () => {
    const unreachable = `http://127.0.0.1:${closedPort}`;
    const explicit = await observe(closedPort, unreachable);
    expect(explicit.kind).toBe("unavailable");
    expect(await observe(closedPort)).toEqual({
      kind: "observed",
      root: COPY_ROOT,
      source: "leveldb-copy",
    });
  });
});
