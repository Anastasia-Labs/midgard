import "../utils.js";

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { createReadStream, existsSync } from "node:fs";
import { mkdir, readdir, readFile, stat, writeFile } from "node:fs/promises";
import { Session } from "node:inspector";
import { availableParallelism, cpus, hostname } from "node:os";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";
import { createInterface } from "node:readline";
import { pathToFileURL } from "node:url";
import { promisify } from "node:util";

import { computeMidgardNativeTxFullHashFromCanonicalCborV1 } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import {
  deserializePhaseACandidate,
  type PhaseAResult,
  type QueuedTx,
  runPhaseAValidation,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { type Address, Data as LucidData } from "@lucid-evolution/lucid";
import {
  Duration,
  Effect,
  Fiber,
  Metric,
  Option,
  Redacted,
  type Scope,
} from "effect";
import { describe, expect, it } from "vitest";

import {
  DepositsDB,
  MempoolLedgerDB,
  MigrationRunner,
} from "../../src/database/index.js";
import { projectDepositsToMempoolLedger } from "../../src/fibers/project-deposits-to-mempool-ledger.js";
import {
  txQueueProcessorDrainOnce,
  validationBatchDurationSummary,
  validationBatchDurationTimer,
  validationClaimDurationTimer,
  validationClaimPayloadLoadDurationTimer,
  validationMempoolInsertDurationTimer,
  validationPhaseADurationTimer,
  validationPhaseBDurationTimer,
} from "../../src/fibers/tx-queue-processor.js";
import type { OpenLoopCorpusRow } from "../../src/open-loop-corpus-format.js";
import { nodeUtxoFromCorpusFunding } from "../../src/open-loop-corpus-format.js";
import { NodeConfig } from "../../src/services/config.js";
import { BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  makeMempoolLedgerCacheService,
  MempoolLedgerCache,
  validationLedgerCacheDeltaApplyCounter,
  validationLedgerCacheFullReloadCounter,
} from "../../src/services/mempool-ledger-cache.js";
import {
  FixedValidationWorkerPool,
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../../src/services/validation-pool.js";
import {
  makeWriteBehind,
  readWriteBehindTelemetry,
  summarizeWriteBehindTelemetry,
  WriteBehind,
} from "../../src/services/write-behind.js";
import type { ValidationCacheStats } from "../../src/workers/utils/validation-pool.js";
import { packPhaseAJob } from "../../src/workers/utils/validation-pool.js";

type WorkerCacheSnapshot = {
  readonly publicKeyCache: ValidationCacheStats;
  readonly addressCache: ValidationCacheStats;
};

type CorpusManifest = {
  readonly chainCount: number;
  readonly chainDepth: number;
  readonly networkId: string;
  readonly feeParams: {
    readonly minFeeA: string;
    readonly minFeeB: string;
  };
  readonly files: {
    readonly corpus: {
      readonly sha256: string;
      readonly rowCount: number;
    };
  };
};

type AdmissionInsert = {
  readonly tx_id: Buffer;
  readonly tx_canonical_cbor: Buffer;
  readonly tx_full_hash_v1: Buffer;
  readonly arrival_seq: bigint;
  readonly status: "queued";
  readonly submit_source: "native";
};

type StageBReplicaReport = {
  readonly database: string;
  readonly writeBehindMaxBatch: number;
  readonly depositProjectionDeltaIntervalMs: number;
  readonly depositProjectionActiveDurationMs: number;
  readonly depositProjectionDeltaBumps: number;
  readonly ledgerCacheDeltaApplies: number;
  readonly ledgerCacheFullReloads: number;
  readonly averageBatchMs: number;
  readonly bumpWindowAverageBatchMs: readonly number[];
  readonly worstBumpThroughputRatio: number | null;
  readonly accepted: number;
  readonly rejected: number;
  readonly batches: number;
  readonly durationMs: number;
  readonly acceptedTps: number;
  readonly p99BatchMs: number;
  readonly averagePhaseAMs: number;
  readonly averagePhaseBMs: number;
  readonly averagePersistMs: number;
  readonly averageClaimMs: number;
  readonly averageClaimPayloadLoadMs: number;
  readonly writeBehindFlushMs: number;
  readonly writeBehindFlushCount: number;
  readonly writeBehindFlushRows: number;
  readonly writeBehindTxDeltaPreparationCborMs: number;
  readonly writeBehindDeltaSqlMs: number;
  readonly writeBehindAddressSqlMs: number;
  readonly writeBehindTransactionMs: number;
  readonly writeBehindTransactionOverheadMs: number;
  readonly writeBehindInlineFallbackCount: number;
  readonly writeBehindFinalFlushMs: number;
  readonly writeBehindRowsBeforeFinalFlush: number;
  readonly serializationRatio: number;
  readonly acceptedAdmissionRows: number;
  readonly queuedAdmissionRows: number;
  readonly validatingAdmissionRows: number;
  readonly rejectedAdmissionRows: number;
  readonly admissionPayloadRows: number;
  readonly mempoolRows: number;
  readonly mempoolLedgerRows: number;
  readonly cachedLedgerRows: number;
  readonly missingExpectedTxIds: number;
  readonly unexpectedAcceptedTxIds: number;
};

const operatorEnabled = process.env.BENCH_PHASE2_OPERATOR === "1";
const assertGate = process.env.BENCH_ASSERT_PHASE2 === "1";
const preflightOnly = process.env.BENCH_PHASE2_PREFLIGHT_ONLY === "1";
const shortAssert = process.env.BENCH_PHASE2_SHORT_ASSERT === "1";
// Diagnostic only: isolates the PostgreSQL cost of the reconstructable
// tx-delta projection without changing the production WriteBehind service.
// The asserted closure gate must never run with this enabled.
const disableTxDeltaWriteBehindDiagnostic =
  process.env.BENCH_PHASE2_DISABLE_TX_DELTA_WRITE_BEHIND === "1";
const minimumAcceptedTps = Number(
  process.env.BENCH_PHASE2_MIN_ACCEPTED_TPS ?? 10_000,
);
const expectedFullCorpusSha256 =
  process.env.PHASE2_EXPECTED_FULL_CORPUS_SHA256 ?? "";
const expectedFullCorpusRows = Number(
  process.env.PHASE2_EXPECTED_FULL_CORPUS_ROWS ?? Number.NaN,
);
const fullGateReplicaDurationMs = 300_000;
const fullGateCorpusCapacityTps = 12_600;
const fullGateMinimumCorpusRows =
  (fullGateReplicaDurationMs / 1_000) * fullGateCorpusCapacityTps;
const reuseDatabases = process.env.BENCH_PHASE2_REUSE_DATABASES === "1";
const databasePrefix =
  process.env.BENCH_PHASE2_DATABASE_PREFIX ?? "midgard_phase2_bench";
const templateDatabase = `${databasePrefix}_template`;
const replicaCount = Number(process.env.BENCH_PHASE2_REPLICA_COUNT ?? 2);
if (!Number.isInteger(replicaCount) || replicaCount < 1 || replicaCount > 2) {
  throw new Error("BENCH_PHASE2_REPLICA_COUNT must be 1 or 2");
}
const replicaDatabases = [
  `${databasePrefix}_a`,
  ...(replicaCount === 2 ? [`${databasePrefix}_b`] : []),
];
const benchmarkDatabasePattern = /^midgard_phase2_bench[a-z0-9_]*$/u;
const corpusPath = resolve(
  process.env.BENCH_PHASE2_CORPUS_PATH ??
    "logs/phase-1-full-corpus-20260709T002743Z/corpus/corpus.ndjson",
);
const manifestPath = resolve(
  process.env.BENCH_PHASE2_CORPUS_MANIFEST_PATH ??
    `${corpusPath}.manifest.json`,
);
const walletsDirectory = resolve(
  process.env.BENCH_PHASE2_WALLETS_DIRECTORY ??
    "logs/phase-1-full-corpus-20260709T002743Z/wallets",
);
const workerEntry = pathToFileURL(resolve("dist/validation.js"));
const outputPath = resolve(
  process.env.BENCH_PHASE2_OUTPUT_PATH ??
    "tests/benchmarks/output/validation-stage-b.json",
);
const cpuProfileDirectory =
  process.env.BENCH_PHASE2_CPU_PROFILE_DIR === undefined
    ? undefined
    : resolve(process.env.BENCH_PHASE2_CPU_PROFILE_DIR);
const poolSize = Number(process.env.BENCH_PHASE2_POOL_SIZE ?? 6);
const chunkSize = Number(process.env.BENCH_PHASE2_CHUNK_SIZE ?? 64);
const batchSize = Number(process.env.BENCH_PHASE2_BATCH_SIZE ?? 2_048);
const drainLoops = Number(process.env.BENCH_PHASE2_DRAIN_LOOPS ?? 4);
const preloadBatchSize = Number(
  process.env.BENCH_PHASE2_PRELOAD_BATCH_SIZE ?? 1_000,
);
const warmupIterations = Number(
  process.env.BENCH_PHASE2_WARMUP_ITERATIONS ?? 2,
);

type CoordinatorCpuProfileHandle = {
  readonly stop: () => Promise<unknown>;
};

const startCoordinatorCpuProfile =
  async (): Promise<CoordinatorCpuProfileHandle> => {
    const session = new Session();
    session.connect();
    await new Promise<void>((resolvePost, rejectPost) => {
      session.post("Profiler.enable", (error) => {
        if (error === null) resolvePost();
        else rejectPost(error);
      });
    });
    await new Promise<void>((resolvePost, rejectPost) => {
      session.post("Profiler.start", (error) => {
        if (error === null) resolvePost();
        else rejectPost(error);
      });
    });
    return {
      stop: async () => {
        try {
          return await new Promise<unknown>((resolvePost, rejectPost) => {
            session.post("Profiler.stop", (error, result) => {
              if (error === null) resolvePost(result.profile);
              else rejectPost(error);
            });
          });
        } finally {
          session.disconnect();
        }
      },
    };
  };
const postgresContainerName = process.env.BENCH_PHASE2_POSTGRES_CONTAINER ?? "";
const nodeContainerName = process.env.BENCH_PHASE2_NODE_CONTAINER ?? "";
const privateNetworkName = process.env.BENCH_PHASE2_PRIVATE_NETWORK ?? "";
const nodeRepoSource = process.env.BENCH_PHASE2_NODE_REPO_SOURCE ?? "";
const nodeRepoDestination =
  process.env.BENCH_PHASE2_NODE_REPO_DESTINATION ?? "/workspace";
const nodeDockerCliSource =
  process.env.BENCH_PHASE2_NODE_DOCKER_CLI_SOURCE ??
  "/mnt/wsl/docker-desktop/cli-tools/usr/bin/docker";
const nodeDockerCliDestination = "/usr/local/bin/docker";
const nodeDockerSocketSource = "/var/run/docker.sock";
const postgresEphemeralDeclared =
  process.env.BENCH_PHASE2_POSTGRES_EPHEMERAL === "1";
const expectedNodeImage = process.env.BENCH_PHASE2_NODE_IMAGE ?? "node:22";
const expectedNodeImageId = process.env.BENCH_PHASE2_NODE_IMAGE_ID ?? "";
const expectedPostgresImage =
  process.env.BENCH_PHASE2_POSTGRES_IMAGE ?? "postgres:15.15-alpine";
const postgresSocketDestination =
  process.env.BENCH_PHASE2_POSTGRES_SOCKET_DESTINATION ?? "";
const execFileAsync = promisify(execFile);

if (assertGate && reuseDatabases) {
  throw new Error(
    "BENCH_ASSERT_PHASE2 requires BENCH_PHASE2_REUSE_DATABASES=0 so corpus bytes and durable transaction identities are recomputed",
  );
}
if (
  assertGate &&
  (expectedNodeImage !== "node:22.22.2" ||
    expectedPostgresImage !== "postgres:15.15-alpine")
) {
  throw new Error(
    "BENCH_ASSERT_PHASE2 requires node:22.22.2 and postgres:15.15-alpine",
  );
}

const expandCpuList = (cpuList: string): readonly number[] =>
  cpuList
    .trim()
    .split(",")
    .flatMap((part) => {
      const [startText, endText] = part.split("-");
      const start = Number(startText);
      const end = endText === undefined ? start : Number(endText);
      return Array.from(
        { length: end - start + 1 },
        (_, offset) => start + offset,
      );
    });

const physicalCoreIdsFor = async (
  logicalCpuIds: readonly number[],
): Promise<readonly string[]> => {
  const physicalCoreIds = await Promise.all(
    logicalCpuIds.map(async (cpuId) => {
      const topologyRoot = `/sys/devices/system/cpu/cpu${cpuId}/topology`;
      const [packageId, coreId] = await Promise.all([
        readFile(`${topologyRoot}/physical_package_id`, "utf8"),
        readFile(`${topologyRoot}/core_id`, "utf8"),
      ]);
      return `${packageId.trim()}:${coreId.trim()}`;
    }),
  );
  return [...new Set(physicalCoreIds)].sort();
};

const readAffinityTopology = async (): Promise<{
  readonly logicalCpuIds: readonly number[];
  readonly physicalCoreIds: readonly string[];
}> => {
  const status = await readFile("/proc/self/status", "utf8");
  const allowedList = /^Cpus_allowed_list:\s*(.+)$/mu.exec(status)?.[1];
  if (allowedList === undefined) {
    throw new Error("Unable to read Cpus_allowed_list from /proc/self/status");
  }
  const logicalCpuIds = expandCpuList(allowedList);
  return {
    logicalCpuIds,
    physicalCoreIds: await physicalCoreIdsFor(logicalCpuIds),
  };
};

type PostgresContainerAffinity = {
  readonly name: string;
  readonly id: string;
  readonly image: string;
  readonly cpuset: string;
  readonly logicalCpuIds: readonly number[];
  readonly physicalCoreIds: readonly string[];
  readonly running: boolean;
  readonly autoRemove: boolean;
  readonly networkMode: string;
  readonly publishedPostgresPorts: readonly number[];
  readonly mounts: readonly {
    readonly type: string;
    readonly source: string;
    readonly destination: string;
    readonly name: string;
    readonly readWrite: boolean;
  }[];
  readonly tmpfsDestinations: readonly string[];
  readonly networks: readonly string[];
};

const readPostgresContainerAffinity = async (): Promise<
  PostgresContainerAffinity | undefined
> => {
  if (postgresContainerName === "") return undefined;
  const { stdout } = await execFileAsync("docker", [
    "inspect",
    postgresContainerName,
  ]);
  const inspected = (
    JSON.parse(stdout) as readonly {
      readonly Id: string;
      readonly Config: { readonly Image: string };
      readonly HostConfig: {
        readonly CpusetCpus: string;
        readonly AutoRemove: boolean;
        readonly NetworkMode: string;
        readonly Tmpfs: Record<string, string> | null;
      };
      readonly State: { readonly Running: boolean };
      readonly NetworkSettings: {
        readonly Ports: Record<
          string,
          readonly { readonly HostPort: string }[] | null
        >;
        readonly Networks: Record<string, unknown>;
      };
      readonly Mounts: readonly {
        readonly Type: string;
        readonly Source: string;
        readonly Destination: string;
        readonly Name?: string;
        readonly RW: boolean;
      }[];
    }[]
  )[0];
  if (inspected === undefined) {
    throw new Error(
      `Docker inspect returned no record for ${postgresContainerName}`,
    );
  }
  const logicalCpuIds =
    inspected.HostConfig.CpusetCpus.trim() === ""
      ? []
      : expandCpuList(inspected.HostConfig.CpusetCpus);
  return {
    name: postgresContainerName,
    id: inspected.Id,
    image: inspected.Config.Image,
    cpuset: inspected.HostConfig.CpusetCpus,
    logicalCpuIds,
    physicalCoreIds: await physicalCoreIdsFor(logicalCpuIds),
    running: inspected.State.Running,
    autoRemove: inspected.HostConfig.AutoRemove,
    networkMode: inspected.HostConfig.NetworkMode,
    publishedPostgresPorts: (
      inspected.NetworkSettings.Ports["5432/tcp"] ?? []
    ).map((binding) => Number(binding.HostPort)),
    mounts: inspected.Mounts.map((mount) => ({
      type: mount.Type,
      source: mount.Source,
      destination: mount.Destination,
      name: mount.Name ?? "",
      readWrite: mount.RW,
    })),
    tmpfsDestinations: Object.keys(inspected.HostConfig.Tmpfs ?? {}).sort(),
    networks: Object.keys(inspected.NetworkSettings.Networks).sort(),
  };
};

type NodeBenchmarkContainerAffinity = {
  readonly name: string;
  readonly id: string;
  readonly image: string;
  readonly imageId: string;
  readonly configuredHostname: string;
  readonly cpuset: string;
  readonly logicalCpuIds: readonly number[];
  readonly physicalCoreIds: readonly string[];
  readonly running: boolean;
  readonly autoRemove: boolean;
  readonly publishedPorts: readonly number[];
  readonly mounts: PostgresContainerAffinity["mounts"];
  readonly networks: readonly string[];
};

const readNodeContainerAffinity = async (): Promise<
  NodeBenchmarkContainerAffinity | undefined
> => {
  if (nodeContainerName === "") return undefined;
  const { stdout } = await execFileAsync("docker", [
    "inspect",
    nodeContainerName,
  ]);
  const inspected = (
    JSON.parse(stdout) as readonly {
      readonly Id: string;
      readonly Image: string;
      readonly Config: { readonly Image: string; readonly Hostname: string };
      readonly HostConfig: {
        readonly CpusetCpus: string;
        readonly AutoRemove: boolean;
      };
      readonly State: { readonly Running: boolean };
      readonly NetworkSettings: {
        readonly Ports: Record<
          string,
          readonly { readonly HostPort: string }[] | null
        >;
        readonly Networks: Record<string, unknown>;
      };
      readonly Mounts: readonly {
        readonly Type: string;
        readonly Source: string;
        readonly Destination: string;
        readonly Name?: string;
        readonly RW: boolean;
      }[];
    }[]
  )[0];
  if (inspected === undefined) {
    throw new Error(
      `Docker inspect returned no record for ${nodeContainerName}`,
    );
  }
  const logicalCpuIds =
    inspected.HostConfig.CpusetCpus.trim() === ""
      ? []
      : expandCpuList(inspected.HostConfig.CpusetCpus);
  return {
    name: nodeContainerName,
    id: inspected.Id,
    image: inspected.Config.Image,
    imageId: inspected.Image,
    configuredHostname: inspected.Config.Hostname,
    cpuset: inspected.HostConfig.CpusetCpus,
    logicalCpuIds,
    physicalCoreIds: await physicalCoreIdsFor(logicalCpuIds),
    running: inspected.State.Running,
    autoRemove: inspected.HostConfig.AutoRemove,
    publishedPorts: Object.values(inspected.NetworkSettings.Ports)
      .flatMap((bindings) => bindings ?? [])
      .map((binding) => Number(binding.HostPort)),
    mounts: inspected.Mounts.map((mount) => ({
      type: mount.Type,
      source: mount.Source,
      destination: mount.Destination,
      name: mount.Name ?? "",
      readWrite: mount.RW,
    })),
    networks: Object.keys(inspected.NetworkSettings.Networks).sort(),
  };
};

type PostgresSocketEvidence = {
  readonly hostDirectory: string;
  readonly containerDirectory: string;
  readonly directoryUid: number;
  readonly directoryGid: number;
  readonly directoryMode: number;
  readonly socketPath: string;
  readonly socketUid: number;
  readonly socketGid: number;
  readonly socketMode: number;
  readonly socketIsSocket: boolean;
};

const readPostgresSocketEvidence = async (): Promise<
  PostgresSocketEvidence | undefined
> => {
  const host = process.env.POSTGRES_HOST ?? "127.0.0.1";
  if (!host.startsWith("/") || postgresSocketDestination === "") {
    return undefined;
  }
  const hostDirectory = resolve(host);
  const socketPath = resolve(hostDirectory, ".s.PGSQL.5432");
  const [directory, socket] = await Promise.all([
    stat(hostDirectory),
    stat(socketPath),
  ]);
  if (!directory.isDirectory()) {
    throw new Error(`PostgreSQL socket host path is not a directory: ${host}`);
  }
  return {
    hostDirectory,
    containerDirectory: postgresSocketDestination,
    directoryUid: directory.uid,
    directoryGid: directory.gid,
    directoryMode: directory.mode,
    socketPath,
    socketUid: socket.uid,
    socketGid: socket.gid,
    socketMode: socket.mode,
    socketIsSocket: socket.isSocket(),
  };
};

const sameCpuIds = (
  left: readonly number[],
  right: readonly number[],
): boolean =>
  left.length === right.length &&
  left.every((cpuId, index) => cpuId === right[index]);

const requireBenchmarkDatabaseName = (database: string): void => {
  if (!benchmarkDatabasePattern.test(database)) {
    throw new Error(
      `Refusing destructive Stage B benchmark operation for database ${JSON.stringify(database)}; names must match ${benchmarkDatabasePattern.source}`,
    );
  }
};

const databaseOptions = (database: string) => {
  const host = process.env.POSTGRES_HOST ?? "127.0.0.1";
  const port = Number(process.env.POSTGRES_PORT ?? 5433);
  return {
    ...(host.startsWith("/")
      ? { path: resolve(host, `.s.PGSQL.${port.toString()}`) }
      : { host, port }),
    username: process.env.POSTGRES_USER ?? "postgres",
    password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
    database,
    maxConnections: 20,
    applicationName: `midgard-phase2-stage-b-${database}`,
  };
};

const runInDatabase = <A, E>(
  database: string,
  effect: Effect.Effect<A, E, SqlClient.SqlClient | Scope.Scope>,
): Promise<A> =>
  Effect.runPromise(
    Effect.scoped(
      effect.pipe(Effect.provide(PgClient.layer(databaseOptions(database)))),
    ),
  );

const readCorpusRows = async (
  path: string,
  limit: number,
): Promise<readonly OpenLoopCorpusRow[]> => {
  const input = createReadStream(path, { encoding: "utf8" });
  const lines = createInterface({ input, crlfDelay: Number.POSITIVE_INFINITY });
  const rows: OpenLoopCorpusRow[] = [];
  for await (const line of lines) {
    if (line.trim().length === 0) continue;
    rows.push(JSON.parse(line) as OpenLoopCorpusRow);
    if (rows.length >= limit) break;
  }
  lines.close();
  input.destroy();
  return rows;
};

const queuedFromCorpusRow = (
  row: OpenLoopCorpusRow,
  arrivalSeq: bigint,
): QueuedTx => ({
  txId: Buffer.from(row.txHash, "hex"),
  txCbor: Buffer.from(row.canonicalCborHex, "hex"),
  arrivalSeq,
  createdAt: new Date(0),
});

const runPoolPhaseA = async (
  pool: FixedValidationWorkerPool,
  queued: readonly QueuedTx[],
): Promise<{
  readonly result: PhaseAResult;
  readonly serializeMs: number;
  readonly deserializeMs: number;
  readonly cacheStats: ReadonlyArray<
    WorkerCacheSnapshot & { readonly workerThreadId: number }
  >;
}> => {
  let serializeMs = 0;
  const requests = [];
  for (let offset = 0; offset < queued.length; offset += chunkSize) {
    const startedAt = performance.now();
    requests.push(
      packPhaseAJob(
        pool.allocateJobId(),
        queued.slice(offset, offset + chunkSize),
      ),
    );
    serializeMs += performance.now() - startedAt;
  }
  const responses = await Promise.all(
    requests.map((request) => pool.submit(request)),
  );
  const deserializeStartedAt = performance.now();
  const accepted = [];
  const rejected = [];
  const cacheStats: Array<
    WorkerCacheSnapshot & { readonly workerThreadId: number }
  > = [];
  for (const response of responses) {
    if (response.kind !== "phase_a") {
      throw new Error(`Unexpected validation worker response ${response.kind}`);
    }
    cacheStats.push({
      workerThreadId: response.workerThreadId,
      publicKeyCache: response.publicKeyCache,
      addressCache: response.addressCache,
    });
    for (const item of response.results) {
      if (item.ok) accepted.push(deserializePhaseACandidate(item.candidate));
      else
        rejected.push({
          txId: Buffer.from(item.txId),
          code: item.code,
          detail: item.detail,
        });
    }
  }
  return {
    result: { accepted, rejected },
    serializeMs,
    deserializeMs: performance.now() - deserializeStartedAt,
    cacheStats,
  };
};

const resetBenchmarkDatabases = async (): Promise<void> => {
  for (const database of [templateDatabase, ...replicaDatabases]) {
    requireBenchmarkDatabaseName(database);
  }
  await runInDatabase(
    process.env.POSTGRES_ADMIN_DB ?? "postgres",
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      for (const database of [...replicaDatabases, templateDatabase]) {
        yield* sql.unsafe(`DROP DATABASE IF EXISTS ${database} WITH (FORCE)`);
      }
      yield* sql.unsafe(`CREATE DATABASE ${templateDatabase}`);
    }),
  );
};

const loadFundingRows = async (): Promise<
  readonly MempoolLedgerDB.EntryNoTimeStamp[]
> => {
  const files = (await readdir(walletsDirectory))
    .filter((name) => /^wallet-\d{4}\.json$/u.test(name))
    .sort();
  const rows: MempoolLedgerDB.EntryNoTimeStamp[] = [];
  for (const file of files) {
    const wallet = JSON.parse(
      await readFile(resolve(walletsDirectory, file), "utf8"),
    ) as {
      readonly latestFunding?: {
        readonly fundingUtxos?: readonly {
          readonly outref: string;
          readonly outputCbor: string;
        }[];
      };
    };
    const funding = wallet.latestFunding?.fundingUtxos?.[0];
    if (funding === undefined) {
      throw new Error(`${file} has no verified funding UTxO`);
    }
    const [txHash, outputIndexText] = funding.outref.split("#");
    if (txHash === undefined || outputIndexText === undefined) {
      throw new Error(`${file} has an invalid funding outref`);
    }
    const utxo = nodeUtxoFromCorpusFunding({
      txHash,
      outputIndex: Number(outputIndexText),
      outputCborHex: funding.outputCbor,
    });
    rows.push({
      [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(txHash, "hex"),
      [MempoolLedgerDB.Columns.OUTREF]: utxo.outrefCbor,
      [MempoolLedgerDB.Columns.OUTPUT]: utxo.outputCbor,
      [MempoolLedgerDB.Columns.ADDRESS]: utxo.address as Address,
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    });
  }
  return rows;
};

const preloadAdmissions = async (
  sql: SqlClient.SqlClient,
  manifest: CorpusManifest,
): Promise<void> => {
  await Effect.runPromise(sql`CREATE TABLE phase2_expected_tx_ids (
    tx_id BYTEA PRIMARY KEY
  )`);
  const input = createReadStream(corpusPath, { encoding: "utf8" });
  const lines = createInterface({ input, crlfDelay: Number.POSITIVE_INFINITY });
  const hash = createHash("sha256");
  const chainSteps = new Map<string, number>();
  let batch: AdmissionInsert[] = [];
  let rowCount = 0;

  const flush = async (): Promise<void> => {
    if (batch.length === 0) return;
    const inserting = batch;
    batch = [];
    await Effect.runPromise(
      sql.withTransaction(
        Effect.gen(function* () {
          yield* sql`INSERT INTO tx_admissions ${sql.insert(
            inserting.map(
              ({
                tx_canonical_cbor: _cbor,
                tx_full_hash_v1: _hash,
                ...admission
              }) => admission,
            ),
          )}`;
          yield* sql`INSERT INTO tx_admission_payloads ${sql.insert(
            inserting.map(({ tx_id, tx_canonical_cbor, tx_full_hash_v1 }) => ({
              tx_id,
              tx_canonical_cbor,
              tx_full_hash_v1,
            })),
          )}`;
          yield* sql`INSERT INTO phase2_expected_tx_ids ${sql.insert(
            inserting.map(({ tx_id }) => ({ tx_id })),
          )}`;
        }),
      ),
    );
  };

  for await (const line of lines) {
    if (line.trim().length === 0) continue;
    hash.update(line);
    hash.update("\n");
    const row = JSON.parse(line) as OpenLoopCorpusRow;
    const walletIndex = Number(
      row.senderWalletId.slice("stress-wallet-".length),
    );
    const step = chainSteps.get(row.senderWalletId) ?? 0;
    chainSteps.set(row.senderWalletId, step + 1);
    const arrivalSeq =
      BigInt(step) * BigInt(manifest.chainCount) + BigInt(walletIndex);
    const txCbor = Buffer.from(row.canonicalCborHex, "hex");
    if (txCbor.length !== row.canonicalCborByteLength) {
      throw new Error(`Corpus byte-length mismatch for ${row.txHash}`);
    }
    batch.push({
      tx_id: Buffer.from(row.txHash, "hex"),
      tx_canonical_cbor: txCbor,
      tx_full_hash_v1:
        computeMidgardNativeTxFullHashFromCanonicalCborV1(txCbor),
      arrival_seq: arrivalSeq,
      status: "queued",
      submit_source: "native",
    });
    rowCount += 1;
    if (batch.length >= preloadBatchSize) await flush();
    if (rowCount % 100_000 === 0) {
      console.log(
        `phase2_stage_b_preload rows=${rowCount.toString()} expected=${manifest.files.corpus.rowCount.toString()}`,
      );
    }
  }
  await flush();
  if (rowCount !== manifest.files.corpus.rowCount) {
    throw new Error(
      `Corpus row count mismatch: expected ${manifest.files.corpus.rowCount.toString()}, got ${rowCount.toString()}`,
    );
  }
  const actualHash = hash.digest("hex");
  if (actualHash !== manifest.files.corpus.sha256) {
    throw new Error(
      `Corpus sha256 mismatch: expected ${manifest.files.corpus.sha256}, got ${actualHash}`,
    );
  }
  await Effect.runPromise(
    sql`SELECT setval(
      pg_get_serial_sequence('tx_admissions', 'arrival_seq'),
      (SELECT MAX(arrival_seq) FROM tx_admissions),
      true
    )`,
  );
};

const prepareBenchmarkDatabases = async (
  manifest: CorpusManifest,
): Promise<void> => {
  await resetBenchmarkDatabases();
  const fundingRows = await loadFundingRows();
  expect(fundingRows).toHaveLength(manifest.chainCount);
  await runInDatabase(
    templateDatabase,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* MigrationRunner.migrate({
        appVersion: "phase2-stage-b-benchmark",
        actor: "phase2-stage-b-benchmark",
      });
      yield* MempoolLedgerDB.insert(fundingRows);
      yield* Effect.tryPromise(() => preloadAdmissions(sql, manifest));
      yield* sql`ANALYZE tx_admissions`;
      yield* sql`ANALYZE mempool_ledger`;
    }),
  );
  await runInDatabase(
    process.env.POSTGRES_ADMIN_DB ?? "postgres",
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      for (const database of replicaDatabases) {
        yield* sql.unsafe(
          `CREATE DATABASE ${database} WITH TEMPLATE ${templateDatabase}`,
        );
      }
      // The replicas are independent physical copies after CREATE DATABASE.
      // Drop the staging template before the timed run so the sustained gate
      // does not retain a third full-corpus copy on the benchmark volume.
      yield* sql.unsafe(`DROP DATABASE ${templateDatabase} WITH (FORCE)`);
    }),
  );
};

const drainReplica = async (
  database: string,
  pool: FixedValidationWorkerPool,
  workerCacheStats: Map<number, WorkerCacheSnapshot>,
): Promise<StageBReplicaReport> => {
  requireBenchmarkDatabaseName(database);
  const phaseAStats = { durationMs: 0, serializationMs: 0 };
  const validationPool: ValidationPoolService = {
    poolSize,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    ready: Effect.void,
    stats: Effect.sync(() => pool.stats()),
    runPhaseAChunk: (txs) =>
      Effect.tryPromise({
        try: async () => {
          const startedAt = performance.now();
          const result = await runPoolPhaseA(pool, txs);
          phaseAStats.durationMs += performance.now() - startedAt;
          phaseAStats.serializationMs +=
            result.serializeMs + result.deserializeMs;
          for (const snapshot of result.cacheStats) {
            workerCacheStats.set(snapshot.workerThreadId, snapshot);
          }
          return result.result;
        },
        catch: (cause) =>
          new ValidationWorkerError({
            message: "Stage B benchmark validation worker failed",
            cause,
          }),
      }),
    evaluateScript: () =>
      Effect.fail(
        new ValidationWorkerError({
          message:
            "The Phase 1 chain corpus is script-free; use the UPLC worker benchmark for script evaluation",
        }),
      ),
  };
  return runInDatabase(
    database,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const baseConfig = yield* NodeConfig;
      const nodeConfig = {
        ...baseConfig,
        VALIDATION_BATCH_SIZE: batchSize,
        VALIDATION_BATCH_HARD_CAP: batchSize,
        VALIDATION_MIN_BATCH: Math.min(128, batchSize),
        VALIDATION_DRAIN_LOOPS: drainLoops,
        VALIDATION_WORKER_POOL_SIZE: poolSize,
        VALIDATION_WORKER_INLINE_THRESHOLD: 0,
        VALIDATION_WORKER_CHUNK_SIZE: chunkSize,
        VALIDATION_UPLC_IN_WORKERS: false,
      };
      const globals = yield* Globals;
      const cache = yield* makeMempoolLedgerCacheService(
        globals,
        MempoolLedgerDB.retrieveSpendable.pipe(
          Effect.provideService(SqlClient.SqlClient, sql),
        ),
      );
      const writeBehind = yield* makeWriteBehind.pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(BatchSql, sql),
      );
      const benchmarkWriteBehind = disableTxDeltaWriteBehindDiagnostic
        ? {
            ...writeBehind,
            enqueueTxDeltas: () => Effect.void,
          }
        : writeBehind;
      const writeBehindFiber = yield* Effect.forkScoped(writeBehind.run);
      yield* cache.withPhaseBLock(cache.currentState);
      const cacheDeltaBefore = yield* Metric.value(
        validationLedgerCacheDeltaApplyCounter,
      );
      const cacheFullReloadBefore = yield* Metric.value(
        validationLedgerCacheFullReloadCounter,
      );
      const metricBefore = yield* Metric.value(validationBatchDurationTimer);
      const claimBefore = yield* Metric.value(validationClaimDurationTimer);
      const claimPayloadLoadBefore = yield* Metric.value(
        validationClaimPayloadLoadDurationTimer,
      );
      const phaseABefore = yield* Metric.value(validationPhaseADurationTimer);
      const phaseBBefore = yield* Metric.value(validationPhaseBDurationTimer);
      const persistBefore = yield* Metric.value(
        validationMempoolInsertDurationTimer,
      );
      const writeBehindBefore = yield* readWriteBehindTelemetry;
      const depositProjectionDeltaIntervalMs = 5_000;
      let depositProjectionDeltaBumps = 0;
      const bumpWindowAverageBatchMs: number[] = [];
      const depositProjectionDeltaFiber = yield* Effect.forkScoped(
        Effect.forever(
          Effect.sleep(Duration.millis(depositProjectionDeltaIntervalMs)).pipe(
            Effect.zipRight(
              Effect.gen(function* () {
                const before = yield* Metric.value(
                  validationBatchDurationTimer,
                );
                const bump = depositProjectionDeltaBumps + 1;
                const hash = (label: string) =>
                  createHash("sha256")
                    .update(`${database}:${bump.toString()}:${label}`)
                    .digest();
                const eventId = Buffer.from(
                  LucidData.to(
                    {
                      transactionId: hash("event").toString("hex"),
                      outputIndex: 0n,
                    },
                    SDK.OutputReference,
                  ),
                  "hex",
                );
                yield* DepositsDB.insertEntries([
                  {
                    [DepositsDB.Columns.ID]: eventId,
                    [DepositsDB.Columns.INFO]: hash("info"),
                    [DepositsDB.Columns.INCLUSION_TIME]: new Date(0),
                    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: hash("l1"),
                    [DepositsDB.Columns.LEDGER_TX_ID]: hash("ledger"),
                    [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from([0x80]),
                    [DepositsDB.Columns.LEDGER_ADDRESS]:
                      "addr_test1_phase2_benchmark_deposit",
                    [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
                    [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
                  },
                ]);
                yield* projectDepositsToMempoolLedger.pipe(
                  Effect.provideService(NodeConfig, nodeConfig),
                );
                depositProjectionDeltaBumps += 1;
                yield* Effect.forkScoped(
                  Effect.sleep("1 second").pipe(
                    Effect.zipRight(
                      Effect.gen(function* () {
                        const after = yield* Metric.value(
                          validationBatchDurationTimer,
                        );
                        const count = after.count - before.count;
                        if (count > 0) {
                          bumpWindowAverageBatchMs.push(
                            (after.sum - before.sum) / count,
                          );
                        }
                      }),
                    ),
                  ),
                );
              }),
            ),
          ),
        ),
      );
      const startedAt = performance.now();
      yield* Effect.all(
        Array.from({ length: drainLoops }, () => txQueueProcessorDrainOnce()),
        { concurrency: "unbounded", discard: true },
      ).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ValidationPool, validationPool),
        Effect.provideService(MempoolLedgerCache, cache),
        Effect.provideService(WriteBehind, benchmarkWriteBehind),
        Effect.provideService(Lucid, {
          api: { currentSlot: () => 0 },
        } as unknown as Lucid),
      );
      const depositProjectionActiveDurationMs = performance.now() - startedAt;
      yield* Fiber.interrupt(depositProjectionDeltaFiber);
      yield* cache.withPhaseBLock(cache.currentState);
      const writeBehindRowsBeforeFinalFlush = (yield* writeBehind.depths)
        .totalDepth;
      const finalFlushStartedAt = performance.now();
      yield* writeBehind.flushNow;
      const writeBehindFinalFlushMs = performance.now() - finalFlushStartedAt;
      yield* Fiber.interrupt(writeBehindFiber);
      const durationMs = performance.now() - startedAt;
      const metricAfter = yield* Metric.value(validationBatchDurationTimer);
      const cacheDeltaAfter = yield* Metric.value(
        validationLedgerCacheDeltaApplyCounter,
      );
      const cacheFullReloadAfter = yield* Metric.value(
        validationLedgerCacheFullReloadCounter,
      );
      const claimAfter = yield* Metric.value(validationClaimDurationTimer);
      const claimPayloadLoadAfter = yield* Metric.value(
        validationClaimPayloadLoadDurationTimer,
      );
      const phaseAAfter = yield* Metric.value(validationPhaseADurationTimer);
      const phaseBAfter = yield* Metric.value(validationPhaseBDurationTimer);
      const persistAfter = yield* Metric.value(
        validationMempoolInsertDurationTimer,
      );
      // Sample only after the explicit tail flush so every successful final
      // transaction and row is included in the reported metric deltas.
      const writeBehindAfter = yield* readWriteBehindTelemetry;
      const writeBehindTelemetry = summarizeWriteBehindTelemetry(
        writeBehindBefore,
        writeBehindAfter,
      );
      const batchSummary = yield* Metric.value(validationBatchDurationSummary);
      const batches = metricAfter.count - metricBefore.count;
      const averageBatchMs =
        (metricAfter.sum - metricBefore.sum) / Math.max(1, batches);
      const worstBumpThroughputRatio =
        bumpWindowAverageBatchMs.length === 0
          ? null
          : Math.min(
              ...bumpWindowAverageBatchMs.map(
                (bumpAverage) => averageBatchMs / bumpAverage,
              ),
            );
      let p99BatchMs = metricAfter.max;
      const p99 = batchSummary.quantiles.find(
        ([quantile]) => quantile === 0.99,
      )?.[1];
      if (p99 !== undefined && Option.isSome(p99)) {
        p99BatchMs = p99.value;
      }
      const cachedLedgerRows = yield* cache.withPhaseBLock(
        cache.currentState.pipe(Effect.map((state) => state.size)),
      );
      const counts = yield* sql<{
        readonly status: string;
        readonly count: bigint | string;
      }>`SELECT status, COUNT(*) AS count FROM tx_admissions GROUP BY status`;
      const countByStatus = new Map(
        counts.map((row) => [row.status, Number(row.count)]),
      );
      const mempoolCount = yield* sql<{
        readonly count: bigint | string;
      }>`SELECT COUNT(*) AS count FROM mempool`;
      const payloadCount = yield* sql<{
        readonly count: bigint | string;
      }>`SELECT COUNT(*) AS count FROM tx_admission_payloads`;
      const ledgerCount = yield* sql<{
        readonly count: bigint | string;
      }>`SELECT COUNT(*) AS count FROM mempool_ledger`;
      const identityCounts = yield* sql<{
        readonly missingExpectedTxIds: bigint | string;
        readonly unexpectedAcceptedTxIds: bigint | string;
      }>`SELECT
        (SELECT COUNT(*) FROM (
          SELECT tx_id FROM phase2_expected_tx_ids
          EXCEPT
          SELECT tx_id FROM tx_admissions WHERE status = 'accepted'
        ) missing) AS "missingExpectedTxIds",
        (SELECT COUNT(*) FROM (
          SELECT tx_id FROM tx_admissions WHERE status = 'accepted'
          EXCEPT
          SELECT tx_id FROM phase2_expected_tx_ids
        ) unexpected) AS "unexpectedAcceptedTxIds"`;
      const accepted = countByStatus.get("accepted") ?? 0;
      const rejected = countByStatus.get("rejected") ?? 0;
      return {
        database,
        writeBehindMaxBatch: nodeConfig.WRITE_BEHIND_MAX_BATCH,
        depositProjectionDeltaIntervalMs,
        depositProjectionActiveDurationMs,
        depositProjectionDeltaBumps,
        ledgerCacheDeltaApplies: Number(
          cacheDeltaAfter.count - cacheDeltaBefore.count,
        ),
        ledgerCacheFullReloads: Number(
          cacheFullReloadAfter.count - cacheFullReloadBefore.count,
        ),
        averageBatchMs,
        bumpWindowAverageBatchMs,
        worstBumpThroughputRatio,
        accepted,
        rejected,
        batches,
        durationMs,
        acceptedTps: accepted / (durationMs / 1_000),
        p99BatchMs,
        averagePhaseAMs:
          (phaseAAfter.sum - phaseABefore.sum) /
          Math.max(1, phaseAAfter.count - phaseABefore.count),
        averagePhaseBMs:
          (phaseBAfter.sum - phaseBBefore.sum) /
          Math.max(1, phaseBAfter.count - phaseBBefore.count),
        averagePersistMs:
          (persistAfter.sum - persistBefore.sum) /
          Math.max(1, persistAfter.count - persistBefore.count),
        averageClaimMs:
          (claimAfter.sum - claimBefore.sum) /
          Math.max(1, claimAfter.count - claimBefore.count),
        averageClaimPayloadLoadMs:
          (claimPayloadLoadAfter.sum - claimPayloadLoadBefore.sum) /
          Math.max(
            1,
            claimPayloadLoadAfter.count - claimPayloadLoadBefore.count,
          ),
        ...writeBehindTelemetry,
        writeBehindFinalFlushMs,
        writeBehindRowsBeforeFinalFlush,
        serializationRatio:
          phaseAStats.serializationMs / Math.max(1, phaseAStats.durationMs),
        acceptedAdmissionRows: countByStatus.get("accepted") ?? 0,
        queuedAdmissionRows: countByStatus.get("queued") ?? 0,
        validatingAdmissionRows: countByStatus.get("validating") ?? 0,
        rejectedAdmissionRows: countByStatus.get("rejected") ?? 0,
        admissionPayloadRows: Number(payloadCount[0]?.count ?? 0),
        mempoolRows: Number(mempoolCount[0]?.count ?? 0),
        mempoolLedgerRows: Number(ledgerCount[0]?.count ?? 0),
        cachedLedgerRows,
        missingExpectedTxIds: Number(
          identityCounts[0]?.missingExpectedTxIds ?? 0,
        ),
        unexpectedAcceptedTxIds: Number(
          identityCounts[0]?.unexpectedAcceptedTxIds ?? 0,
        ),
      } satisfies StageBReplicaReport;
    }).pipe(
      Effect.tagMetrics("phase2_replica", database),
      Effect.provide(Globals.Default),
      Effect.provide(NodeConfig.layer),
    ),
  );
};

describe("Phase 2 sustained real-Postgres Stage B operator benchmark", () => {
  it.skipIf(!operatorEnabled)(
    "drains two preloaded corpus replicas through claim, workers, Phase B, and accepted persistence",
    async () => {
      for (const path of [corpusPath, manifestPath, walletsDirectory]) {
        expect(existsSync(path), `missing benchmark input ${path}`).toBe(true);
      }
      expect(existsSync(workerEntry)).toBe(true);
      const manifest = JSON.parse(
        await readFile(manifestPath, "utf8"),
      ) as CorpusManifest;
      if (assertGate && !shortAssert) {
        if (
          !/^[0-9a-f]{64}$/u.test(expectedFullCorpusSha256) ||
          !Number.isSafeInteger(expectedFullCorpusRows) ||
          expectedFullCorpusRows < fullGateMinimumCorpusRows
        ) {
          throw new Error(
            `The full Phase 2 gate requires PHASE2_EXPECTED_FULL_CORPUS_SHA256 and PHASE2_EXPECTED_FULL_CORPUS_ROWS for an exact corpus with at least ${fullGateMinimumCorpusRows.toLocaleString("en-US")} rows per continuous replica (${fullGateCorpusCapacityTps.toLocaleString("en-US")} tx/s capacity for ${String(fullGateReplicaDurationMs / 1_000)} seconds)`,
          );
        }
        if (
          manifest.files.corpus.sha256 !== expectedFullCorpusSha256 ||
          manifest.files.corpus.rowCount !== expectedFullCorpusRows
        ) {
          throw new Error(
            `Full Phase 2 corpus identity mismatch: declared sha256=${expectedFullCorpusSha256},rows=${expectedFullCorpusRows.toString()} manifest sha256=${manifest.files.corpus.sha256},rows=${manifest.files.corpus.rowCount.toString()}`,
          );
        }
      }
      const affinityTopology = await readAffinityTopology();
      const postgresAffinity = await readPostgresContainerAffinity();
      const nodeContainerAffinity = await readNodeContainerAffinity();
      const postgresSocketEvidence = await readPostgresSocketEvidence();
      const nodePinnedEightCore =
        availableParallelism() === 8 &&
        affinityTopology.logicalCpuIds.length === 8 &&
        affinityTopology.physicalCoreIds.length === 8 &&
        poolSize === 6;
      const benchmarkPostgresPort = Number(process.env.POSTGRES_PORT ?? 5433);
      const benchmarkPostgresHost = process.env.POSTGRES_HOST ?? "127.0.0.1";
      const nodeRepoMount = nodeContainerAffinity?.mounts.find(
        (mount) =>
          mount.type === "bind" &&
          mount.source === nodeRepoSource &&
          mount.destination === nodeRepoDestination &&
          mount.readWrite,
      );
      const nodeDockerCliMount = nodeContainerAffinity?.mounts.find(
        (mount) =>
          mount.type === "bind" &&
          mount.source === nodeDockerCliSource &&
          mount.destination === nodeDockerCliDestination &&
          !mount.readWrite,
      );
      const nodeDockerSocketMount = nodeContainerAffinity?.mounts.find(
        (mount) =>
          mount.type === "bind" &&
          mount.source === nodeDockerSocketSource &&
          mount.destination === nodeDockerSocketSource &&
          !mount.readWrite,
      );
      const nodeMountsProved =
        nodeContainerAffinity !== undefined &&
        nodeContainerAffinity.mounts.length === 3 &&
        nodeRepoMount !== undefined &&
        nodeDockerCliMount !== undefined &&
        nodeDockerSocketMount !== undefined;
      const samePrivateNetwork =
        privateNetworkName !== "" &&
        privateNetworkName !== "bridge" &&
        nodeContainerAffinity !== undefined &&
        postgresAffinity !== undefined &&
        nodeContainerAffinity.networks.length === 1 &&
        postgresAffinity.networks.length === 1 &&
        nodeContainerAffinity.networks[0] === privateNetworkName &&
        postgresAffinity.networks[0] === privateNetworkName;
      const nodeContainerMatchesProcess =
        nodeContainerAffinity !== undefined &&
        (nodeContainerAffinity.id.startsWith(hostname()) ||
          nodeContainerAffinity.configuredHostname === hostname());
      const nodeContainerProved =
        nodeContainerAffinity !== undefined &&
        nodeContainerAffinity.running &&
        nodeContainerAffinity.autoRemove &&
        nodeContainerAffinity.image === expectedNodeImage &&
        /^sha256:[0-9a-f]{64}$/u.test(expectedNodeImageId) &&
        nodeContainerAffinity.imageId === expectedNodeImageId &&
        process.version === "v22.22.2" &&
        nodeContainerAffinity.publishedPorts.length === 0 &&
        nodeContainerAffinity.logicalCpuIds.length === 8 &&
        nodeContainerAffinity.physicalCoreIds.length === 8 &&
        sameCpuIds(
          affinityTopology.logicalCpuIds,
          nodeContainerAffinity.logicalCpuIds,
        ) &&
        nodeContainerMatchesProcess &&
        nodeMountsProved &&
        samePrivateNetwork;
      const socketBind = postgresAffinity?.mounts.find(
        (mount) =>
          mount.type === "bind" &&
          mount.source === postgresSocketEvidence?.hostDirectory &&
          mount.destination === postgresSocketEvidence.containerDirectory &&
          mount.readWrite,
      );
      const connectedByDeclaredUnixSocket =
        postgresSocketEvidence !== undefined &&
        postgresSocketEvidence.socketIsSocket &&
        postgresSocketEvidence.containerDirectory === "/var/run/postgresql" &&
        socketBind !== undefined;
      const connectedByPrivateNetwork =
        benchmarkPostgresHost === "pg" &&
        samePrivateNetwork &&
        postgresAffinity !== undefined &&
        postgresAffinity.publishedPostgresPorts.length === 0;
      const socketOwnershipProved =
        postgresSocketEvidence === undefined ||
        (postgresSocketEvidence.directoryUid === 70 &&
          postgresSocketEvidence.socketUid === 70 &&
          postgresSocketEvidence.socketGid === 70);
      const connectedToDeclaredPostgresContainer =
        postgresAffinity !== undefined &&
        (connectedByPrivateNetwork ||
          connectedByDeclaredUnixSocket ||
          (["127.0.0.1", "localhost"].includes(benchmarkPostgresHost) &&
            (postgresAffinity.publishedPostgresPorts.includes(
              benchmarkPostgresPort,
            ) ||
              (postgresAffinity.networkMode === "host" &&
                benchmarkPostgresPort === 5432))));
      const postgresDataIsEphemeral =
        postgresAffinity !== undefined &&
        (postgresAffinity.tmpfsDestinations.includes(
          "/var/lib/postgresql/data",
        ) ||
          postgresAffinity.mounts.some(
            (mount) =>
              mount.destination === "/var/lib/postgresql/data" &&
              mount.type === "volume" &&
              /^[0-9a-f]{64}$/u.test(mount.name),
          ));
      const postgresImagePinned =
        postgresAffinity?.image === expectedPostgresImage;
      const hasUnexpectedBindMount =
        postgresAffinity?.mounts.some(
          (mount) => mount.type === "bind" && mount !== socketBind,
        ) ?? false;
      const wholeSystemPinnedEightCore =
        nodePinnedEightCore &&
        nodeContainerProved &&
        postgresEphemeralDeclared &&
        postgresAffinity !== undefined &&
        postgresAffinity.running &&
        postgresAffinity.autoRemove &&
        postgresImagePinned &&
        postgresDataIsEphemeral &&
        !hasUnexpectedBindMount &&
        socketOwnershipProved &&
        connectedToDeclaredPostgresContainer &&
        postgresAffinity.logicalCpuIds.length === 8 &&
        postgresAffinity.physicalCoreIds.length === 8 &&
        sameCpuIds(
          affinityTopology.logicalCpuIds,
          postgresAffinity.logicalCpuIds,
        ) &&
        sameCpuIds(
          nodeContainerAffinity!.logicalCpuIds,
          postgresAffinity.logicalCpuIds,
        );
      if (assertGate && !wholeSystemPinnedEightCore) {
        throw new Error(
          `BENCH_ASSERT_PHASE2 requires an inspected ${expectedNodeImage} v22.22.2 AutoRemove benchmark container whose immutable image ID exactly matches BENCH_PHASE2_NODE_IMAGE_ID and a ${expectedPostgresImage} AutoRemove container on the same declared private network and eight-core cpuset, exact read/write repo plus read-only Docker inspect mounts, ephemeral PostgreSQL data, no published PostgreSQL port, and exactly six validation workers`,
        );
      }
      if (preflightOnly) {
        if (!assertGate) {
          throw new Error(
            "BENCH_PHASE2_PREFLIGHT_ONLY requires BENCH_ASSERT_PHASE2=1",
          );
        }
        const postgresFingerprint = await runInDatabase(
          process.env.POSTGRES_ADMIN_DB ?? "postgres",
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const rows = yield* sql<{
              readonly version: string;
              readonly serverAddress: string | null;
              readonly serverPort: number | null;
              readonly dataDirectory: string;
            }>`SELECT
              version() AS version,
              inet_server_addr()::text AS "serverAddress",
              inet_server_port() AS "serverPort",
              current_setting('data_directory') AS "dataDirectory"`;
            return rows[0]!;
          }),
        );
        expect(postgresFingerprint.version).toContain("PostgreSQL 15.15");
        console.log(
          JSON.stringify({
            preflightOnly: true,
            wholeSystemPinnedEightCore,
            affinityTopology,
            nodeContainerAffinity,
            nodeContainerMatchesProcess,
            nodeMountsProved,
            samePrivateNetwork,
            nodeContainerProved,
            postgresAffinity,
            postgresSocketEvidence,
            connectedByDeclaredUnixSocket,
            connectedByPrivateNetwork,
            postgresDataIsEphemeral,
            postgresImagePinned,
            hasUnexpectedBindMount,
            connectedToDeclaredPostgresContainer,
            postgresFingerprint,
          }),
        );
        return;
      }
      if (!reuseDatabases) await prepareBenchmarkDatabases(manifest);

      const phaseAConfig = {
        expectedNetworkId: BigInt(manifest.networkId),
        minFeeA: BigInt(manifest.feeParams.minFeeA),
        minFeeB: BigInt(manifest.feeParams.minFeeB),
        concurrency: 1,
        strictnessProfile: "phase2_stage_b_operator",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      } as const;
      const probeRows = await readCorpusRows(corpusPath, batchSize);
      const probeQueued = probeRows.map((row, index) =>
        queuedFromCorpusRow(row, BigInt(index + 1)),
      );
      for (let iteration = 0; iteration < warmupIterations; iteration += 1) {
        await Effect.runPromise(runPhaseAValidation(probeQueued, phaseAConfig));
      }
      const inlineStartedAt = performance.now();
      const inline = await Effect.runPromise(
        runPhaseAValidation(probeQueued, phaseAConfig),
      );
      const inlinePhaseAMs = performance.now() - inlineStartedAt;
      expect(inline.rejected).toHaveLength(0);
      const rssBeforeWorkersBytes = process.memoryUsage().rss;

      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        { config: phaseAConfig, signatureVerifier: "node" },
      );
      const reports: StageBReplicaReport[] = [];
      const workerCacheStats = new Map<number, WorkerCacheSnapshot>();
      let poolProbePhaseAMs = 0;
      let rssAfterWorkerWarmupBytes = rssBeforeWorkersBytes;
      let rssAfterDrainsBytes = rssBeforeWorkersBytes;
      let coordinatorCpuProfile: CoordinatorCpuProfileHandle | undefined;
      const cpuProfileArtifacts: string[] = [];
      try {
        await pool.start();
        for (let iteration = 0; iteration < warmupIterations; iteration += 1) {
          await runPoolPhaseA(pool, probeQueued);
        }
        const poolProbeStartedAt = performance.now();
        const poolProbe = await runPoolPhaseA(pool, probeQueued);
        poolProbePhaseAMs = performance.now() - poolProbeStartedAt;
        rssAfterWorkerWarmupBytes = process.memoryUsage().rss;
        expect(poolProbe.result.rejected).toHaveLength(0);
        for (const snapshot of poolProbe.cacheStats) {
          workerCacheStats.set(snapshot.workerThreadId, snapshot);
        }
        expect(
          poolProbe.result.accepted.map((tx) =>
            tx.ledgerTx.txId.toString("hex"),
          ),
        ).toStrictEqual(
          inline.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
        );
        if (cpuProfileDirectory !== undefined) {
          await mkdir(cpuProfileDirectory, { recursive: true });
          await pool.startCpuProfiles();
          coordinatorCpuProfile = await startCoordinatorCpuProfile();
        }
        for (const database of replicaDatabases) {
          reports.push(await drainReplica(database, pool, workerCacheStats));
        }
        if (
          cpuProfileDirectory !== undefined &&
          coordinatorCpuProfile !== undefined
        ) {
          const activeCoordinatorProfile = coordinatorCpuProfile;
          coordinatorCpuProfile = undefined;
          const [coordinatorProfile, workerProfiles] = await Promise.all([
            activeCoordinatorProfile.stop(),
            pool.stopCpuProfiles(),
          ]);
          const coordinatorPath = resolve(
            cpuProfileDirectory,
            "coordinator.cpuprofile",
          );
          await writeFile(
            coordinatorPath,
            `${JSON.stringify(coordinatorProfile)}\n`,
          );
          cpuProfileArtifacts.push(coordinatorPath);
          for (const workerProfile of workerProfiles) {
            const workerPath = resolve(
              cpuProfileDirectory,
              `worker-${workerProfile.workerIndex.toString()}-thread-${workerProfile.threadId.toString()}.cpuprofile`,
            );
            await writeFile(workerPath, `${workerProfile.profileJson}\n`);
            cpuProfileArtifacts.push(workerPath);
          }
        }
        rssAfterDrainsBytes = process.memoryUsage().rss;
      } finally {
        if (coordinatorCpuProfile !== undefined) {
          await coordinatorCpuProfile.stop();
        }
        await pool.close();
      }

      const accepted = reports.reduce(
        (total, report) => total + report.accepted,
        0,
      );
      const durationMs = reports.reduce(
        (total, report) => total + report.durationMs,
        0,
      );
      const allBatchP99UpperBound = Math.max(
        ...reports.map((report) => report.p99BatchMs),
      );
      const serializationRatio =
        reports.reduce(
          (total, report) => total + report.serializationRatio,
          0,
        ) / reports.length;
      const expectedAccepted =
        manifest.files.corpus.rowCount * replicaDatabases.length;
      const expectedLedgerRows =
        manifest.chainCount * (manifest.chainDepth + 1);
      const sumReplicaField = (
        field:
          | "writeBehindFlushMs"
          | "writeBehindFlushCount"
          | "writeBehindFlushRows"
          | "writeBehindTxDeltaPreparationCborMs"
          | "writeBehindDeltaSqlMs"
          | "writeBehindAddressSqlMs"
          | "writeBehindTransactionMs"
          | "writeBehindTransactionOverheadMs"
          | "writeBehindInlineFallbackCount"
          | "writeBehindFinalFlushMs"
          | "writeBehindRowsBeforeFinalFlush",
      ): number =>
        reports.reduce((total, replica) => total + replica[field], 0);
      const writeBehindTelemetry = {
        writeBehindFlushMs: sumReplicaField("writeBehindFlushMs"),
        writeBehindFlushCount: sumReplicaField("writeBehindFlushCount"),
        writeBehindFlushRows: sumReplicaField("writeBehindFlushRows"),
        writeBehindTxDeltaPreparationCborMs: sumReplicaField(
          "writeBehindTxDeltaPreparationCborMs",
        ),
        writeBehindDeltaSqlMs: sumReplicaField("writeBehindDeltaSqlMs"),
        writeBehindAddressSqlMs: sumReplicaField("writeBehindAddressSqlMs"),
        writeBehindTransactionMs: sumReplicaField("writeBehindTransactionMs"),
        writeBehindTransactionOverheadMs: sumReplicaField(
          "writeBehindTransactionOverheadMs",
        ),
        writeBehindInlineFallbackCount: sumReplicaField(
          "writeBehindInlineFallbackCount",
        ),
        writeBehindFinalFlushMs: sumReplicaField("writeBehindFinalFlushMs"),
        writeBehindRowsBeforeFinalFlush: sumReplicaField(
          "writeBehindRowsBeforeFinalFlush",
        ),
      };
      const report = {
        generatedAtIso: new Date().toISOString(),
        host: hostname(),
        cpuModel: cpus()[0]?.model ?? "unknown",
        availableParallelism: availableParallelism(),
        affinityLogicalCpuIds: affinityTopology.logicalCpuIds,
        affinityPhysicalCoreIds: affinityTopology.physicalCoreIds,
        affinityPhysicalCoreCount: affinityTopology.physicalCoreIds.length,
        pinnedEightCore: nodePinnedEightCore,
        nodePinnedEightCore,
        nodeVersion: process.version,
        expectedNodeImage,
        expectedNodeImageId,
        nodeImage: nodeContainerAffinity?.image ?? expectedNodeImage,
        nodeImageId: nodeContainerAffinity?.imageId,
        containerIdentity:
          nodeContainerAffinity === undefined
            ? undefined
            : {
                proved: nodeContainerProved,
                image: nodeContainerAffinity.image,
                imageId: nodeContainerAffinity.imageId,
                id: nodeContainerAffinity.id,
              },
        expectedPostgresImage,
        reuseDatabases,
        replicaCount,
        nodeContainerAffinity,
        nodeContainerMatchesProcess,
        nodeMountsProved,
        samePrivateNetwork,
        nodeContainerProved,
        postgresEphemeralDeclared,
        postgresSocketEvidence,
        connectedByDeclaredUnixSocket,
        connectedByPrivateNetwork,
        socketOwnershipProved,
        postgresDataIsEphemeral,
        postgresImagePinned,
        hasUnexpectedBindMount,
        connectedToDeclaredPostgresContainer,
        postgresAffinity,
        wholeSystemPinnedEightCore,
        corpusPath,
        corpusSha256: manifest.files.corpus.sha256,
        corpusRowCount: manifest.files.corpus.rowCount,
        expectedAccepted,
        expectedLedgerRows,
        poolSize,
        signatureVerifier: "node",
        drainLoops,
        batchSize,
        chunkSize,
        writeBehindMaxBatch:
          reports[0]?.writeBehindMaxBatch ??
          Number(process.env.WRITE_BEHIND_MAX_BATCH ?? 1_000),
        ...writeBehindTelemetry,
        disableTxDeltaWriteBehindDiagnostic,
        warmupIterations,
        shortAssert,
        minimumAcceptedTps,
        accepted,
        lostTransactions: expectedAccepted - accepted,
        durationMs,
        acceptedTps: accepted / (durationMs / 1_000),
        p99BatchMs: allBatchP99UpperBound,
        inlinePhaseAMs,
        poolProbePhaseAMs,
        phaseASpeedup: inlinePhaseAMs / poolProbePhaseAMs,
        serializationRatio,
        workerCacheStats: [...workerCacheStats.entries()]
          .sort(([left], [right]) => left - right)
          .map(([workerThreadId, snapshot]) => ({
            workerThreadId,
            ...snapshot,
          })),
        rssBeforeWorkersBytes,
        rssAfterWorkerWarmupBytes,
        rssAfterDrainsBytes,
        workerPoolRssDeltaBytes: Math.max(
          0,
          rssAfterWorkerWarmupBytes - rssBeforeWorkersBytes,
        ),
        rssPerWorkerUpperBoundBytes:
          Math.max(0, rssAfterWorkerWarmupBytes - rssBeforeWorkersBytes) /
          poolSize,
        cpuProfileArtifacts,
        replicas: reports,
        gateAsserted:
          assertGate &&
          wholeSystemPinnedEightCore &&
          !reuseDatabases &&
          replicaCount === 2 &&
          warmupIterations === 2 &&
          !disableTxDeltaWriteBehindDiagnostic,
      };
      await writeFile(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      console.log(JSON.stringify(report));

      for (const replica of reports) {
        expect(replica.accepted).toBe(manifest.files.corpus.rowCount);
        expect(replica.rejected).toBe(0);
        expect(replica.acceptedAdmissionRows).toBe(
          manifest.files.corpus.rowCount,
        );
        expect(replica.queuedAdmissionRows).toBe(0);
        expect(replica.validatingAdmissionRows).toBe(0);
        expect(replica.rejectedAdmissionRows).toBe(0);
        expect(replica.admissionPayloadRows).toBe(
          manifest.files.corpus.rowCount,
        );
        expect(replica.mempoolRows).toBe(manifest.files.corpus.rowCount);
        expect(replica.mempoolLedgerRows).toBe(
          expectedLedgerRows + replica.depositProjectionDeltaBumps,
        );
        expect(replica.cachedLedgerRows).toBe(expectedLedgerRows);
        expect(replica.missingExpectedTxIds).toBe(0);
        expect(replica.unexpectedAcceptedTxIds).toBe(0);
      }
      expect(accepted).toBe(expectedAccepted);

      if (assertGate) {
        if (!shortAssert) {
          expect(durationMs).toBeGreaterThanOrEqual(600_000);
          for (const replica of reports) {
            expect(replica.durationMs).toBeGreaterThanOrEqual(300_000);
            expect(replica.acceptedTps).toBeGreaterThanOrEqual(
              minimumAcceptedTps,
            );
          }
        }
        expect(report.acceptedTps).toBeGreaterThanOrEqual(minimumAcceptedTps);
        expect(report.p99BatchMs).toBeLessThanOrEqual(1_000);
        expect(report.phaseASpeedup).toBeGreaterThanOrEqual(4);
        expect(report.serializationRatio).toBeLessThanOrEqual(0.1);
      }
    },
    1_800_000,
  );
});
