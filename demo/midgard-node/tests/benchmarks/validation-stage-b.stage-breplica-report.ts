import { createReadStream } from "node:fs";
import { readFile } from "node:fs/promises";
import { Session } from "node:inspector";
import { resolve } from "node:path";
import { createInterface } from "node:readline";

import { type QueuedTx } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted, type Scope } from "effect";

import type { OpenLoopCorpusRow } from "../../src/open-loop-corpus-format.js";
import type { ValidationCacheStats } from "../../src/workers/utils/validation-pool.js";

export type WorkerCacheSnapshot = {
  readonly publicKeyCache: ValidationCacheStats;
  readonly addressCache: ValidationCacheStats;
};

export type CorpusManifest = {
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

export type AdmissionInsert = {
  readonly tx_id: Buffer;
  readonly tx_canonical_cbor: Buffer;
  readonly tx_full_hash_v1: Buffer;
  readonly arrival_seq: bigint;
  readonly status: "queued";
  readonly submit_source: "native";
};

export type StageBReplicaReport = {
  readonly database: string;
  readonly writeBehindMaxBatch: number;
  readonly depositIngestionIntervalMs: number;
  readonly depositIngestionActiveDurationMs: number;
  readonly depositIngestions: number;
  readonly ledgerCacheDeltaApplies: number;
  readonly ledgerCacheFullReloads: number;
  readonly averageBatchMs: number;
  readonly depositWindowAverageBatchMs: readonly number[];
  readonly worstDepositWindowThroughputRatio: number | null;
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

export const operatorEnabled = process.env.BENCH_PHASE2_OPERATOR === "1";

export const assertGate = process.env.BENCH_ASSERT_PHASE2 === "1";

export const preflightOnly = process.env.BENCH_PHASE2_PREFLIGHT_ONLY === "1";

export const shortAssert = process.env.BENCH_PHASE2_SHORT_ASSERT === "1";

// Diagnostic only: isolates the PostgreSQL cost of the reconstructable
// tx-delta projection without changing the production WriteBehind service.
// The asserted closure gate must never run with this enabled.
export const disableTxDeltaWriteBehindDiagnostic =
  process.env.BENCH_PHASE2_DISABLE_TX_DELTA_WRITE_BEHIND === "1";

export const minimumAcceptedTps = Number(
  process.env.BENCH_PHASE2_MIN_ACCEPTED_TPS ?? 10_000,
);

export const expectedFullCorpusSha256 =
  process.env.PHASE2_EXPECTED_FULL_CORPUS_SHA256 ?? "";

export const expectedFullCorpusRows = Number(
  process.env.PHASE2_EXPECTED_FULL_CORPUS_ROWS ?? Number.NaN,
);

export const fullGateReplicaDurationMs = 300_000;

export const fullGateCorpusCapacityTps = 12_600;

export const fullGateMinimumCorpusRows =
  (fullGateReplicaDurationMs / 1_000) * fullGateCorpusCapacityTps;

export const reuseDatabases = process.env.BENCH_PHASE2_REUSE_DATABASES === "1";

export const databasePrefix =
  process.env.BENCH_PHASE2_DATABASE_PREFIX ?? "midgard_phase2_bench";

export const templateDatabase = `${databasePrefix}_template`;

export const replicaCount = Number(process.env.BENCH_PHASE2_REPLICA_COUNT ?? 2);

export type CoordinatorCpuProfileHandle = {
  readonly stop: () => Promise<unknown>;
};

export const startCoordinatorCpuProfile =
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

export const expandCpuList = (cpuList: string): readonly number[] =>
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

export const physicalCoreIdsFor = async (
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

export const readAffinityTopology = async (): Promise<{
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

export type PostgresContainerAffinity = {
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

export type NodeBenchmarkContainerAffinity = {
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

export type PostgresSocketEvidence = {
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

export const sameCpuIds = (
  left: readonly number[],
  right: readonly number[],
): boolean =>
  left.length === right.length &&
  left.every((cpuId, index) => cpuId === right[index]);

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

export const runInDatabase = <A, E>(
  database: string,
  effect: Effect.Effect<A, E, SqlClient.SqlClient | Scope.Scope>,
): Promise<A> =>
  Effect.runPromise(
    Effect.scoped(
      effect.pipe(Effect.provide(PgClient.layer(databaseOptions(database)))),
    ),
  );

export const readCorpusRows = async (
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

export const queuedFromCorpusRow = (
  row: OpenLoopCorpusRow,
  arrivalSeq: bigint,
): QueuedTx => ({
  txId: Buffer.from(row.txHash, "hex"),
  txCbor: Buffer.from(row.canonicalCborHex, "hex"),
  arrivalSeq,
  createdAt: new Date(0),
});
