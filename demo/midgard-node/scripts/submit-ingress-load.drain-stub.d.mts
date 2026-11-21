import type { SqlClient } from "@effect/sql";
import type { SqlError } from "@effect/sql/SqlError";
import type { Effect, ManagedRuntime } from "effect";

export declare const DRAIN_STUB_REJECT_CODE: "E_LOAD_DRAIN_STUB";

export type HarnessPg = ManagedRuntime.ManagedRuntime<
  SqlClient.SqlClient,
  unknown
>;

export declare const makeHarnessPg: (
  url: string,
  applicationName: string,
) => HarnessPg;

export declare const drainQueuedAdmissions: (
  limit: number,
) => Effect.Effect<number, SqlError, SqlClient.SqlClient>;

export type DrainStubStats = {
  readonly marked: number;
  readonly ticks: number;
  readonly errors: number;
  readonly lastError: string | null;
};

export declare const startDrainStub: (options: {
  readonly runtime: HarnessPg;
  readonly ratePerSecond: number;
  readonly tickMs?: number;
}) => {
  readonly stats: DrainStubStats;
  readonly stop: () => Promise<DrainStubStats>;
};

export type PostgresSample = {
  readonly atMs: number;
  readonly xactCommit: number;
  readonly tupInserted: number;
  readonly tupUpdated: number;
  readonly tupDeleted: number;
  readonly admissions: Record<string, number>;
  readonly drainedByStub: number;
};

export declare const samplePostgres: Effect.Effect<
  PostgresSample,
  SqlError,
  SqlClient.SqlClient
>;

export declare const postgresRates: (
  before: PostgresSample,
  after: PostgresSample,
) => {
  readonly seconds: number;
  readonly admissionRowsPerSecond: number;
  readonly commitsPerSecond: number;
  readonly tuplesInsertedPerSecond: number;
  readonly tuplesUpdatedPerSecond: number;
  readonly tuplesDeletedPerSecond: number;
  readonly drainedByStub: number;
  readonly admissionsAtEnd: Record<string, number>;
};
