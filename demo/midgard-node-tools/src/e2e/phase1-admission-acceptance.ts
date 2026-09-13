import { resolve } from "node:path";

const DATABASE_NAME_PATTERN = /^midgard_phase1_acceptance_[a-z0-9_]+$/u;
const RUN_TOKEN_PATTERN = /^[a-z0-9_]+$/u;
const LOOPBACK_HOSTS = new Set(["127.0.0.1", "localhost", "::1"]);
const LIVE_POSTGRES_PORT = 5433;
const LIVE_HTTP_PORT = 3000;

export type Phase1AdmissionIsolation = {
  readonly runToken: string;
  readonly database: string;
  readonly postgresHost: string;
  readonly postgresPort: number;
  readonly httpHost: "127.0.0.1";
  readonly httpPort: number;
  readonly corpusPath: string;
};

const parsePort = (value: string | undefined, label: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 1 || parsed > 65_535) {
    throw new Error(`${label} must be an integer between 1 and 65535`);
  }
  return parsed;
};

/**
 * Fail-closed boundary for the destructive/timed Phase 1 operator harness.
 * The live demo uses database `midgard`, PostgreSQL port 5433, and HTTP port
 * 3000, none of which are accepted here even when the opt-in is present.
 */
export const requirePhase1AdmissionIsolation = (
  env: NodeJS.ProcessEnv,
): Phase1AdmissionIsolation => {
  if (env.PHASE1_ADMISSION_OPERATOR !== "1") {
    throw new Error("PHASE1_ADMISSION_OPERATOR=1 is required");
  }

  const runToken = env.PHASE1_ADMISSION_RUN_TOKEN ?? "";
  if (!RUN_TOKEN_PATTERN.test(runToken)) {
    throw new Error(
      "PHASE1_ADMISSION_RUN_TOKEN must contain only lowercase letters, digits, and underscores",
    );
  }

  const database = env.POSTGRES_DB ?? "";
  if (!DATABASE_NAME_PATTERN.test(database)) {
    throw new Error(
      `POSTGRES_DB must match ${DATABASE_NAME_PATTERN.source}; got ${JSON.stringify(database)}`,
    );
  }
  if (database !== `midgard_phase1_acceptance_${runToken}`) {
    throw new Error(
      "POSTGRES_DB must equal midgard_phase1_acceptance_<PHASE1_ADMISSION_RUN_TOKEN>",
    );
  }
  const postgresHost = env.POSTGRES_HOST ?? "";
  if (!LOOPBACK_HOSTS.has(postgresHost)) {
    throw new Error(
      `POSTGRES_HOST must be loopback for Phase 1 acceptance; got ${JSON.stringify(postgresHost)}`,
    );
  }
  const postgresPort = parsePort(env.POSTGRES_PORT, "POSTGRES_PORT");
  if (postgresPort === LIVE_POSTGRES_PORT) {
    throw new Error(
      `POSTGRES_PORT=${LIVE_POSTGRES_PORT.toString()} is reserved for the live demo`,
    );
  }
  const httpPort = parsePort(
    env.PHASE1_ADMISSION_HTTP_PORT,
    "PHASE1_ADMISSION_HTTP_PORT",
  );
  if (httpPort === LIVE_HTTP_PORT || httpPort === postgresPort) {
    throw new Error(
      "PHASE1_ADMISSION_HTTP_PORT must differ from the live HTTP and isolated PostgreSQL ports",
    );
  }
  const corpusPath = env.PHASE1_ADMISSION_CORPUS_PATH ?? "";
  if (corpusPath.trim().length === 0) {
    throw new Error("PHASE1_ADMISSION_CORPUS_PATH is required");
  }

  return {
    runToken,
    database,
    postgresHost,
    postgresPort,
    httpHost: "127.0.0.1",
    httpPort,
    corpusPath: resolve(corpusPath),
  };
};

export type LogicalCpuTopology = {
  readonly logicalCpuIds: readonly number[];
  readonly physicalCoreIds: readonly string[];
};

export const parseLinuxCpuTopology = (
  lscpuCsv: string,
  allowedLogicalCpuIds: readonly number[],
): LogicalCpuTopology => {
  const allowed = new Set(allowedLogicalCpuIds);
  const rows = lscpuCsv
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"))
    .map((line) => line.split(","));
  const selected = rows.filter(([cpu]) => allowed.has(Number(cpu)));
  if (selected.length !== allowed.size) {
    throw new Error(
      `CPU topology is missing allowed logical CPUs: expected=${allowed.size.toString()},found=${selected.length.toString()}`,
    );
  }
  const logicalCpuIds = selected.map(([cpu]) => Number(cpu));
  const physicalCoreIds = selected.map(
    ([, core, socket]) => `${socket ?? ""}:${core ?? ""}`,
  );
  if (new Set(physicalCoreIds).size !== physicalCoreIds.length) {
    throw new Error(
      "CPU affinity contains SMT siblings, not distinct physical cores",
    );
  }
  return { logicalCpuIds, physicalCoreIds };
};

export type StageAAdmissionReport = {
  readonly measuredDurationMs: number;
  readonly offered: number;
  readonly accepted202: number;
  readonly duplicate200: number;
  readonly rejectedOrFailed: number;
  readonly latencyMs: { readonly p99: number | null };
  readonly corpus: {
    readonly sha256: string;
    readonly expectedSha256: string;
  };
  readonly topology: LogicalCpuTopology;
};

export type StageAAdmissionGate = {
  readonly passed: boolean;
  readonly acceptedTps: number;
  readonly reasons: readonly string[];
};

/** Fail-closed evaluator for the exact numeric Phase 1 Stage A gate. */
export const evaluateStageAAdmissionGate = (
  report: StageAAdmissionReport,
): StageAAdmissionGate => {
  const reasons: string[] = [];
  const acceptedTps =
    report.measuredDurationMs > 0
      ? report.accepted202 / (report.measuredDurationMs / 1_000)
      : 0;
  if (report.measuredDurationMs < 300_000) {
    reasons.push("measured duration is below five minutes");
  }
  if (acceptedTps < 5_000) {
    reasons.push(`accepted TPS ${acceptedTps.toFixed(3)} is below 5000`);
  }
  if (report.latencyMs.p99 === null) {
    reasons.push("submit latency p99 is missing");
  } else if (report.latencyMs.p99 > 1_000) {
    reasons.push(
      `submit latency p99 ${report.latencyMs.p99.toFixed(3)}ms exceeds 1000ms`,
    );
  }
  if (report.duplicate200 !== 0 || report.rejectedOrFailed !== 0) {
    reasons.push(
      `non-new outcomes duplicate=${report.duplicate200.toString()},rejected_or_failed=${report.rejectedOrFailed.toString()}`,
    );
  }
  if (report.accepted202 !== report.offered) {
    reasons.push(
      `accepted/offered mismatch ${report.accepted202.toString()}/${report.offered.toString()}`,
    );
  }
  if (report.corpus.sha256 !== report.corpus.expectedSha256) {
    reasons.push("corpus SHA-256 does not match the pinned manifest");
  }
  if (
    report.topology.logicalCpuIds.length !== 8 ||
    report.topology.physicalCoreIds.length !== 8 ||
    new Set(report.topology.physicalCoreIds).size !== 8
  ) {
    reasons.push(
      "server affinity is not exactly eight distinct physical cores",
    );
  }
  return { passed: reasons.length === 0, acceptedTps, reasons };
};
