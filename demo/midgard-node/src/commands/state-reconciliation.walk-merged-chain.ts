import * as SDK from "@al-ft/midgard-sdk";
import { type Assets } from "@lucid-evolution/lucid";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import {
  type CheckId,
  type CheckStatus,
  COMPARES,
  type HeaderRoots,
  type JournalSummary,
  type L1Observation,
  type L1QueueHeader,
  type L1StateView,
  type NativeRootObservation,
  type ReconciliationCheck,
  type SqlStateSnapshot,
} from "./state-reconciliation.compares.js";

export type StateReconciliationInput = {
  readonly l1: L1Observation;
  readonly sql: SqlStateSnapshot;
  readonly native: NativeRootObservation;
  readonly allowInFlight: boolean;
  readonly attempts?: number;
  /** Wall-clock time, taken no later than the L1 read, that DA deadlines are judged at. */
  readonly nowMs: number;
  /** da_attestation_timeout_ms of the deployment profile the node runs. */
  readonly daAttestationTimeoutMs: number;
};

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

export const JOURNAL_STATUS = PendingBlockFinalizationsDB.Status;

export const ACTIVE_JOURNAL_STATUSES: ReadonlySet<string> = new Set([
  JOURNAL_STATUS.PendingSubmission,
  JOURNAL_STATUS.SubmittedLocalFinalizationPending,
  JOURNAL_STATUS.SubmittedUnconfirmed,
  JOURNAL_STATUS.ObservedWaitingStability,
]);

export const LOCALLY_FINALIZED_ACTIVE_STATUSES: ReadonlySet<string> = new Set([
  JOURNAL_STATUS.SubmittedUnconfirmed,
  JOURNAL_STATUS.ObservedWaitingStability,
]);

export const toHex = (value: unknown): string => {
  if (Buffer.isBuffer(value)) return value.toString("hex");
  if (value instanceof Uint8Array) return Buffer.from(value).toString("hex");
  if (typeof value === "string") return value.toLowerCase();
  return String(value);
};

export const toHexOrNull = (value: unknown): string | null =>
  value === null || value === undefined ? null : toHex(value);

/**
 * Strips anything that could carry a credential (provider or database URLs)
 * from an error message before it reaches the report.
 */
export const redactSensitive = (message: string): string =>
  message
    .replace(/\b[a-z][a-z0-9+.-]*:\/\/[^\s"'`,)]+/giu, "<redacted-url>")
    .replace(/password=[^\s&"']+/giu, "password=<redacted>");

export const describeError = (error: unknown): string => {
  if (error instanceof Error) {
    const cause =
      "cause" in error && error.cause !== undefined
        ? `: ${typeof error.cause === "string" ? error.cause : error.cause instanceof Error ? error.cause.message : ""}`
        : "";
    return redactSensitive(`${error.message}${cause}`.trim());
  }
  if (typeof error === "object" && error !== null && "message" in error) {
    const record = error as { message: unknown; cause?: unknown };
    const cause =
      typeof record.cause === "string"
        ? `: ${record.cause}`
        : record.cause instanceof Error
          ? `: ${record.cause.message}`
          : "";
    return redactSensitive(`${String(record.message)}${cause}`);
  }
  return redactSensitive(String(error));
};

export const short = (hex: string | null): string =>
  hex === null ? "<none>" : hex.length > 20 ? `${hex.slice(0, 16)}..` : hex;

export const assetsLabel = (assets: Assets): string =>
  Object.entries(assets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([unit, quantity]) => `${unit}=${quantity.toString()}`)
    .join(",");

export const plural = (count: number, singular: string, pluralForm?: string) =>
  `${count.toString()} ${count === 1 ? singular : (pluralForm ?? `${singular}s`)}`;

type CheckAccumulator = {
  readonly failures: string[];
  readonly inFlight: string[];
  readonly notes: string[];
};

export const newAccumulator = (): CheckAccumulator => ({
  failures: [],
  inFlight: [],
  notes: [],
});

export const finishCheck = (
  id: CheckId,
  acc: CheckAccumulator,
  allowInFlight: boolean,
  passSummary: string,
): ReconciliationCheck => {
  let status: CheckStatus;
  let reason: string;
  if (acc.failures.length > 0) {
    status = "FAIL";
    reason = `${plural(acc.failures.length, "inconsistency", "inconsistencies")}; first: ${acc.failures[0]!}`;
  } else if (acc.inFlight.length > 0 && !allowInFlight) {
    status = "FAIL";
    reason = `${plural(acc.inFlight.length, "in-flight state")} could not be verified (rerun, or pass --allow-in-flight to accept); first: ${acc.inFlight[0]!}`;
  } else {
    status = "PASS";
    reason =
      acc.inFlight.length > 0
        ? `${passSummary}; accepted ${plural(acc.inFlight.length, "in-flight state")} (--allow-in-flight)`
        : passSummary;
  }
  return {
    id,
    status,
    reason,
    compares: COMPARES[id],
    failures: acc.failures,
    inFlight: acc.inFlight,
    notes: acc.notes,
  };
};

export const skipCheck = (
  id: CheckId,
  reason: string,
): ReconciliationCheck => ({
  id,
  status: "SKIPPED",
  reason,
  compares: COMPARES[id],
  failures: [],
  inFlight: [],
  notes: [],
});

// ---------------------------------------------------------------------------
// Derived context shared by the checks
// ---------------------------------------------------------------------------

type MergedWalk = {
  readonly headers: ReadonlySet<string>;
  /** True when the walk reached the genesis sentinel. */
  readonly complete: boolean;
  readonly stopReason: string;
  /** Journals marked abandoned that the confirmed chain nevertheless passes through. */
  readonly abandonedOnChain: readonly string[];
};

export type Context = {
  readonly l1: L1StateView | null;
  readonly l1UnavailableReason: string;
  readonly sql: SqlStateSnapshot;
  readonly journals: ReadonlyMap<string, JournalSummary>;
  readonly onChainUnmerged: ReadonlyMap<string, L1QueueHeader>;
  readonly merged: MergedWalk | null;
  readonly settlementHeaders: ReadonlySet<string>;
  readonly activeHeaders: ReadonlySet<string>;
  readonly latestCommittedEndTimeMs: number | null;
  /** SQL confirmed ledger differs from the L1 confirmed root. */
  readonly sqlMergeLag: boolean;
};

const walkMergedChain = (
  confirmedHeaderHash: string,
  journals: ReadonlyMap<string, JournalSummary>,
): MergedWalk => {
  const headers = new Set<string>();
  const abandonedOnChain: string[] = [];
  let current = confirmedHeaderHash;
  const limit = journals.size + 2;
  for (let step = 0; step <= limit; step += 1) {
    if (current === SDK.GENESIS_HEADER_HASH) {
      return {
        headers,
        complete: true,
        stopReason: "reached genesis",
        abandonedOnChain,
      };
    }
    if (headers.has(current)) {
      return {
        headers,
        complete: false,
        stopReason: `cycle at ${current}`,
        abandonedOnChain,
      };
    }
    headers.add(current);
    const journal = journals.get(current);
    if (journal !== undefined) {
      if (journal.status === JOURNAL_STATUS.Abandoned) {
        abandonedOnChain.push(current);
      }
      current = journal.baseTailHeaderHash;
      continue;
    }
    return {
      headers,
      complete: false,
      stopReason: `history before ${current} is not known locally`,
      abandonedOnChain,
    };
  }
  return {
    headers,
    complete: false,
    stopReason: "walk exceeded the number of known headers",
    abandonedOnChain,
  };
};

export const buildContext = (input: StateReconciliationInput): Context => {
  const sql = input.sql;
  const journals = new Map(sql.journals.map((j) => [j.headerHash, j]));
  const l1 = input.l1.kind === "observed" ? input.l1.view : null;
  const onChainUnmerged = new Map(
    (l1?.unmerged ?? []).map((header) => [header.headerHash, header]),
  );
  const settlementHeaders = new Set(
    (l1?.settlements ?? []).flatMap((s) => s.tokens.map((t) => t.assetName)),
  );
  const endTimes = (l1?.unmerged ?? [])
    .map((h) => h.endTimeMs)
    .filter((t): t is number => t !== null);
  return {
    l1,
    l1UnavailableReason:
      input.l1.kind === "unavailable" ? input.l1.reason : "L1 observed",
    sql,
    journals,
    onChainUnmerged,
    merged:
      l1 === null ? null : walkMergedChain(l1.confirmed.headerHash, journals),
    settlementHeaders,
    activeHeaders: new Set(sql.activeHeaderHashes),
    sqlMergeLag: l1 !== null && l1.confirmed.utxoRoot !== sql.confirmedRoot,
    latestCommittedEndTimeMs:
      l1 === null
        ? null
        : endTimes.length > 0
          ? Math.max(...endTimes)
          : l1.confirmed.endTimeMs,
  };
};

export const isMerged = (ctx: Context, headerHash: string): boolean =>
  ctx.merged?.headers.has(headerHash) === true ||
  ctx.settlementHeaders.has(headerHash);

export const rootsDiff = (
  label: string,
  onChain: Partial<HeaderRoots>,
  local: Partial<HeaderRoots>,
): string[] =>
  (Object.keys(onChain) as (keyof HeaderRoots)[])
    .filter(
      (key) =>
        local[key] !== undefined &&
        onChain[key] !== undefined &&
        onChain[key] !== local[key],
    )
    .map(
      (key) =>
        `${label}: ${key} root differs (L1=${short(onChain[key] ?? null)}, SQL=${short(local[key] ?? null)})`,
    );
