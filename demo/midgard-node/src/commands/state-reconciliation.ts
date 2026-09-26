/**
 * Read-only state reconciliation.
 *
 * Compares the four places a node's view of the rollup lives:
 *
 * - L1, read through the node's configured Lucid provider (state queue,
 *   deposit and withdrawal orders, payouts, settlements);
 * - SQL, read inside one `REPEATABLE READ, READ ONLY` transaction so every
 *   table is observed at the same database instant;
 * - the persisted native ledger root (the Architecture-G owner's durable root,
 *   or the MPF LevelDB root marker);
 * - the SQL ledger cache (`mempool_ledger`) that serves L2 UTxO queries.
 *
 * Nothing here writes: no leases, no audit records, no LevelDB opens on the
 * live store (the offline reader opens a private copy). The L1 and native-root
 * reads are repeated after the SQL snapshot and the whole collection retried
 * when either moved, so a comparison never mixes two different chain points.
 *
 * Every check reports PASS, FAIL or SKIPPED with a reason. Transient states the
 * reconciler can prove (a value equal to a recomputed later ledger point) PASS
 * with a note; transient states it cannot prove FAIL unless the operator passes
 * `allowInFlight`, in which case they are listed and accepted.
 */
import { cp, mkdtemp, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { assetsEqual, valueToAssets } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  type Assets,
  Data as LucidData,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either, Option } from "effect";
import { Level } from "level";

import {
  ConfirmedLedgerDB,
  DepositsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  WithdrawalsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { depositUTxOToEntry } from "../fibers/fetch-and-insert-deposit-utxos.js";
import { withdrawalUTxOToEntry } from "../fibers/fetch-and-insert-withdrawal-utxos.js";
import { resolveTxDeltaForCommit } from "../mpf/commit-rejection.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import { parseStoredRootHex, ROOT_KEY } from "../mpf/store-primitives.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { materializeConfirmedLedgerDeltaChain } from "../transactions/state-queue/confirmed-ledger-snapshot.js";

// ---------------------------------------------------------------------------
// Report model
// ---------------------------------------------------------------------------

export type CheckStatus = "PASS" | "FAIL" | "SKIPPED";

export const STATE_RECONCILIATION_CHECK_IDS = [
  "confirmed-root",
  "native-root",
  "state-queue-journal",
  "state-queue-tail-root",
  "deposits",
  "withdrawals",
  "payouts",
  "settlements",
  "ledger-cache",
] as const;

export type CheckId = (typeof STATE_RECONCILIATION_CHECK_IDS)[number];

export type ReconciliationCheck = {
  readonly id: CheckId;
  readonly status: CheckStatus;
  readonly reason: string;
  /** What the check compares and where each side is read from. */
  readonly compares: string;
  /** Inconsistencies; any entry makes the check FAIL. */
  readonly failures: readonly string[];
  /** Transient states the reconciler could not prove. */
  readonly inFlight: readonly string[];
  /** Informational observations, including proven transient states. */
  readonly notes: readonly string[];
};

export type ReconciliationReport = {
  readonly ok: boolean;
  readonly exitCode: 0 | 1;
  readonly allowInFlight: boolean;
  readonly snapshot: {
    readonly attempts: number;
    readonly l1: string;
    readonly nativeRoot: string;
    readonly sqlConfirmedRoot: string;
    readonly finalizedTip: string;
    readonly activeJournal: string;
  };
  readonly summary: {
    readonly pass: number;
    readonly fail: number;
    readonly skipped: number;
  };
  readonly checks: readonly ReconciliationCheck[];
};

const COMPARES: Record<CheckId, string> = {
  "confirmed-root":
    "L1 state-queue root node ConfirmedState.utxoRoot (provider) vs the MPF root recomputed from SQL confirmed_ledger",
  "native-root":
    "persisted native ledger root (Architecture-G owner durableRoot from node readiness, else the LEDGER_MPF_DB_PATH __root__ marker read from a private copy) vs the root recomputed from SQL confirmed_ledger plus the finalized-but-unmerged pending_block_finalizations deltas (the committed tip), or plus the active journal delta",
  "state-queue-journal":
    "L1 state-queue headers (provider) vs pending_block_finalizations, foreign_tip_reconciliations, blocks and the admitted correction transitions in state_queue_terminal_observer_states (SQL)",
  "state-queue-tail-root":
    "utxosRoot of the last L1 state-queue header (ConfirmedState.utxoRoot when the queue is empty) vs the persisted native ledger root",
  deposits:
    "L1 deposit orders (provider, decoded exactly as ingestion does) vs deposits_utxos payload, status and projected header; every SQL header assignment vs the L1 queue and the merged chain",
  withdrawals:
    "L1 withdrawal orders (provider, decoded exactly as ingestion does) vs withdrawal_utxos payload (including l2_value), status and projected header; every SQL header assignment vs the L1 queue and the merged chain",
  payouts:
    "L1 payout UTxOs and their PayoutDatum (provider) vs the withdrawal_utxos row with the same asset name: finalized, WithdrawalIsValid, merged header, equal l2_value, l1_address and l1_datum",
  settlements:
    "L1 settlement UTxOs and their SettlementDatum (provider) vs the merged chain and the expected event roots of the journal or foreign reconciliation for that header (SQL)",
  "ledger-cache":
    "SQL mempool_ledger vs the ledger recomputed at the committed tip (or active journal) plus unincluded projected deposit rows plus the effects of every mempool and processed_mempool transaction",
};

// ---------------------------------------------------------------------------
// Evaluator input model (plain data; hex strings throughout)
// ---------------------------------------------------------------------------

export type HeaderRoots = {
  readonly utxos: string;
  readonly deposits: string;
  readonly withdrawals: string;
  readonly forcedTransactions: string;
  readonly transactions: string;
};

export type L1QueueHeader = {
  readonly outRef: string;
  /** Header hash named by the node's asset name. */
  readonly headerHash: string;
  /** Header hash recomputed from the datum header; null when undecodable. */
  readonly recomputedHeaderHash: string | null;
  readonly prevHeaderHash: string | null;
  readonly endTimeMs: number | null;
  readonly roots: HeaderRoots | null;
  readonly decodeError: string | null;
};

export type DepositPayload = {
  readonly eventId: string;
  readonly info: string;
  readonly inclusionTimeMs: number;
  readonly l1TxHash: string;
  readonly ledgerTxId: string;
  readonly ledgerOutput: string;
  readonly ledgerAddress: string;
};

export type WithdrawalPayload = {
  readonly eventId: string;
  readonly rawEventInfo: string;
  readonly inclusionTimeMs: number;
  readonly l1TxHash: string;
  readonly l1OutputIndex: number;
  readonly assetName: string;
  readonly l2Outref: string;
  readonly l2Owner: string;
  readonly l2Value: string;
  readonly l1Address: string;
  readonly l1Datum: string;
  readonly refundAddress: string;
  readonly refundDatum: string;
};

export type L1EventOrder<P> = {
  readonly outRef: string;
  readonly payload: P | null;
  readonly decodeError: string | null;
};

export type L1Payout = {
  readonly outRef: string;
  /** Asset names carried under the payout policy (each with its quantity). */
  readonly tokens: readonly {
    readonly assetName: string;
    readonly quantity: string;
  }[];
  readonly l2Value: Assets | null;
  readonly l1AddressCbor: string | null;
  readonly l1DatumCbor: string | null;
  readonly decodeError: string | null;
};

export type L1Settlement = {
  readonly outRef: string;
  readonly tokens: readonly {
    readonly assetName: string;
    readonly quantity: string;
  }[];
  readonly roots: Omit<HeaderRoots, "utxos"> | null;
  readonly decodeError: string | null;
};

export type L1StateView = {
  readonly confirmed: {
    readonly outRef: string;
    readonly headerHash: string;
    readonly utxoRoot: string;
    readonly endTimeMs: number;
  };
  readonly unmerged: readonly L1QueueHeader[];
  readonly deposits: readonly L1EventOrder<DepositPayload>[];
  readonly withdrawals: readonly L1EventOrder<WithdrawalPayload>[];
  readonly payouts: readonly L1Payout[];
  readonly settlements: readonly L1Settlement[];
};

export type JournalSummary = {
  readonly headerHash: string;
  readonly status: string;
  readonly baseTailHeaderHash: string;
  readonly baseUtxosRoot: string;
  readonly expected: HeaderRoots;
  readonly correctionTransitionDigest: string | null;
  readonly submittedTxHash: string | null;
  /** Header end time decoded from the journal's header CBOR. */
  readonly endTimeMs: number | null;
};

export type ForeignSummary = {
  readonly headerHash: string;
  readonly status: string;
  readonly prevHeaderHash: string | null;
  readonly roots: Omit<HeaderRoots, "utxos" | "transactions">;
  readonly transactionsRoot: string | null;
  readonly utxosRoot: string | null;
};

export type LedgerPoint = {
  readonly label: string;
  /** Null for the confirmed ledger itself. */
  readonly headerHash: string | null;
  readonly root: string;
  /** Ledger at this point: §5.3 outref hex -> output hex. */
  readonly entries: ReadonlyMap<string, string>;
  /** Headers applied on top of the confirmed ledger to reach this point. */
  readonly chainHeaderHashes: readonly string[];
};

export type LedgerPointResult =
  | { readonly kind: "materialized"; readonly point: LedgerPoint }
  | {
      readonly kind: "failed";
      readonly label: string;
      readonly headerHash: string | null;
      readonly reason: string;
      /** The delta chain reaches a header with no local journal. */
      readonly parentMissing: boolean;
    };

export type SqlDepositRow = {
  readonly payload: DepositPayload;
  readonly status: string;
  readonly projectedHeaderHash: string | null;
  /** §5.3 ledger outref of the deposit UTxO, null if unconvertible. */
  readonly ledgerOutref: string | null;
};

export type SqlWithdrawalRow = {
  readonly payload: WithdrawalPayload;
  readonly status: string;
  readonly validity: string | null;
  readonly projectedHeaderHash: string | null;
};

export type PendingTxDelta = {
  readonly txId: string;
  readonly source: "mempool" | "processed_mempool";
  readonly delta: {
    readonly spent: readonly string[];
    readonly produced: readonly {
      readonly outref: string;
      readonly output: string;
    }[];
  } | null;
  readonly rejectDetail: string | null;
};

export type ObserverTransition = {
  readonly transactionHash: string;
  readonly transitionKind: string;
  readonly removedHeaderHashes: readonly string[];
  readonly transitionDigest: string;
};

export type ObserverSnapshot =
  | { readonly kind: "absent" }
  | { readonly kind: "invalid"; readonly reason: string }
  | {
      readonly kind: "present";
      readonly admitted: readonly ObserverTransition[];
      readonly pendingCount: number;
    };

export type SqlStateSnapshot = {
  readonly confirmedRoot: string;
  /** Set when a confirmed_ledger row cannot be encoded into the MPF. */
  readonly confirmedRootError: string | null;
  readonly confirmedEntryCount: number;
  readonly journals: readonly JournalSummary[];
  readonly activeHeaderHashes: readonly string[];
  readonly finalizedTip: LedgerPointResult;
  readonly activeTip: LedgerPointResult | null;
  readonly deposits: readonly SqlDepositRow[];
  readonly withdrawals: readonly SqlWithdrawalRow[];
  readonly mempoolLedger: readonly {
    readonly outref: string;
    readonly output: string;
    readonly sourceEventId: string | null;
  }[];
  readonly pendingTxs: readonly PendingTxDelta[];
  readonly blockHeaderHashes: readonly string[];
  readonly foreign: readonly ForeignSummary[];
  readonly observer: ObserverSnapshot;
};

export type NativeRootObservation =
  | {
      readonly kind: "observed";
      readonly root: string;
      readonly source: "node-readiness" | "leveldb-copy";
    }
  | { readonly kind: "unavailable"; readonly reason: string };

export type L1Observation =
  | { readonly kind: "observed"; readonly view: L1StateView }
  | { readonly kind: "unavailable"; readonly reason: string };

export type StateReconciliationInput = {
  readonly l1: L1Observation;
  readonly sql: SqlStateSnapshot;
  readonly native: NativeRootObservation;
  readonly allowInFlight: boolean;
  readonly attempts?: number;
};

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

const JOURNAL_STATUS = PendingBlockFinalizationsDB.Status;

const ACTIVE_JOURNAL_STATUSES: ReadonlySet<string> = new Set([
  JOURNAL_STATUS.PendingSubmission,
  JOURNAL_STATUS.SubmittedLocalFinalizationPending,
  JOURNAL_STATUS.SubmittedUnconfirmed,
  JOURNAL_STATUS.ObservedWaitingStability,
]);

const LOCALLY_FINALIZED_ACTIVE_STATUSES: ReadonlySet<string> = new Set([
  JOURNAL_STATUS.SubmittedUnconfirmed,
  JOURNAL_STATUS.ObservedWaitingStability,
]);

const toHex = (value: unknown): string => {
  if (Buffer.isBuffer(value)) return value.toString("hex");
  if (value instanceof Uint8Array) return Buffer.from(value).toString("hex");
  if (typeof value === "string") return value.toLowerCase();
  return String(value);
};

const toHexOrNull = (value: unknown): string | null =>
  value === null || value === undefined ? null : toHex(value);

/**
 * Strips anything that could carry a credential (provider or database URLs)
 * from an error message before it reaches the report.
 */
export const redactSensitive = (message: string): string =>
  message
    .replace(/\b[a-z][a-z0-9+.-]*:\/\/[^\s"'`,)]+/giu, "<redacted-url>")
    .replace(/password=[^\s&"']+/giu, "password=<redacted>");

const describeError = (error: unknown): string => {
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

const short = (hex: string | null): string =>
  hex === null ? "<none>" : hex.length > 20 ? `${hex.slice(0, 16)}..` : hex;

const assetsLabel = (assets: Assets): string =>
  Object.entries(assets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([unit, quantity]) => `${unit}=${quantity.toString()}`)
    .join(",");

const plural = (count: number, singular: string, pluralForm?: string) =>
  `${count.toString()} ${count === 1 ? singular : (pluralForm ?? `${singular}s`)}`;

type CheckAccumulator = {
  readonly failures: string[];
  readonly inFlight: string[];
  readonly notes: string[];
};

const newAccumulator = (): CheckAccumulator => ({
  failures: [],
  inFlight: [],
  notes: [],
});

const finishCheck = (
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

const skipCheck = (id: CheckId, reason: string): ReconciliationCheck => ({
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

type Context = {
  readonly l1: L1StateView | null;
  readonly l1UnavailableReason: string;
  readonly sql: SqlStateSnapshot;
  readonly journals: ReadonlyMap<string, JournalSummary>;
  readonly foreign: ReadonlyMap<string, ForeignSummary>;
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
  foreign: ReadonlyMap<string, ForeignSummary>,
): MergedWalk => {
  const headers = new Set<string>();
  const abandonedOnChain: string[] = [];
  let current = confirmedHeaderHash;
  const limit = journals.size + foreign.size + 2;
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
    const foreignRow = foreign.get(current);
    if (foreignRow?.prevHeaderHash != null) {
      current = foreignRow.prevHeaderHash;
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

const buildContext = (input: StateReconciliationInput): Context => {
  const sql = input.sql;
  const journals = new Map(sql.journals.map((j) => [j.headerHash, j]));
  const foreign = new Map(sql.foreign.map((f) => [f.headerHash, f]));
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
    foreign,
    onChainUnmerged,
    merged:
      l1 === null
        ? null
        : walkMergedChain(l1.confirmed.headerHash, journals, foreign),
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

const isMerged = (ctx: Context, headerHash: string): boolean =>
  ctx.merged?.headers.has(headerHash) === true ||
  ctx.settlementHeaders.has(headerHash);

const rootsDiff = (
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

// ---------------------------------------------------------------------------
// Checks
// ---------------------------------------------------------------------------

const checkConfirmedRoot = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "confirmed-root",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const l1Root = ctx.l1.confirmed.utxoRoot;
  const sqlRoot = ctx.sql.confirmedRoot;
  if (ctx.sql.confirmedRootError !== null) {
    acc.failures.push(
      `SQL confirmed_ledger (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")}) cannot be encoded into the ledger MPF: ${ctx.sql.confirmedRootError}`,
    );
  } else if (l1Root !== sqlRoot) {
    // Lag diagnosis: SQL still at the pre-merge root of a header L1 has
    // already merged means the merge's SQL application has not run yet.
    const laggingJournal = [...(ctx.merged?.headers ?? [])]
      .map((hash) => ctx.journals.get(hash))
      .find((journal) => journal?.baseUtxosRoot === sqlRoot);
    if (laggingJournal !== undefined) {
      acc.inFlight.push(
        `SQL confirmed ledger root ${short(sqlRoot)} is the pre-merge root of header ${laggingJournal.headerHash}, which L1 has already merged (confirmed root ${short(l1Root)}); the merge has not been applied to SQL yet`,
      );
    } else {
      acc.failures.push(
        `L1 confirmed utxoRoot ${l1Root} != SQL confirmed_ledger root ${sqlRoot} (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")})`,
      );
    }
  }
  return finishCheck(
    "confirmed-root",
    acc,
    allowInFlight,
    `L1 confirmed utxoRoot equals the SQL confirmed_ledger root ${sqlRoot} (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")}, confirmed header ${ctx.l1.confirmed.headerHash})`,
  );
};

/**
 * Why no ledger point could be recomputed. A delta chain that stops at a
 * missing parent is a justified skip when the parent is history this node did
 * not produce (a foreign header); when SQL's confirmed ledger also disagrees
 * with L1 the chain stopped because the confirmed base is wrong, which the
 * confirmed-root check reports as the failure.
 */
const unrecomputableReason = (ctx: Context, tip: LedgerPointResult): string => {
  const base = `no ledger point could be recomputed: ${tip.kind === "failed" ? tip.reason : "no candidate"}`;
  if (ctx.sqlMergeLag) {
    return `${base}; the SQL confirmed ledger root differs from L1, so the journal chain has no valid base (reported by confirmed-root)`;
  }
  return `${base}; the chain reaches history not produced by this node`;
};

const unencodableConfirmedReason = (ctx: Context): string =>
  `the SQL confirmed ledger cannot be encoded, so no ledger point can be recomputed (reported by confirmed-root): ${ctx.sql.confirmedRootError ?? ""}`;

type NativeCandidate = { readonly label: string; readonly root: string };

const materializedCandidates = (sql: SqlStateSnapshot): NativeCandidate[] =>
  [sql.finalizedTip, sql.activeTip]
    .filter(
      (r): r is Extract<LedgerPointResult, { kind: "materialized" }> =>
        r?.kind === "materialized",
    )
    .map((r) => ({ label: r.point.label, root: r.point.root }));

const checkNativeRoot = (
  ctx: Context,
  native: NativeRootObservation,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (native.kind === "unavailable") {
    return skipCheck("native-root", native.reason);
  }
  if (ctx.sql.confirmedRootError !== null) {
    return skipCheck("native-root", unencodableConfirmedReason(ctx));
  }
  const tip = ctx.sql.finalizedTip;
  const candidates = materializedCandidates(ctx.sql);
  const acc = newAccumulator();
  if (tip.kind === "failed" && !tip.parentMissing) {
    acc.failures.push(
      `SQL journal delta chain for ${tip.label} does not reproduce its expected root: ${tip.reason}`,
    );
  }
  if (
    ctx.sql.activeTip?.kind === "failed" &&
    !ctx.sql.activeTip.parentMissing
  ) {
    acc.failures.push(
      `SQL journal delta chain for ${ctx.sql.activeTip.label} does not reproduce its expected root: ${ctx.sql.activeTip.reason}`,
    );
  }
  if (candidates.length === 0) {
    if (acc.failures.length > 0) {
      return finishCheck("native-root", acc, allowInFlight, "");
    }
    return skipCheck("native-root", unrecomputableReason(ctx, tip));
  }
  const matched = candidates.find((c) => c.root === native.root);
  if (matched === undefined) {
    acc.failures.push(
      `native root ${native.root} (${native.source}) matches no recomputed ledger point: ${candidates.map((c) => `${c.label}=${c.root}`).join("; ")}`,
    );
  } else if (tip.kind === "materialized" && matched.root !== tip.point.root) {
    acc.notes.push(
      `native root is one block ahead of the committed tip, at ${matched.label} (promotion follows block submission)`,
    );
  }
  if (tip.kind === "failed" && tip.parentMissing) {
    acc.notes.push(`committed tip not recomputable: ${tip.reason}`);
  }
  return finishCheck(
    "native-root",
    acc,
    allowInFlight,
    `native root ${native.root} (${native.source}) equals the recomputed root at ${matched?.label ?? "?"}`,
  );
};

const checkStateQueueJournal = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null || ctx.merged === null) {
    return skipCheck(
      "state-queue-journal",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const merged = ctx.merged;
  let previousHeaderHash = ctx.l1.confirmed.headerHash;
  for (const header of ctx.l1.unmerged) {
    const label = `L1 header ${header.headerHash}`;
    if (header.decodeError !== null) {
      acc.failures.push(
        `${label}: datum header undecodable: ${header.decodeError}`,
      );
      previousHeaderHash = header.headerHash;
      continue;
    }
    if (header.recomputedHeaderHash !== header.headerHash) {
      acc.failures.push(
        `${label}: recomputed header hash ${header.recomputedHeaderHash ?? "<none>"} differs from its node key`,
      );
    }
    if (
      header.prevHeaderHash !== null &&
      header.prevHeaderHash !== previousHeaderHash
    ) {
      acc.failures.push(
        `${label}: prevHeaderHash ${header.prevHeaderHash} does not link to the preceding queue node ${previousHeaderHash}`,
      );
    }
    previousHeaderHash = header.headerHash;
    const journal = ctx.journals.get(header.headerHash);
    if (journal !== undefined) {
      if (header.roots !== null) {
        acc.failures.push(...rootsDiff(label, header.roots, journal.expected));
      }
      if (
        header.prevHeaderHash !== null &&
        journal.baseTailHeaderHash !== header.prevHeaderHash
      ) {
        acc.failures.push(
          `${label}: journal base tail ${journal.baseTailHeaderHash} differs from the header's prevHeaderHash ${header.prevHeaderHash}`,
        );
      }
      if (journal.status === JOURNAL_STATUS.Finalized) continue;
      if (journal.status === JOURNAL_STATUS.Abandoned) {
        acc.failures.push(
          `${label}: its journal is marked abandoned (correction digest ${journal.correctionTransitionDigest ?? "<none>"}) but the header is still on L1`,
        );
        continue;
      }
      acc.notes.push(
        `${label}: journal is ${journal.status}, roots match, awaiting L1 stability`,
      );
      continue;
    }
    const foreignRow = ctx.foreign.get(header.headerHash);
    if (foreignRow !== undefined) {
      if (header.roots !== null) {
        acc.failures.push(
          ...rootsDiff(label, header.roots, {
            ...foreignRow.roots,
            ...(foreignRow.transactionsRoot === null
              ? {}
              : { transactions: foreignRow.transactionsRoot }),
          }),
        );
      }
      if (foreignRow.status === "awaiting") {
        acc.inFlight.push(`${label}: foreign header awaiting reconciliation`);
      } else {
        acc.notes.push(`${label}: foreign header, reconciled`);
      }
      continue;
    }
    acc.failures.push(
      `${label}: no journal and no foreign-tip reconciliation exists for this on-chain header`,
    );
  }

  for (const hash of merged.abandonedOnChain) {
    acc.failures.push(
      `header ${hash} is on the merged L1 chain but its journal is marked abandoned`,
    );
  }

  for (const journal of ctx.sql.journals) {
    const onChain = ctx.onChainUnmerged.has(journal.headerHash);
    const isMergedHeader = merged.headers.has(journal.headerHash);
    if (
      journal.status === JOURNAL_STATUS.Finalized &&
      !onChain &&
      !isMergedHeader
    ) {
      if (ctx.settlementHeaders.has(journal.headerHash)) {
        acc.notes.push(
          `finalized journal ${journal.headerHash} is merged per its settlement (merged walk: ${merged.stopReason})`,
        );
      } else if (
        !merged.complete &&
        journal.endTimeMs !== null &&
        journal.endTimeMs <= ctx.l1.confirmed.endTimeMs
      ) {
        acc.notes.push(
          `finalized journal ${journal.headerHash} lies beyond the locally known merged chain (${merged.stopReason}) and ends at or before the confirmed state; treated as merged`,
        );
      } else {
        acc.failures.push(
          `finalized journal ${journal.headerHash} is neither on the L1 queue nor merged (merged walk: ${merged.stopReason})`,
        );
      }
    }
    if (ACTIVE_JOURNAL_STATUSES.has(journal.status) && !onChain) {
      if (isMergedHeader) {
        acc.failures.push(
          `journal ${journal.headerHash} is still ${journal.status} but its header is already merged on L1`,
        );
      } else {
        acc.notes.push(
          `active journal ${journal.headerHash} (${journal.status}) is not on L1 yet${journal.submittedTxHash === null ? " (not submitted)" : ` (submitted in ${journal.submittedTxHash})`}`,
        );
      }
    }
  }
  if (ctx.sql.activeHeaderHashes.length > 1) {
    acc.failures.push(
      `${ctx.sql.activeHeaderHashes.length.toString()} journals are active at once: ${ctx.sql.activeHeaderHashes.join(", ")}`,
    );
  }

  const observer = ctx.sql.observer;
  if (observer.kind === "absent") {
    acc.notes.push(
      "no state-queue correction observer state is stored for this state-queue policy; removal marking could not be cross-checked against admitted transitions",
    );
  } else if (observer.kind === "invalid") {
    acc.failures.push(
      `stored correction observer state is invalid: ${observer.reason}`,
    );
  } else {
    for (const transition of observer.admitted) {
      if (transition.transitionKind === "merge") continue;
      for (const removed of transition.removedHeaderHashes) {
        const label = `header ${removed} removed by admitted ${transition.transitionKind} ${transition.transactionHash}`;
        if (ctx.onChainUnmerged.has(removed)) {
          acc.failures.push(`${label} is still on the L1 queue`);
        }
        const journal = ctx.journals.get(removed);
        if (journal === undefined) {
          acc.notes.push(
            `${label}: no local journal (not produced by this node)`,
          );
        } else if (journal.status === JOURNAL_STATUS.Abandoned) {
          if (
            journal.correctionTransitionDigest !== transition.transitionDigest
          ) {
            acc.failures.push(
              `${label}: journal abandoned with correction digest ${journal.correctionTransitionDigest ?? "<none>"}, expected ${transition.transitionDigest}`,
            );
          }
        } else {
          acc.inFlight.push(
            `${label}: journal is still ${journal.status}; correction reinclusion has not run`,
          );
        }
      }
    }
    if (observer.pendingCount > 0) {
      acc.notes.push(
        `${plural(observer.pendingCount, "correction transition")} observed but not yet final`,
      );
    }
  }

  for (const hash of ctx.sql.blockHeaderHashes) {
    if (ctx.onChainUnmerged.has(hash) || ctx.activeHeaders.has(hash)) continue;
    const journal = ctx.journals.get(hash);
    if (merged.headers.has(hash) && ctx.sqlMergeLag) {
      acc.inFlight.push(
        `blocks table still references merged header ${hash}; SQL has not applied the latest L1 merge`,
      );
      continue;
    }
    acc.failures.push(
      `blocks table still references header ${hash}, which is neither on the L1 queue nor the active journal (journal status ${journal?.status ?? "<none>"}${merged.headers.has(hash) ? ", merged" : ""})`,
    );
  }

  return finishCheck(
    "state-queue-journal",
    acc,
    allowInFlight,
    `${plural(ctx.l1.unmerged.length, "unmerged L1 header")} and ${plural(ctx.sql.journals.length, "journal")} agree (merged walk: ${merged.stopReason}, ${plural(merged.headers.size, "merged header")})`,
  );
};

const checkTailRoot = (
  ctx: Context,
  native: NativeRootObservation,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "state-queue-tail-root",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  if (native.kind === "unavailable") {
    return skipCheck("state-queue-tail-root", native.reason);
  }
  const tail = ctx.l1.unmerged.at(-1);
  const tailRoot =
    tail === undefined
      ? ctx.l1.confirmed.utxoRoot
      : (tail.roots?.utxos ?? null);
  const tailLabel =
    tail === undefined
      ? `confirmed state ${ctx.l1.confirmed.headerHash}`
      : `tail header ${tail.headerHash}`;
  const acc = newAccumulator();
  if (tailRoot === null) {
    acc.failures.push(`${tailLabel} datum is undecodable`);
  } else if (tailRoot !== native.root) {
    const active = ctx.sql.activeTip;
    if (
      active?.kind === "materialized" &&
      active.point.root === native.root &&
      active.point.headerHash !== null &&
      !ctx.onChainUnmerged.has(active.point.headerHash)
    ) {
      acc.inFlight.push(
        `native root is at ${active.point.label}, whose header is not on L1 yet (${tailLabel} root ${short(tailRoot)})`,
      );
    } else {
      acc.failures.push(
        `${tailLabel} utxosRoot ${tailRoot} != native root ${native.root} (${native.source})`,
      );
    }
  }
  return finishCheck(
    "state-queue-tail-root",
    acc,
    allowInFlight,
    `${tailLabel} utxosRoot equals the native root ${native.root} (${native.source})`,
  );
};

type HeaderPlacement =
  | "none"
  | "on-chain"
  | "merged"
  | "active-pending"
  | "unknown";

const placeHeader = (
  ctx: Context,
  headerHash: string | null,
): HeaderPlacement => {
  if (headerHash === null) return "none";
  if (ctx.onChainUnmerged.has(headerHash)) return "on-chain";
  if (isMerged(ctx, headerHash)) return "merged";
  if (ctx.activeHeaders.has(headerHash)) return "active-pending";
  return "unknown";
};

const describeHeaderPlacement = (ctx: Context, headerHash: string): string => {
  const journal = ctx.journals.get(headerHash);
  return `header ${headerHash} is not on the L1 queue, not merged and not the active journal (journal status ${journal?.status ?? "<none>"}; merged walk ${ctx.merged?.stopReason ?? "n/a"})`;
};

const payloadDiff = <P extends Record<string, unknown>>(
  l1: P,
  local: P,
): string[] => Object.keys(l1).filter((key) => l1[key] !== local[key]);

const missingOrderIsImmature = (
  ctx: Context,
  inclusionTimeMs: number | undefined,
): boolean =>
  inclusionTimeMs !== undefined &&
  ctx.latestCommittedEndTimeMs !== null &&
  inclusionTimeMs > ctx.latestCommittedEndTimeMs;

const checkDeposits = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck("deposits", `L1 unavailable: ${ctx.l1UnavailableReason}`);
  }
  const acc = newAccumulator();
  const sqlById = new Map(
    ctx.sql.deposits.map((row) => [row.payload.eventId, row]),
  );
  for (const order of ctx.l1.deposits) {
    if (order.payload === null) {
      acc.failures.push(
        `L1 deposit ${order.outRef} is undecodable: ${order.decodeError ?? "unknown"}`,
      );
      continue;
    }
    const row = sqlById.get(order.payload.eventId);
    const label = `deposit ${order.payload.eventId} (${order.outRef})`;
    if (row === undefined) {
      if (missingOrderIsImmature(ctx, order.payload.inclusionTimeMs)) {
        acc.inFlight.push(
          `${label} is on L1 but not yet ingested; its inclusion time is after every committed block`,
        );
      } else {
        acc.failures.push(`${label} is on L1 but unknown to SQL`);
      }
      continue;
    }
    const diff = payloadDiff(order.payload, row.payload);
    if (diff.length > 0) {
      acc.failures.push(
        `${label}: SQL payload differs from L1 in ${diff.join(", ")}`,
      );
    }
  }
  let assigned = 0;
  for (const row of ctx.sql.deposits) {
    const label = `SQL deposit ${row.payload.eventId}`;
    const header = row.projectedHeaderHash;
    const placement = placeHeader(ctx, header);
    if (header === null) continue;
    assigned += 1;
    if (placement === "unknown") {
      acc.failures.push(`${label}: ${describeHeaderPlacement(ctx, header)}`);
      continue;
    }
    if (placement === "active-pending") {
      acc.notes.push(
        `${label} is assigned to active journal ${header}, not on L1 yet`,
      );
    }
    // Header assignment happens with the deposit already projected into the
    // L2 ledger; the merge marks it consumed (an L2 spend may do so earlier).
    if (row.status === DepositsDB.Status.Awaiting) {
      acc.failures.push(
        `${label}: assigned to ${placement} header ${header} but status is ${row.status}`,
      );
    } else if (
      placement === "merged" &&
      row.status !== DepositsDB.Status.Consumed
    ) {
      const message = `${label}: header ${header} is merged but status is ${row.status}, expected consumed`;
      if (ctx.sqlMergeLag) {
        acc.inFlight.push(
          `${message} (SQL confirmed ledger has not applied the latest L1 merge)`,
        );
      } else {
        acc.failures.push(message);
      }
    }
  }
  return finishCheck(
    "deposits",
    acc,
    allowInFlight,
    `${plural(ctx.l1.deposits.length, "L1 deposit order")} match SQL; ${plural(assigned, "SQL deposit")} with a header assignment point at on-chain or merged headers`,
  );
};

const checkWithdrawals = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "withdrawals",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const sqlById = new Map(
    ctx.sql.withdrawals.map((row) => [row.payload.eventId, row]),
  );
  for (const order of ctx.l1.withdrawals) {
    if (order.payload === null) {
      acc.failures.push(
        `L1 withdrawal ${order.outRef} is undecodable: ${order.decodeError ?? "unknown"}`,
      );
      continue;
    }
    const row = sqlById.get(order.payload.eventId);
    const label = `withdrawal ${order.payload.eventId} (${order.outRef})`;
    if (row === undefined) {
      if (missingOrderIsImmature(ctx, order.payload.inclusionTimeMs)) {
        acc.inFlight.push(
          `${label} is on L1 but not yet ingested; its inclusion time is after every committed block`,
        );
      } else {
        acc.failures.push(`${label} is on L1 but unknown to SQL`);
      }
      continue;
    }
    const diff = payloadDiff(order.payload, row.payload);
    if (diff.length > 0) {
      acc.failures.push(
        `${label}: SQL payload differs from L1 in ${diff.join(", ")}`,
      );
    }
  }
  let assigned = 0;
  for (const row of ctx.sql.withdrawals) {
    const label = `SQL withdrawal ${row.payload.eventId}`;
    const header = row.projectedHeaderHash;
    const placement = placeHeader(ctx, header);
    if (header === null) {
      if (row.status === WithdrawalsDB.Status.Finalized) {
        acc.failures.push(
          `${label}: status is finalized without a header assignment`,
        );
      }
      continue;
    }
    assigned += 1;
    if (placement === "unknown") {
      acc.failures.push(`${label}: ${describeHeaderPlacement(ctx, header)}`);
      continue;
    }
    if (placement === "active-pending") {
      acc.notes.push(
        `${label} is assigned to active journal ${header}, not on L1 yet`,
      );
    }
    // Local finalization of the block (and the merge) marks its withdrawals
    // finalized; before that they are projected.
    const journalFinalized =
      ctx.journals.get(header)?.status === JOURNAL_STATUS.Finalized;
    if (row.status === WithdrawalsDB.Status.Awaiting) {
      acc.failures.push(
        `${label}: assigned to ${placement} header ${header} but status is ${row.status}`,
      );
    } else if (
      journalFinalized &&
      row.status !== WithdrawalsDB.Status.Finalized
    ) {
      acc.failures.push(
        `${label}: journal ${header} is finalized but the withdrawal status is ${row.status}, expected finalized`,
      );
    } else if (
      placement === "merged" &&
      row.status !== WithdrawalsDB.Status.Finalized
    ) {
      const message = `${label}: header ${header} is merged but status is ${row.status}, expected finalized`;
      if (ctx.sqlMergeLag) {
        acc.inFlight.push(
          `${message} (SQL confirmed ledger has not applied the latest L1 merge)`,
        );
      } else {
        acc.failures.push(message);
      }
    }
  }
  return finishCheck(
    "withdrawals",
    acc,
    allowInFlight,
    `${plural(ctx.l1.withdrawals.length, "L1 withdrawal order")} match SQL (payload and l2_value); ${plural(assigned, "SQL withdrawal")} with a header assignment point at on-chain or merged headers`,
  );
};

const canonicalDataCbor = (hex: string, schema: unknown): string =>
  LucidData.to(LucidData.from(hex, schema as never), schema as never);

const checkPayouts = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck("payouts", `L1 unavailable: ${ctx.l1UnavailableReason}`);
  }
  const acc = newAccumulator();
  const byAssetName = new Map<string, SqlWithdrawalRow[]>();
  for (const row of ctx.sql.withdrawals) {
    const list = byAssetName.get(row.payload.assetName) ?? [];
    list.push(row);
    byAssetName.set(row.payload.assetName, list);
  }
  const seen = new Map<string, string>();
  for (const payout of ctx.l1.payouts) {
    const label = `payout ${payout.outRef}`;
    if (payout.tokens.length !== 1 || payout.tokens[0]!.quantity !== "1") {
      acc.failures.push(
        `${label}: expected exactly one payout token, found ${payout.tokens.map((t) => `${t.assetName}x${t.quantity}`).join(",") || "none"}`,
      );
      continue;
    }
    const assetName = payout.tokens[0]!.assetName;
    const duplicate = seen.get(assetName);
    if (duplicate !== undefined) {
      acc.failures.push(
        `${label}: payout token ${assetName} also held by ${duplicate}`,
      );
    }
    seen.set(assetName, payout.outRef);
    const rows = byAssetName.get(assetName) ?? [];
    if (rows.length !== 1) {
      acc.failures.push(
        `${label}: ${rows.length === 0 ? "no SQL withdrawal" : `${rows.length.toString()} SQL withdrawals`} carry asset name ${assetName}`,
      );
      continue;
    }
    const row = rows[0]!;
    const rowLabel = `${label} (withdrawal ${row.payload.eventId})`;
    if (row.status !== WithdrawalsDB.Status.Finalized) {
      acc.failures.push(
        `${rowLabel}: SQL status is ${row.status}, expected finalized`,
      );
    }
    if (row.validity !== WithdrawalsDB.Validity.WithdrawalIsValid) {
      acc.failures.push(
        `${rowLabel}: SQL validity is ${row.validity ?? "<unclassified>"}, expected WithdrawalIsValid`,
      );
    }
    if (
      row.projectedHeaderHash === null ||
      !isMerged(ctx, row.projectedHeaderHash)
    ) {
      acc.failures.push(
        `${rowLabel}: withdrawal header ${row.projectedHeaderHash ?? "<none>"} is not merged`,
      );
    }
    if (payout.decodeError !== null || payout.l2Value === null) {
      acc.failures.push(
        `${rowLabel}: payout datum undecodable: ${payout.decodeError ?? "missing"}`,
      );
      continue;
    }
    try {
      const sqlAssets = valueToAssets(
        LucidData.from(row.payload.l2Value, SDK.Value) as SDK.Value,
      );
      if (!assetsEqual(sqlAssets, payout.l2Value)) {
        acc.failures.push(
          `${rowLabel}: payout l2_value {${assetsLabel(payout.l2Value)}} != SQL l2_value {${assetsLabel(sqlAssets)}}`,
        );
      }
      if (
        canonicalDataCbor(row.payload.l1Address, SDK.AddressData) !==
        payout.l1AddressCbor
      ) {
        acc.failures.push(`${rowLabel}: payout l1_address differs from SQL`);
      }
      if (
        canonicalDataCbor(row.payload.l1Datum, SDK.CardanoDatum) !==
        payout.l1DatumCbor
      ) {
        acc.failures.push(`${rowLabel}: payout l1_datum differs from SQL`);
      }
    } catch (error) {
      acc.failures.push(
        `${rowLabel}: SQL payout fields undecodable: ${describeError(error)}`,
      );
    }
  }
  return finishCheck(
    "payouts",
    acc,
    allowInFlight,
    `${plural(ctx.l1.payouts.length, "L1 payout")} match finalized valid SQL withdrawals with equal l2_value, l1_address and l1_datum`,
  );
};

const checkSettlements = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null || ctx.merged === null) {
    return skipCheck(
      "settlements",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const merged = ctx.merged;
  const seen = new Map<string, string>();
  let compared = 0;
  for (const settlement of ctx.l1.settlements) {
    const label = `settlement ${settlement.outRef}`;
    if (
      settlement.tokens.length !== 1 ||
      settlement.tokens[0]!.quantity !== "1"
    ) {
      acc.failures.push(
        `${label}: expected exactly one settlement token, found ${settlement.tokens.map((t) => `${t.assetName}x${t.quantity}`).join(",") || "none"}`,
      );
      continue;
    }
    const headerHash = settlement.tokens[0]!.assetName;
    const duplicate = seen.get(headerHash);
    if (duplicate !== undefined) {
      acc.failures.push(
        `${label}: settlement for header ${headerHash} also at ${duplicate}`,
      );
    }
    seen.set(headerHash, settlement.outRef);
    const headerLabel = `${label} (header ${headerHash})`;
    const journal = ctx.journals.get(headerHash);
    const foreignRow = ctx.foreign.get(headerHash);
    if (!merged.headers.has(headerHash)) {
      if (ctx.onChainUnmerged.has(headerHash)) {
        acc.failures.push(
          `${headerLabel}: header is still unmerged on the L1 queue`,
        );
        continue;
      }
      if (merged.complete) {
        acc.failures.push(
          `${headerLabel}: header is not on the merged chain (merged walk: ${merged.stopReason})`,
        );
        continue;
      }
      if (journal === undefined && foreignRow === undefined) {
        acc.notes.push(
          `${headerLabel}: header predates locally known history (${merged.stopReason}); roots not comparable`,
        );
        continue;
      }
      acc.notes.push(
        `${headerLabel}: header lies beyond the locally known merged chain (${merged.stopReason}); roots still compared`,
      );
    }
    if (settlement.roots === null) {
      acc.failures.push(
        `${headerLabel}: settlement datum undecodable: ${settlement.decodeError ?? "missing"}`,
      );
      continue;
    }
    if (journal !== undefined) {
      compared += 1;
      acc.failures.push(
        ...rootsDiff(headerLabel, settlement.roots, journal.expected),
      );
    } else if (foreignRow !== undefined) {
      compared += 1;
      acc.failures.push(
        ...rootsDiff(headerLabel, settlement.roots, {
          ...foreignRow.roots,
          ...(foreignRow.transactionsRoot === null
            ? {}
            : { transactions: foreignRow.transactionsRoot }),
        }),
      );
    } else {
      acc.notes.push(
        `${headerLabel}: merged header without a local journal or foreign row; roots not comparable`,
      );
    }
  }
  return finishCheck(
    "settlements",
    acc,
    allowInFlight,
    `${plural(ctx.l1.settlements.length, "L1 settlement")} belong to merged headers; ${plural(compared, "settlement datum", "settlement datums")} equal the local expected event roots`,
  );
};

type LedgerCacheAttempt = {
  readonly label: string;
  readonly missing: readonly string[];
  readonly unexpected: readonly string[];
  readonly mismatched: readonly string[];
  readonly spentAbsent: readonly string[];
};

const attemptLedgerCache = (
  ctx: Context,
  point: LedgerPoint,
  depositByOutref: ReadonlyMap<string, SqlDepositRow>,
  deltas: readonly NonNullable<PendingTxDelta["delta"]>[],
): LedgerCacheAttempt => {
  const chain = new Set(point.chainHeaderHashes);
  const expected = new Map(point.entries);
  const cache = new Map(ctx.sql.mempoolLedger.map((row) => [row.outref, row]));
  // Deposits projected into the cache but not yet part of this ledger point.
  for (const row of ctx.sql.mempoolLedger) {
    if (row.sourceEventId === null || expected.has(row.outref)) continue;
    const deposit = depositByOutref.get(row.outref);
    if (deposit === undefined || deposit.payload.eventId !== row.sourceEventId)
      continue;
    if (deposit.payload.ledgerOutput !== row.output) continue;
    const header = deposit.projectedHeaderHash;
    const beyondPoint =
      header === null ||
      (!chain.has(header) &&
        (ctx.activeHeaders.has(header) ||
          (ctx.onChainUnmerged.has(header) && !isMerged(ctx, header))));
    if (beyondPoint) expected.set(row.outref, row.output);
  }
  for (const delta of deltas) {
    for (const produced of delta.produced)
      expected.set(produced.outref, produced.output);
  }
  const spentAbsent: string[] = [];
  for (const delta of deltas) {
    for (const spent of delta.spent) {
      if (!expected.delete(spent) && !depositByOutref.has(spent)) {
        spentAbsent.push(spent);
      }
    }
  }
  const missing: string[] = [];
  const mismatched: string[] = [];
  for (const [outref, output] of expected) {
    const cached = cache.get(outref);
    if (cached === undefined) missing.push(outref);
    else if (cached.output !== output) mismatched.push(outref);
  }
  const unexpected = [...cache.keys()].filter(
    (outref) => !expected.has(outref),
  );
  return { label: point.label, missing, unexpected, mismatched, spentAbsent };
};

const attemptIsClean = (attempt: LedgerCacheAttempt): boolean =>
  attempt.missing.length === 0 &&
  attempt.unexpected.length === 0 &&
  attempt.mismatched.length === 0 &&
  attempt.spentAbsent.length === 0;

const checkLedgerCache = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.sql.confirmedRootError !== null) {
    return skipCheck("ledger-cache", unencodableConfirmedReason(ctx));
  }
  const rejected = ctx.sql.pendingTxs.filter((tx) => tx.delta === null);
  if (rejected.length > 0) {
    return skipCheck(
      "ledger-cache",
      `${plural(rejected.length, "pending transaction")} has no stored delta and cannot be decoded (first ${rejected[0]!.txId}: ${rejected[0]!.rejectDetail ?? "decode failed"}); the commit stage will reject it, so the expected cache is undefined until then`,
    );
  }
  const points: LedgerPoint[] = [];
  const tip = ctx.sql.finalizedTip;
  if (tip.kind === "materialized") points.push(tip.point);
  const active = ctx.sql.activeTip;
  const activeStatus =
    active?.kind === "materialized" && active.point.headerHash !== null
      ? ctx.journals.get(active.point.headerHash)?.status
      : undefined;
  if (
    active?.kind === "materialized" &&
    activeStatus !== undefined &&
    LOCALLY_FINALIZED_ACTIVE_STATUSES.has(activeStatus)
  ) {
    points.push(active.point);
  }
  if (points.length === 0) {
    if (tip.kind === "failed" && !tip.parentMissing) {
      const acc = newAccumulator();
      acc.failures.push(`committed tip not recomputable: ${tip.reason}`);
      return finishCheck("ledger-cache", acc, allowInFlight, "");
    }
    return skipCheck("ledger-cache", unrecomputableReason(ctx, tip));
  }
  const depositByOutref = new Map(
    ctx.sql.deposits
      .filter((row) => row.ledgerOutref !== null)
      .map((row) => [row.ledgerOutref!, row]),
  );
  const deltas = ctx.sql.pendingTxs.map((tx) => tx.delta!);
  const attempts = points.map((point) =>
    attemptLedgerCache(ctx, point, depositByOutref, deltas),
  );
  const clean = attempts.find(attemptIsClean);
  const acc = newAccumulator();
  if (clean === undefined) {
    const best = attempts.at(-1)!;
    const describe = (kind: string, list: readonly string[]) =>
      list.length === 0
        ? []
        : [`${plural(list.length, kind)} (first ${list[0]!})`];
    acc.failures.push(
      `mempool_ledger differs from the recomputed ledger at ${best.label} plus pending effects: ${[
        ...describe("missing outref", best.missing),
        ...describe("unexpected outref", best.unexpected),
        ...describe("mismatched output", best.mismatched),
        ...describe("pending spend of an absent outref", best.spentAbsent),
      ].join("; ")}`,
    );
  } else if (clean.label !== points[0]!.label) {
    acc.notes.push(
      `matched at ${clean.label} (locally finalized active journal)`,
    );
  }
  return finishCheck(
    "ledger-cache",
    acc,
    allowInFlight,
    `mempool_ledger (${plural(ctx.sql.mempoolLedger.length, "row")}) equals the ledger at ${clean?.label ?? "?"} plus ${plural(ctx.sql.pendingTxs.length, "pending transaction")}`,
  );
};

// ---------------------------------------------------------------------------
// Evaluation entry point (pure)
// ---------------------------------------------------------------------------

const pointLabel = (result: LedgerPointResult | null): string =>
  result === null
    ? "none"
    : result.kind === "materialized"
      ? `${result.point.label} root=${result.point.root}`
      : `${result.label} (not recomputable: ${result.reason})`;

export const evaluateStateReconciliation = (
  input: StateReconciliationInput,
): ReconciliationReport => {
  const ctx = buildContext(input);
  const allow = input.allowInFlight;
  const checks: ReconciliationCheck[] = [
    checkConfirmedRoot(ctx, allow),
    checkNativeRoot(ctx, input.native, allow),
    checkStateQueueJournal(ctx, allow),
    checkTailRoot(ctx, input.native, allow),
    checkDeposits(ctx, allow),
    checkWithdrawals(ctx, allow),
    checkPayouts(ctx, allow),
    checkSettlements(ctx, allow),
    checkLedgerCache(ctx, allow),
  ];
  const summary = {
    pass: checks.filter((c) => c.status === "PASS").length,
    fail: checks.filter((c) => c.status === "FAIL").length,
    skipped: checks.filter((c) => c.status === "SKIPPED").length,
  };
  const ok = summary.fail === 0;
  return {
    ok,
    exitCode: ok ? 0 : 1,
    allowInFlight: allow,
    snapshot: {
      attempts: input.attempts ?? 1,
      l1:
        input.l1.kind === "observed"
          ? `observed (confirmed header ${input.l1.view.confirmed.headerHash}, ${plural(input.l1.view.unmerged.length, "unmerged header")})`
          : `unavailable: ${input.l1.reason}`,
      nativeRoot:
        input.native.kind === "observed"
          ? `${input.native.root} (${input.native.source})`
          : `unavailable: ${input.native.reason}`,
      sqlConfirmedRoot: input.sql.confirmedRoot,
      finalizedTip: pointLabel(input.sql.finalizedTip),
      activeJournal: pointLabel(input.sql.activeTip),
    },
    summary,
    checks,
  };
};

export const formatStateReconciliationReport = (
  report: ReconciliationReport,
): string => {
  const lines: string[] = [
    "Midgard state reconciliation (read-only)",
    `  snapshot attempts : ${report.snapshot.attempts.toString()}`,
    `  L1                : ${report.snapshot.l1}`,
    `  native root       : ${report.snapshot.nativeRoot}`,
    `  SQL confirmed root: ${report.snapshot.sqlConfirmedRoot}`,
    `  committed tip     : ${report.snapshot.finalizedTip}`,
    `  active journal    : ${report.snapshot.activeJournal}`,
    "",
  ];
  for (const check of report.checks) {
    lines.push(`[${check.status}] ${check.id}: ${check.reason}`);
    lines.push(`         compares: ${check.compares}`);
    for (const failure of check.failures)
      lines.push(`         FAIL: ${failure}`);
    for (const item of check.inFlight)
      lines.push(`         IN-FLIGHT: ${item}`);
    for (const note of check.notes) lines.push(`         note: ${note}`);
  }
  lines.push("");
  lines.push(
    `Result: ${report.summary.pass.toString()} PASS, ${report.summary.fail.toString()} FAIL, ${report.summary.skipped.toString()} SKIPPED -> ${report.ok ? "consistent" : "INCONSISTENT"} (exit ${report.exitCode.toString()})`,
  );
  return lines.join("\n");
};

// ---------------------------------------------------------------------------
// L1 collection
// ---------------------------------------------------------------------------

const outRefOf = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const policyTokens = (utxo: UTxO, policyId: string) =>
  Object.entries(utxo.assets)
    .filter(([unit]) => unit !== "lovelace" && unit.startsWith(policyId))
    .map(([unit, quantity]) => ({
      assetName: unit.slice(policyId.length),
      quantity: quantity.toString(),
    }));

const depositPayloadOf = (entry: DepositsDB.Entry): DepositPayload => ({
  eventId: toHex(entry[DepositsDB.Columns.ID]),
  info: toHex(entry[DepositsDB.Columns.INFO]),
  inclusionTimeMs: new Date(entry[DepositsDB.Columns.INCLUSION_TIME]).getTime(),
  l1TxHash: toHex(entry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH]),
  ledgerTxId: toHex(entry[DepositsDB.Columns.LEDGER_TX_ID]),
  ledgerOutput: toHex(entry[DepositsDB.Columns.LEDGER_OUTPUT]),
  ledgerAddress: String(entry[DepositsDB.Columns.LEDGER_ADDRESS]),
});

const withdrawalPayloadOf = (
  entry: WithdrawalsDB.Entry,
): WithdrawalPayload => ({
  eventId: toHex(entry[WithdrawalsDB.Columns.ID]),
  rawEventInfo: toHex(entry[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
  inclusionTimeMs: new Date(
    entry[WithdrawalsDB.Columns.INCLUSION_TIME],
  ).getTime(),
  l1TxHash: toHex(entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]),
  l1OutputIndex: Number(
    entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX],
  ),
  assetName: toHex(entry[WithdrawalsDB.Columns.ASSET_NAME]),
  l2Outref: toHex(entry[WithdrawalsDB.Columns.L2_OUTREF]),
  l2Owner: toHex(entry[WithdrawalsDB.Columns.L2_OWNER]),
  l2Value: toHex(entry[WithdrawalsDB.Columns.L2_VALUE]),
  l1Address: toHex(entry[WithdrawalsDB.Columns.L1_ADDRESS]),
  l1Datum: toHex(entry[WithdrawalsDB.Columns.L1_DATUM]),
  refundAddress: toHex(entry[WithdrawalsDB.Columns.REFUND_ADDRESS]),
  refundDatum: toHex(entry[WithdrawalsDB.Columns.REFUND_DATUM]),
});

const decodeQueueHeader = (node: SDK.StateQueueUTxO) =>
  Effect.gen(function* () {
    const headerHash = (yield* SDK.headerHashFromStateQueueUTxO(
      node,
    )).toLowerCase();
    const decoded = yield* Effect.either(
      Effect.gen(function* () {
        const header = yield* SDK.getHeaderFromStateQueueDatum(node.datum);
        const recomputed = yield* SDK.hashBlockHeader(header);
        return { header, recomputed };
      }),
    );
    if (Either.isLeft(decoded)) {
      return {
        outRef: outRefOf(node.utxo),
        headerHash,
        recomputedHeaderHash: null,
        prevHeaderHash: null,
        endTimeMs: null,
        roots: null,
        decodeError: describeError(decoded.left),
      } satisfies L1QueueHeader;
    }
    const { header, recomputed } = decoded.right;
    return {
      outRef: outRefOf(node.utxo),
      headerHash,
      recomputedHeaderHash: recomputed,
      prevHeaderHash: header.prevHeaderHash,
      endTimeMs: Number(header.endTime),
      roots: {
        utxos: header.utxosRoot,
        deposits: header.depositsRoot,
        withdrawals: header.withdrawalsRoot,
        forcedTransactions: header.forcedTransactionsRoot,
        transactions: header.transactionsRoot,
      },
      decodeError: null,
    } satisfies L1QueueHeader;
  });

const lucidPromise = <A>(message: string, run: () => Promise<A>) =>
  Effect.tryPromise({
    try: run,
    catch: (cause) => new SDK.LucidError({ message, cause }),
  });

/** One read of every L1 collection the checks compare. */
export const collectL1StateView: Effect.Effect<
  L1StateView,
  unknown,
  Lucid | MidgardContracts | NodeConfig
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  const queue = yield* SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  });
  const rootNode = queue[0];
  if (rootNode === undefined || rootNode.datum.key !== "Empty") {
    return yield* Effect.fail(
      new Error("L1 state queue has no confirmed-state root node"),
    );
  }
  const confirmed = yield* SDK.getConfirmedStateFromStateQueueDatum(
    rootNode.datum,
  );
  const unmerged = yield* Effect.forEach(queue.slice(1), decodeQueueHeader, {
    concurrency: 1,
  });
  const eventHistory = SDK.requireEventHistoryContracts(contracts);
  const depositUtxos = yield* SDK.fetchDepositUTxOsProgram(lucid, {
    ...SDK.eventHistoryDeploymentFromContracts(eventHistory.deposit),
  });
  const deposits = yield* Effect.forEach(depositUtxos, (utxo) =>
    Effect.either(depositUTxOToEntry(utxo, nodeConfig.NETWORK)).pipe(
      Effect.map(
        (result): L1EventOrder<DepositPayload> => ({
          outRef: outRefOf(utxo.utxo),
          payload: Either.isRight(result)
            ? depositPayloadOf(result.right)
            : null,
          decodeError: Either.isLeft(result)
            ? describeError(result.left)
            : null,
        }),
      ),
    ),
  );
  const withdrawalUtxos = yield* SDK.fetchWithdrawalUTxOsProgram(lucid, {
    ...SDK.eventHistoryDeploymentFromContracts(eventHistory.withdrawal),
  });
  const withdrawals = yield* Effect.forEach(withdrawalUtxos, (utxo) =>
    Effect.either(withdrawalUTxOToEntry(utxo)).pipe(
      Effect.map(
        (result): L1EventOrder<WithdrawalPayload> => ({
          outRef: outRefOf(utxo.utxo),
          payload: Either.isRight(result)
            ? withdrawalPayloadOf(result.right)
            : null,
          decodeError: Either.isLeft(result)
            ? describeError(result.left)
            : null,
        }),
      ),
    ),
  );
  const payoutUtxos = yield* lucidPromise("Failed to fetch payout UTxOs", () =>
    lucid.utxosAt(contracts.payout.spendingScriptAddress),
  );
  const payouts = payoutUtxos
    .map((utxo): L1Payout | null => {
      const tokens = policyTokens(utxo, contracts.payout.policyId);
      if (tokens.length === 0) return null;
      try {
        if (utxo.datum == null)
          throw new Error("payout UTxO has no inline datum");
        const datum = LucidData.from(
          utxo.datum,
          SDK.PayoutDatum,
        ) as SDK.PayoutDatum;
        return {
          outRef: outRefOf(utxo),
          tokens,
          l2Value: valueToAssets(datum.l2_value),
          l1AddressCbor: LucidData.to(datum.l1_address, SDK.AddressData),
          l1DatumCbor: LucidData.to(datum.l1_datum, SDK.CardanoDatum),
          decodeError: null,
        };
      } catch (error) {
        return {
          outRef: outRefOf(utxo),
          tokens,
          l2Value: null,
          l1AddressCbor: null,
          l1DatumCbor: null,
          decodeError: describeError(error),
        };
      }
    })
    .filter((payout): payout is L1Payout => payout !== null);
  const settlementUtxos = yield* lucidPromise(
    "Failed to fetch settlement UTxOs",
    () => lucid.utxosAt(contracts.settlement.spendingScriptAddress),
  );
  const settlements = settlementUtxos
    .map((utxo): L1Settlement | null => {
      const tokens = policyTokens(utxo, contracts.settlement.policyId);
      if (tokens.length === 0) return null;
      try {
        if (utxo.datum == null)
          throw new Error("settlement UTxO has no inline datum");
        const datum = LucidData.from(
          utxo.datum,
          SDK.SettlementDatum,
        ) as SDK.SettlementDatum;
        return {
          outRef: outRefOf(utxo),
          tokens,
          roots: {
            deposits: datum.deposits_root,
            withdrawals: datum.withdrawals_root,
            forcedTransactions: datum.forced_transactions_root,
            transactions: datum.transactions_root,
          },
          decodeError: null,
        };
      } catch (error) {
        return {
          outRef: outRefOf(utxo),
          tokens,
          roots: null,
          decodeError: describeError(error),
        };
      }
    })
    .filter((settlement): settlement is L1Settlement => settlement !== null);
  return {
    confirmed: {
      outRef: outRefOf(rootNode.utxo),
      headerHash: confirmed.data.headerHash,
      utxoRoot: confirmed.data.utxoRoot,
      endTimeMs: Number(confirmed.data.endTime),
    },
    unmerged,
    deposits,
    withdrawals,
    payouts,
    settlements,
  };
});

/**
 * Identity of an L1 read: every UTxO position plus the confirmed header. Two
 * reads with equal fingerprints observed the same L1 state for the checks.
 */
export const l1Fingerprint = (view: L1StateView): string =>
  JSON.stringify([
    view.confirmed.outRef,
    view.confirmed.headerHash,
    view.unmerged.map((h) => h.outRef),
    view.deposits.map((d) => d.outRef).sort(),
    view.withdrawals.map((w) => w.outRef).sort(),
    view.payouts.map((p) => p.outRef).sort(),
    view.settlements.map((s) => s.outRef).sort(),
  ]);

// ---------------------------------------------------------------------------
// SQL snapshot collection
// ---------------------------------------------------------------------------

type JournalRow = {
  readonly header_hash: Buffer;
  readonly status: string;
  readonly base_tail_header_hash: Buffer;
  readonly base_utxos_root: string;
  readonly expected_utxos_root: string;
  readonly expected_deposits_root: string;
  readonly expected_withdrawals_root: string;
  readonly expected_forced_transactions_root: string;
  readonly expected_transactions_root: string;
  readonly correction_transition_digest: string | null;
  readonly submitted_tx_hash: Buffer | string | null;
  readonly header_cbor: Buffer | null;
};

const headerEndTimeMs = (cbor: Buffer | null): number | null => {
  if (cbor === null || cbor.length === 0) return null;
  try {
    const header = LucidData.from(toHex(cbor), SDK.Header) as SDK.Header;
    return Number(header.endTime);
  } catch {
    return null;
  }
};

type ForeignRow = {
  readonly foreign_header_hash: Buffer;
  readonly status: string;
  readonly foreign_header_cbor: Buffer;
  readonly deposits_root: string;
  readonly withdrawals_root: string;
  readonly forced_transactions_root: string;
};

const entriesMap = (entries: readonly Ledger.Entry[]): Map<string, string> =>
  new Map(
    entries.map((entry) => [
      toHex(entry[Ledger.Columns.OUTREF]),
      toHex(entry[Ledger.Columns.OUTPUT]),
    ]),
  );

const chainFrom = (
  headerHash: string,
  confirmedRoot: string,
  journals: ReadonlyMap<string, JournalSummary>,
): string[] => {
  const chain: string[] = [];
  let current = journals.get(headerHash);
  while (current !== undefined && chain.length <= journals.size) {
    chain.push(current.headerHash);
    if (current.baseUtxosRoot === confirmedRoot) break;
    current = journals.get(current.baseTailHeaderHash);
  }
  return chain.reverse();
};

const materializePoint = (
  label: string,
  headerHash: string,
  confirmedEntries: readonly Ledger.Entry[],
  confirmedRoot: string,
  journals: ReadonlyMap<string, JournalSummary>,
): Effect.Effect<LedgerPointResult, never, Database> =>
  Effect.gen(function* () {
    const journal = journals.get(headerHash);
    if (journal !== undefined && journal.expected.utxos === confirmedRoot) {
      return {
        kind: "materialized",
        point: {
          label: `${label} (equals the confirmed ledger)`,
          headerHash,
          root: confirmedRoot,
          entries: entriesMap(confirmedEntries),
          chainHeaderHashes: [],
        },
      } satisfies LedgerPointResult;
    }
    const record = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
      Buffer.from(headerHash, "hex"),
    ).pipe(Effect.either);
    if (Either.isLeft(record) || Option.isNone(record.right)) {
      return {
        kind: "failed",
        label,
        headerHash,
        reason: Either.isLeft(record)
          ? describeError(record.left)
          : "journal row vanished",
        parentMissing: false,
      } satisfies LedgerPointResult;
    }
    const snapshot = yield* Effect.either(
      materializeConfirmedLedgerDeltaChain({
        record: record.right.value,
        confirmedEntries,
        retrieveParent: (parent) =>
          PendingBlockFinalizationsDB.retrieveByHeaderHash(parent),
      }),
    );
    if (Either.isLeft(snapshot)) {
      const reason = describeError(snapshot.left);
      return {
        kind: "failed",
        label,
        headerHash,
        reason,
        parentMissing: reason.includes("parent journal is missing"),
      } satisfies LedgerPointResult;
    }
    return {
      kind: "materialized",
      point: {
        label,
        headerHash,
        root: snapshot.right.root,
        entries: entriesMap(snapshot.right.entries),
        chainHeaderHashes: chainFrom(headerHash, confirmedRoot, journals),
      },
    } satisfies LedgerPointResult;
  });

const decodeObserverState = (
  raw: unknown,
  policyIdHex: string,
): ObserverSnapshot => {
  const value = typeof raw === "string" ? (JSON.parse(raw) as unknown) : raw;
  if (typeof value !== "object" || value === null) {
    return { kind: "invalid", reason: "state_record is not an object" };
  }
  const record = value as {
    stateQueuePolicyId?: unknown;
    admitted?: unknown;
    pending?: unknown;
  };
  if (record.stateQueuePolicyId !== policyIdHex) {
    return {
      kind: "invalid",
      reason: "state_record names a different state-queue policy",
    };
  }
  if (!Array.isArray(record.admitted) || !Array.isArray(record.pending)) {
    return {
      kind: "invalid",
      reason: "state_record lacks admitted/pending transition lists",
    };
  }
  const admitted: ObserverTransition[] = [];
  for (const candidate of record.admitted as unknown[]) {
    const transition =
      candidate as Partial<SDK.StateQueueAuthenticatedTransition>;
    if (
      typeof transition.transactionHash !== "string" ||
      typeof transition.transitionKind !== "string" ||
      typeof transition.transitionDigest !== "string" ||
      !Array.isArray(transition.removedHeaderHashes)
    ) {
      return { kind: "invalid", reason: "admitted transition is malformed" };
    }
    admitted.push({
      transactionHash: transition.transactionHash,
      transitionKind: transition.transitionKind,
      transitionDigest: transition.transitionDigest,
      removedHeaderHashes: transition.removedHeaderHashes.map(String),
    });
  }
  return { kind: "present", admitted, pendingCount: record.pending.length };
};

/**
 * Reads every SQL table the checks compare inside one repeatable-read,
 * read-only transaction, and recomputes the committed-tip and active-journal
 * ledgers with the node's own delta-chain materializer.
 *
 * `committedTipHeaderHash` selects the committed tip: the last L1 queue header
 * with a finalized journal (null means the confirmed ledger itself).
 */
export const collectSqlStateSnapshot = ({
  committedTipHeaderHash,
  stateQueuePolicyId,
}: {
  readonly committedTipHeaderHash: (
    journals: ReadonlyMap<string, JournalSummary>,
  ) => string | null;
  readonly stateQueuePolicyId: string;
}): Effect.Effect<SqlStateSnapshot, unknown, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY`;
        const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
        // Pure computation: a failure here cannot abort the transaction.
        const confirmedRootResult = yield* Effect.either(
          computeLedgerMpfRootFromLedgerEntries(confirmedEntries),
        );
        const confirmedRootError = Either.isLeft(confirmedRootResult)
          ? describeError(confirmedRootResult.left)
          : null;
        const confirmedRoot = Either.isRight(confirmedRootResult)
          ? confirmedRootResult.right
          : "<unencodable>";
        const journalRows = yield* sql<JournalRow>`SELECT
            header_hash, status, base_tail_header_hash, base_utxos_root,
            expected_utxos_root, expected_deposits_root, expected_withdrawals_root,
            expected_forced_transactions_root, expected_transactions_root,
            correction_transition_digest, submitted_tx_hash, header_cbor
          FROM ${sql(PendingBlockFinalizationsDB.tableName)}
          ORDER BY created_at ASC, header_hash ASC`;
        const journals: JournalSummary[] = journalRows.map((row) => ({
          headerHash: toHex(row.header_hash),
          status: row.status,
          baseTailHeaderHash: toHex(row.base_tail_header_hash),
          baseUtxosRoot: row.base_utxos_root,
          expected: {
            utxos: row.expected_utxos_root,
            deposits: row.expected_deposits_root,
            withdrawals: row.expected_withdrawals_root,
            forcedTransactions: row.expected_forced_transactions_root,
            transactions: row.expected_transactions_root,
          },
          correctionTransitionDigest: row.correction_transition_digest,
          submittedTxHash: toHexOrNull(row.submitted_tx_hash),
          endTimeMs: headerEndTimeMs(row.header_cbor),
        }));
        const journalsByHash = new Map(journals.map((j) => [j.headerHash, j]));
        const tipHash = committedTipHeaderHash(journalsByHash);
        const unencodable = (
          label: string,
          headerHash: string,
        ): LedgerPointResult => ({
          kind: "failed",
          label,
          headerHash,
          reason: `SQL confirmed_ledger cannot be encoded: ${confirmedRootError ?? ""}`,
          parentMissing: false,
        });
        const finalizedTip: LedgerPointResult =
          confirmedRootError !== null
            ? unencodable(
                `committed tip ${tipHash ?? "confirmed"}`,
                tipHash ?? "",
              )
            : tipHash === null
              ? {
                  kind: "materialized",
                  point: {
                    label: "confirmed ledger (no finalized unmerged block)",
                    headerHash: null,
                    root: confirmedRoot,
                    entries: entriesMap(confirmedEntries),
                    chainHeaderHashes: [],
                  },
                }
              : yield* materializePoint(
                  `committed tip ${tipHash}`,
                  tipHash,
                  confirmedEntries,
                  confirmedRoot,
                  journalsByHash,
                );
        const activeHeaderHashes = journals
          .filter((j) => ACTIVE_JOURNAL_STATUSES.has(j.status))
          .map((j) => j.headerHash);
        const activeHash = activeHeaderHashes[0] ?? null;
        const activeTip =
          activeHash === null
            ? null
            : confirmedRootError !== null
              ? unencodable(`active journal ${activeHash}`, activeHash)
              : yield* materializePoint(
                  `active journal ${activeHash} (${journalsByHash.get(activeHash)?.status ?? "?"})`,
                  activeHash,
                  confirmedEntries,
                  confirmedRoot,
                  journalsByHash,
                );

        const depositEntries = yield* DepositsDB.retrieveAllEntries();
        const deposits = yield* Effect.forEach(depositEntries, (entry) =>
          Effect.either(DepositsDB.toLedgerEntry(entry)).pipe(
            Effect.map(
              (ledger): SqlDepositRow => ({
                payload: depositPayloadOf(entry),
                status: entry[DepositsDB.Columns.STATUS],
                projectedHeaderHash: toHexOrNull(
                  entry[DepositsDB.Columns.PROJECTED_HEADER_HASH],
                ),
                ledgerOutref: Either.isRight(ledger)
                  ? toHex(ledger.right[Ledger.Columns.OUTREF])
                  : null,
              }),
            ),
          ),
        );
        const withdrawalEntries = yield* WithdrawalsDB.retrieveAllEntries();
        const withdrawals = withdrawalEntries.map(
          (entry): SqlWithdrawalRow => ({
            payload: withdrawalPayloadOf(entry),
            status: entry[WithdrawalsDB.Columns.STATUS],
            validity: entry[WithdrawalsDB.Columns.VALIDITY],
            projectedHeaderHash: toHexOrNull(
              entry[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH],
            ),
          }),
        );
        const mempoolLedgerRows = yield* MempoolLedgerDB.retrieve;
        const mempoolLedger = mempoolLedgerRows.map((row) => ({
          outref: toHex(row[MempoolLedgerDB.Columns.OUTREF]),
          output: toHex(row[MempoolLedgerDB.Columns.OUTPUT]),
          sourceEventId: toHexOrNull(
            row[MempoolLedgerDB.Columns.SOURCE_EVENT_ID],
          ),
        }));
        const mempoolTxs = yield* Tx.retrieveAllEntries(MempoolDB.tableName);
        const processedTxs = yield* ProcessedMempoolDB.retrieve;
        const allPending = [
          ...mempoolTxs.map((entry) => ({ entry, source: "mempool" as const })),
          ...processedTxs.map((entry) => ({
            entry,
            source: "processed_mempool" as const,
          })),
        ];
        const storedDeltas = yield* MempoolTxDeltasDB.retrieveByTxIds(
          allPending.map(({ entry }) => entry[Tx.Columns.TX_ID]),
        );
        const pendingTxs = yield* Effect.forEach(
          allPending,
          ({ entry, source }) =>
            resolveTxDeltaForCommit(
              entry,
              storedDeltas.get(toHex(entry[Tx.Columns.TX_ID])),
            ).pipe(
              Effect.map(
                (resolved): PendingTxDelta => ({
                  txId: toHex(entry[Tx.Columns.TX_ID]),
                  source,
                  delta:
                    resolved._tag === "Decoded"
                      ? {
                          spent: resolved.spent.map(toHex),
                          produced: resolved.produced.map((p) => ({
                            outref: toHex(p[Ledger.Columns.OUTREF]),
                            output: toHex(p[Ledger.Columns.OUTPUT]),
                          })),
                        }
                      : null,
                  rejectDetail:
                    resolved._tag === "Decoded"
                      ? null
                      : redactSensitive(
                          String(
                            resolved.rejection.reject_detail ?? "decode failed",
                          ),
                        ),
                }),
              ),
            ),
        );
        const blockRows = yield* sql<{ readonly header_hash: Buffer }>`
          SELECT DISTINCT header_hash FROM blocks`;
        const foreignRows = yield* sql<ForeignRow>`SELECT
            foreign_header_hash, status, foreign_header_cbor,
            deposits_root, withdrawals_root, forced_transactions_root
          FROM foreign_tip_reconciliations`;
        const foreign = foreignRows.map((row): ForeignSummary => {
          let header: SDK.Header | null = null;
          try {
            header = LucidData.from(
              toHex(row.foreign_header_cbor),
              SDK.Header,
            ) as SDK.Header;
          } catch {
            header = null;
          }
          return {
            headerHash: toHex(row.foreign_header_hash),
            status: row.status,
            prevHeaderHash: header?.prevHeaderHash ?? null,
            roots: {
              deposits: row.deposits_root,
              withdrawals: row.withdrawals_root,
              forcedTransactions: row.forced_transactions_root,
            },
            transactionsRoot: header?.transactionsRoot ?? null,
            utxosRoot: header?.utxosRoot ?? null,
          };
        });
        const observerRows = yield* sql<{ readonly state_record: unknown }>`
          SELECT state_record FROM state_queue_terminal_observer_states
          WHERE state_queue_policy_id = ${Buffer.from(stateQueuePolicyId, "hex")}`;
        let observer: ObserverSnapshot;
        if (observerRows.length === 0) {
          observer = { kind: "absent" };
        } else if (observerRows.length > 1) {
          observer = {
            kind: "invalid",
            reason: "more than one observer state row for this policy",
          };
        } else {
          try {
            observer = decodeObserverState(
              observerRows[0]!.state_record,
              stateQueuePolicyId,
            );
          } catch (error) {
            observer = { kind: "invalid", reason: describeError(error) };
          }
        }
        return {
          confirmedRoot,
          confirmedRootError,
          confirmedEntryCount: confirmedEntries.length,
          journals,
          activeHeaderHashes,
          finalizedTip,
          activeTip,
          deposits,
          withdrawals,
          mempoolLedger,
          pendingTxs,
          blockHeaderHashes: blockRows.map((row) => toHex(row.header_hash)),
          foreign,
          observer,
        } satisfies SqlStateSnapshot;
      }),
    );
  });

/**
 * Committed tip given an L1 view: the last unmerged queue header whose journal
 * is finalized. Without L1, fall back to the unique finalized journal no other
 * finalized journal builds on (null if none or ambiguous).
 */
export const committedTipSelector =
  (l1: L1StateView | null) =>
  (journals: ReadonlyMap<string, JournalSummary>): string | null => {
    if (l1 !== null) {
      for (const header of [...l1.unmerged].reverse()) {
        if (
          journals.get(header.headerHash)?.status === JOURNAL_STATUS.Finalized
        ) {
          return header.headerHash;
        }
      }
      return null;
    }
    const finalized = [...journals.values()].filter(
      (j) => j.status === JOURNAL_STATUS.Finalized,
    );
    const referenced = new Set(finalized.map((j) => j.baseTailHeaderHash));
    const leaves = finalized.filter((j) => !referenced.has(j.headerHash));
    return leaves.length === 1 ? leaves[0]!.headerHash : null;
  };

// ---------------------------------------------------------------------------
// Native root observation
// ---------------------------------------------------------------------------

const HEX_32 = /^[0-9a-f]{64}$/u;

/** Reads the Architecture-G owner's durable root from node readiness. */
export const readNativeRootFromReadiness = async (
  nodeUrl: string,
  timeoutMs = 5_000,
): Promise<NativeRootObservation> => {
  let response: Response;
  try {
    response = await fetch(new URL("/readiness", nodeUrl), {
      signal: AbortSignal.timeout(timeoutMs),
    });
  } catch (error) {
    return {
      kind: "unavailable",
      reason: `node readiness endpoint unreachable (${describeError(error)})`,
    };
  }
  if (response.status !== 200 && response.status !== 503) {
    return {
      kind: "unavailable",
      reason: `node readiness endpoint answered HTTP ${response.status.toString()}`,
    };
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    return {
      kind: "unavailable",
      reason: "node readiness response is not JSON",
    };
  }
  const owner =
    typeof body === "object" && body !== null
      ? (body as { nativeMpfOwner?: unknown }).nativeMpfOwner
      : undefined;
  if (owner === null || owner === undefined || typeof owner !== "object") {
    return {
      kind: "unavailable",
      reason: "node readiness carries no native MPF owner diagnostics",
    };
  }
  const { healthy, durableRoot } = owner as {
    healthy?: unknown;
    durableRoot?: unknown;
  };
  if (healthy !== true) {
    return {
      kind: "unavailable",
      reason: "node reports its native MPF owner unhealthy",
    };
  }
  if (typeof durableRoot !== "string" || !HEX_32.test(durableRoot)) {
    return {
      kind: "unavailable",
      reason: "node readiness durableRoot is malformed",
    };
  }
  return { kind: "observed", root: durableRoot, source: "node-readiness" };
};

/**
 * Reads the persisted `__root__` marker from a private copy of the LevelDB
 * directory. The live store is never opened (LevelDB holds an exclusive lock
 * and opening can write), so this is safe beside a running node; a copy taken
 * mid-write may be stale, which the before/after comparison catches.
 */
export const readNativeRootFromLevelCopy = async (
  levelPath: string,
): Promise<NativeRootObservation> => {
  try {
    const info = await stat(levelPath);
    if (!info.isDirectory()) {
      return {
        kind: "unavailable",
        reason: "LEDGER_MPF_DB_PATH is not a directory",
      };
    }
  } catch {
    return {
      kind: "unavailable",
      reason: "no MPF LevelDB exists at LEDGER_MPF_DB_PATH",
    };
  }
  const copy = await mkdtemp(join(tmpdir(), "midgard-state-reconcile-"));
  try {
    await cp(levelPath, copy, {
      recursive: true,
      filter: (source) => basename(source) !== "LOCK",
    });
    const db = new Level<string, unknown>(copy, {
      valueEncoding: "json",
      createIfMissing: false,
    });
    await db.open();
    try {
      const marker = await db.get(ROOT_KEY);
      return {
        kind: "observed",
        root: parseStoredRootHex(marker).toString("hex"),
        source: "leveldb-copy",
      };
    } finally {
      await db.close();
    }
  } catch (error) {
    return {
      kind: "unavailable",
      reason: `MPF LevelDB copy unreadable (${describeError(error)})`,
    };
  } finally {
    await rm(copy, { recursive: true, force: true });
  }
};

export type NativeRootSourceOptions = {
  readonly nodeUrl?: string;
};

export const readNativeRoot = (
  options: NativeRootSourceOptions,
): Effect.Effect<NativeRootObservation, never, NodeConfig> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    if (config.MPF_ENGINE === "architecture_g") {
      const url =
        options.nodeUrl ?? `http://127.0.0.1:${config.PORT.toString()}`;
      const fromReadiness = yield* Effect.promise(() =>
        readNativeRootFromReadiness(url),
      );
      if (fromReadiness.kind === "observed") return fromReadiness;
      const fromCopy = yield* Effect.promise(() =>
        readNativeRootFromLevelCopy(config.LEDGER_MPF_DB_PATH),
      );
      return fromCopy.kind === "observed"
        ? fromCopy
        : {
            kind: "unavailable",
            reason: `${fromReadiness.reason}; ${fromCopy.reason}`,
          };
    }
    return yield* Effect.promise(() =>
      readNativeRootFromLevelCopy(config.LEDGER_MPF_DB_PATH),
    );
  });

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

export type StateReconciliationOptions = NativeRootSourceOptions & {
  readonly allowInFlight?: boolean;
  readonly maxAttempts?: number;
};

const nativeKey = (native: NativeRootObservation): string =>
  native.kind === "observed"
    ? `${native.source}:${native.root}`
    : "unavailable";

/**
 * Collects L1, native root and SQL, re-reads L1 and the native root after the
 * SQL snapshot, and retries until both were stable across it. Returns the
 * evaluated report; never writes.
 */
export const stateReconciliationProgram = (
  options: StateReconciliationOptions = {},
): Effect.Effect<
  ReconciliationReport,
  unknown,
  Database | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts ?? 3));
    let last:
      | {
          readonly l1: L1Observation;
          readonly native: NativeRootObservation;
          readonly sql: SqlStateSnapshot;
        }
      | undefined;
    let attempts = 0;
    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      attempts = attempt;
      const l1Before = yield* Effect.either(collectL1StateView);
      const nativeBefore = yield* readNativeRoot(options);
      const sql = yield* collectSqlStateSnapshot({
        committedTipHeaderHash: committedTipSelector(
          Either.isRight(l1Before) ? l1Before.right : null,
        ),
        stateQueuePolicyId: contracts.stateQueue.policyId,
      });
      const l1After = yield* Effect.either(collectL1StateView);
      const nativeAfter = yield* readNativeRoot(options);
      const nativeStable = nativeKey(nativeBefore) === nativeKey(nativeAfter);
      const native: NativeRootObservation = nativeStable
        ? nativeAfter
        : {
            kind: "unavailable",
            reason: `native root changed while SQL was read, in each of ${attempt.toString()} snapshot attempts`,
          };
      let l1: L1Observation;
      let l1Stable = false;
      if (Either.isLeft(l1Before)) {
        l1 = {
          kind: "unavailable",
          reason: `L1 read failed: ${describeError(l1Before.left)}`,
        };
      } else if (Either.isLeft(l1After)) {
        l1 = {
          kind: "unavailable",
          reason: `L1 read failed: ${describeError(l1After.left)}`,
        };
      } else if (
        l1Fingerprint(l1Before.right) !== l1Fingerprint(l1After.right)
      ) {
        l1 = {
          kind: "unavailable",
          reason: `L1 state changed while SQL was read, in each of ${attempt.toString()} snapshot attempts; L1 comparisons need a quiescent window`,
        };
      } else {
        l1 = { kind: "observed", view: l1After.right };
        l1Stable = true;
      }
      last = { l1, native, sql };
      if (l1Stable && nativeStable) break;
    }
    if (last === undefined) {
      return yield* Effect.fail(
        new Error("state reconciliation took no snapshot"),
      );
    }
    const final = last;
    const { l1, native } = final;
    return evaluateStateReconciliation({
      l1,
      sql: final.sql,
      native,
      allowInFlight: options.allowInFlight === true,
      attempts,
    });
  });
