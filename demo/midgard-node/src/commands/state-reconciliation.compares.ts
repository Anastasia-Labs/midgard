import * as SDK from "@al-ft/midgard-sdk";
import { type Assets } from "@lucid-evolution/lucid";

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
  "da-attestation",
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

export const COMPARES: Record<CheckId, string> = {
  "confirmed-root":
    "L1 state-queue root node ConfirmedState.utxoRoot (provider) vs the MPF root recomputed from SQL confirmed_ledger",
  "native-root":
    "persisted native ledger root (Architecture-G owner durableRoot from node /readyz, else, when no --node-url was given and the node gave no owner answer, the LEDGER_MPF_DB_PATH __root__ marker read from a private copy) vs the root recomputed from SQL confirmed_ledger plus the finalized-but-unmerged pending_block_finalizations deltas (the committed tip), or plus the active journal delta",
  "state-queue-journal":
    "L1 state-queue headers (provider) vs pending_block_finalizations, blocks and the admitted correction transitions in state_queue_terminal_observer_states (SQL)",
  "state-queue-tail-root":
    "utxosRoot of the last L1 state-queue header (ConfirmedState.utxoRoot when the queue is empty) vs the persisted native ledger root",
  deposits:
    "L1 deposit orders (provider, decoded exactly as ingestion does) vs deposits_utxos payload, status and projected header; every SQL deposit whose header is not merged vs the L1 deposit orders; every SQL header assignment vs the L1 queue and the merged chain",
  withdrawals:
    "L1 withdrawal orders (provider, decoded exactly as ingestion does) vs withdrawal_utxos payload (including l2_value), status and projected header; every SQL header assignment vs the L1 queue and the merged chain",
  payouts:
    "L1 payout UTxOs and their PayoutDatum (provider) vs the withdrawal_utxos row with the same asset name: finalized, WithdrawalIsValid, merged header, equal l2_value, l1_address and l1_datum",
  settlements:
    "L1 settlement UTxOs and their SettlementDatum (provider) vs the merged chain and the expected event roots of the journal for that header (SQL)",
  "ledger-cache":
    "SQL mempool_ledger vs the ledger recomputed at the committed tip (or active journal) plus unincluded projected deposit rows plus the effects of every mempool and processed_mempool transaction",
  "da-attestation":
    "DA status of every unmerged L1 state-queue header (provider) vs its attestation deadline, header end_time + da_attestation_timeout_ms of the selected deployment profile, at the wall-clock time of the L1 read",
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
  /** The node's on-chain DA status; null when undecodable. */
  readonly daStatus: SDK.DaAvailabilityStateQueueStatusKind | null;
  readonly decodeError: string | null;
};

/**
 * A deposit's payload as both sides can state it. It carries no L1 tx hash:
 * the node holds the immutable admission tx, an L1 list read only the
 * order's current output, which a later list insertion moves.
 */
export type DepositPayload = {
  readonly eventId: string;
  readonly info: string;
  readonly inclusionTimeMs: number;
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
  readonly observer: ObserverSnapshot;
};

export type NativeRootObservation =
  | {
      readonly kind: "observed";
      readonly root: string;
      readonly source: "node-readiness" | "leveldb-copy";
    }
  /** The node reports its Architecture-G owner unhealthy. */
  | { readonly kind: "unhealthy"; readonly reason: string }
  | { readonly kind: "unavailable"; readonly reason: string };

export type L1Observation =
  | { readonly kind: "observed"; readonly view: L1StateView }
  | { readonly kind: "unavailable"; readonly reason: string };
