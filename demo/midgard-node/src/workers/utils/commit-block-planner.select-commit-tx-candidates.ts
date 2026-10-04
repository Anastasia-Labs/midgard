import { canonicalCborArgumentSize } from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../../database/utils/tx.js";
import {
  type CommitDaFrameNotice,
  commitDaFrameNoticeForOutcome,
} from "./commit-block-planner.commit-da-frame-notice.js";
import {
  type CommitBatchBudgetLimits,
  type CommitBatchPlan,
  type CommitBatchStopReason,
  type CommitDaFrameMeasurement,
  type CommitDaFrameStepDown,
  type CommitTxCandidateSelection,
  type CommitTxSourceTable,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  type PlannedCommitBatchSelection,
} from "./commit-block-planner.commit-scheduler-evidence-key.js";

export const selectCommitTxCandidates = ({
  mempoolTxs,
  processedMempoolTxs,
}: {
  readonly mempoolTxs: readonly EntryWithTimeStamp[];
  readonly processedMempoolTxs: readonly EntryWithTimeStamp[];
}): CommitTxCandidateSelection => {
  const candidateTxs =
    processedMempoolTxs.length > 0 ? processedMempoolTxs : mempoolTxs;
  const sourceTable =
    processedMempoolTxs.length > 0
      ? "processed_mempool"
      : mempoolTxs.length > 0
        ? "mempool"
        : "none";

  return {
    candidateTxs,
    candidateTxHashes: candidateTxs.map((entry) =>
      Buffer.from(entry[TxColumns.TX_ID]),
    ),
    candidateTxsSize: candidateTxs.reduce(
      (total, entry) => total + entry[TxColumns.TX].length,
      0,
    ),
    sourceTable,
  };
};

export const buildCommitTxCandidateSelection = (
  candidateTxs: readonly EntryWithTimeStamp[],
  sourceTable: CommitTxSourceTable,
): CommitTxCandidateSelection => ({
  candidateTxs,
  candidateTxHashes: candidateTxs.map((entry) =>
    Buffer.from(entry[TxColumns.TX_ID]),
  ),
  candidateTxsSize: candidateTxs.reduce(
    (total, entry) => total + entry[TxColumns.TX].length,
    0,
  ),
  sourceTable: candidateTxs.length > 0 ? sourceTable : "none",
});

/** Exact canonical Plutus-Data CBOR bytes of a `length`-byte byte string. */
const plutusBytesEncodedSize = (length: number): number => {
  const chunk = 64;
  if (length <= chunk) return canonicalCborArgumentSize(length) + length;
  const remainder = length % chunk;
  return (
    2 +
    Math.floor(length / chunk) * (canonicalCborArgumentSize(chunk) + chunk) +
    (remainder === 0 ? 0 : canonicalCborArgumentSize(remainder) + remainder)
  );
};

const UINT64_MAX = 2n ** 64n - 1n;
// Wider than any V1 root, hash or key, so a header built from these values
// encodes at least as long as any header the node can produce.
const WIDEST_HASH = "ff".repeat(64);

/** The longest-encoding header, for sizing a block before its header exists. */
export const DA_PAYLOAD_UPPER_BOUND_HEADER: SDK.Header = {
  prevUtxosRoot: WIDEST_HASH,
  utxosRoot: WIDEST_HASH,
  withdrawalsRoot: WIDEST_HASH,
  forcedTransactionsRoot: WIDEST_HASH,
  transactionsRoot: WIDEST_HASH,
  depositsRoot: WIDEST_HASH,
  transitionTraceRoot: WIDEST_HASH,
  eventToStepRoot: WIDEST_HASH,
  validationTracesRoot: WIDEST_HASH,
  withdrawalCount: UINT64_MAX,
  forcedTransactionCount: UINT64_MAX,
  l2TransactionCount: UINT64_MAX,
  depositCount: UINT64_MAX,
  totalEventCount: UINT64_MAX,
  transitionStepCount: UINT64_MAX,
  validationTraceCount: UINT64_MAX,
  startTime: UINT64_MAX,
  endTime: UINT64_MAX,
  blockSlot: UINT64_MAX,
  expectedNetworkId: UINT64_MAX,
  minFeeA: UINT64_MAX,
  minFeeB: UINT64_MAX,
  prevHeaderHash: WIDEST_HASH,
  operatorVkey: WIDEST_HASH,
  protocolVersion: UINT64_MAX,
};

export const DA_PAYLOAD_UPPER_BOUND_HEADER_HASH = WIDEST_HASH;

/**
 * Upper bound on the inner DaPayloadV1 bytes of a block with no events over a
 * ledger of `utxos`: the least any block on that ledger can carry.
 */
export const emptyBlockDaPayloadUpperBoundBytes = (
  utxos: SDK.DaPayloadEntrySizeAggregate,
): number =>
  SDK.daPayloadEncodedSizeFromUtxoAggregate(
    {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
        header: DA_PAYLOAD_UPPER_BOUND_HEADER,
        utxos: [],
        withdrawals: [],
        forced_transactions: [],
        transactions: [],
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
        deposits: [],
        transition_trace: [],
        event_to_step: [],
        validation_traces: [],
        validation_trace_witnesses: [],
        counts: {
          withdrawalCount: UINT64_MAX,
          forcedTransactionCount: UINT64_MAX,
          l2TransactionCount: UINT64_MAX,
          depositCount: UINT64_MAX,
          totalEventCount: UINT64_MAX,
          transitionStepCount: UINT64_MAX,
          validationTraceCount: UINT64_MAX,
        },
      },
    },
    utxos,
  );

/**
 * The DA bytes one transaction is planned to add: its source entry and its
 * preimage entry (the block carries the transaction twice), ledger growth
 * bounded by twice its bytes (an output's UTxO entry adds its outref to the
 * output it copies), and the allowance for program material and traces.
 */
export const estimatedTxDaPayloadBytes = (
  txBytes: number,
  limits: CommitBatchBudgetLimits,
): number => {
  const copyBytes =
    2 + plutusBytesEncodedSize(32) + plutusBytesEncodedSize(txBytes);
  return (
    2 * copyBytes +
    2 * plutusBytesEncodedSize(txBytes) +
    limits.estimatedDaOverheadBytesPerTx +
    (limits.estimatedDaTraceAllowanceBytesPerTx ?? 0)
  );
};

const estimateCommitBatchPlan = (
  txs: readonly EntryWithTimeStamp[],
  limits: CommitBatchBudgetLimits,
  stopReason: CommitBatchStopReason,
  baseDaPayloadBytes: number,
): CommitBatchPlan => {
  const selectedTxBytes = txs.reduce(
    (total, entry) => total + entry[TxColumns.TX].length,
    0,
  );
  const selectedTxCount = txs.length;
  return {
    selectedTxCount,
    selectedTxBytes,
    selectedLedgerOpCount: selectedTxCount * limits.estimatedLedgerOpsPerTx,
    selectedTransitionStepCount:
      selectedTxCount * limits.estimatedTransitionStepsPerTx,
    estimatedDaPayloadBytes: txs.reduce(
      (total, entry) =>
        total + estimatedTxDaPayloadBytes(entry[TxColumns.TX].length, limits),
      baseDaPayloadBytes,
    ),
    estimatedCommitTxBytes:
      limits.estimatedCommitTxOverheadBytes + selectedTxCount * 32,
    estimatedCommitBuildMs:
      selectedTxCount * limits.estimatedCommitBuildMsPerTx,
    stopReason,
  };
};

const firstExceededBudget = (
  plan: CommitBatchPlan,
  limits: CommitBatchBudgetLimits,
  enforceDaBudget: boolean,
): CommitBatchStopReason | null => {
  if (plan.selectedTxCount > limits.maxL2TxCount) {
    return "tx_count_budget";
  }
  if (plan.selectedTxBytes > limits.maxCanonicalTxBytes) {
    return "tx_bytes_budget";
  }
  if (plan.selectedLedgerOpCount > limits.maxLedgerOpCount) {
    return "ledger_ops_budget";
  }
  if (plan.selectedTransitionStepCount > limits.maxTransitionStepCount) {
    return "transition_steps_budget";
  }
  if (
    enforceDaBudget &&
    plan.estimatedDaPayloadBytes > limits.maxDaPayloadBytes
  ) {
    return "da_payload_budget";
  }
  if (plan.estimatedCommitTxBytes > limits.maxCommitTxBytes) {
    return "commit_tx_budget";
  }
  if (plan.estimatedCommitBuildMs > limits.maxEstimatedCommitBuildMs) {
    return "latency_budget";
  }
  return null;
};

export const planCommitBatchBudgets = ({
  candidateSelection,
  limits = DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  baseUtxoPayloadAggregate,
}: {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly limits?: CommitBatchBudgetLimits;
  /** The commit base's UTxO aggregate, once the base is resolved. */
  readonly baseUtxoPayloadAggregate?: SDK.DaPayloadEntrySizeAggregate;
}): PlannedCommitBatchSelection => {
  // An empty base-ledger upper bound does not rule out ledger-consuming work.
  // Stand aside when it exceeds the frame; complete-prefix accounting and
  // final-header admission decide the selected family.
  const baseDaPayloadBytes =
    baseUtxoPayloadAggregate === undefined
      ? 0
      : emptyBlockDaPayloadUpperBoundBytes(baseUtxoPayloadAggregate);
  const daBudgetApplies = baseDaPayloadBytes <= limits.maxDaPayloadBytes;
  const selected: EntryWithTimeStamp[] = [];
  let stopReason: CommitBatchStopReason = "mempool_exhausted";
  let selectedTxBytes = 0;
  let estimatedDaPayloadBytes = baseDaPayloadBytes;

  for (const candidate of candidateSelection.candidateTxs) {
    const selectedTxCount = selected.length + 1;
    const nextSelectedTxBytes =
      selectedTxBytes + candidate[TxColumns.TX].length;
    const nextEstimatedDaPayloadBytes =
      estimatedDaPayloadBytes +
      estimatedTxDaPayloadBytes(candidate[TxColumns.TX].length, limits);
    const nextPlan: CommitBatchPlan = {
      selectedTxCount,
      selectedTxBytes: nextSelectedTxBytes,
      selectedLedgerOpCount: selectedTxCount * limits.estimatedLedgerOpsPerTx,
      selectedTransitionStepCount:
        selectedTxCount * limits.estimatedTransitionStepsPerTx,
      estimatedDaPayloadBytes: nextEstimatedDaPayloadBytes,
      estimatedCommitTxBytes:
        limits.estimatedCommitTxOverheadBytes + selectedTxCount * 32,
      estimatedCommitBuildMs:
        selectedTxCount * limits.estimatedCommitBuildMsPerTx,
      stopReason: "mempool_exhausted",
    };
    // The DA estimate never keeps out the first transaction: whether one
    // transaction fits is for the exact post-build measurement to decide, so
    // an allowance alone can never hold the head of the queue out of a block.
    const exceeded = firstExceededBudget(
      nextPlan,
      limits,
      daBudgetApplies && selectedTxCount > 1,
    );
    if (exceeded !== null) {
      stopReason = exceeded;
      break;
    }
    selected.push(candidate);
    selectedTxBytes = nextSelectedTxBytes;
    estimatedDaPayloadBytes = nextEstimatedDaPayloadBytes;
  }

  // Empty is the only safe result when the first candidate does not fit. The
  // former "always include one" fallback silently exceeded consensus bounds.
  const candidateTxs = selected;
  const finalPlan = estimateCommitBatchPlan(
    candidateTxs,
    limits,
    stopReason,
    baseDaPayloadBytes,
  );
  return {
    candidateSelection: buildCommitTxCandidateSelection(
      candidateTxs,
      candidateSelection.sourceTable,
    ),
    plan: finalPlan,
    originalTxCount: candidateSelection.candidateTxs.length,
    prunedTxCount: Math.max(
      0,
      candidateSelection.candidateTxs.length - candidateTxs.length,
    ),
  };
};

/** Preserves the existing conservative event-work budget. The complete-prefix
 * search now needs at most two processing passes, below this bound. */
export const commitDaFrameStepDownPassBound = (txCount: number): number => {
  let passes = 2;
  for (let reach = 1; reach < txCount; reach *= 2) passes += 1;
  return txCount <= 0 ? 1 : passes;
};

export const planCommitDaFrameStepDown = ({
  measurement,
  maxInnerBytes,
}: {
  readonly measurement: CommitDaFrameMeasurement;
  readonly maxInnerBytes: number;
}): CommitDaFrameStepDown => {
  const n = measurement.acceptedTxCount;
  if (
    !Number.isSafeInteger(n) ||
    n < 0 ||
    measurement.acceptedTxIds.length !== n ||
    new Set(measurement.acceptedTxIds.map((id) => id.toString("hex"))).size !==
      n ||
    measurement.prefixes.length !== n + 1 ||
    measurement.prefixes.some(
      (prefix) =>
        !Number.isSafeInteger(prefix.innerBytesUpperBound) ||
        prefix.innerBytesUpperBound < 0 ||
        !/^[0-9a-f]{64}$/.test(prefix.materialDigest),
    ) ||
    measurement.prefixes[n]?.innerBytesUpperBound !==
      measurement.innerBytesUpperBound
  )
    return { status: "incomplete" };
  const first = measurement.hasMandatoryWork ? 0 : 1;
  if (first > n) return { status: "exact_check_required" };
  let minimum = first;
  let fitting: number | undefined;
  for (let prefix = first; prefix <= n; prefix += 1) {
    const bytes = measurement.prefixes[prefix]!.innerBytesUpperBound;
    if (bytes <= maxInnerBytes) fitting = prefix;
    if (bytes <= measurement.prefixes[minimum]!.innerBytesUpperBound)
      minimum = prefix;
  }
  const chosen = fitting ?? minimum;
  return chosen === n
    ? { status: fitting === undefined ? "exact_check_required" : "fits" }
    : { status: "step_down", nextTxCount: chosen };
};

/** Enumerates all accepted prefixes once, then rebuilds at most one chosen
 * prefix from actual accepted order. Changed material is held before signing. */
export const stepDownCommitSelectionToDaFrame = <P, E, R, E2, R2>({
  candidateSelection,
  baseUtxoPayloadAggregate,
  maxInnerBytes,
  notify,
  process,
  measure,
  rebase,
}: {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly baseUtxoPayloadAggregate: SDK.DaPayloadEntrySizeAggregate;
  readonly maxInnerBytes: number;
  readonly notify?: (notice: CommitDaFrameNotice) => Effect.Effect<void>;
  readonly process: (
    selection: CommitTxCandidateSelection,
  ) => Effect.Effect<P, E, R>;
  readonly measure: (
    processed: P,
  ) => Effect.Effect<CommitDaFrameMeasurement | undefined, E2, R2>;
  readonly rebase: Effect.Effect<void, E2, R2>;
}): Effect.Effect<
  {
    readonly processed: P;
    readonly candidateSelection: CommitTxCandidateSelection;
    readonly passes: number;
    readonly outcome:
      | "fits"
      | "exact_check_required"
      | "incomplete"
      | "unmeasured";
  },
  E | E2,
  R | R2
> =>
  Effect.gen(function* () {
    const baseEmptyBlockInnerBytes = emptyBlockDaPayloadUpperBoundBytes(
      baseUtxoPayloadAggregate,
    );
    let selection = candidateSelection;
    let processed = yield* process(selection);
    const initial = yield* measure(processed);
    let measurement = initial;
    let passes = 1;
    let decision =
      measurement === undefined
        ? undefined
        : planCommitDaFrameStepDown({ measurement, maxInnerBytes });
    if (decision?.status === "step_down" && initial !== undefined) {
      const rows = new Map(
        selection.candidateTxs.map((entry) => [
          entry[TxColumns.TX_ID].toString("hex"),
          entry,
        ]),
      );
      const selectedIds = initial.acceptedTxIds.slice(0, decision.nextTxCount);
      const selectedRows = selectedIds.map((id) =>
        rows.get(id.toString("hex")),
      );
      if (
        rows.size !== selection.candidateTxs.length ||
        selectedRows.some((row) => row === undefined)
      )
        decision = { status: "incomplete" };
      else {
        selection = buildCommitTxCandidateSelection(
          selectedRows.filter(
            (row): row is EntryWithTimeStamp => row !== undefined,
          ),
          selection.sourceTable,
        );
        yield* rebase;
        processed = yield* process(selection);
        passes = 2;
        measurement = yield* measure(processed);
        const expected = initial.prefixes[decision.nextTxCount]!;
        const actual = measurement?.prefixes[measurement.acceptedTxCount];
        const rebuiltDecision =
          measurement === undefined
            ? undefined
            : planCommitDaFrameStepDown({ measurement, maxInnerBytes });
        const same =
          measurement !== undefined &&
          measurement.acceptedTxCount === selectedIds.length &&
          measurement.rejectedTxIds.length === 0 &&
          measurement.acceptedTxIds.every((id, index) =>
            id.equals(selectedIds[index]!),
          ) &&
          measurement.hasMandatoryWork === initial.hasMandatoryWork &&
          actual?.materialDigest === expected.materialDigest &&
          actual.innerBytesUpperBound === expected.innerBytesUpperBound;
        decision =
          same &&
          measurement !== undefined &&
          rebuiltDecision?.status !== "incomplete"
            ? {
                status:
                  measurement.innerBytesUpperBound <= maxInnerBytes
                    ? "fits"
                    : "exact_check_required",
              }
            : { status: "incomplete" };
      }
    }
    const outcome =
      decision?.status === "step_down"
        ? "incomplete"
        : (decision?.status ?? "unmeasured");
    const notice = commitDaFrameNoticeForOutcome({
      outcome,
      passes,
      measurement,
      initialCandidateInnerBytesUpperBound: initial?.innerBytesUpperBound,
      baseEmptyBlockInnerBytes,
      maxInnerBytes,
    });
    if (notice !== undefined && notify !== undefined) yield* notify(notice);
    return { processed, candidateSelection: selection, passes, outcome };
  });
