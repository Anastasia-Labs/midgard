import { canonicalCborArgumentSize } from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../../database/utils/tx.js";
import {
  COMMIT_DA_FRAME_STEP_DOWN_SAFETY,
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
  // Every block carries the whole base ledger. When even its empty block
  // exceeds the frame no selection fits, and the DA budget stands aside so the
  // pre-submit check refuses that block by its ledger, exactly as before.
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

/**
 * Decides the next pass after a built block was measured. A block that fits
 * stands. Otherwise the selection shrinks to the transaction count the
 * measured per-transaction cost says fits, and from the second step-down on
 * to at most half, so a selection of `n` reaches the empty floor in at most
 * `3 + log2(n)` passes. A base ledger whose empty block exceeds the frame goes
 * straight to the floor: only withdrawals can shrink the ledger the block
 * carries, so a block without transactions is the one that may still fit. An
 * overflow with no transaction left to drop goes on to the pre-submit check,
 * which refuses it exactly as before.
 */
export const planCommitDaFrameStepDown = ({
  measurement,
  baseEmptyBlockInnerBytes,
  maxInnerBytes,
  pass,
}: {
  readonly measurement: CommitDaFrameMeasurement;
  readonly baseEmptyBlockInnerBytes: number;
  readonly maxInnerBytes: number;
  /** Zero-based index of the pass that was measured. */
  readonly pass: number;
}): CommitDaFrameStepDown => {
  if (measurement.innerBytesUpperBound <= maxInnerBytes) {
    return { status: "fits" };
  }
  const txCount = measurement.acceptedTxCount;
  if (txCount === 0) {
    return { status: "no_transactions_to_drop" };
  }
  if (baseEmptyBlockInnerBytes >= maxInnerBytes) {
    return { status: "step_down", nextTxCount: 0 };
  }
  const proportional = Math.floor(
    (txCount *
      COMMIT_DA_FRAME_STEP_DOWN_SAFETY *
      (maxInnerBytes - baseEmptyBlockInnerBytes)) /
      Math.max(1, measurement.innerBytesUpperBound - baseEmptyBlockInnerBytes),
  );
  return {
    status: "step_down",
    nextTxCount: Math.max(
      0,
      Math.min(
        txCount - 1,
        proportional,
        pass === 0 ? txCount : Math.floor(txCount / 2),
      ),
    ),
  };
};

/**
 * Builds a block from `candidateSelection` and, while its measured DA payload
 * cannot fit the frame, rebuilds it from a shorter prefix of the transactions
 * the previous pass accepted. Every pass is a pure function of the previous
 * pass's measurement, so the same mempool steps down to the same block. Only
 * the final pass reaches commit; `rebase` discards a superseded pass's
 * speculative state before the next one is built.
 */
export const stepDownCommitSelectionToDaFrame = <P, E, R, E2, R2>({
  candidateSelection,
  baseEmptyBlockInnerBytes,
  maxInnerBytes,
  process,
  measure,
  rebase,
}: {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly baseEmptyBlockInnerBytes: number;
  readonly maxInnerBytes: number;
  readonly process: (
    candidateSelection: CommitTxCandidateSelection,
  ) => Effect.Effect<P, E, R>;
  /** Undefined leaves the block to the pre-submit check unmeasured. */
  readonly measure: (
    processed: P,
  ) => Effect.Effect<CommitDaFrameMeasurement | undefined, E2, R2>;
  readonly rebase: Effect.Effect<void, E2, R2>;
}): Effect.Effect<
  {
    readonly processed: P;
    readonly candidateSelection: CommitTxCandidateSelection;
    readonly passes: number;
  },
  E | E2,
  R | R2
> =>
  Effect.gen(function* () {
    let selection = candidateSelection;
    for (let pass = 0; ; pass += 1) {
      if (pass > 0) yield* rebase;
      const processed = yield* process(selection);
      const measurement = yield* measure(processed);
      const decision =
        measurement === undefined
          ? undefined
          : planCommitDaFrameStepDown({
              measurement,
              baseEmptyBlockInnerBytes,
              maxInnerBytes,
              pass,
            });
      if (decision?.status !== "step_down") {
        if (decision !== undefined && decision.status !== "fits") {
          yield* Effect.logWarning(
            `commit_da_frame_step_down=refused reason=${decision.status} pass=${pass.toString()} inner_bytes_upper_bound=${measurement!.innerBytesUpperBound.toString()} base_empty_block_inner_bytes=${baseEmptyBlockInnerBytes.toString()} effective_inner_limit=${maxInnerBytes.toString()}`,
          );
        }
        return { processed, candidateSelection: selection, passes: pass + 1 };
      }
      yield* Effect.logWarning(
        `commit_da_frame_step_down=stepping pass=${pass.toString()} inner_bytes_upper_bound=${measurement!.innerBytesUpperBound.toString()} base_empty_block_inner_bytes=${baseEmptyBlockInnerBytes.toString()} effective_inner_limit=${maxInnerBytes.toString()} accepted_tx_count=${measurement!.acceptedTxCount.toString()} next_tx_count=${decision.nextTxCount.toString()}`,
      );
      const rejected = new Set(
        measurement!.rejectedTxIds.map((txId) => txId.toString("hex")),
      );
      selection = buildCommitTxCandidateSelection(
        selection.candidateTxs
          .filter(
            (entry) => !rejected.has(entry[TxColumns.TX_ID].toString("hex")),
          )
          .slice(0, decision.nextTxCount),
        selection.sourceTable,
      );
    }
  });
