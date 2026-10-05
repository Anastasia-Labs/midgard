import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Logger } from "effect";

import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../../src/database/utils/tx.js";
import * as WithdrawalsDB from "../../src/database/withdrawals.js";
import type {
  RetainedTransitionTraceMember,
  RetainedValidationTraceMember,
  UtxoPayloadSizeAggregate,
} from "../../src/mpf/index.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../../src/mpf/transition-cbor.js";
import { assertPreSubmitDaPayloadSize } from "../../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import {
  type CommitBatchBudgetLimits,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
} from "../../src/workers/utils/commit-block-planner.js";

// A canonical V1 transaction: the pre-submit check re-derives its source
// value, so the bytes must decode.
export const CANONICAL_TX = Buffer.from(
  (
    JSON.parse(
      readFileSync(
        fileURLToPath(
          new URL(
            "../fixtures/transaction-root-v1.generated.json",
            import.meta.url,
          ),
        ),
        "utf8",
      ),
    ) as { transactions: { canonicalTransactionCborHex: string }[] }
  ).transactions[0]!.canonicalTransactionCborHex,
  "hex",
);

// Shapes measured on the lc1 devnet: a one-transaction block retained 112
// validation-trace witnesses totalling 169,337 bytes, and the base ledger held
// 9 UTxOs in 1,333 tuple bytes (a mean entry of 148 bytes).
export const LC1_WITNESSES_PER_TX = 112;
export const LC1_WITNESS_VALUE_BYTES = 1_470;
export const LC1_BASE_LEDGER: UtxoPayloadSizeAggregate = {
  entryCount: 9,
  encodedTupleBytes: 1_333,
};
export const LC1_MEAN_ENTRY_BYTES = 148;

// Every limit the commit program passes, at its configured default.
export const PROGRAM_LIMITS = (
  envelopeMode: DaPayloadEmissionMode,
): CommitBatchBudgetLimits => ({
  ...DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  maxDaPayloadBytes: maxDaPayloadInnerBytes(envelopeMode),
  maxL2TxCount: 10_000,
  maxLedgerOpCount: 40_000,
  maxTransitionStepCount: 40_000,
});

export const header: SDK.Header = {
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "aa".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: "bb".repeat(32),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "cc".repeat(32),
  eventToStepRoot: "dd".repeat(32),
  validationTracesRoot: "ee".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1_780_000_000_000n,
  endTime: 1_780_000_060_000n,
  blockSlot: 120_000_000n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155_381n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(32),
  protocolVersion: 1n,
};

export const mkCandidate = (seed: number): EntryWithTimeStamp => ({
  [TxColumns.TX_ID]: Buffer.from(seed.toString(16).padStart(64, "0"), "hex"),
  [TxColumns.TX]: CANONICAL_TX,
  [TxColumns.TIMESTAMPTZ]: new Date(1_780_000_000_000 + seed),
});

/**
 * One transaction's retained validation trace. Strings are immutable, so every
 * witness shares one value string and a large block costs little memory.
 */
export const traceFor = (
  seed: number,
  witnessCount: number,
  witnessValue: string,
): RetainedValidationTraceMember => {
  const key = seed.toString(16).padStart(80, "0");
  return {
    eventKey: {
      L2TransactionEventKey: { tx_id: seed.toString(16).padStart(64, "0") },
    },
    keyCbor: Buffer.from(key, "hex"),
    valueCbor: Buffer.alloc(220, 7),
    witnesses: Array.from(
      { length: witnessCount },
      (_, index): SDK.DaPayloadEntry => [
        `${key}${index.toString(16).padStart(8, "0")}`,
        witnessValue,
      ],
    ),
  } as unknown as RetainedValidationTraceMember;
};

/** A transition-trace member of `valueBytes`, standing in for event content. */
export const eventContentOf = (
  valueBytes: number,
): RetainedTransitionTraceMember =>
  ({
    stepIndex: 0n,
    value: {
      schema_version: 1n,
      step_index: 0n,
      phase: "Withdrawal",
      event_key: {
        WithdrawalEventKey: {
          withdrawal_id: { transactionId: "fe".repeat(32), outputIndex: 0n },
        },
      },
      pre_utxos_root: "aa".repeat(32),
      post_utxos_root: "aa".repeat(32),
    },
    keyCbor: Buffer.alloc(8, 1),
    valueCbor: Buffer.alloc(valueBytes, 2),
  }) as unknown as RetainedTransitionTraceMember;

export type BlockShape = {
  readonly base?: UtxoPayloadSizeAggregate;
  readonly witnessCount?: number;
  readonly witnessValueBytes?: number;
  readonly eventBytes?: number;
  /** Base entries the block's withdrawals remove from the ledger it carries. */
  readonly withdrawnEntryCount?: number;
};

/**
 * The DA content of the block the node builds from `txs`: each transfer spends
 * one entry and creates two, and every transaction retains a validation trace.
 */
export const blockContentFor = (
  txs: readonly EntryWithTimeStamp[],
  {
    base = LC1_BASE_LEDGER,
    witnessCount = LC1_WITNESSES_PER_TX,
    witnessValueBytes = LC1_WITNESS_VALUE_BYTES,
    eventBytes = 0,
    withdrawnEntryCount = 0,
  }: BlockShape = {},
) => {
  const witnessValue = "ab".repeat(witnessValueBytes);
  const before = eventBytes > 0 || withdrawnEntryCount > 0 ? 1 : 0;
  const normal = txs.map((tx, index): RetainedTransitionTraceMember => {
    const value: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: BigInt(before + index),
      event_key: {
        L2TransactionEventKey: { tx_id: tx[TxColumns.TX_ID].toString("hex") },
      },
      phase: "L2Transaction",
      pre_utxos_root: "aa".repeat(32),
      post_utxos_root: "aa".repeat(32),
    };
    return {
      stepIndex: value.step_index,
      value,
      keyCbor: encodeTransitionIntegerCbor(value.step_index),
      valueCbor: encodeTransitionStepCbor(value),
    };
  });
  const transitionTraceMembers = [
    ...(before > 0 ? [eventContentOf(eventBytes)] : []),
    ...normal,
  ];
  const utxoPayloadAggregatesByPrefix = Array.from(
    { length: txs.length + 1 },
    (_, prefix) => ({
      entryCount: base.entryCount + prefix - withdrawnEntryCount,
      encodedTupleBytes:
        base.encodedTupleBytes +
        (prefix - withdrawnEntryCount) * LC1_MEAN_ENTRY_BYTES,
    }),
  );
  return {
    utxoPayloadAggregate: utxoPayloadAggregatesByPrefix[txs.length]!,
    utxoPayloadAggregatesByPrefix,
    includedDepositEntries: [],
    includedForcedTransactionEntries: [],
    includedWithdrawalEntries:
      before > 0
        ? [
            {
              [WithdrawalsDB.Columns.ID]: Buffer.from("fe".repeat(32), "hex"),
              [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: Buffer.from([1]),
            } as unknown as WithdrawalsDB.Entry,
          ]
        : [],
    processedMempoolTxs: txs,
    transitionTraceMembers,
    eventToStepMembers: transitionTraceMembers.map((member) => {
      const value: SDK.EventToStepValue = {
        step_index: member.stepIndex,
        phase: member.value.phase,
      };
      return {
        eventKey: member.value.event_key,
        keyCbor: encodeTransitionEventKeyCbor(member.value.event_key),
        valueCbor: encodeEventToStepValueCbor(value),
        value,
      };
    }),
    validationTraceMembers: txs.map((tx) =>
      traceFor(
        Number.parseInt(tx[TxColumns.TX_ID].toString("hex"), 16),
        witnessCount,
        witnessValue,
      ),
    ),
  } as const;
};

/** The block the node builds from `txs`, sized by the real pre-submit check. */
export const preSubmit = (
  txs: readonly EntryWithTimeStamp[],
  envelopeMode: DaPayloadEmissionMode,
  shape: BlockShape = {},
) =>
  Effect.runPromise(
    Effect.either(
      assertPreSubmitDaPayloadSize({
        headerHash: "33".repeat(28),
        header: {
          ...header,
          l2TransactionCount: BigInt(txs.length),
          totalEventCount: BigInt(txs.length),
          transitionStepCount: BigInt(txs.length),
          validationTraceCount: BigInt(txs.length),
        },
        ...blockContentFor(txs, shape),
        cekProgramMaterial: [],
        envelopeMode,
      }).pipe(Effect.provide(Logger.remove(Logger.defaultLogger))),
    ),
  );

export const REFUSAL =
  "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame";

export const MODES = ["identity", "zstd"] as const;

/** Explicit synthetic vectors for planner/state-write composition tests. */
export const syntheticCommitPrefixMeasurement = (
  acceptedIds: readonly (Buffer | string)[],
  prefixBytes: readonly number[],
  hasMandatoryWork = false,
) => {
  const acceptedTxIds = acceptedIds.map((id) =>
    typeof id === "string" ? Buffer.from(id, "hex") : id,
  );
  return {
    acceptedTxCount: acceptedTxIds.length,
    acceptedTxIds,
    rejectedTxIds: [],
    hasMandatoryWork,
    innerBytesUpperBound: prefixBytes[acceptedTxIds.length]!,
    prefixes: prefixBytes
      .slice(0, acceptedTxIds.length + 1)
      .map((innerBytesUpperBound, index) => ({
        innerBytesUpperBound,
        materialDigest: index.toString(16).padStart(64, "0"),
      })),
  };
};

export const syntheticStepDownStateWriteMeasurement = (
  accepted: readonly string[],
) =>
  syntheticCommitPrefixMeasurement(
    accepted,
    [1_000, 30_000, 50_000, 110_000, 150_000],
    true,
  );
