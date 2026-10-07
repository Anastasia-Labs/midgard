import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";

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
import { resolveTxDeltaForCommit } from "../mpf/commit-rejection.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import { Database } from "../services/index.js";
import {
  depositPayloadOf,
  withdrawalPayloadOf,
} from "./state-reconciliation.check-ledger-cache.js";
import {
  entriesMap,
  headerEndTimeMs,
  type JournalRow,
} from "./state-reconciliation.collect-l1-state-view.js";
import {
  type JournalSummary,
  type L1StateView,
  type LedgerPointResult,
  type ObserverSnapshot,
  type PendingTxDelta,
  type SqlDepositRow,
  type SqlStateSnapshot,
  type SqlWithdrawalRow,
} from "./state-reconciliation.compares.js";
import {
  decodeObserverState,
  materializePoint,
} from "./state-reconciliation.materialize-point.js";
import {
  ACTIVE_JOURNAL_STATUSES,
  describeError,
  JOURNAL_STATUS,
  redactSensitive,
  toHex,
  toHexOrNull,
} from "./state-reconciliation.walk-merged-chain.js";

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

export const HEX_32 = /^[0-9a-f]{64}$/u;
