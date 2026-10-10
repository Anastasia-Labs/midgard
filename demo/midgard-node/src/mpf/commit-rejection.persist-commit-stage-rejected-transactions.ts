/**
 * Commit-stage rejections with the working ledger's rejection closure
 * (`closeRejections`, the one every rebuild applies): a rejected transaction
 * takes every pending transaction that spends its outputs ("dependent") with
 * it, transitively. A closure that reaches a transaction the block being
 * built accepts takes it out of the block: Phase B runs again without it.
 */

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as TxRejectionsDB from "../database/txRejections.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { Database } from "../services/index.js";
import {
  hex,
  loadPendingTxsKeepingUndecodable,
  type PendingTx,
} from "../services/working-ledger-recompute.pending-txs.js";
import {
  closeRejections,
  recordRejections,
  type RejectionCodeOf,
  txIdHex,
} from "../services/working-ledger-recompute.reject-closure.js";
import {
  COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
  type CommitStageInputPostState,
  type CommitStageTxEffects,
  revertCommitStageRejectedLedgerEffects,
} from "./commit-rejection.js";

type RejectionEntry = TxRejectionsDB.EntryNoTimestamp;

export type CommitStageRejectionOutcome =
  | {
      readonly _tag: "Persisted";
      /** Every rejection recorded, the closure's included. */
      readonly recorded: readonly RejectionEntry[];
      readonly ledgerChanged: boolean;
    }
  | {
      /** Nothing was written: the closure reaches these block members. */
      readonly _tag: "BlockMembersRejected";
      readonly txIds: readonly Buffer[];
    };

const effectsOf = (tx: PendingTx): CommitStageTxEffects => ({
  txId: tx.entry.tx_id,
  spent: tx.spent,
  produced: tx.produced.map((row) => ({
    [Ledger.Columns.OUTREF]: row.outref,
    [Ledger.Columns.OUTPUT]: row.output,
  })),
});

const entryId = (entry: RejectionEntry) =>
  hex(entry[TxRejectionsDB.Columns.TX_ID]);

/**
 * Applies a commit-stage rejection as one database mutation. The closure
 * over `rejectionEntries` ("direct", each with its own code) is taken over
 * the pending transactions; if it reaches one of
 * `blockTxIds` nothing is written. Otherwise its ledger effects are reverted
 * against the block post-state, and every transaction in it is recorded
 * rejected (`recordRejections`). A rejected
 * transaction that is no longer pending is left as it is.
 */
export const persistCommitStageRejectedTransactions = ({
  rejectionEntries,
  resolveInputPostState,
  blockTxIds = [],
}: {
  readonly rejectionEntries: readonly RejectionEntry[];
  readonly resolveInputPostState: CommitStageInputPostState;
  readonly blockTxIds?: readonly Buffer[];
}): Effect.Effect<CommitStageRejectionOutcome, DatabaseError, Database> => {
  if (rejectionEntries.length === 0)
    return Effect.succeed({
      _tag: "Persisted",
      recorded: [],
      ledgerChanged: false,
    });
  const direct = new Map(
    rejectionEntries.map((entry) => [entryId(entry), entry] as const),
  );
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pending = yield* loadPendingTxsKeepingUndecodable;
        const pendingById = new Map(
          pending.map((tx) => [txIdHex(tx), tx] as const),
        );
        const dependentDetail = new Map<string, string>();
        const rejected = yield* closeRejections({
          pending,
          spread: (reject, done) => {
            let changed = false;
            for (const id of direct.keys()) {
              const tx = pendingById.get(id);
              if (tx === undefined || done.has(id)) continue;
              reject(tx, "direct");
              changed = true;
            }
            const producers = new Map(
              [...done.values()].flatMap(({ tx }) =>
                tx.produced.map(
                  (row) => [hex(row.outref), txIdHex(tx)] as const,
                ),
              ),
            );
            for (const tx of pending) {
              const id = txIdHex(tx);
              if (done.has(id)) continue;
              const input = tx.spent.find((outRef) =>
                producers.has(hex(outRef)),
              );
              if (input === undefined) continue;
              dependentDetail.set(
                id,
                `Transaction spends L2 outref ${hex(input)}, an output of transaction ${producers.get(hex(input))!}, which was rejected at commit`,
              );
              reject(tx, "dependent", [
                ...new Set(
                  tx.spent.flatMap((outRef) => {
                    const producer = producers.get(hex(outRef));
                    return producer === undefined ? [] : [producer];
                  }),
                ),
              ]);
              changed = true;
            }
            return changed;
          },
        });
        const reached = blockTxIds.filter((txId) => rejected.has(hex(txId)));
        if (reached.length > 0)
          return {
            _tag: "BlockMembersRejected",
            txIds: reached,
          } satisfies CommitStageRejectionOutcome;
        const ledgerChanged = yield* revertCommitStageRejectedLedgerEffects({
          reverted: [...rejected.values()].map(({ tx }) => effectsOf(tx)),
          remaining: pending
            .filter((tx) => !rejected.has(txIdHex(tx)))
            .map(effectsOf),
          resolveInputPostState,
        });
        const codeOf: RejectionCodeOf = (id, { reason }) => {
          if (reason === "direct") {
            const entry = direct.get(id)!;
            return {
              code: entry[TxRejectionsDB.Columns.REJECT_CODE],
              detail: entry[TxRejectionsDB.Columns.REJECT_DETAIL] ?? "",
            };
          }
          return {
            code: COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
            detail: dependentDetail.get(id)!,
          };
        };
        yield* recordRejections(rejected, codeOf);
        return {
          _tag: "Persisted",
          recorded: [...rejected.entries()].map(([id, rejection]) => {
            const { code, detail } = codeOf(id, rejection);
            return {
              [TxRejectionsDB.Columns.TX_ID]: Buffer.from(id, "hex"),
              [TxRejectionsDB.Columns.REJECT_CODE]: code,
              [TxRejectionsDB.Columns.REJECT_DETAIL]: detail,
            };
          }),
          ledgerChanged,
        } satisfies CommitStageRejectionOutcome;
      }),
    );
  }).pipe(
    withFollowerWrite,
    sqlErrorToDatabaseError(
      TxRejectionsDB.tableName,
      "Failed to persist commit-stage transaction rejections",
    ),
  );
};

/**
 * Runs Phase B (`evaluate`) over `candidates` and persists the commit-stage
 * rejections, the pre-Phase-B `rejectionEntries` and Phase B's own. While the
 * closure reaches a transaction Phase B accepted, that transaction leaves the
 * candidates and Phase B runs again; every pass drops at least one, so the
 * loop ends. Returns Phase B's last accepted set and every candidate
 * rejection, the closure's included.
 */
export const settleCommitStageRejections = <
  C extends { readonly ledgerTx: { readonly txId: Uint8Array } },
>(input: {
  readonly candidates?: readonly C[];
  readonly evaluate?: (candidates: readonly C[]) => Effect.Effect<
    {
      readonly accepted: readonly C[];
      readonly rejected: readonly {
        readonly txId: Uint8Array;
        readonly code: string;
        readonly detail: string | null;
      }[];
    },
    DatabaseError
  >;
  readonly rejectionEntries: readonly RejectionEntry[];
  readonly resolveInputPostState: (
    accepted: readonly C[],
  ) => CommitStageInputPostState;
  readonly onLedgerReverted?: Effect.Effect<void, DatabaseError>;
}) =>
  Effect.gen(function* () {
    const candidateId = (candidate: C) =>
      Buffer.from(candidate.ledgerTx.txId).toString("hex");
    let candidates = input.candidates ?? [];
    for (;;) {
      const phaseB =
        candidates.length === 0 || input.evaluate === undefined
          ? { accepted: [] as readonly C[], rejected: [] }
          : yield* input.evaluate(candidates);
      const phaseBEntries: RejectionEntry[] = phaseB.rejected.map(
        (rejected) => ({
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(rejected.txId),
          [TxRejectionsDB.Columns.REJECT_CODE]: rejected.code,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: rejected.detail,
        }),
      );
      const outcome = yield* persistCommitStageRejectedTransactions({
        rejectionEntries: [...input.rejectionEntries, ...phaseBEntries],
        resolveInputPostState: input.resolveInputPostState(phaseB.accepted),
        blockTxIds: phaseB.accepted.map((accepted) =>
          Buffer.from(accepted.ledgerTx.txId),
        ),
      });
      if (outcome._tag === "BlockMembersRejected") {
        const reached = new Set(outcome.txIds.map(hex));
        candidates = candidates.filter(
          (candidate) => !reached.has(candidateId(candidate)),
        );
        continue;
      }
      if (outcome.recorded.length > 0)
        yield* Effect.logWarning(
          `Dropping ${outcome.recorded.length} transaction(s) from MempoolDB`,
        );
      if (outcome.ledgerChanged && input.onLedgerReverted !== undefined)
        yield* input.onLedgerReverted;
      const candidateIds = new Set((input.candidates ?? []).map(candidateId));
      const known = new Set(
        [...input.rejectionEntries, ...phaseBEntries].map(entryId),
      );
      return {
        accepted: phaseB.accepted,
        rejectionEntries: [
          ...phaseBEntries,
          ...outcome.recorded.filter(
            (entry) =>
              candidateIds.has(entryId(entry)) && !known.has(entryId(entry)),
          ),
        ],
      };
    }
  });
