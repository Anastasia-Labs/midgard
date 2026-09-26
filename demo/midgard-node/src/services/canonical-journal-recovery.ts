import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Cause, Effect, Option, Runtime } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  ImmutableDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { withHistoryWrite } from "./event-history-producer.js";
import { Database, Lucid, MidgardContracts } from "./index.js";

export type CanonicalCommittedHeaderIdentity = {
  readonly headerHash: Buffer;
  readonly endTimeMs: number;
  readonly blockUTxO?: SerializedStateQueueUTxO;
};

export type CanonicalCommittedHeader = CanonicalCommittedHeaderIdentity & {
  readonly journal: Option.Option<PendingBlockFinalizationsDB.Record>;
};

export const localJournalHasPayloadMembers = (
  journal: PendingBlockFinalizationsDB.Record,
): boolean =>
  journal.depositEventIds.length > 0 ||
  journal.forcedTransactionEventIds.length > 0 ||
  journal.withdrawalEventIds.length > 0 ||
  journal.mempoolTxIds.length > 0;

const J = PendingBlockFinalizationsDB.Columns;
const Status = PendingBlockFinalizationsDB.Status;
const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");
const SIGNED_INTENT_REPLACEMENT_DOMAIN = "midgard-signed-intent-replacement-v1";

/**
 * The correction digest a journal is abandoned under when its signed commit
 * is replaced because it can no longer land on the observed chain (see
 * history-expired-intent-release). It is a function of the journal's own
 * immutable signed identity, so the abandonment names its cause without a
 * schema change and is told apart from an admitted correction's transition
 * digest. Undefined for a journal without a signed intent: it can never be
 * replaced this way.
 */
export const signedIntentReplacementDigest = (
  record: PendingBlockFinalizationsDB.Record,
): string | undefined => {
  const intended = record[J.INTENDED_TX_HASH];
  const signed = record[J.SIGNED_TX_CBOR];
  if (intended == null || signed == null) return undefined;
  return sha(
    eventHistoryCanonicalJson({
      domain: SIGNED_INTENT_REPLACEMENT_DOMAIN,
      headerHash: record[J.HEADER_HASH].toString("hex"),
      intendedTxHash: intended.toString("hex"),
      signedTxCborSha256: sha(signed),
    }),
  );
};

/**
 * Why a journal was abandoned:
 *  - `unattributed`: no correction digest (a stale or unsubmitted block the
 *    confirmation path abandoned);
 *  - `replacement`: its signed commit was replaced while still unlanded; if
 *    that commit wins its state-queue slot after all, the journal is revived;
 *  - `correction`: an admitted timeout or fraud correction removed it (or a
 *    descendant it could never land after). Such a journal is never revived:
 *    a retracted correction is reconciled by the correction observer alone.
 */
export const journalAbandonment = (
  record: PendingBlockFinalizationsDB.Record,
): "unattributed" | "replacement" | "correction" => {
  const digest = record[J.CORRECTION_TRANSITION_DIGEST];
  if (digest == null) return "unattributed";
  return digest === signedIntentReplacementDigest(record)
    ? "replacement"
    : "correction";
};

/** Explicit integrity failure: a replaced signed commit won its state-queue
 * slot, but the node has already moved its local ledger past the replaced
 * block's base (a sibling built on the same base was locally finalized, a
 * member was committed elsewhere, or the ledger root advanced). The node
 * cannot reconcile to the landed block and refuses to continue. */
export class SignedIntentReplacementIntegrityError extends Error {
  readonly headerHash: string;
  constructor(headerHash: string, detail: string) {
    super(
      `Signed-intent replacement integrity failure: replaced block ${headerHash} won its state-queue slot on the observed chain, but ${detail}. This node cannot reconcile to the landed block and refuses to continue.`,
    );
    this.name = "SignedIntentReplacementIntegrityError";
    this.headerHash = headerHash;
  }
}

/** The replacement integrity failure a cause carries, however deeply it was
 * wrapped on its way out. */
export const findSignedIntentReplacementIntegrityError = (
  cause: Cause.Cause<unknown>,
): SignedIntentReplacementIntegrityError | undefined => {
  const seen = new Set<unknown>();
  const search = (
    value: unknown,
  ): SignedIntentReplacementIntegrityError | undefined => {
    if (value === null || typeof value !== "object" || seen.has(value))
      return undefined;
    seen.add(value);
    if (value instanceof SignedIntentReplacementIntegrityError) return value;
    if (Cause.isCause(value)) return searchCause(value);
    if (Runtime.isFiberFailure(value))
      return searchCause(value[Runtime.FiberFailureCauseId]);
    return value instanceof Error ? search(value.cause) : undefined;
  };
  const searchCause = (
    inner: Cause.Cause<unknown>,
  ): SignedIntentReplacementIntegrityError | undefined => {
    for (const value of [...Cause.failures(inner), ...Cause.defects(inner)]) {
      const found = search(value);
      if (found !== undefined) return found;
    }
    return undefined;
  };
  return searchCause(cause);
};

/** Statuses a sibling may hold when a replaced journal is revived: every
 * other block built on the same base must have been abandoned. A sibling
 * that landed or wrote local finalization makes the revival an integrity
 * failure; an active unlanded one must be abandoned first by the caller. */
export const REVIVAL_BLOCKING_SIBLING_STATUSES: readonly PendingBlockFinalizationsDB.Status[] =
  [
    Status.SubmittedUnconfirmed,
    Status.ObservedWaitingStability,
    Status.Finalized,
  ];

/**
 * Takes a replaced journal back once its signed commit won its state-queue
 * slot: the inverse of the replacement's reinclusion for a block that never
 * wrote local finalization. Its deposits, forced transactions and withdrawals
 * are assigned to it again (withdrawals reopened by it or by any later
 * replacement built on the same base), the journal returns to observed and
 * the SQL MPF marker moves to its candidate root. Its transactions stay in
 * the mempool, as for any observed block, until local finalization moves them
 * to ImmutableDB. Native replay to the candidate root follows through the
 * journal's retained replay, exactly as for a normally observed block.
 *
 * One transaction. A sibling on the same base that landed or was locally
 * finalized, a member already committed or no longer pending, or a ledger
 * marker off this journal's roots is an integrity failure; nothing is
 * written.
 */
export const reviveReplacedCanonicalJournal = (
  headerHash: Buffer,
): Effect.Effect<
  PendingBlockFinalizationsDB.Record,
  DatabaseError | SignedIntentReplacementIntegrityError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const header = headerHash.toString("hex");
    const integrity = (detail: string) =>
      Effect.fail(new SignedIntentReplacementIntegrityError(header, detail));
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const found = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          headerHash,
          true,
        );
        if (
          Option.isNone(found) ||
          found.value[J.STATUS] !== Status.Abandoned ||
          journalAbandonment(found.value) !== "replacement"
        )
          return yield* Effect.fail(
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message: "Only a replaced abandoned journal can be revived",
              cause: `header_hash=${header}`,
            }),
          );
        const record = found.value;
        yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(record);
        const siblings = yield* sql<{
          header_hash: Buffer;
          status: PendingBlockFinalizationsDB.Status;
        }>`SELECT header_hash, status FROM pending_block_finalizations
          WHERE base_tail_out_ref = ${record[J.BASE_TAIL_OUT_REF]}
            AND header_hash <> ${headerHash}
          ORDER BY created_at, header_hash FOR UPDATE`;
        const landed = siblings.find(({ status }) =>
          REVIVAL_BLOCKING_SIBLING_STATUSES.includes(status),
        );
        if (landed !== undefined)
          return yield* integrity(
            `block ${landed.header_hash.toString("hex")} built on the same base is already ${landed.status}`,
          );
        const active = siblings.find(
          ({ status }) => status !== Status.Abandoned,
        );
        if (active !== undefined)
          return yield* Effect.fail(
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message:
                "A replaced journal is revived only after every sibling on its base is abandoned",
              cause: `header_hash=${header},sibling=${active.header_hash.toString("hex")},status=${active.status}`,
            }),
          );
        const txIds = record.txMembers.map((member) =>
          Buffer.from(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
          ),
        );
        if (txIds.length > 0) {
          const committed = yield* sql<{ tx_id: Buffer }>`
            SELECT tx_id FROM ${sql(ImmutableDB.tableName)}
            WHERE tx_id IN ${sql.in(txIds)}`;
          if (committed.length > 0)
            return yield* integrity(
              `its transaction ${committed[0]!.tx_id.toString("hex")} is already committed locally`,
            );
          const pending = yield* sql<{ tx_id: Buffer }>`
            SELECT tx_id FROM ${sql(MempoolDB.tableName)} WHERE tx_id IN ${sql.in(txIds)}
            UNION SELECT tx_id FROM ${sql(ProcessedMempoolDB.tableName)} WHERE tx_id IN ${sql.in(txIds)}`;
          const present = new Set(
            pending.map(({ tx_id }) => tx_id.toString("hex")),
          );
          const missing = txIds.find((id) => !present.has(id.toString("hex")));
          if (missing !== undefined)
            return yield* integrity(
              `its transaction ${missing.toString("hex")} is no longer pending (rejected or dropped after the replacement reopened it)`,
            );
        }
        yield* DepositsDB.markProjectedByEventIds(
          record.depositEventIds,
          headerHash,
        );
        yield* ForcedTransactionsDB.markProjectedByEventIds(
          record.forcedTransactionEventIds,
          headerHash,
        );
        yield* WithdrawalsDB.restoreCorrectedClassification(
          record.withdrawalMembers.map((member) => ({
            eventId:
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            settlementEventInfo:
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            validity:
              member[
                PendingBlockFinalizationsDB.WithdrawalMemberColumns.VALIDITY
              ],
            validityDetail:
              member[
                PendingBlockFinalizationsDB.WithdrawalMemberColumns
                  .VALIDITY_DETAIL
              ],
          })),
          headerHash,
          [headerHash, ...siblings.map(({ header_hash }) => header_hash)],
        );
        yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
          headerHash,
          BigInt(Date.now()),
        );
        // The replacement moved the marker to this journal's base; a later
        // replacement on the same base leaves it there too. Anything else
        // means the local ledger advanced past the base.
        const aggregate = record.utxoPayloadAggregate;
        const engine = yield* sql`UPDATE mpf_engine_state
          SET root_hex = ${record[J.EXPECTED_UTXOS_ROOT]},
            utxo_payload_entry_count = ${aggregate?.entryCount ?? null},
            utxo_payload_encoded_tuple_bytes = ${aggregate?.encodedTupleBytes ?? null},
            updated_at = NOW()
          WHERE store_name = 'ledger'
            AND root_hex IN (${record[J.BASE_UTXOS_ROOT]}, ${record[J.EXPECTED_UTXOS_ROOT]})
          RETURNING store_name`;
        if (engine.length !== 1)
          return yield* integrity(
            `the local ledger root is no longer its base ${record[J.BASE_UTXOS_ROOT]}`,
          );
        return record;
      }),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      PendingBlockFinalizationsDB.tableName,
      "Failed to revive a replaced journal",
    ),
  );

export const withCanonicalHeaderJournals = (
  headers: readonly CanonicalCommittedHeaderIdentity[],
): Effect.Effect<
  readonly CanonicalCommittedHeader[],
  DatabaseError,
  Database
> =>
  Effect.forEach(
    headers,
    (header) =>
      Effect.gen(function* () {
        const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          header.headerHash,
        );
        return {
          ...header,
          journal,
        } satisfies CanonicalCommittedHeader;
      }),
    { concurrency: 1 },
  );

export const fetchCanonicalCommittedHeaders = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const committedBlocks = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    },
  );
  const headers: CanonicalCommittedHeaderIdentity[] = [];
  for (const block of committedBlocks) {
    if (block.datum.key === "Empty") {
      continue;
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
    headers.push({
      headerHash: Buffer.from(yield* SDK.hashBlockHeader(header), "hex"),
      endTimeMs: Number(header.endTime),
    });
  }
  return yield* withCanonicalHeaderJournals(headers);
});

/** An abandoned payload-bearing local journal of a block on the canonical
 * queue. A journal an admitted correction abandoned is never revivable here,
 * even when the same members appear in another journal. */
const revivableCanonicalJournal = ({ journal }: CanonicalCommittedHeader) =>
  Option.isSome(journal) &&
  journal.value[J.STATUS] === Status.Abandoned &&
  localJournalHasPayloadMembers(journal.value) &&
  journalAbandonment(journal.value) !== "correction";

export const findEarliestCanonicalPayloadJournal = (
  canonicalHeaders: readonly CanonicalCommittedHeader[],
): Option.Option<CanonicalCommittedHeader> => {
  const candidate = canonicalHeaders.find(revivableCanonicalJournal);
  return candidate === undefined ? Option.none() : Option.some(candidate);
};

/**
 * Revives the earliest abandoned payload-bearing journal whose block is on
 * the canonical queue, so local finalization replays it before later
 * canonical descendants. A journal abandoned by a signed-intent replacement
 * is taken back in full (reviveReplacedCanonicalJournal). One an admitted
 * correction abandoned is never revived: the correction observer alone
 * reconciles a retracted correction.
 */
export const reviveEarliestCanonicalPayloadJournal = ({
  canonicalHeaders,
  logPrefix,
}: {
  readonly canonicalHeaders: readonly CanonicalCommittedHeader[];
  readonly logPrefix: string;
}): Effect.Effect<
  Option.Option<CanonicalCommittedHeader>,
  DatabaseError | SignedIntentReplacementIntegrityError,
  Database
> =>
  Effect.gen(function* () {
    for (const { headerHash, journal } of canonicalHeaders)
      if (
        Option.isSome(journal) &&
        journal.value[J.STATUS] === Status.Abandoned &&
        localJournalHasPayloadMembers(journal.value) &&
        journalAbandonment(journal.value) === "correction"
      )
        yield* Effect.logWarning(
          `${logPrefix} will not revive canonical block ${headerHash.toString("hex")}: its journal was abandoned by admitted correction ${journal.value[J.CORRECTION_TRANSITION_DIGEST]!}; only the correction observer reconciles a retracted correction.`,
        );
    const candidateIndex = canonicalHeaders.findIndex(
      revivableCanonicalJournal,
    );
    if (candidateIndex < 0) {
      return Option.none<CanonicalCommittedHeader>();
    }

    const candidate = canonicalHeaders[candidateIndex]!;
    const active = yield* PendingBlockFinalizationsDB.retrieveActive();
    if (Option.isSome(active)) {
      const activeHeaderHash =
        active.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
      if (activeHeaderHash.equals(candidate.headerHash)) {
        return Option.some(candidate);
      }

      const activeCanonicalIndex = canonicalHeaders.findIndex(
        ({ headerHash }) => headerHash.equals(activeHeaderHash),
      );
      if (
        activeCanonicalIndex > candidateIndex &&
        !localJournalHasPayloadMembers(active.value)
      ) {
        yield* PendingBlockFinalizationsDB.markAbandoned(activeHeaderHash);
        yield* Effect.logWarning(
          `${logPrefix} demoted active empty canonical pending-finalization journal ${activeHeaderHash.toString("hex")} so earlier payload-bearing canonical block ${candidate.headerHash.toString("hex")} can recover local finalization first.`,
        );
      } else {
        yield* Effect.logInfo(
          `${logPrefix} skipping abandoned canonical payload journal revival for ${candidate.headerHash.toString("hex")}; active pending-finalization journal ${activeHeaderHash.toString("hex")} must resolve first.`,
        );
        return Option.none<CanonicalCommittedHeader>();
      }
    }

    if (
      Option.isSome(candidate.journal) &&
      journalAbandonment(candidate.journal.value) === "replacement"
    ) {
      yield* reviveReplacedCanonicalJournal(candidate.headerHash);
      yield* Effect.logWarning(
        `${logPrefix} revived replaced journal ${candidate.headerHash.toString("hex")}: its signed commit won its state-queue slot, so its reopened members were taken back and local finalization will replay it.`,
      );
      return Option.some(candidate);
    }
    yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
      candidate.headerHash,
      BigInt(Date.now()),
    );
    yield* Effect.logWarning(
      `${logPrefix} revived abandoned pending-finalization journal for canonical payload-bearing block ${candidate.headerHash.toString("hex")}; local finalization recovery will replay that block before later canonical descendants.`,
    );
    return Option.some(candidate);
  });
