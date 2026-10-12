import { createHash } from "node:crypto";

import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { losslessCanonicalJson } from "../lossless-canonical-json.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { Database, MidgardContracts } from "./index.js";
import { requireLandedStateQueue } from "./landed-state-queue.js";

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
 * is disposed of by the landed-block rebase (`landed-blocks/own-journals`),
 * whichever lands wins. It is a function of the journal's own
 * immutable signed identity, so the abandonment names its cause without a
 * schema change and is told apart from an admitted correction's transition
 * digest. Undefined for a journal without a signed intent: it can never be
 * replaced this way.
 */
export const signedIntentReplacementDigest = (
  record: Pick<
    PendingBlockFinalizationsDB.Record,
    typeof J.HEADER_HASH | typeof J.INTENDED_TX_HASH | typeof J.SIGNED_TX_CBOR
  >,
): string | undefined => {
  const intended = record[J.INTENDED_TX_HASH];
  const signed = record[J.SIGNED_TX_CBOR];
  if (intended == null || signed == null) return undefined;
  return sha(
    losslessCanonicalJson({
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
  const contracts = yield* MidgardContracts;
  const queue = yield* requireLandedStateQueue(
    contracts.stateQueue,
    "canonical journal recovery",
  );
  const headers: CanonicalCommittedHeaderIdentity[] = queue.nodes.map(
    (node) => ({
      headerHash: Buffer.from(node.headerHash, "hex"),
      endTimeMs: Number(node.endTimeMs),
    }),
  );
  return yield* withCanonicalHeaderJournals(headers);
});

/** An abandoned payload-bearing local journal of a block on the canonical
 * queue whose abandonment is unattributed. A journal an admitted correction
 * abandoned is never revivable here, even when the same members appear in
 * another journal; one disposed of under its replacement digest is revived
 * by the landed-block rebase once its block is processed. */
const revivableCanonicalJournal = ({ journal }: CanonicalCommittedHeader) =>
  Option.isSome(journal) &&
  journal.value[J.STATUS] === Status.Abandoned &&
  localJournalHasPayloadMembers(journal.value) &&
  journalAbandonment(journal.value) === "unattributed";

export const findEarliestCanonicalPayloadJournal = (
  canonicalHeaders: readonly CanonicalCommittedHeader[],
): Option.Option<CanonicalCommittedHeader> => {
  const candidate = canonicalHeaders.find(revivableCanonicalJournal);
  return candidate === undefined ? Option.none() : Option.some(candidate);
};

/**
 * Revives the earliest abandoned payload-bearing journal whose block is on
 * the canonical queue, so local finalization replays it before later
 * canonical descendants. Only an unattributed abandonment is revived here.
 * One an admitted correction abandoned is never revived: the correction
 * observer alone reconciles a retracted correction. One disposed of under
 * its replacement digest is revived by the landed-block rebase, from the
 * follower's processed landed blocks, not from this unauthenticated view.
 */
export const reviveEarliestCanonicalPayloadJournal = ({
  canonicalHeaders,
  logPrefix,
}: {
  readonly canonicalHeaders: readonly CanonicalCommittedHeader[];
  readonly logPrefix: string;
}): Effect.Effect<
  Option.Option<CanonicalCommittedHeader>,
  DatabaseError,
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
    for (const { headerHash, journal } of canonicalHeaders)
      if (
        Option.isSome(journal) &&
        journal.value[J.STATUS] === Status.Abandoned &&
        localJournalHasPayloadMembers(journal.value) &&
        journalAbandonment(journal.value) === "replacement"
      )
        yield* Effect.logInfo(
          `${logPrefix} leaves canonical block ${headerHash.toString("hex")} to the landed-block rebase: its journal was disposed of under its replacement digest, and the rebase revives it once the follower processed the block.`,
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

    yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
      candidate.headerHash,
      BigInt(Date.now()),
    );
    yield* Effect.logWarning(
      `${logPrefix} revived abandoned pending-finalization journal for canonical payload-bearing block ${candidate.headerHash.toString("hex")}; local finalization recovery will replay that block before later canonical descendants.`,
    );
    return Option.some(candidate);
  });
