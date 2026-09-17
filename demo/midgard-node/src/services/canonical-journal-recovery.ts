import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";
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

export const findEarliestCanonicalPayloadJournal = (
  canonicalHeaders: readonly CanonicalCommittedHeader[],
): Option.Option<CanonicalCommittedHeader> => {
  const candidate = canonicalHeaders.find(
    ({ journal }) =>
      Option.isSome(journal) &&
      journal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
        PendingBlockFinalizationsDB.Status.Abandoned &&
      localJournalHasPayloadMembers(journal.value),
  );
  return candidate === undefined ? Option.none() : Option.some(candidate);
};

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
    const candidateIndex = canonicalHeaders.findIndex(
      ({ journal }) =>
        Option.isSome(journal) &&
        journal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Abandoned &&
        localJournalHasPayloadMembers(journal.value),
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
