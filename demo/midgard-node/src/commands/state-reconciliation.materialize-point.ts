import { Effect, Either, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import { Database } from "../services/index.js";
import { materializeConfirmedLedgerDeltaChain } from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import {
  chainFrom,
  entriesMap,
} from "./state-reconciliation.collect-l1-state-view.js";
import {
  type JournalSummary,
  type LedgerPointResult,
} from "./state-reconciliation.compares.js";
import { describeError } from "./state-reconciliation.walk-merged-chain.js";

export const materializePoint = (
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
