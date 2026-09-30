import * as SDK from "@al-ft/midgard-sdk";
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
  type ObserverSnapshot,
  type ObserverTransition,
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

export const decodeObserverState = (
  raw: unknown,
  policyIdHex: string,
): ObserverSnapshot => {
  const value = typeof raw === "string" ? (JSON.parse(raw) as unknown) : raw;
  if (typeof value !== "object" || value === null) {
    return { kind: "invalid", reason: "state_record is not an object" };
  }
  const record = value as {
    stateQueuePolicyId?: unknown;
    admitted?: unknown;
    pending?: unknown;
  };
  if (record.stateQueuePolicyId !== policyIdHex) {
    return {
      kind: "invalid",
      reason: "state_record names a different state-queue policy",
    };
  }
  if (!Array.isArray(record.admitted) || !Array.isArray(record.pending)) {
    return {
      kind: "invalid",
      reason: "state_record lacks admitted/pending transition lists",
    };
  }
  const admitted: ObserverTransition[] = [];
  for (const candidate of record.admitted as unknown[]) {
    const transition =
      candidate as Partial<SDK.StateQueueAuthenticatedTransition>;
    if (
      typeof transition.transactionHash !== "string" ||
      typeof transition.transitionKind !== "string" ||
      typeof transition.transitionDigest !== "string" ||
      !Array.isArray(transition.removedHeaderHashes)
    ) {
      return { kind: "invalid", reason: "admitted transition is malformed" };
    }
    admitted.push({
      transactionHash: transition.transactionHash,
      transitionKind: transition.transitionKind,
      transitionDigest: transition.transitionDigest,
      removedHeaderHashes: transition.removedHeaderHashes.map(String),
    });
  }
  return { kind: "present", admitted, pendingCount: record.pending.length };
};
