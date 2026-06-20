import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { type Database, Lucid } from "@/services/index.js";
import {
  serializeStateQueueUTxO,
  type WorkerInput,
} from "@/workers/utils/commit-block-header.js";

import {
  fetchLatestCommittedBlockLocal,
  stateQueueBaseHeaderHash,
  stateQueueOutRef,
} from "./state-queue.js";

export const buildPendingJournalMetadata = ({
  latestBlock,
  workerInput,
  blockEndTimeMs,
  expectedRoots,
  expectedCounts,
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly workerInput: WorkerInput;
  readonly blockEndTimeMs: number;
  readonly expectedRoots: PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata["expectedRoots"];
  readonly expectedCounts: PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata["expectedCounts"];
}): Effect.Effect<
  PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata,
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | DatabaseError,
  never
> =>
  Effect.gen(function* () {
    const stateQueueLeaseToken =
      yield* requireStateQueueLeaseToken(workerInput);
    const serializedBase = yield* serializeStateQueueUTxO(latestBlock);
    const baseHeaderHash = yield* stateQueueBaseHeaderHash(latestBlock);
    const baseTailOutRef = stateQueueOutRef(latestBlock);
    return {
      stateQueueLeaseToken,
      baseSnapshotId:
        workerInput.data.baseSnapshotId ??
        `worker:${baseTailOutRef}:${workerInput.data.currentBlockStartTimeMs}:${blockEndTimeMs}`,
      baseTailOutRef,
      baseTailHeaderHash: Buffer.from(fromHex(baseHeaderHash)),
      baseTailDatumCbor: serializedBase.datum,
      baseRoots: expectedRoots,
      blockStartTime: new Date(workerInput.data.currentBlockStartTimeMs),
      expectedRoots,
      expectedCounts,
    };
  });

const requireStateQueueLeaseToken = (
  workerInput: WorkerInput,
): Effect.Effect<string, DatabaseError> =>
  Effect.gen(function* () {
    const token = workerInput.data.stateQueueLeaseToken;
    if (token === undefined || token.length <= 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: StateQueueMutationLeasesDB.tableName,
          message:
            "Refusing to build or submit a block commitment without a state-queue mutation lease token",
          cause: `base_snapshot_id=${workerInput.data.baseSnapshotId ?? "missing"}`,
        }),
      );
    }
    return token;
  });

export const assertLiveTailCommitBase = (
  contracts: SDK.MidgardValidators,
  expectedTail: SDK.StateQueueUTxO,
): Effect.Effect<void, SDK.LucidError | SDK.StateQueueError, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const liveTail = yield* fetchLatestCommittedBlockLocal(lucid.api, {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    });
    const expectedOutRef = stateQueueOutRef(expectedTail);
    const liveOutRef = stateQueueOutRef(liveTail);
    if (expectedOutRef !== liveOutRef) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `expected_tail=${expectedOutRef},live_tail=${liveOutRef}`,
        }),
      );
    }
  });

export const assertPendingJournalCompleteness = ({
  utxoRoot,
  utxoMemberCount,
  txRoot,
  emptyTxRoot,
  txMemberCount,
  depositsRoot,
  depositMemberCount,
  forcedTransactionsRoot,
  forcedTransactionMemberCount,
  withdrawalsRoot,
  withdrawalMemberCount,
  transitionTraceRoot,
  transitionTraceMemberCount,
  eventToStepRoot,
  eventToStepMemberCount,
}: {
  readonly utxoRoot: string;
  readonly utxoMemberCount: number;
  readonly txRoot: string;
  readonly emptyTxRoot: string;
  readonly txMemberCount: number;
  readonly depositsRoot: string;
  readonly depositMemberCount: number;
  readonly forcedTransactionsRoot: string;
  readonly forcedTransactionMemberCount: number;
  readonly withdrawalsRoot: string;
  readonly withdrawalMemberCount: number;
  readonly transitionTraceRoot: string;
  readonly transitionTraceMemberCount: number;
  readonly eventToStepRoot: string;
  readonly eventToStepMemberCount: number;
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    if (utxoRoot !== emptyTxRoot && utxoMemberCount <= 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty UTxO root would have no pending journal UTxO members",
          cause: `utxo_root=${utxoRoot}`,
        }),
      );
    }
    if (txRoot !== emptyTxRoot && txMemberCount <= 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty transaction root would have no pending journal tx members",
          cause: `tx_root=${txRoot}`,
        }),
      );
    }
    if (
      depositsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      depositMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty deposit root would have no pending journal deposit members",
          cause: `deposits_root=${depositsRoot}`,
        }),
      );
    }
    if (
      forcedTransactionsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      forcedTransactionMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty forced transaction root would have no pending journal forced transaction members",
          cause: `forced_transactions_root=${forcedTransactionsRoot}`,
        }),
      );
    }
    if (
      withdrawalsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      withdrawalMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty withdrawal root would have no pending journal withdrawal members",
          cause: `withdrawals_root=${withdrawalsRoot}`,
        }),
      );
    }
    if (
      transitionTraceRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      transitionTraceMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty transition trace root would have no pending journal trace members",
          cause: `transition_trace_root=${transitionTraceRoot}`,
        }),
      );
    }
    if (
      eventToStepRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      eventToStepMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty event-to-step root would have no pending journal event-to-step members",
          cause: `event_to_step_root=${eventToStepRoot}`,
        }),
      );
    }
  });

export const revalidateStateQueueLease = (
  workerInput: WorkerInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const token = yield* requireStateQueueLeaseToken(workerInput);
    yield* StateQueueMutationLeasesDB.revalidate(token);
  });
