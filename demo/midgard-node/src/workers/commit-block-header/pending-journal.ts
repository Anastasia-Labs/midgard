import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import {
  type ContractDeploymentIdentityValue,
  type Database,
  Lucid,
} from "@/services/index.js";
import {
  serializeStateQueueUTxO,
  type WorkerInput,
} from "@/workers/utils/commit-block-header.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  computeUtxoPayloadRoot,
  type LedgerDelta,
  type UtxoPayloadEntry,
} from "@/workers/utils/mpf.js";

import {
  fetchExpectedStateQueueTailLocal,
  getConfirmedStateFromStateQueueDatumLocal,
  getHeaderV1FromStateQueueDatumLocal,
  stateQueueBaseHeaderHash,
  stateQueueOutRef,
} from "./state-queue.js";

const baseRootsFromStateQueueTail = (
  latestBlock: SDK.StateQueueUTxO,
  _consensusProfile: ContractDeploymentIdentityValue["consensusProfile"],
): Effect.Effect<
  PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata["baseRoots"],
  SDK.DataCoercionError,
  never
> =>
  Effect.gen(function* () {
    if (latestBlock.datum.key === "Empty") {
      const { data: confirmedState } =
        yield* getConfirmedStateFromStateQueueDatumLocal(latestBlock.datum);
      return {
        utxosRoot: confirmedState.utxoRoot,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      };
    }

    const header = yield* getHeaderV1FromStateQueueDatumLocal(
      latestBlock.datum,
    );
    return {
      utxosRoot: header.utxosRoot,
      forcedTransactionsRoot: header.forcedTransactionsRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
    };
  });

export const resolvePendingJournalLedgerState = ({
  recordedBaseUtxosRoot,
  selectedBaseUtxosRoot,
  expectedFinalUtxosRoot,
  expectedFinalEntryCount,
  implicitGenesisEntries,
  transitionDelta,
}: {
  readonly recordedBaseUtxosRoot: string;
  readonly selectedBaseUtxosRoot: string;
  readonly expectedFinalUtxosRoot: string;
  readonly expectedFinalEntryCount: number;
  readonly implicitGenesisEntries: readonly Ledger.MinimalEntry[];
  readonly transitionDelta: LedgerDelta;
}): Effect.Effect<
  {
    readonly ledgerDelta: LedgerDelta;
  },
  DatabaseError
> =>
  Effect.gen(function* () {
    if (recordedBaseUtxosRoot === selectedBaseUtxosRoot) {
      return { ledgerDelta: transitionDelta };
    }
    if (
      recordedBaseUtxosRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
      implicitGenesisEntries.length === 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to journal ledger state whose selected MPF base differs from the recorded state-queue base",
          cause: `recorded_base_root=${recordedBaseUtxosRoot},selected_base_root=${selectedBaseUtxosRoot},implicit_genesis_entries=${implicitGenesisEntries.length.toString()}`,
        }),
      );
    }
    const implicitGenesisRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      implicitGenesisEntries,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Failed to verify the implicit genesis ledger base before journaling",
            cause,
          }),
      ),
    );
    if (implicitGenesisRoot !== selectedBaseUtxosRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Implicit genesis entries do not reproduce the selected commit base root",
          cause: `implicit_genesis_root=${implicitGenesisRoot},selected_base_root=${selectedBaseUtxosRoot}`,
        }),
      );
    }

    const finalEntries = new Map<string, UtxoPayloadEntry>(
      implicitGenesisEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        {
          outref: Buffer.from(entry[Ledger.Columns.OUTREF]),
          output: Buffer.from(entry[Ledger.Columns.OUTPUT]),
        },
      ]),
    );
    for (const spent of transitionDelta.spent) {
      finalEntries.delete(spent.toString("hex"));
    }
    for (const produced of transitionDelta.produced) {
      finalEntries.set(produced.outref.toString("hex"), {
        outref: Buffer.from(produced.outref),
        output: Buffer.from(produced.output),
      });
    }
    const produced = [...finalEntries.values()];
    if (produced.length !== expectedFinalEntryCount) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "First-block V1 ledger delta has the wrong final UTxO count",
          cause: `actual=${produced.length.toString()},expected=${expectedFinalEntryCount.toString()}`,
        }),
      );
    }
    const recoveredRoot = yield* computeUtxoPayloadRoot(produced).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Failed to verify the first-block V1 ledger delta",
            cause,
          }),
      ),
    );
    if (recoveredRoot !== expectedFinalUtxosRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "First-block V1 ledger delta does not reproduce the expected UTxO root",
          cause: `recovered_root=${recoveredRoot},expected_root=${expectedFinalUtxosRoot}`,
        }),
      );
    }
    return {
      ledgerDelta: {
        spent: [],
        produced,
      },
    };
  });

export const buildPendingJournalMetadata = ({
  latestBlock,
  workerInput,
  blockEndTimeMs,
  expectedRoots,
  expectedCounts,
  consensusProfile,
  deploymentMarker,
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly workerInput: WorkerInput;
  readonly blockEndTimeMs: number;
  readonly expectedRoots: PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata["expectedRoots"];
  readonly expectedCounts: PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata["expectedCounts"];
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
  readonly deploymentMarker: NonNullable<
    ContractDeploymentIdentityValue["deploymentMarker"]
  >;
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
    const baseRoots = yield* baseRootsFromStateQueueTail(
      latestBlock,
      consensusProfile,
    );
    return {
      deploymentMarker,
      consensusProfileId: consensusProfile.profileId,
      stateQueueLeaseToken,
      baseSnapshotId:
        workerInput.data.baseSnapshotId ??
        `worker:${baseTailOutRef}:${workerInput.data.currentBlockStartTimeMs}:${blockEndTimeMs}`,
      baseTailOutRef,
      baseTailHeaderHash: Buffer.from(fromHex(baseHeaderHash)),
      baseTailDatumCbor: serializedBase.datum,
      baseRoots,
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
    const liveTail = yield* fetchExpectedStateQueueTailLocal(
      lucid.api,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      expectedTail,
    );
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

export const resolveLiveTailCommitBase = (
  contracts: SDK.MidgardValidators,
  expectedTail: SDK.StateQueueUTxO,
  _consensusProfile: ContractDeploymentIdentityValue["consensusProfile"],
): Effect.Effect<
  SDK.StateQueueUTxO,
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError,
  Lucid
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const liveTail = yield* fetchExpectedStateQueueTailLocal(
      lucid.api,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      expectedTail,
    );
    const expectedOutRef = stateQueueOutRef(expectedTail);
    const liveOutRef = stateQueueOutRef(liveTail);
    if (expectedOutRef === liveOutRef) {
      return expectedTail;
    }

    const expectedHeaderHash = yield* stateQueueBaseHeaderHash(expectedTail);
    const liveHeaderHash = yield* stateQueueBaseHeaderHash(liveTail);
    if (expectedHeaderHash === liveHeaderHash) {
      yield* Effect.logWarning(
        `🔹 Commit base tail was replaced without advancing the logical header; rebuilding on live tail ${liveOutRef} instead of stale tail ${expectedOutRef}.`,
      );
      return liveTail;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Commit base is stale; aborting block build before creating a pending journal",
        cause: `expected_tail=${expectedOutRef},live_tail=${liveOutRef},expected_header=${expectedHeaderHash},live_header=${liveHeaderHash}`,
      }),
    );
  });

export const assertPendingJournalCompleteness = ({
  utxoRoot,
  utxoMemberCount,
  hasLedgerDelta = false,
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
  validationTracesRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTraceMemberCount = 0,
  expectedValidationTraceCount = 0,
}: {
  readonly utxoRoot: string;
  readonly utxoMemberCount: number;
  readonly hasLedgerDelta?: boolean;
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
  readonly validationTracesRoot?: string;
  readonly validationTraceMemberCount?: number;
  readonly expectedValidationTraceCount?: number;
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    if (utxoRoot !== emptyTxRoot && utxoMemberCount <= 0 && !hasLedgerDelta) {
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
    if (
      validationTracesRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      validationTraceMemberCount <= 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because a non-empty validation traces root would have no pending journal validation descriptors",
          cause: `validation_traces_root=${validationTracesRoot}`,
        }),
      );
    }
    if (validationTraceMemberCount !== expectedValidationTraceCount) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to submit commit because validation trace descriptor membership does not match the header count",
          cause: `validation_trace_members=${validationTraceMemberCount.toString()},expected_validation_trace_count=${expectedValidationTraceCount.toString()}`,
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
