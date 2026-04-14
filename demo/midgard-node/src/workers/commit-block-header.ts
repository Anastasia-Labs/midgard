/**
 * Production block-commit worker entrypoint.
 * This module orchestrates mempool trie processing, commit transaction
 * assembly, submission, and recovery by composing the smaller worker helpers.
 */
import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Data, Effect, Option, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  deserializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";
import {
  ConfigError,
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
  DatabaseInitializationError,
} from "@/services/index.js";
import {
  MempoolDB,
  ProcessedMempoolDB,
  DepositsDB,
  PendingBlockFinalizationsDB,
  TxUtils as TxTable,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  CML,
  TxBuilder,
  Data as LucidData,
  UTxO,
  fromHex,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  MidgardMpt,
  MptError,
  emptyRootHexProgram,
  keyValueMptRoot,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import { FileSystemError } from "@/utils.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import {
  rootsMatchConfirmedHeader,
  shouldAttemptLocalFinalizationRecovery,
  shouldDeferCommitSubmission,
  selectCommitRoots,
} from "@/workers/utils/commit-block-planner.js";
import { buildDeterministicCommitTxBuilder } from "@/workers/utils/commit-tx-builder.js";
import {
  failedSubmissionProgram,
  recoverSubmittedTxHashByHeaderProgram,
  skippedSubmissionProgram,
  successfulLocalFinalizationRecoveryProgram,
} from "@/workers/utils/commit-submission.js";
import { resolveAlignedCommitEndTime } from "@/workers/utils/commit-end-time.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import {
  fetchRealStateQueueWitnessContext,
  type RealStateQueueWitnessContext,
} from "@/workers/utils/scheduler-refresh.js";
import {
  isPotentiallyStaleOperatorWalletViewError,
  reloadOperatorWalletView,
  type OperatorWalletView,
} from "@/operator-wallet-view.js";
import { DatabaseError } from "@/database/utils/common.js";
import { formatUnknownError } from "@/error-format.js";

const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const ACTIVE_OPERATOR_MATURITY_DURATION_MS = 30n;
const COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES = 1;
const ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA = LucidData.Enum([
  LucidData.Object({
    Some: LucidData.Tuple([LucidData.Integer()]),
  }),
  LucidData.Literal("None"),
]);
const ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  bond_unlock_time: ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA,
});
const ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA = LucidData.Nullable(
  LucidData.Integer(),
);

type ActiveOperatorDatumEncoding = "aiken" | "record" | "option";

const decodeActiveOperatorDatum = (
  data: SDK.NodeDatum["data"],
): {
  readonly encoding: ActiveOperatorDatumEncoding;
  readonly commitmentTime: bigint | null;
} => {
  try {
    const parsedAiken = LucidData.castFrom(
      data as never,
      ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
    ) as { bond_unlock_time: bigint | null };
    return {
      encoding: "aiken",
      commitmentTime:
        parsedAiken.bond_unlock_time === null
          ? null
          : BigInt(parsedAiken.bond_unlock_time),
    };
  } catch {
    // Fall through to legacy encodings.
  }
  try {
    const parsedRecord = LucidData.castFrom(data, SDK.ActiveOperatorDatum);
    return {
      encoding: "record",
      commitmentTime:
        parsedRecord.commitmentTime === null
          ? null
          : BigInt(parsedRecord.commitmentTime),
    };
  } catch {
    const parsedOption = LucidData.castFrom(
      data as never,
      ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA as never,
    ) as bigint | null;
    return {
      encoding: "option",
      commitmentTime: parsedOption,
    };
  }
};

const encodeActiveOperatorDatum = (
  encoding: ActiveOperatorDatumEncoding,
  commitmentTime: bigint | null,
) => {
  switch (encoding) {
    case "aiken":
      return LucidData.castTo(
        { bond_unlock_time: commitmentTime } as never,
        ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
      );
    case "record":
      return LucidData.castTo({ commitmentTime }, SDK.ActiveOperatorDatum);
    case "option":
      return LucidData.castTo(
        commitmentTime as never,
        ACTIVE_OPERATOR_DATUM_OPTION_SCHEMA as never,
      );
  }
};

class LocalFinalizationPendingError extends Data.TaggedError(
  "LocalFinalizationPendingError",
)<{
  readonly submittedTxHash: string;
  readonly txSize: number;
  readonly mempoolTxsCount: number;
  readonly sizeOfBlocksTxs: number;
  readonly blockEndTimeMs: number;
  readonly cause: unknown;
}> {}

class CommitWorkerInvariantError extends Data.TaggedError(
  "CommitWorkerInvariantError",
)<{
  readonly message: string;
}> {}

const provideCommitBlockWorkerServices = <A, E>(
  effect: Effect.Effect<A, E, MidgardContracts | Database | Lucid | NodeConfig>,
): Effect.Effect<A, E | ConfigError | DatabaseInitializationError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContracts.Default),
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

// The sibling SDK is built in its own TypeScript program, so its exported
// `Effect` values carry a distinct branded generator identity during DTS emit.
// Normalize the few SDK helpers we yield from inside this worker onto the local
// `effect` type graph once at the boundary.
const localizeSdkEffect = <A, E, R = never>(
  effect: unknown,
): Effect.Effect<A, E, R> => effect as Effect.Effect<A, E, R>;

const getConfirmedStateFromStateQueueDatumLocal = (
  nodeDatum: SDK.StateQueueDatum,
): Effect.Effect<
  { readonly data: SDK.ConfirmedState; readonly link: unknown },
  SDK.DataCoercionError
> => localizeSdkEffect(SDK.getConfirmedStateFromStateQueueDatum(nodeDatum));

const getHeaderFromStateQueueDatumLocal = (
  nodeDatum: SDK.StateQueueDatum,
): Effect.Effect<SDK.Header, SDK.DataCoercionError> =>
  localizeSdkEffect(SDK.getHeaderFromStateQueueDatum(nodeDatum));

const hashBlockHeaderLocal = (
  header: SDK.Header,
): Effect.Effect<string, SDK.HashingError> =>
  localizeSdkEffect(SDK.hashBlockHeader(header));

const updateLatestBlocksDatumAndGetTheNewHeaderLocal = (
  lucid: Parameters<
    typeof SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram
  >[0],
  latestBlocksDatum: SDK.StateQueueDatum,
  newUTxOsRoot: string,
  transactionsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  endTime: bigint,
): Effect.Effect<
  { readonly nodeDatum: SDK.StateQueueDatum; readonly header: SDK.Header },
  SDK.DataCoercionError | SDK.LucidError | SDK.HashingError
> =>
  localizeSdkEffect(
    SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      latestBlocksDatum,
      newUTxOsRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
      endTime,
    ),
  );

const getLatestBlockDatumEndTime = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<Date, SDK.DataCoercionError> =>
  latestBlocksDatum.key === "Empty"
    ? getConfirmedStateFromStateQueueDatumLocal(latestBlocksDatum).pipe(
        Effect.map(
          ({ data: confirmedState }) =>
            new Date(Number(confirmedState.endTime)),
        ),
      )
    : getHeaderFromStateQueueDatumLocal(latestBlocksDatum).pipe(
        Effect.map((latestHeader) => new Date(Number(latestHeader.endTime))),
      );

const getLatestBlockHeaderRoots = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<
  Option.Option<{
    readonly utxoRoot: string;
    readonly txRoot: string;
  }>,
  SDK.DataCoercionError
> =>
  latestBlocksDatum.key === "Empty"
    ? Effect.succeed(Option.none())
    : getHeaderFromStateQueueDatumLocal(latestBlocksDatum).pipe(
        Effect.map((latestHeader) =>
          Option.some({
            utxoRoot: latestHeader.utxosRoot,
            txRoot: latestHeader.transactionsRoot,
          }),
        ),
      );

const establishEndTimeFromTxRequests = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
): Effect.Effect<Option.Option<Date>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length <= 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
      );

      const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;

      if (processedMempoolTxs.length <= 0) {
        // No transaction requests are available for inclusion in a block. By
        // setting `endTime` to `undefined` here, the code below can decide
        // whether it can stop if no
        return Option.none();
      } else {
        // No new transactions received, but there are uncommitted transactions
        // in the MPT. So its root must be used to submit a new block, and if
        // successful, `ProcessedMempoolDB` must be cleared. Following functions
        // should work fine with 0 mempool txs.
        return Option.some(processedMempoolTxs[0][TxColumns.TIMESTAMPTZ]);
      }
    } else {
      yield* Effect.logInfo(`🔹 ${mempoolTxs.length} retrieved.`);
      return Option.some(mempoolTxs[0][TxColumns.TIMESTAMPTZ]);
    }
  });

const resolveDepositsRoot = (
  depositEntries: readonly DepositsDB.Entry[],
): Effect.Effect<Option.Option<string>, MptError, never> =>
  Effect.gen(function* () {
    if (depositEntries.length <= 0) {
      return Option.none();
    }
    const eventIds = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.ID],
    );
    const eventInfos = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.INFO],
    );
    const root = yield* keyValueMptRoot(eventIds, eventInfos);
    return Option.some(root);
  });

const COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS = 4;

const buildUnsignedTx = (
  contracts: SDK.MidgardValidators,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  endDate: Date,
  initialOperatorWalletView?: OperatorWalletView,
) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const stateQueueAuthValidator = contracts.stateQueue;
    const latestEndTime = Number(
      (yield* getLatestBlockDatumEndTime(latestBlock.datum)).getTime(),
    );
    // The worker's Lucid service starts without a selected wallet. Select the
    // operator wallet before any scheduler refresh or witness lookup that
    // depends on wallet address or spendable operator inputs.
    yield* lucid.switchToOperatorsMainWallet;
    /**
     * Resolves the commit window that should be used for the current block-header attempt.
     */
    const resolveCommitWindow = () =>
      resolveAlignedCommitEndTime({
        lucid: lucid.api,
        latestEndTime,
        candidateEndTime: endDate.getTime(),
      });
    let commitWindow = resolveCommitWindow();
    let witnessContext: RealStateQueueWitnessContext | undefined;
    let stabilizationAttempts = 0;
    while (stabilizationAttempts < COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS) {
      witnessContext = yield* fetchRealStateQueueWitnessContext(
        lucid.api,
        contracts,
        commitWindow.resolvedEndTime,
        witnessContext?.operatorWalletView ?? initialOperatorWalletView,
      );
      const refreshedCommitWindow = resolveCommitWindow();
      if (
        refreshedCommitWindow.resolvedEndTime === commitWindow.resolvedEndTime
      ) {
        commitWindow = refreshedCommitWindow;
        break;
      }
      stabilizationAttempts += 1;
      yield* Effect.logWarning(
        `Commit end-time advanced while preparing scheduler-aligned witness context; rebuilding with refreshed window (previous=${commitWindow.resolvedEndTime}, next=${refreshedCommitWindow.resolvedEndTime}, candidate=${refreshedCommitWindow.alignedCandidateEndTime}, latestEnd=${latestEndTime}, attempt=${stabilizationAttempts}/${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}).`,
      );
      commitWindow = refreshedCommitWindow;
    }
    if (witnessContext === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to stabilize the commit window before building the block commitment transaction",
          cause: `attempts=${COMMIT_WINDOW_STABILIZATION_MAX_ATTEMPTS}`,
        }),
      );
    }
    const {
      alignedCandidateEndTime,
      minimumMonotonicEndTime,
      resolvedEndTime: alignedEndTime,
    } = commitWindow;
    if (alignedEndTime !== alignedCandidateEndTime) {
      yield* Effect.logWarning(
        `Adjusted commit end-time to maintain monotonic header timing (candidate=${alignedCandidateEndTime}, minimum=${minimumMonotonicEndTime}, selected=${alignedEndTime}, latestEnd=${latestEndTime}).`,
      );
    }
    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* updateLatestBlocksDatumAndGetTheNewHeaderLocal(
        lucid.api,
        latestBlock.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        "00".repeat(32),
        BigInt(alignedEndTime),
      );

    const newHeaderHash = yield* hashBlockHeaderLocal(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);
    yield* Effect.logInfo(
      "🔹 Building commitment with real state_queue witness context.",
    );

    yield* Effect.logInfo("🔹 Building block commitment transaction...");

    const headerNodeUnit = toUnit(
      stateQueueAuthValidator.policyId,
      SDK.NODE_ASSET_NAME + newHeaderHash,
    );
    const commitMintAssets = {
      [headerNodeUnit]: 1n,
    };
    const headerNodeOutputAssets = {
      lovelace: STATE_QUEUE_HEADER_NODE_LOVELACE,
      ...commitMintAssets,
    };
    const appendedNodeDatum: SDK.StateQueueDatum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: LucidData.castTo(newHeader, SDK.Header),
    };
    const appendedNodeDatumCbor = LucidData.to(
      appendedNodeDatum,
      SDK.StateQueueDatum,
    );
    const updatedNodeDatumCbor = LucidData.to(
      updatedNodeDatum,
      SDK.StateQueueDatum,
    );
    const updatedActiveOperatorDatumCbor = yield* Effect.try({
      try: () => {
        const activeOperatorNodeDatum = LucidData.from(
          witnessContext.activeOperatorInput.datum,
          SDK.NodeDatum,
        );
        const activeOperatorDatum = decodeActiveOperatorDatum(
          activeOperatorNodeDatum.data,
        );
        const updatedActiveOperatorNodeDatum: SDK.NodeDatum = {
          ...activeOperatorNodeDatum,
          data: encodeActiveOperatorDatum(
            activeOperatorDatum.encoding,
            BigInt(alignedEndTime) + ACTIVE_OPERATOR_MATURITY_DURATION_MS,
          ),
        };
        return LucidData.to(updatedActiveOperatorNodeDatum, SDK.NodeDatum);
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to update active-operator bond-hold datum for commit tx",
          cause,
        }),
    });

    /**
     * Builds the shared transaction skeleton for a block-header commit attempt.
     */
    const makeBaseCommitTx = () =>
      lucid.api
        .newTx()
        .validTo(alignedEndTime)
        .collectFrom([latestBlock.utxo], LucidData.void())
        .pay.ToContract(
          stateQueueAuthValidator.spendingScriptAddress,
          {
            kind: "inline",
            value: LucidData.to(appendedNodeDatum, SDK.StateQueueDatum),
          },
          headerNodeOutputAssets,
        )
        .pay.ToContract(
          stateQueueAuthValidator.spendingScriptAddress,
          {
            kind: "inline",
            value: LucidData.to(updatedNodeDatum, SDK.StateQueueDatum),
          },
          latestBlock.utxo.assets,
        );

    const txBuilder = yield* buildDeterministicCommitTxBuilder({
      lucid: lucid.api,
      contracts,
      latestBlockInput: latestBlock.utxo,
      witness: witnessContext,
      headerNodeUnit,
      appendedNodeDatumCbor,
      previousHeaderNodeDatumCbor: updatedNodeDatumCbor,
      updatedActiveOperatorDatumCbor,
      commitMintAssets,
      makeBaseCommitTx,
    });

    const txSize = txBuilder.toCBOR().length / 2;
    yield* Effect.logInfo(`🔹 Transaction built successfully. Size: ${txSize}`);

    const signAndSubmitProgram = handleSignSubmitNoConfirmation(
      lucid.api,
      txBuilder,
    ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

    return {
      newHeaderHash,
      signAndSubmitProgram,
      txSize,
    };
  });

const toLocalFinalizationPendingError = ({
  submittedTxHash,
  txSize,
  currentBlockMempoolTxsCount,
  sizeOfProcessedTxs,
  blockEndTimeMs,
  workerInput,
  cause,
}: {
  readonly submittedTxHash: string;
  readonly txSize: number;
  readonly currentBlockMempoolTxsCount: number;
  readonly sizeOfProcessedTxs: number;
  readonly blockEndTimeMs: number;
  readonly workerInput: WorkerInput;
  readonly cause: unknown;
}) =>
  new LocalFinalizationPendingError({
    submittedTxHash,
    txSize,
    mempoolTxsCount:
      currentBlockMempoolTxsCount + workerInput.data.mempoolTxsCountSoFar,
    sizeOfBlocksTxs:
      sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
    blockEndTimeMs,
    cause,
  });

const submitDepositOnlyCommit = ({
  contracts,
  latestBlock,
  endTime,
  includedDepositEntries,
  includedDepositEventIds,
  workerInput,
  utxoRoot,
  txRoot,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly workerInput: WorkerInput;
  readonly utxoRoot: string;
  readonly txRoot: string;
}) =>
  Effect.gen(function* () {
    const optDepositsRoot = yield* resolveDepositsRoot(includedDepositEntries);
    if (Option.isNone(optDepositsRoot)) {
      yield* Effect.logInfo("🔹 Nothing to commit.");
      return {
        type: "NothingToCommitOutput",
      } as WorkerOutput;
    }

    const depositsRoot = optDepositsRoot.value;
    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
    ) =>
      Effect.succeed({
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash,
        txSize,
        mempoolTxsCount: workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs: endTime.getTime(),
      } satisfies WorkerOutput);
    const emptyRoot = yield* emptyRootHexProgram;
    const roots = selectCommitRoots({
      hasTxRequests: false,
      computedUtxoRoot: utxoRoot,
      computedTxRoot: txRoot,
      emptyRoot,
    });
    const { newHeaderHash, signAndSubmitProgram, txSize } =
      yield* buildUnsignedTx(
        contracts,
        latestBlock,
        roots.utxoRoot,
        roots.txRoot,
        depositsRoot,
        endTime,
      );
    const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
    yield* PendingBlockFinalizationsDB.preparePendingSubmission({
      headerHash: headerHashBuffer,
      blockEndTime: endTime,
      depositEventIds: includedDepositEventIds,
      mempoolTxIds: [],
    });

    return yield* Effect.matchEffect(signAndSubmitProgram, {
      onFailure: (error) =>
        Effect.gen(function* () {
          yield* PendingBlockFinalizationsDB.markAbandoned(
            headerHashBuffer,
          ).pipe(Effect.catchAll(() => Effect.void));
          const detail = formatUnknownError(error);
          yield* Effect.logError(
            `🔹 Deposit-only commit submission failed: ${detail}`,
          );
          return {
            type: "FailureOutput",
            error: `Deposit-only commit submission failed: ${detail}`,
          } satisfies WorkerOutput;
        }),
      onSuccess: (txHash) =>
        PendingBlockFinalizationsDB.markSubmitted(
          headerHashBuffer,
          Buffer.from(fromHex(txHash)),
        ).pipe(
          Effect.andThen(submittedAwaitingConfirmationOutput(txHash, txSize)),
        ),
    });
  });

const submitTxBackedCommit = ({
  contracts,
  latestBlock,
  endTime,
  includedDepositEntries,
  includedDepositEventIds,
  utxoRoot,
  txRoot,
  mempoolTrie,
  processedMempoolTxs,
  mempoolTxHashes,
  workerInput,
  sizeOfProcessedTxs,
}: {
  readonly contracts: SDK.MidgardValidators;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly mempoolTrie: MidgardMpt;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const optDepositsRoot = yield* resolveDepositsRoot(includedDepositEntries);
    const depositsRoot = Option.isSome(optDepositsRoot)
      ? optDepositsRoot.value
      : yield* emptyRootHexProgram;
    const currentBlockMempoolTxsCount = processedMempoolTxs.length;
    const blockEndTimeMs = endTime.getTime();
    const submittedAwaitingConfirmationOutput = (
      submittedTxHash: string,
      txSize: number,
    ) =>
      Effect.succeed({
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash,
        txSize,
        mempoolTxsCount:
          currentBlockMempoolTxsCount + workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs:
          sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs,
      } satisfies WorkerOutput);

    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);

    /**
     * Submits a prepared block-header commit transaction and waits for the resulting status.
     */
    const submitCommitAttempt = (
      initialOperatorWalletView?: OperatorWalletView,
    ) =>
      buildUnsignedTx(
        contracts,
        latestBlock,
        utxoRoot,
        txRoot,
        depositsRoot,
        endTime,
        initialOperatorWalletView,
      ).pipe(
        Effect.flatMap(({ newHeaderHash, signAndSubmitProgram, txSize }) => {
          const headerHashBuffer = Buffer.from(fromHex(newHeaderHash));
          return PendingBlockFinalizationsDB.preparePendingSubmission({
            headerHash: headerHashBuffer,
            blockEndTime: endTime,
            depositEventIds: includedDepositEventIds,
            mempoolTxIds: processedMempoolTxs.map(
              (entry) => entry[TxColumns.TX_ID],
            ),
          }).pipe(
            Effect.andThen(
              Effect.matchEffect(signAndSubmitProgram, {
                onFailure: (error) => {
                  if (error instanceof TxSignError) {
                    return Effect.gen(function* () {
                      yield* PendingBlockFinalizationsDB.markAbandoned(
                        headerHashBuffer,
                      ).pipe(Effect.catchAll(() => Effect.void));
                      const detail = formatUnknownError(error);
                      yield* Effect.logError(
                        `🔹 Commit signing failed: ${detail}`,
                      );
                      return {
                        type: "FailureOutput",
                        error: `Commit signing failed: ${detail}`,
                      } satisfies WorkerOutput;
                    });
                  }

                  return Effect.gen(function* () {
                    const recoveredTxHash =
                      yield* recoverSubmittedTxHashByHeaderProgram(
                        contracts.stateQueue,
                        newHeaderHash,
                      );
                    if (Option.isSome(recoveredTxHash)) {
                      return yield* PendingBlockFinalizationsDB.markSubmitted(
                        headerHashBuffer,
                        Buffer.from(fromHex(recoveredTxHash.value)),
                      ).pipe(
                        Effect.andThen(
                          submittedAwaitingConfirmationOutput(
                            recoveredTxHash.value,
                            txSize,
                          ),
                        ),
                      );
                    }

                    yield* PendingBlockFinalizationsDB.markAbandoned(
                      headerHashBuffer,
                    ).pipe(Effect.catchAll(() => Effect.void));
                    const transferResult = yield* Effect.either(
                      skippedSubmissionProgram(
                        processedMempoolTxs,
                        mempoolTxHashes,
                      ),
                    );
                    if (transferResult._tag === "Left") {
                      const detail = formatUnknownError(transferResult.left);
                      yield* Effect.logError(
                        `🔹 Commit submission failed and deferred transfer failed: submit=${formatUnknownError(
                          error,
                        )}; transfer=${detail}`,
                      );
                      return {
                        type: "FailureOutput",
                        error: `Commit submission failed and deferred transfer failed: submit=${formatUnknownError(
                          error,
                        )}; transfer=${detail}`,
                      } satisfies WorkerOutput;
                    }

                    return yield* failedSubmissionProgram(
                      mempoolTrie,
                      currentBlockMempoolTxsCount,
                      sizeOfProcessedTxs,
                      error,
                    );
                  });
                },
                onSuccess: (txHash) =>
                  PendingBlockFinalizationsDB.markSubmitted(
                    headerHashBuffer,
                    Buffer.from(fromHex(txHash)),
                  ).pipe(
                    Effect.andThen(
                      submittedAwaitingConfirmationOutput(txHash, txSize),
                    ),
                  ),
              }),
            ),
          );
        }),
      );

    let lastResult = yield* Effect.either(submitCommitAttempt());
    let retryCount = 0;
    while (
      lastResult._tag === "Left" &&
      lastResult.left instanceof TxSubmitError &&
      isPotentiallyStaleOperatorWalletViewError(lastResult.left) &&
      retryCount < COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES
    ) {
      retryCount += 1;
      yield* lucid.switchToOperatorsMainWallet;
      const reloadedOperatorWalletView = yield* Effect.tryPromise({
        try: () => reloadOperatorWalletView(lucid.api),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to reload operator wallet view after stale commit submission",
            cause,
          }),
      });
      yield* Effect.logWarning(
        `Commit submission hit a stale operator-wallet input class error; reloading wallet view and rebuilding (attempt=${retryCount}/${COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES}).`,
      );
      lastResult = yield* Effect.either(
        submitCommitAttempt(reloadedOperatorWalletView),
      );
    }

    if (lastResult._tag === "Left") {
      return yield* Effect.fail(lastResult.left);
    }
    return lastResult.right;
  });

const deferProcessedCommitPayloadUntilConfirmation = ({
  processedMempoolTxs,
  mempoolTxHashes,
  mempoolTxsCount,
  sizeOfProcessedTxs,
}: {
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxsCount: number;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 No confirmed blocks available. Transferring to ProcessedMempoolDB...",
    );
    const transferResult = yield* Effect.either(
      skippedSubmissionProgram(processedMempoolTxs, mempoolTxHashes),
    );
    if (transferResult._tag === "Left") {
      const detail = formatUnknownError(transferResult.left);
      yield* Effect.logError(
        `🔹 Failed to defer processed txs while waiting for confirmation: ${detail}`,
      );
      return {
        type: "FailureOutput",
        error: `Failed to transfer deferred commit payload to ProcessedMempoolDB: ${detail}`,
      } satisfies WorkerOutput;
    }
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    } satisfies WorkerOutput;
  });

const recoverLocalFinalizationAgainstConfirmedBlock = ({
  latestBlock,
  utxoRoot,
  txRoot,
  mempoolTrie,
  processedMempoolTxs,
  mempoolTxHashes,
  includedDepositEventIds,
  workerInput,
  sizeOfProcessedTxs,
}: {
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly mempoolTrie: MidgardMpt;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly workerInput: WorkerInput;
  readonly sizeOfProcessedTxs: number;
}) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔹 Attempting local finalization recovery against confirmed block roots...",
    );
    const optHeaderRoots = yield* getLatestBlockHeaderRoots(latestBlock.datum);
    if (Option.isNone(optHeaderRoots)) {
      return {
        type: "FailureOutput",
        error:
          "Confirmed block datum does not contain a recoverable header for local finalization",
      } satisfies WorkerOutput;
    }
    const rootsMatch = rootsMatchConfirmedHeader({
      computedUtxoRoot: utxoRoot,
      computedTxRoot: txRoot,
      confirmedUtxoRoot: optHeaderRoots.value.utxoRoot,
      confirmedTxRoot: optHeaderRoots.value.txRoot,
    });
    if (!rootsMatch) {
      return {
        type: "FailureOutput",
        error:
          "Local finalization recovery aborted: computed roots do not match the confirmed block header",
      } satisfies WorkerOutput;
    }
    const confirmedHeader = yield* getHeaderFromStateQueueDatumLocal(
      latestBlock.datum,
    );
    const confirmedHeaderHash = yield* hashBlockHeaderLocal(confirmedHeader);
    return yield* successfulLocalFinalizationRecoveryProgram(
      mempoolTrie,
      processedMempoolTxs,
      mempoolTxHashes,
      includedDepositEventIds,
      confirmedHeaderHash,
      workerInput,
      sizeOfProcessedTxs,
    );
  });

const databaseOperationsProgram = (
  workerInput: WorkerInput,
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const processedPendingTxs =
      mempoolTxs.length > 0 ? [] : yield* ProcessedMempoolDB.retrieve;
    const depositIngestionBarrierTime =
      yield* fetchAndInsertDepositUTxOsForCommitBarrier(new Date());
    const depositOnlyEndTime = depositIngestionBarrierTime;

    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const availableLocalFinalizationBlock =
      workerInput.data.availableLocalFinalizationBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    const hasAvailableLocalFinalizationBlock =
      availableLocalFinalizationBlock !== "";
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock: workerInput.data.localFinalizationPending
          ? hasAvailableLocalFinalizationBlock
          : hasAvailableConfirmedBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no recoverable confirmed block is available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const {
      utxoRoot,
      txRoot,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
    } = yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs, {
      currentBlockStartTime:
        availableConfirmedBlock !== "" &&
        !workerInput.data.localFinalizationPending
          ? currentBlockStartTime
          : undefined,
      processedOnlyEndTime: processedPendingTxs[0]?.[TxColumns.TIMESTAMPTZ],
      depositVisibilityBarrierTime:
        availableConfirmedBlock !== "" &&
        !workerInput.data.localFinalizationPending
          ? depositIngestionBarrierTime
          : undefined,
      depositOnlyEndTime:
        availableConfirmedBlock !== "" &&
        !workerInput.data.localFinalizationPending
          ? depositOnlyEndTime
          : undefined,
    });

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }
    if (includedDepositEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedDepositEntriesCount} due deposit UTxO(s).`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime: Option.Option<Date> =
      yield* establishEndTimeFromTxRequests(processedMempoolTxs);

    const contracts = yield* MidgardContracts;

    if (availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      return yield* deferProcessedCommitPayloadUntilConfirmation({
        processedMempoolTxs,
        mempoolTxHashes,
        mempoolTxsCount,
        sizeOfProcessedTxs,
      });
    } else {
      yield* Effect.logInfo(
        "🔹 Previous submitted block is now confirmed, deserializing...",
      );
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );

      if (
        shouldAttemptLocalFinalizationRecovery({
          localFinalizationPending: workerInput.data.localFinalizationPending,
          hasAvailableConfirmedBlock: hasAvailableLocalFinalizationBlock,
        })
      ) {
        if (!hasAvailableLocalFinalizationBlock) {
          return {
            type: "NothingToCommitOutput",
          } satisfies WorkerOutput;
        }
        const recoverableConfirmedBlock = yield* deserializeStateQueueUTxO(
          availableLocalFinalizationBlock,
        );
        return yield* recoverLocalFinalizationAgainstConfirmedBlock({
          latestBlock: recoverableConfirmedBlock,
          utxoRoot,
          txRoot,
          mempoolTrie,
          processedMempoolTxs,
          mempoolTxHashes,
          includedDepositEventIds,
          workerInput,
          sizeOfProcessedTxs,
        });
      }

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        return yield* submitDepositOnlyCommit({
          contracts,
          latestBlock,
          endTime: depositOnlyEndTime,
          includedDepositEntries,
          includedDepositEventIds,
          workerInput,
          utxoRoot,
          txRoot,
        });
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        return yield* submitTxBackedCommit({
          contracts,
          latestBlock,
          endTime,
          includedDepositEntries,
          includedDepositEventIds,
          utxoRoot,
          txRoot,
          mempoolTrie,
          processedMempoolTxs,
          mempoolTxHashes,
          workerInput,
          sizeOfProcessedTxs,
        });
      }
    }
  });

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const { ledgerTrie, mempoolTrie } = yield* makeMpts;
    const closeTries = Effect.all(
      [
        ledgerTrie.close().pipe(Effect.catchAll(() => Effect.void)),
        mempoolTrie.close().pipe(Effect.catchAll(() => Effect.void)),
      ],
      { discard: true },
    );

    const result = yield* withTrieTransaction(
      ledgerTrie,
      databaseOperationsProgram(workerInput, ledgerTrie, mempoolTrie),
    ).pipe(
      Effect.catchAll((error) =>
        error instanceof LocalFinalizationPendingError
          ? Effect.succeed({
              type: "SubmittedAwaitingLocalFinalizationOutput",
              submittedTxHash: error.submittedTxHash,
              txSize: error.txSize,
              mempoolTxsCount: error.mempoolTxsCount,
              sizeOfBlocksTxs: error.sizeOfBlocksTxs,
              blockEndTimeMs: error.blockEndTimeMs,
              error: formatUnknownError(error.cause),
            } as WorkerOutput)
          : Effect.fail(error),
      ),
      Effect.ensuring(closeTries),
    );
    if (result === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Block commitment worker completed without producing a worker output",
        }),
      );
    }
    return result;
  });

if (parentPort !== null) {
  const inputData = workerData as WorkerInput;

  const program = provideCommitBlockWorkerServices(
    runCommitBlockHeaderWorkerProgram(inputData),
  );

  Effect.runPromise(
    program.pipe(
      Effect.catchAllCause((cause) =>
        Effect.succeed({
          type: "FailureOutput",
          error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
        }),
      ),
    ),
  ).then((output) => {
    Effect.runSync(
      Effect.logInfo(
        `👷 Block commitment work completed (${JSON.stringify(output)}).`,
      ),
    );
    parentPort?.postMessage(output);
  });
}
