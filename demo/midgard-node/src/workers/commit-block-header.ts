import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Data, Effect, Match, Option, Schedule, pipe } from "effect";
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
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  ProcessedMempoolDB,
  DepositsDB,
  TxUtils as TxTable,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  CML,
  LucidEvolution,
  Network,
  TxBuilder,
  Data as LucidData,
  Script,
  UTxO,
  credentialToAddress,
  coreToTxOutput,
  fromHex,
  paymentCredentialOf,
  scriptHashToCredential,
  slotToUnixTime,
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
import { FileSystemError, batchProgram } from "@/utils.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import {
  buildSuccessfulCommitBatches,
  SuccessfulCommitBatch,
  rootsMatchConfirmedHeader,
  shouldAttemptLocalFinalizationRecovery,
  shouldDeferCommitSubmission,
  selectCommitRoots,
} from "@/workers/utils/commit-block-planner.js";
import {
  ActiveOperatorSpendRedeemerSchema,
  StateQueueCommitLayout,
  deriveStateQueueCommitLayout,
  enumerateCommitLayoutCandidates,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
} from "@/workers/utils/commit-redeemers.js";
import { resolveAlignedCommitEndTime } from "@/workers/utils/commit-end-time.js";
import {
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";
import { DatabaseError } from "@/database/utils/common.js";

const BATCH_SIZE = 100;
const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const SKIPPED_SUBMISSION_TRANSFER_RETRIES = 2;
const SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF = "250 millis";

class LocalFinalizationPendingError extends Data.TaggedError(
  "LocalFinalizationPendingError",
)<{
  readonly submittedTxHash: string;
  readonly txSize: number;
  readonly mempoolTxsCount: number;
  readonly sizeOfBlocksTxs: number;
  readonly cause: unknown;
}> {}

const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

const getLatestBlockDatumEndTime = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<Date, SDK.DataCoercionError, never> =>
  Effect.gen(function* () {
    let endTimeBigInt: bigint;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* SDK.getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = confirmedState.endTime;
    } else {
      const latestHeader =
        yield* SDK.getHeaderFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = latestHeader.endTime;
    }
    return new Date(Number(endTimeBigInt));
  });

const getLatestBlockHeaderRoots = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<
  Option.Option<{ readonly utxoRoot: string; readonly txRoot: string }>,
  SDK.DataCoercionError,
  never
> =>
  Effect.gen(function* () {
    if (latestBlocksDatum.key === "Empty") {
      return Option.none();
    }
    const latestHeader =
      yield* SDK.getHeaderFromStateQueueDatum(latestBlocksDatum);
    return Option.some({
      utxoRoot: latestHeader.utxosRoot,
      txRoot: latestHeader.transactionsRoot,
    });
  });

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

const finalizeCommittedBlockLocally = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
): Effect.Effect<void, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    const filterAlreadyCommittedTxs = (
      candidateBatches: readonly SuccessfulCommitBatch[],
    ): Effect.Effect<
      readonly SuccessfulCommitBatch[],
      DatabaseError,
      Database
    > =>
      Effect.gen(function* () {
        const candidateHashes = Array.from(
          new Set(
            candidateBatches
              .flatMap((batch) => batch.blockTxHashes)
              .map((hash) => hash.toString("hex")),
          ),
        ).map((hex) => Buffer.from(hex, "hex"));

        if (candidateHashes.length <= 0) {
          return candidateBatches;
        }

        const existing =
          yield* ImmutableDB.retrieveTxEntriesByHashes(candidateHashes);
        if (existing.length <= 0) {
          return candidateBatches;
        }

        const alreadyCommitted = new Set(
          existing.map((entry) => entry[TxColumns.TX_ID].toString("hex")),
        );
        yield* Effect.logWarning(
          `🔹 Filtering ${alreadyCommitted.size} already-committed tx id(s) from local finalization payload before BlocksDB insertion.`,
        );

        return candidateBatches.map((batch) => {
          const filteredTxs: TxTable.EntryWithTimeStamp[] = [];
          const filteredHashes: Buffer[] = [];

          for (let i = 0; i < batch.blockTxHashes.length; i += 1) {
            const txHash = batch.blockTxHashes[i];
            if (alreadyCommitted.has(txHash.toString("hex"))) {
              continue;
            }
            filteredHashes.push(txHash);
            if (i < batch.txsToInsertImmutable.length) {
              filteredTxs.push(batch.txsToInsertImmutable[i]);
            }
          }

          return {
            txsToInsertImmutable: filteredTxs,
            blockTxHashes: filteredHashes,
            clearMempoolTxHashes: batch.clearMempoolTxHashes,
          };
        });
      });

    const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

    const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;
    const batches = buildSuccessfulCommitBatches(
      mempoolTxs,
      mempoolTxHashes,
      processedMempoolTxs,
      Math.floor(BATCH_SIZE / 2),
    );
    const filteredBatches = yield* filterAlreadyCommittedTxs(batches);

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB and ProcessedMempoolDB, and deleting mempool LevelDB...",
    );
    yield* Effect.all(
      [
        Effect.forEach(
          filteredBatches,
          (batch, i) => {
            const clearMempoolProgram =
              batch.clearMempoolTxHashes.length === 0
                ? Effect.void
                : MempoolDB.clearTxs([...batch.clearMempoolTxHashes]).pipe(
                    Effect.withSpan(`mempool-db-clear-txs-batch-${i}`),
                  );
            return Effect.all(
              [
                ImmutableDB.insertTxsValidatedNative([
                  ...batch.txsToInsertImmutable,
                ]).pipe(Effect.withSpan(`immutable-db-insert-batch-${i}`)),
                BlocksDB.insert(newHeaderHashBuffer, [
                  ...batch.blockTxHashes,
                ]).pipe(Effect.withSpan(`blocks-db-insert-batch-${i}`)),
                clearMempoolProgram,
              ],
              { concurrency: "unbounded" },
            );
          },
          {
            concurrency: "unbounded",
          },
        ),
        ProcessedMempoolDB.clear, // uses `TRUNCATE` so no need for batching.
        mempoolTrie.delete(),
      ],
      { concurrency: "unbounded" },
    );
  }).pipe(
    Effect.tapError((error) =>
      Effect.gen(function* () {
        yield* Effect.logError(
          `🔹 Local commit finalization failed (header=${newHeaderHash},error=${formatUnknownError(error)})`,
        );
      }),
    ),
  );
const successfulSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
  workerInput: WorkerInput,
  txSize: number,
  sizeOfProcessedTxs: number,
  txHash: string,
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    yield* finalizeCommittedBlockLocally(
      mempoolTrie,
      mempoolTxs,
      mempoolTxHashes,
      newHeaderHash,
    );

    return {
      type: "SuccessfulSubmissionOutput",
      submittedTxHash: txHash,
      txSize,
      mempoolTxsCount:
        mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
      sizeOfBlocksTxs:
        sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
    };
  });

const successfulLocalFinalizationRecoveryProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  confirmedHeaderHash: string,
  workerInput: WorkerInput,
  sizeOfProcessedTxs: number,
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    yield* finalizeCommittedBlockLocally(
      mempoolTrie,
      mempoolTxs,
      mempoolTxHashes,
      confirmedHeaderHash,
    );
    return {
      type: "SuccessfulLocalFinalizationRecoveryOutput",
      mempoolTxsCount:
        mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
      sizeOfBlocksTxs:
        sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
    };
  });

const skippedSubmissionProgram = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length !== mempoolTxHashes.length) {
      return yield* Effect.fail(
        new DatabaseError({
          message:
            "Failed to transfer deferred commit payload: tx metadata length mismatch",
          cause: `mempool_txs=${mempoolTxs.length},mempool_tx_hashes=${mempoolTxHashes.length}`,
          table: "mempool,processed_mempool",
        }),
      );
    }
    yield* batchProgram(
      BATCH_SIZE,
      mempoolTxs.length,
      "skipped-submission-db-transfer",
      (startIndex: number, endIndex: number) =>
        Effect.gen(function* () {
          const batchTxs = mempoolTxs.slice(startIndex, endIndex);
          const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
          // Order matters: insert to processed_mempool first, then clear mempool.
          yield* ProcessedMempoolDB.insertTxs(batchTxs).pipe(
            Effect.withSpan(`processed-mempool-db-insert-${startIndex}`),
          );
          yield* MempoolDB.clearTxs(batchHashes).pipe(
            Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
          );
        }),
      1,
    );
  }).pipe(
    Effect.retry(
      Schedule.compose(
        Schedule.exponential(SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF),
        Schedule.recurs(SKIPPED_SUBMISSION_TRANSFER_RETRIES),
      ),
    ),
  );

const failedSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxsCount: number,
  sizeOfProcessedTxs: number,
  err: TxSubmitError,
): Effect.Effect<WorkerOutput> =>
  Effect.gen(function* () {
    yield* Effect.logError(`🔹 ⚠️  Tx submit failed: ${err}`);
    yield* Effect.logError(
      "🔹 ⚠️  Mempool trie will be preserved, but db will be cleared.",
    );
    yield* Effect.logInfo("🔹 Mempool Trie stats:");
    console.dir(mempoolTrie.databaseStats(), { depth: null });
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    };
  });

const recoverSubmittedTxHashByHeaderProgram = (
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
  expectedHeaderHash: string,
): Effect.Effect<Option.Option<string>, never, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const latestBlock = yield* SDK.fetchLatestCommittedBlockProgram(
      lucid.api,
      fetchConfig,
    );
    const latestHeader = yield* SDK.getHeaderFromStateQueueDatum(
      latestBlock.datum,
    );
    const latestHeaderHash = yield* SDK.hashBlockHeader(latestHeader);
    if (latestHeaderHash === expectedHeaderHash) {
      yield* Effect.logWarning(
        `🔹 Submit errored but on-chain header already advanced to ${expectedHeaderHash}; recovering submission state.`,
      );
      return Option.some(latestBlock.utxo.txHash);
    }
    return Option.none();
  }).pipe(
    Effect.catchAll((error) =>
      Effect.gen(function* () {
        yield* Effect.logWarning(
          `🔹 Could not verify submit recovery on-chain: ${formatUnknownError(error)}`,
        );
        return Option.none<string>();
      }),
    ),
  );

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an `Effect` that finds
 * the MPT root of those events.
 */
const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  Option.Option<Effect.Effect<string, MptError>>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const events = yield* retrieveTimeBoundEntries(
      tableName,
      startDate,
      endDate,
    );

    if (events.length <= 0) {
      yield* Effect.logInfo(
        `🔹 No events found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      return Option.none();
    } else {
      yield* Effect.logInfo(
        `🔹 ${events.length} event(s) found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      const eventIDs = events.map((event) => event[UserEventsColumns.ID]);
      const eventInfos = events.map((event) => event[UserEventsColumns.INFO]);
      return Option.some(keyValueMptRoot(eventIDs, eventInfos));
    }
  });

type RealStateQueueWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO & { datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly chainedWalletOutputs: readonly UTxO[];
  readonly consumedWalletFeeInputs: readonly UTxO[];
};

type SchedulerAlignmentResult = {
  readonly schedulerRefInput: UTxO;
  readonly chainedWalletOutputs: readonly UTxO[];
  readonly consumedWalletFeeInputs: readonly UTxO[];
};

const SCHEDULER_REFRESH_POLL_INTERVAL = "2 seconds";
const SCHEDULER_REFRESH_MAX_POLLS = 30;
const REAL_STATE_QUEUE_SCHEDULER_ASSET_NAME = "";
const MIN_SCHEDULER_WITNESS_LOVELACE = 5_000_000n;

const compareOutRefs = (a: UTxO, b: UTxO): number => {
  const txHashComparison = a.txHash.localeCompare(b.txHash);
  if (txHashComparison !== 0) {
    return txHashComparison;
  }
  return a.outputIndex - b.outputIndex;
};

const selectFeeInput = (
  walletUtxos: readonly UTxO[],
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const sorted = [...walletUtxos].sort((a, b) => {
      const lovelaceA = a.assets.lovelace ?? 0n;
      const lovelaceB = b.assets.lovelace ?? 0n;
      if (lovelaceA === lovelaceB) {
        return compareOutRefs(a, b);
      }
      return lovelaceA > lovelaceB ? -1 : 1;
    });
    const feeInput = sorted[0];
    if (feeInput === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "No wallet UTxO available to fund real state_queue commit tx",
          cause: "empty wallet",
        }),
      );
    }
    return feeInput;
  });

type ProviderRedeemerTag =
  | "spend"
  | "mint"
  | "publish"
  | "withdraw"
  | "vote"
  | "propose";

type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

type IndexedTxOutput = ReturnType<typeof coreToTxOutput> & {
  readonly index: number;
};

const DUMMY_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;

const assetsEqual = (
  left: Readonly<Record<string, bigint>>,
  right: Readonly<Record<string, bigint>>,
): boolean => {
  const keys = new Set([...Object.keys(left), ...Object.keys(right)]);
  for (const key of keys) {
    if ((left[key] ?? 0n) !== (right[key] ?? 0n)) {
      return false;
    }
  }
  return true;
};

const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex}`;

const dedupeUtxosByOutRef = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const byOutRef = new Map<string, UTxO>();
  for (const utxo of utxos) {
    const label = outRefLabel(utxo);
    if (!byOutRef.has(label)) {
      byOutRef.set(label, utxo);
    }
  }
  return [...byOutRef.values()];
};

const extractAddressOutputsFromSubmittedTx = (
  tx: CML.Transaction,
  txHash: string,
  address: string,
): readonly UTxO[] => {
  const outputs = tx.body().outputs();
  const matching: UTxO[] = [];
  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const txOutput = coreToTxOutput(outputs.get(outputIndex));
    if (txOutput.address !== address) {
      continue;
    }
    matching.push({
      txHash,
      outputIndex,
      ...txOutput,
    });
  }
  return matching;
};

const findInputIndexByOutRef = (
  inputs: CML.TransactionInputList,
  target: UTxO,
): number | undefined => {
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    if (
      input.transaction_id().to_hex() === target.txHash &&
      Number(input.index()) === target.outputIndex
    ) {
      return index;
    }
  }
  return undefined;
};

const collectIndexedOutputs = (
  outputs: CML.TransactionOutputList,
): readonly IndexedTxOutput[] => {
  const indexed: IndexedTxOutput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    indexed.push({
      index,
      ...coreToTxOutput(outputs.get(index)),
    });
  }
  return indexed;
};

const getRedeemerPointersInContextOrder = (
  tx: CML.Transaction,
): readonly RedeemerPointer[] => {
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return [];
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    const pointers: RedeemerPointer[] = [];
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
      pointers.push({
        tag: redeemer.tag(),
        index: redeemer.index(),
      });
    }
    return pointers;
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return [];
  }
  const pointers: RedeemerPointer[] = [];
  const keys = map.keys();
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    pointers.push({
      tag: key.tag(),
      index: key.index(),
    });
  }
  return pointers;
};

const toProviderRedeemerTag = (tag: number): ProviderRedeemerTag => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`Unsupported redeemer tag: ${tag}`);
  }
};

const withStubbedProviderEvaluation = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
): Promise<A> => {
  const provider = lucid.config().provider as {
    evaluateTx?: (
      tx: string,
      additionalUTxOs?: readonly UTxO[],
    ) => Promise<
      readonly {
        redeemer_tag: ProviderRedeemerTag;
        redeemer_index: number;
        ex_units: { mem: number; steps: number };
      }[]
    >;
  };
  if (typeof provider.evaluateTx !== "function") {
    return run();
  }

  const originalEvaluateTx = provider.evaluateTx.bind(provider);
  provider.evaluateTx = async (txCbor) => {
    const tx = CML.Transaction.from_cbor_hex(txCbor);
    return getRedeemerPointersInContextOrder(tx).map((pointer) => ({
      redeemer_tag: toProviderRedeemerTag(pointer.tag),
      redeemer_index: Number(pointer.index),
      ex_units: DUMMY_REDEEMER_EX_UNITS,
    }));
  };
  try {
    return await run();
  } finally {
    provider.evaluateTx = originalEvaluateTx;
  }
};

const deriveCommitLayoutFromDraftTx = ({
  tx,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueueAddress,
  headerNodeUnit,
  headerNodeDatum,
  previousHeaderNodeDatum,
}: {
  readonly tx: CML.Transaction;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly headerNodeDatum: string;
  readonly previousHeaderNodeDatum: string;
}): StateQueueCommitLayout => {
  const txBody = tx.body();
  const inputList = txBody.inputs();
  const referenceInputList = txBody.reference_inputs();
  const indexedOutputs = collectIndexedOutputs(txBody.outputs());

  const activeNodeInputIndex = findInputIndexByOutRef(
    inputList,
    activeOperatorInput,
  );
  if (activeNodeInputIndex === undefined) {
    throw new Error(
      `Unable to find active-operator input ${outRefLabel(activeOperatorInput)} in balanced draft tx body inputs`,
    );
  }

  if (referenceInputList === undefined) {
    throw new Error(
      "Balanced draft tx body did not include reference inputs for scheduler witness",
    );
  }
  const schedulerRefInputIndex = findInputIndexByOutRef(
    referenceInputList,
    schedulerRefInput,
  );
  if (schedulerRefInputIndex === undefined) {
    throw new Error(
      `Unable to find scheduler reference input ${outRefLabel(schedulerRefInput)} in balanced draft tx reference inputs`,
    );
  }
  const hubOracleRefInputIndex = findInputIndexByOutRef(
    referenceInputList,
    hubOracleRefInput,
  );
  if (hubOracleRefInputIndex === undefined) {
    throw new Error(
      `Unable to find hub-oracle reference input ${outRefLabel(hubOracleRefInput)} in balanced draft tx reference inputs`,
    );
  }

  const headerNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === headerNodeDatum &&
      (output.assets[headerNodeUnit] ?? 0n) === 1n,
  );
  if (headerNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one header-node output at ${stateQueueAddress} with datum ${headerNodeDatum.slice(0, 24)}..., found ${headerNodeOutputCandidates.length}`,
    );
  }
  const headerNodeOutputIndex = headerNodeOutputCandidates[0].index;

  const previousHeaderOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === previousHeaderNodeDatum,
  );
  if (previousHeaderOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one previous-header output at ${stateQueueAddress} with datum ${previousHeaderNodeDatum.slice(0, 24)}..., found ${previousHeaderOutputCandidates.length}`,
    );
  }
  const previousHeaderNodeOutputIndex = previousHeaderOutputCandidates[0].index;

  const activeNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === activeOperatorInput.address &&
      assetsEqual(output.assets, activeOperatorInput.assets),
  );
  if (activeNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one active-operator output at ${activeOperatorInput.address} with unchanged assets, found ${activeNodeOutputCandidates.length}`,
    );
  }
  const activeNodeOutputIndex = activeNodeOutputCandidates[0].index;

  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  if (redeemerPointers.length <= 0) {
    throw new Error("Balanced draft tx did not contain redeemers");
  }
  const activeOperatorSpendRedeemerIndex = redeemerPointers.findIndex(
    (pointer) =>
      pointer.tag === CML.RedeemerTag.Spend &&
      pointer.index === BigInt(activeNodeInputIndex),
  );
  if (activeOperatorSpendRedeemerIndex < 0) {
    throw new Error(
      `Unable to find active-operator spend redeemer for input index ${activeNodeInputIndex}`,
    );
  }
  const mintRedeemerCandidates = redeemerPointers
    .map((pointer, index) => ({ pointer, index }))
    .filter(({ pointer }) => pointer.tag === CML.RedeemerTag.Mint);
  if (mintRedeemerCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one mint redeemer pointer for state_queue commit, found ${mintRedeemerCandidates.length}`,
    );
  }
  const stateQueueRedeemerIndex = mintRedeemerCandidates[0].index;

  return {
    schedulerRefInputIndex: BigInt(schedulerRefInputIndex),
    activeNodeInputIndex: BigInt(activeNodeInputIndex),
    headerNodeOutputIndex: BigInt(headerNodeOutputIndex),
    previousHeaderNodeOutputIndex: BigInt(previousHeaderNodeOutputIndex),
    activeOperatorsRedeemerIndex: BigInt(activeOperatorSpendRedeemerIndex),
    activeNodeOutputIndex: BigInt(activeNodeOutputIndex),
    hubOracleRefInputIndex: BigInt(hubOracleRefInputIndex),
    stateQueueRedeemerIndex: BigInt(stateQueueRedeemerIndex),
  };
};

const redeemerTagLabel = (tag: number): string => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "cert";
    case CML.RedeemerTag.Reward:
      return "reward";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      return `unknown(${tag})`;
  }
};

const describeUtxoAssets = (
  assets: Readonly<Record<string, bigint>>,
): string => {
  const nonAda = Object.entries(assets)
    .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
    .map(([unit, amount]) => `${unit}:${amount.toString()}`);
  return `lovelace=${(assets.lovelace ?? 0n).toString()},nonAda=${
    nonAda.length > 0 ? nonAda.join("|") : "none"
  }`;
};

const findTxInputAtIndex = (
  inputs: CML.TransactionInputList,
  index: bigint,
): string | undefined => {
  const i = Number(index);
  if (!Number.isSafeInteger(i) || i < 0 || i >= inputs.len()) {
    return undefined;
  }
  const input = inputs.get(i);
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const findTxReferenceInputAtIndex = (
  referenceInputs: CML.TransactionInputList | undefined,
  index: bigint,
): string | undefined => {
  if (referenceInputs === undefined) {
    return undefined;
  }
  const i = Number(index);
  if (!Number.isSafeInteger(i) || i < 0 || i >= referenceInputs.len()) {
    return undefined;
  }
  const input = referenceInputs.get(i);
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const listRequiredSigners = (tx: CML.Transaction): readonly string[] => {
  const signers = tx.body().required_signers();
  if (signers === undefined) {
    return [];
  }
  const hashes: string[] = [];
  for (let i = 0; i < signers.len(); i += 1) {
    hashes.push(signers.get(i).to_hex());
  }
  return hashes;
};

const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number | undefined => {
  const network = lucid.config().network;
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time !== "number" ||
      typeof provider.slot !== "number"
    ) {
      return undefined;
    }
    const slotLength = 1000;
    const zeroTime = provider.time - provider.slot * slotLength;
    return zeroTime + slot * slotLength;
  }
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

const findRedeemerDataCbor = (
  tx: CML.Transaction,
  pointer: RedeemerPointer | undefined,
): string | undefined => {
  if (pointer === undefined) {
    return undefined;
  }
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return undefined;
  }
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
      if (
        redeemer.tag() === pointer.tag &&
        redeemer.index() === pointer.index
      ) {
        return redeemer.data().to_cbor_hex();
      }
    }
    return undefined;
  }
  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return undefined;
  }
  const keys = map.keys();
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    if (key.tag() !== pointer.tag || key.index() !== pointer.index) {
      continue;
    }
    const value = map.get(key);
    return value?.data().to_cbor_hex();
  }
  return undefined;
};

const buildRealCommitDraftDiagnostics = ({
  tx,
  layout,
  operatorKeyHash,
  latestBlockInput,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueueAddress,
  headerNodeUnit,
  appendedNodeDatumCbor,
  previousHeaderNodeDatumCbor,
  schedulerPolicyId,
  hubOraclePolicyId,
  activeOperatorsPolicyId,
  txValidityUpperBoundSlot,
  txValidityUpperBoundUnixTime,
}: {
  readonly tx: CML.Transaction;
  readonly layout: StateQueueCommitLayout;
  readonly operatorKeyHash: string;
  readonly latestBlockInput: UTxO;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly appendedNodeDatumCbor: string;
  readonly previousHeaderNodeDatumCbor: string;
  readonly schedulerPolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly activeOperatorsPolicyId: string;
  readonly txValidityUpperBoundSlot: string | undefined;
  readonly txValidityUpperBoundUnixTime: string | undefined;
}): Record<string, unknown> => {
  const txBody = tx.body();
  const inputs = txBody.inputs();
  const referenceInputs = txBody.reference_inputs();
  const outputs = collectIndexedOutputs(txBody.outputs());
  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  const requiredSigners = listRequiredSigners(tx);

  const expectedActiveUnit = toUnit(
    activeOperatorsPolicyId,
    SDK.NODE_ASSET_NAME + operatorKeyHash,
  );
  const expectedSchedulerUnit = toUnit(
    schedulerPolicyId,
    REAL_STATE_QUEUE_SCHEDULER_ASSET_NAME,
  );
  const expectedHubOracleUnit = toUnit(
    hubOraclePolicyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const headerOutputIndex = Number(layout.headerNodeOutputIndex);
  const previousHeaderOutputIndex = Number(
    layout.previousHeaderNodeOutputIndex,
  );
  const activeRedeemerLayoutIndex = Number(layout.activeOperatorsRedeemerIndex);
  const stateQueueRedeemerLayoutIndex = Number(layout.stateQueueRedeemerIndex);
  const activeRedeemerPointer =
    activeRedeemerLayoutIndex >= 0 &&
    activeRedeemerLayoutIndex < redeemerPointers.length
      ? redeemerPointers[activeRedeemerLayoutIndex]
      : undefined;
  const stateQueueRedeemerPointer =
    stateQueueRedeemerLayoutIndex >= 0 &&
    stateQueueRedeemerLayoutIndex < redeemerPointers.length
      ? redeemerPointers[stateQueueRedeemerLayoutIndex]
      : undefined;
  const activeRedeemerCbor = findRedeemerDataCbor(tx, activeRedeemerPointer);
  const stateQueueRedeemerCbor = findRedeemerDataCbor(
    tx,
    stateQueueRedeemerPointer,
  );
  const headerOutput = outputs.find(
    (output) => output.index === headerOutputIndex,
  );
  const previousHeaderOutput = outputs.find(
    (output) => output.index === previousHeaderOutputIndex,
  );
  const parsedHeaderDatum =
    headerOutput === undefined || headerOutput.datum == null
      ? undefined
      : (() => {
          try {
            const nodeDatum = LucidData.from(
              headerOutput.datum,
              SDK.StateQueueDatum,
            );
            const header = LucidData.castFrom(nodeDatum.data, SDK.Header);
            return {
              startTime: header.startTime.toString(),
              endTime: header.endTime.toString(),
              operatorVkey: header.operatorVkey,
            };
          } catch {
            return undefined;
          }
        })();
  const parsedPreviousDatum =
    previousHeaderOutput === undefined || previousHeaderOutput.datum == null
      ? undefined
      : (() => {
          try {
            const nodeDatum = LucidData.from(
              previousHeaderOutput.datum,
              SDK.StateQueueDatum,
            );
            try {
              const header = LucidData.castFrom(nodeDatum.data, SDK.Header);
              return {
                kind: "Header" as const,
                endTime: header.endTime.toString(),
              };
            } catch {
              const confirmedState = LucidData.castFrom(
                nodeDatum.data,
                SDK.ConfirmedState,
              );
              return {
                kind: "ConfirmedState" as const,
                endTime: confirmedState.endTime.toString(),
              };
            }
          } catch {
            return undefined;
          }
        })();
  const parsedSchedulerDatum =
    schedulerRefInput.datum == null
      ? undefined
      : (() => {
          try {
            const schedulerDatum = LucidData.from(
              schedulerRefInput.datum,
              SDK.SchedulerDatum,
            );
            return {
              operator: schedulerDatum.operator,
              startTime: schedulerDatum.startTime.toString(),
            };
          } catch {
            return undefined;
          }
        })();

  return {
    requiredSigners,
    operatorSignerPresent: requiredSigners.includes(operatorKeyHash),
    inputs: [...Array(inputs.len()).keys()].map((i) => {
      const input = inputs.get(i);
      return `${i}:${input.transaction_id().to_hex()}#${input.index().toString()}`;
    }),
    referenceInputs:
      referenceInputs === undefined
        ? []
        : [...Array(referenceInputs.len()).keys()].map((i) => {
            const input = referenceInputs.get(i);
            return `${i}:${input.transaction_id().to_hex()}#${input.index().toString()}`;
          }),
    expectedLatestBlockInput: outRefLabel(latestBlockInput),
    expectedSchedulerRefInput: outRefLabel(schedulerRefInput),
    expectedHubOracleRefInput: outRefLabel(hubOracleRefInput),
    expectedActiveOperatorInput: outRefLabel(activeOperatorInput),
    layout: {
      schedulerRefInputIndex: layout.schedulerRefInputIndex.toString(),
      activeNodeInputIndex: layout.activeNodeInputIndex.toString(),
      headerNodeOutputIndex: layout.headerNodeOutputIndex.toString(),
      previousHeaderNodeOutputIndex:
        layout.previousHeaderNodeOutputIndex.toString(),
      activeOperatorsRedeemerIndex:
        layout.activeOperatorsRedeemerIndex.toString(),
      activeNodeOutputIndex: layout.activeNodeOutputIndex.toString(),
      hubOracleRefInputIndex: layout.hubOracleRefInputIndex.toString(),
      stateQueueRedeemerIndex: layout.stateQueueRedeemerIndex.toString(),
    },
    activeNodeInputAtLayoutIndex: findTxInputAtIndex(
      inputs,
      layout.activeNodeInputIndex,
    ),
    schedulerRefInputAtLayoutIndex: findTxReferenceInputAtIndex(
      referenceInputs,
      layout.schedulerRefInputIndex,
    ),
    hubOracleRefInputAtLayoutIndex: findTxReferenceInputAtIndex(
      referenceInputs,
      layout.hubOracleRefInputIndex,
    ),
    activeRedeemerPointer:
      activeRedeemerPointer === undefined
        ? "missing"
        : `${redeemerTagLabel(activeRedeemerPointer.tag)}:${activeRedeemerPointer.index.toString()}`,
    stateQueueRedeemerPointer:
      stateQueueRedeemerPointer === undefined
        ? "missing"
        : `${redeemerTagLabel(stateQueueRedeemerPointer.tag)}:${stateQueueRedeemerPointer.index.toString()}`,
    activeRedeemerShape:
      activeRedeemerCbor === undefined
        ? "missing"
        : (() => {
            try {
              return LucidData.from(
                activeRedeemerCbor,
                ActiveOperatorSpendRedeemerSchema,
              );
            } catch {
              return `decode-failed:${activeRedeemerCbor}`;
            }
          })(),
    stateQueueRedeemerShape:
      stateQueueRedeemerCbor === undefined
        ? "missing"
        : (() => {
            try {
              return LucidData.from(
                stateQueueRedeemerCbor,
                SDK.StateQueueRedeemer,
              );
            } catch {
              return `decode-failed:${stateQueueRedeemerCbor}`;
            }
          })(),
    headerTiming: {
      header: parsedHeaderDatum,
      previous: parsedPreviousDatum,
      startMatchesPreviousEnd:
        parsedHeaderDatum === undefined || parsedPreviousDatum === undefined
          ? undefined
          : parsedHeaderDatum.startTime === parsedPreviousDatum.endTime,
      endMatchesTxValidityUpperBound:
        parsedHeaderDatum === undefined ||
        txValidityUpperBoundUnixTime === undefined
          ? undefined
          : parsedHeaderDatum.endTime === txValidityUpperBoundUnixTime,
      txValidityUpperBoundSlot: txValidityUpperBoundSlot ?? "missing",
      txValidityUpperBoundUnixTime: txValidityUpperBoundUnixTime ?? "missing",
    },
    schedulerDatum: parsedSchedulerDatum,
    redeemerPointersInOrder: redeemerPointers.map(
      (pointer, index) =>
        `${index}:${redeemerTagLabel(pointer.tag)}:${pointer.index.toString()}`,
    ),
    headerOutputAtLayoutIndex:
      headerOutput === undefined
        ? "missing"
        : {
            address: headerOutput.address,
            datumMatches: headerOutput.datum === appendedNodeDatumCbor,
            stateQueueTokenQty: (
              headerOutput.assets[headerNodeUnit] ?? 0n
            ).toString(),
            assets: describeUtxoAssets(headerOutput.assets),
          },
    previousHeaderOutputAtLayoutIndex:
      previousHeaderOutput === undefined
        ? "missing"
        : {
            address: previousHeaderOutput.address,
            datumMatches:
              previousHeaderOutput.datum === previousHeaderNodeDatumCbor,
            assets: describeUtxoAssets(previousHeaderOutput.assets),
            matchesLatestInputAssets: assetsEqual(
              previousHeaderOutput.assets,
              latestBlockInput.assets,
            ),
          },
    latestBlockInputAssets: describeUtxoAssets(latestBlockInput.assets),
    activeOperatorInputAssets: describeUtxoAssets(activeOperatorInput.assets),
    activeOperatorExpectedTokenQty: (
      activeOperatorInput.assets[expectedActiveUnit] ?? 0n
    ).toString(),
    schedulerRefInputAssets: describeUtxoAssets(schedulerRefInput.assets),
    schedulerExpectedTokenQty: (
      schedulerRefInput.assets[expectedSchedulerUnit] ?? 0n
    ).toString(),
    hubOracleRefInputAssets: describeUtxoAssets(hubOracleRefInput.assets),
    hubOracleExpectedTokenQty: (
      hubOracleRefInput.assets[expectedHubOracleUnit] ?? 0n
    ).toString(),
    stateQueueAddress,
  };
};

const getOperatorKeyHash = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet does not have a key payment credential",
          cause: operatorAddress,
        }),
      );
    }
    return paymentCredential.hash;
  });

const selectActiveOperatorInput = (
  activeOperatorUtxos: readonly UTxO[],
  operatorKeyHash: string,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    for (const utxo of activeOperatorUtxos) {
      const nodeDatumEither = yield* Effect.either(
        SDK.getNodeDatumFromUTxO(utxo),
      );
      if (nodeDatumEither._tag === "Left") {
        continue;
      }
      if (
        nodeDatumEither.right.key !== "Empty" &&
        nodeDatumEither.right.key.Key.key === operatorKeyHash
      ) {
        return utxo;
      }
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "No active-operators node for current operator key hash; cannot build real state_queue commit witness",
        cause: operatorKeyHash,
      }),
    );
  });

const getSchedulerDatumFromUTxO = (
  schedulerUtxo: UTxO,
): Effect.Effect<SDK.SchedulerDatum, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (schedulerUtxo.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Scheduler UTxO must include inline datum",
          cause: `${schedulerUtxo.txHash}#${schedulerUtxo.outputIndex}`,
        }),
      );
    }
    return yield* Effect.try({
      try: () => LucidData.from(schedulerUtxo.datum!, SDK.SchedulerDatum),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to decode scheduler datum",
          cause,
        }),
    });
  });

const ensureRealSchedulerWitnessUtxo = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  targetStartTime: bigint,
  schedulerUtxos: readonly UTxO[],
): Effect.Effect<UTxO, SDK.StateQueueError | TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      REAL_STATE_QUEUE_SCHEDULER_ASSET_NAME,
    );
    const existingWitness = [...schedulerUtxos]
      .filter((utxo) => (utxo.assets[schedulerWitnessUnit] ?? 0n) > 0n)
      .sort(compareOutRefs)[0];
    if (existingWitness !== undefined) {
      return existingWitness;
    }

    yield* Effect.logInfo(
      "🔹 Scheduler witness token with empty asset-name missing; creating one for real state_queue commits.",
    );
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch wallet UTxOs for scheduler witness bootstrap",
          cause,
        }),
    });
    const feeInput = yield* selectFeeInput(walletUtxos);
    const bootstrapDatum: SDK.SchedulerDatum = {
      operator: operatorKeyHash,
      startTime: targetStartTime,
    };
    const bootstrapTx = yield* Effect.tryPromise({
      try: () =>
        lucid
          .newTx()
          .collectFrom([feeInput])
          .mintAssets({ [schedulerWitnessUnit]: 1n }, LucidData.void())
          .pay.ToContract(
            contracts.scheduler.spendingScriptAddress,
            {
              kind: "inline",
              value: LucidData.to(bootstrapDatum, SDK.SchedulerDatum),
            },
            {
              lovelace: MIN_SCHEDULER_WITNESS_LOVELACE,
              [schedulerWitnessUnit]: 1n,
            },
          )
          .attach.Script(contracts.scheduler.mintingScript)
          .complete({ localUPLCEval: false }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to build scheduler witness bootstrap tx: ${cause}`,
          cause,
        }),
    });

    const bootstrapTxHash = yield* handleSignSubmitNoConfirmation(
      lucid,
      bootstrapTx,
    );
    yield* Effect.logInfo(
      `🔹 Scheduler witness bootstrap submitted: ${bootstrapTxHash}`,
    );

    let pollCount = 0;
    while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
      const witnessUtxos = yield* Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(
            contracts.scheduler.spendingScriptAddress,
            schedulerWitnessUnit,
          ),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler witness UTxOs while waiting for bootstrap confirmation",
            cause,
          }),
      });
      const witness = [...witnessUtxos].sort(compareOutRefs)[0];
      if (witness !== undefined) {
        return witness;
      }
      pollCount += 1;
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for scheduler witness UTxO bootstrap confirmation",
        cause: bootstrapTxHash,
      }),
    );
  });

const ensureSchedulerAlignedForCommit = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  schedulerRefInput: UTxO,
  alignedEndTime: number,
  schedulerWitnessUnit: string,
): Effect.Effect<
  SchedulerAlignmentResult,
  SDK.StateQueueError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const targetStartTime = BigInt(alignedEndTime);
    const schedulerDatumEither = yield* Effect.either(
      getSchedulerDatumFromUTxO(schedulerRefInput),
    );
    if (schedulerDatumEither._tag === "Right") {
      const schedulerDatum = schedulerDatumEither.right;
      if (
        schedulerDatum.operator === operatorKeyHash &&
        schedulerDatum.startTime === targetStartTime
      ) {
        return {
          schedulerRefInput,
          chainedWalletOutputs: [],
          consumedWalletFeeInputs: [],
        };
      }
      yield* Effect.logInfo(
        `🔹 Refreshing scheduler witness datum for commit window (from operator=${schedulerDatum.operator}, startTime=${schedulerDatum.startTime.toString()} to operator=${operatorKeyHash}, startTime=${targetStartTime.toString()}).`,
      );
    } else {
      yield* Effect.logInfo(
        "🔹 Refreshing scheduler witness datum for commit window (scheduler datum missing or invalid).",
      );
    }

    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch wallet UTxOs for scheduler refresh tx",
          cause,
        }),
    });
    const feeInput = yield* selectFeeInput(walletUtxos);
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch operator wallet address for tx chaining",
          cause,
        }),
    });

    const refreshedSchedulerDatum: SDK.SchedulerDatum = {
      operator: operatorKeyHash,
      startTime: targetStartTime,
    };
    const refreshTx = yield* Effect.tryPromise({
      try: () =>
        lucid
          .newTx()
          .collectFrom([feeInput])
          .collectFrom([schedulerRefInput], LucidData.void())
          .pay.ToContract(
            contracts.scheduler.spendingScriptAddress,
            {
              kind: "inline",
              value: LucidData.to(refreshedSchedulerDatum, SDK.SchedulerDatum),
            },
            schedulerRefInput.assets,
          )
          .attach.Script(contracts.scheduler.spendingScript)
          .complete({ localUPLCEval: false }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to build scheduler refresh tx: ${cause}`,
          cause,
        }),
    });

    const refreshTxHash = yield* handleSignSubmitNoConfirmation(
      lucid,
      refreshTx,
    );
    const chainedWalletOutputs = extractAddressOutputsFromSubmittedTx(
      refreshTx.toTransaction(),
      refreshTxHash,
      walletAddress,
    );
    yield* Effect.logInfo(
      `🔹 Scheduler refresh transaction submitted: ${refreshTxHash}`,
    );
    yield* Effect.logInfo(
      `🔹 Scheduler refresh tx chaining outputs for operator wallet: ${chainedWalletOutputs.length}.`,
    );

    let pollCount = 0;
    while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
      const schedulerWitnessUtxos = yield* Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(
            contracts.scheduler.spendingScriptAddress,
            schedulerWitnessUnit,
          ),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler witness UTxOs while waiting for scheduler refresh",
            cause,
          }),
      });
      for (const utxo of [...schedulerWitnessUtxos].sort(compareOutRefs)) {
        const utxoDatumEither = yield* Effect.either(
          getSchedulerDatumFromUTxO(utxo),
        );
        if (utxoDatumEither._tag === "Left") {
          continue;
        }
        if (
          utxoDatumEither.right.operator === operatorKeyHash &&
          utxoDatumEither.right.startTime === targetStartTime
        ) {
          return {
            schedulerRefInput: utxo,
            chainedWalletOutputs,
            consumedWalletFeeInputs: [feeInput],
          };
        }
      }

      pollCount += 1;
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for refreshed scheduler UTxO to appear on-chain",
        cause: refreshTxHash,
      }),
    );
  });

const fetchRealStateQueueWitnessContext = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  alignedEndTime: number,
): Effect.Effect<
  RealStateQueueWitnessContext,
  SDK.StateQueueError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const targetStartTime = BigInt(alignedEndTime);
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      REAL_STATE_QUEUE_SCHEDULER_ASSET_NAME,
    );

    const schedulerUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.scheduler.spendingScriptAddress,
      contracts.scheduler.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to fetch scheduler UTxOs for state_queue commit",
            cause,
          }),
      ),
    );
    const initialSchedulerRefInput = yield* ensureRealSchedulerWitnessUtxo(
      lucid,
      contracts,
      operatorKeyHash,
      targetStartTime,
      schedulerUtxos,
    );
    const schedulerRefInput = yield* ensureSchedulerAlignedForCommit(
      lucid,
      contracts,
      operatorKeyHash,
      initialSchedulerRefInput,
      alignedEndTime,
      schedulerWitnessUnit,
    );
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve Cardano network for hub-oracle witness lookup",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const hubOracleWitnessUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch hub-oracle UTxOs for state_queue commit witness",
          cause,
        }),
    });
    if (hubOracleWitnessUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve unique hub-oracle UTxO for state_queue commit witness",
          cause: `expected=1,found=${hubOracleWitnessUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
        }),
      );
    }
    const hubOracleRefInput = hubOracleWitnessUtxos[0];

    const activeOperatorUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch active-operators UTxOs for state_queue commit",
            cause,
          }),
      ),
    );
    const activeOperatorInput = yield* selectActiveOperatorInput(
      activeOperatorUtxos,
      operatorKeyHash,
    );

    if (activeOperatorInput.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Active-operators UTxO must include inline datum for real state_queue commit",
          cause: `${activeOperatorInput.txHash}#${activeOperatorInput.outputIndex}`,
        }),
      );
    }

    return {
      operatorKeyHash,
      schedulerRefInput: schedulerRefInput.schedulerRefInput,
      hubOracleRefInput,
      activeOperatorInput: activeOperatorInput as UTxO & { datum: string },
      activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
      chainedWalletOutputs: schedulerRefInput.chainedWalletOutputs,
      consumedWalletFeeInputs: schedulerRefInput.consumedWalletFeeInputs,
    };
  });

const buildUnsignedTx = (
  contracts: SDK.MidgardValidators,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  endDate: Date,
) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const stateQueueAuthValidator = contracts.stateQueue;
    const latestEndTime = Number(
      (yield* getLatestBlockDatumEndTime(latestBlock.datum)).getTime(),
    );
    const {
      alignedCandidateEndTime,
      minimumMonotonicEndTime,
      resolvedEndTime: alignedEndTime,
    } = resolveAlignedCommitEndTime({
      lucid: lucid.api,
      latestEndTime,
      candidateEndTime: endDate.getTime(),
    });
    if (alignedEndTime !== alignedCandidateEndTime) {
      yield* Effect.logWarning(
        `Adjusted commit end-time to maintain monotonic header timing (candidate=${alignedCandidateEndTime}, minimum=${minimumMonotonicEndTime}, selected=${alignedEndTime}, latestEnd=${latestEndTime}).`,
      );
    }
    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    yield* lucid.switchToOperatorsMainWallet;
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid.api,
        latestBlock.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        "00".repeat(32),
        BigInt(alignedEndTime),
      );

    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

    const witnessContext = yield* fetchRealStateQueueWitnessContext(
      lucid.api,
      contracts,
      alignedEndTime,
    );
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

    const formatLayout = (layout: {
      readonly schedulerRefInputIndex: bigint;
      readonly activeNodeInputIndex: bigint;
      readonly activeOperatorsRedeemerIndex: bigint;
      readonly stateQueueRedeemerIndex: bigint;
      readonly headerNodeOutputIndex: bigint;
      readonly previousHeaderNodeOutputIndex: bigint;
      readonly activeNodeOutputIndex: bigint;
      readonly hubOracleRefInputIndex: bigint;
    }) =>
      `scheduler_ref_input_index=${layout.schedulerRefInputIndex.toString()},active_node_input_index=${layout.activeNodeInputIndex.toString()},active_operators_redeemer_index=${layout.activeOperatorsRedeemerIndex.toString()},state_queue_redeemer_index=${layout.stateQueueRedeemerIndex.toString()},header_node_output_index=${layout.headerNodeOutputIndex.toString()},previous_header_node_output_index=${layout.previousHeaderNodeOutputIndex.toString()},active_node_output_index=${layout.activeNodeOutputIndex.toString()},hub_oracle_ref_input_index=${layout.hubOracleRefInputIndex.toString()}`;

    const txBuilder = yield* Effect.gen(function* () {
      const witness = witnessContext;
      const walletUtxos = yield* Effect.tryPromise({
        try: () => lucid.api.wallet().getUtxos(),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch operator wallet UTxOs for real state_queue commit",
            cause,
          }),
      });
      const consumedWalletFeeInputSet = new Set(
        witness.consumedWalletFeeInputs.map((utxo) => outRefLabel(utxo)),
      );
      const providerWalletCandidates = walletUtxos.filter(
        (utxo) => !consumedWalletFeeInputSet.has(outRefLabel(utxo)),
      );
      const feeInputCandidates = dedupeUtxosByOutRef([
        ...witness.chainedWalletOutputs,
        ...providerWalletCandidates,
      ]);
      const feeInput = yield* selectFeeInput(feeInputCandidates);
      const feeInputNonAdaUnits = Object.entries(feeInput.assets)
        .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
        .map(([unit, amount]) => `${unit}:${amount.toString()}`);
      yield* Effect.logInfo(
        `🔹 Selected fee input ${outRefLabel(feeInput)} from ${feeInputCandidates.length} candidate(s) (provider=${providerWalletCandidates.length}, chained=${witness.chainedWalletOutputs.length}, consumed_excluded=${witness.consumedWalletFeeInputs.length}) (non-ADA units: ${
          feeInputNonAdaUnits.length > 0
            ? feeInputNonAdaUnits.join(",")
            : "none"
        }).`,
      );

      const makeCommitTxForLayout = (commitLayout: StateQueueCommitLayout) =>
        makeBaseCommitTx()
          .collectFrom([feeInput])
          .readFrom([witness.schedulerRefInput, witness.hubOracleRefInput])
          .collectFrom(
            [witness.activeOperatorInput],
            encodeActiveOperatorCommitRedeemer(commitLayout),
          )
          .pay.ToContract(
            witness.activeOperatorInput.address,
            {
              kind: "inline",
              value: witness.activeOperatorInput.datum,
            },
            witness.activeOperatorInput.assets,
          )
          .attach.Script(witness.activeOperatorsSpendingScript)
          .addSignerKey(witness.operatorKeyHash)
          .mintAssets(
            commitMintAssets,
            encodeStateQueueCommitRedeemer(
              witness.operatorKeyHash,
              commitLayout,
            ),
          )
          .attach.Script(stateQueueAuthValidator.spendingScript)
          .attach.Script(stateQueueAuthValidator.mintingScript);

      const seedLayout = deriveStateQueueCommitLayout({
        latestBlockInput: latestBlock.utxo,
        activeOperatorInput: witness.activeOperatorInput,
        txInputs: [latestBlock.utxo, witness.activeOperatorInput, feeInput],
      });
      const { commitLayout, draftDiagnostics } = yield* Effect.tryPromise({
        try: async () => {
          const [, , draftSignBuilder] = await withStubbedProviderEvaluation(
            lucid.api,
            () =>
              makeCommitTxForLayout(seedLayout).chain({
                localUPLCEval: false,
              }),
          );
          const draftTx = draftSignBuilder.toTransaction();
          const draftTtl = draftTx.body().ttl();
          const txValidityUpperBoundSlot =
            draftTtl === undefined ? undefined : Number(draftTtl);
          const txValidityUpperBoundUnixTime =
            txValidityUpperBoundSlot === undefined
              ? undefined
              : slotToUnixTimeForLucid(lucid.api, txValidityUpperBoundSlot);
          const commitLayout = deriveCommitLayoutFromDraftTx({
            tx: draftTx,
            schedulerRefInput: witness.schedulerRefInput,
            hubOracleRefInput: witness.hubOracleRefInput,
            activeOperatorInput: witness.activeOperatorInput,
            stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
            headerNodeUnit,
            headerNodeDatum: appendedNodeDatumCbor,
            previousHeaderNodeDatum: updatedNodeDatumCbor,
          });
          const draftDiagnostics = buildRealCommitDraftDiagnostics({
            tx: draftTx,
            layout: commitLayout,
            operatorKeyHash: witness.operatorKeyHash,
            latestBlockInput: latestBlock.utxo,
            schedulerRefInput: witness.schedulerRefInput,
            hubOracleRefInput: witness.hubOracleRefInput,
            activeOperatorInput: witness.activeOperatorInput,
            stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
            headerNodeUnit,
            appendedNodeDatumCbor,
            previousHeaderNodeDatumCbor: updatedNodeDatumCbor,
            schedulerPolicyId: contracts.scheduler.policyId,
            hubOraclePolicyId: contracts.hubOracle.policyId,
            activeOperatorsPolicyId: contracts.activeOperators.policyId,
            txValidityUpperBoundSlot: txValidityUpperBoundSlot?.toString(),
            txValidityUpperBoundUnixTime:
              txValidityUpperBoundUnixTime?.toString(),
          });
          return {
            commitLayout,
            draftDiagnostics,
          };
        },
        catch: (cause) =>
          new SDK.StateQueueError({
            message: `Failed to derive deterministic commit redeemer layout from balanced draft tx: ${formatUnknownError(
              cause,
            )}`,
            cause,
          }),
      });
      yield* Effect.logInfo(
        `🔹 Using commit redeemer layout: ${formatLayout(commitLayout)}`,
      );
      yield* Effect.logInfo(
        `🔹 Real commit draft diagnostics: ${JSON.stringify(
          draftDiagnostics,
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
        )}`,
      );
      const layoutCandidates = enumerateCommitLayoutCandidates(commitLayout);
      let lastRemoteError: SDK.StateQueueError | undefined = undefined;
      for (const candidateLayout of layoutCandidates) {
        const remoteEvalResult = yield* Effect.either(
          Effect.tryPromise({
            try: () =>
              makeCommitTxForLayout(candidateLayout).complete({
                localUPLCEval: false,
              }),
            catch: (e) =>
              new SDK.StateQueueError({
                message: `Failed to build block header commitment transaction with derived layout (${formatLayout(candidateLayout)}): ${e}`,
                cause: e,
              }),
          }),
        );
        if (remoteEvalResult._tag === "Right") {
          if (candidateLayout !== commitLayout) {
            yield* Effect.logWarning(
              `Derived commit layout was rejected; using fallback layout (${formatLayout(candidateLayout)}).`,
            );
          }
          return remoteEvalResult.right;
        }

        const localEvalDiagnostic = yield* Effect.either(
          Effect.tryPromise({
            try: () =>
              makeCommitTxForLayout(candidateLayout).complete({
                localUPLCEval: true,
              }),
            catch: (e) =>
              new SDK.StateQueueError({
                message: `Local UPLC eval diagnostic failed for derived layout (${formatLayout(candidateLayout)}): ${e}`,
                cause: e,
              }),
          }),
        );
        const localDiagnosticMessage =
          localEvalDiagnostic._tag === "Left"
            ? formatUnknownError(localEvalDiagnostic.left)
            : "local UPLC eval unexpectedly succeeded";
        yield* Effect.logWarning(
          `Commit layout attempt failed (${formatLayout(candidateLayout)}). Remote eval: ${formatUnknownError(remoteEvalResult.left)}. Local eval diagnostic: ${localDiagnosticMessage}`,
        );
        lastRemoteError = remoteEvalResult.left;
      }

      return yield* Effect.fail(
        lastRemoteError ??
          new SDK.StateQueueError({
            message:
              "Failed to build block header commitment transaction with derived layout candidates",
            cause: "exhausted-derived-layout-candidates",
          }),
      );
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

const databaseOperationsProgram = (
  workerInput: WorkerInput,
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
): Effect.Effect<
  WorkerOutput,
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxSignError
  | TxSubmitError
  | DatabaseError
  | FileSystemError
  | LocalFinalizationPendingError
  | MptError,
  MidgardContracts | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;

    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no confirmed block available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      };
    }

    const {
      utxoRoot,
      txRoot,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount,
    } = yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime: Option.Option<Date> =
      yield* establishEndTimeFromTxRequests(processedMempoolTxs);

    const contracts = yield* MidgardContracts;
    const { stateQueue: stateQueueAuthValidator } = contracts;

    if (availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
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
        };
      }
      return {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount,
        sizeOfProcessedTxs,
      };
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
          hasAvailableConfirmedBlock: true,
        })
      ) {
        yield* Effect.logInfo(
          "🔹 Attempting local finalization recovery against confirmed block roots...",
        );
        const optHeaderRoots = yield* getLatestBlockHeaderRoots(
          latestBlock.datum,
        );
        if (Option.isNone(optHeaderRoots)) {
          return {
            type: "FailureOutput",
            error:
              "Confirmed block datum does not contain a recoverable header for local finalization",
          };
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
          };
        }
        const confirmedHeader = yield* SDK.getHeaderFromStateQueueDatum(
          latestBlock.datum,
        );
        const confirmedHeaderHash = yield* SDK.hashBlockHeader(confirmedHeader);
        return yield* successfulLocalFinalizationRecoveryProgram(
          mempoolTrie,
          processedMempoolTxs,
          mempoolTxHashes,
          confirmedHeaderHash,
          workerInput,
          sizeOfProcessedTxs,
        );
      }

      const startTime = yield* getLatestBlockDatumEndTime(latestBlock.datum);

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        const endDate = new Date();
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const optDepositsRootProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endDate,
        );
        if (Option.isNone(optDepositsRootProgram)) {
          yield* Effect.logInfo("🔹 Nothing to commit.");
          return {
            type: "NothingToCommitOutput",
          } as WorkerOutput;
        } else {
          const depositsRootFiber = yield* Effect.fork(
            optDepositsRootProgram.value,
          );
          const depositsRoot = yield* depositsRootFiber;
          yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
          const emptyRoot = yield* emptyRootHexProgram;
          const roots = selectCommitRoots({
            hasTxRequests: false,
            computedUtxoRoot: utxoRoot,
            computedTxRoot: txRoot,
            emptyRoot,
          });
          const { signAndSubmitProgram, txSize } = yield* buildUnsignedTx(
            contracts,
            latestBlock,
            roots.utxoRoot,
            roots.txRoot,
            depositsRoot,
            endDate,
          );

          return yield* Effect.matchEffect(signAndSubmitProgram, {
            onFailure: (error) =>
              Effect.gen(function* () {
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
              Effect.succeed({
                type: "SuccessfulSubmissionOutput",
                submittedTxHash: txHash,
                txSize,
                mempoolTxsCount: 0,
                sizeOfBlocksTxs: workerInput.data.sizeOfProcessedTxsSoFar,
              } as WorkerOutput),
          });
        }
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const optDepositsRootProgram = yield* userEventsProgram(
          DepositsDB.tableName,
          startTime,
          endTime,
        );

        let depositsRoot: string = yield* emptyRootHexProgram;
        if (Option.isSome(optDepositsRootProgram)) {
          const depositsRootFiber = yield* Effect.fork(
            optDepositsRootProgram.value,
          );
          depositsRoot = yield* depositsRootFiber;
        }
        yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);

        const { newHeaderHash, signAndSubmitProgram, txSize } =
          yield* buildUnsignedTx(
            contracts,
            latestBlock,
            utxoRoot,
            txRoot,
            depositsRoot,
            endTime,
          );

        return yield* Effect.matchEffect(signAndSubmitProgram, {
          onFailure: Match.valueTags({
            TxSignError: (e) =>
              Effect.gen(function* () {
                const detail = formatUnknownError(e);
                yield* Effect.logError(`🔹 Commit signing failed: ${detail}`);
                return {
                  type: "FailureOutput",
                  error: `Commit signing failed: ${detail}`,
                } satisfies WorkerOutput;
              }),
            TxSubmitError: (e) =>
              Effect.gen(function* () {
                const recoveredTxHash =
                  yield* recoverSubmittedTxHashByHeaderProgram(
                    stateQueueAuthValidator,
                    newHeaderHash,
                  );
                if (Option.isSome(recoveredTxHash)) {
                  return yield* successfulSubmissionProgram(
                    mempoolTrie,
                    processedMempoolTxs,
                    mempoolTxHashes,
                    newHeaderHash,
                    workerInput,
                    txSize,
                    sizeOfProcessedTxs,
                    recoveredTxHash.value,
                  ).pipe(
                    Effect.mapError(
                      (cause) =>
                        new LocalFinalizationPendingError({
                          submittedTxHash: recoveredTxHash.value,
                          txSize,
                          mempoolTxsCount:
                            processedMempoolTxs.length +
                            workerInput.data.mempoolTxsCountSoFar,
                          sizeOfBlocksTxs:
                            sizeOfProcessedTxs +
                            workerInput.data.sizeOfProcessedTxsSoFar,
                          cause,
                        }),
                    ),
                  );
                }

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
                      e,
                    )}; transfer=${detail}`,
                  );
                  return {
                    type: "FailureOutput",
                    error: `Commit submission failed and deferred transfer failed: submit=${formatUnknownError(
                      e,
                    )}; transfer=${detail}`,
                  } satisfies WorkerOutput;
                }

                return yield* failedSubmissionProgram(
                  mempoolTrie,
                  mempoolTxsCount,
                  sizeOfProcessedTxs,
                  e,
                );
              }),
          }),
          onSuccess: (txHash) =>
            successfulSubmissionProgram(
              mempoolTrie,
              processedMempoolTxs,
              mempoolTxHashes,
              newHeaderHash,
              workerInput,
              txSize,
              sizeOfProcessedTxs,
              txHash,
            ).pipe(
              Effect.mapError(
                (cause) =>
                  new LocalFinalizationPendingError({
                    submittedTxHash: txHash,
                    txSize,
                    mempoolTxsCount:
                      processedMempoolTxs.length +
                      workerInput.data.mempoolTxsCountSoFar,
                    sizeOfBlocksTxs:
                      sizeOfProcessedTxs +
                      workerInput.data.sizeOfProcessedTxsSoFar,
                    cause,
                  }),
              ),
            ),
        });
      }
    }
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput | undefined,
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | ConfigError
  | DatabaseInitializationError
  | DatabaseError
  | FileSystemError
  | LocalFinalizationPendingError
  | TxSignError
  | TxSubmitError
  | MptError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const { ledgerTrie, mempoolTrie } = yield* makeMpts;

    const result: void | WorkerOutput = yield* withTrieTransaction(
      ledgerTrie,
      databaseOperationsProgram(workerInput, ledgerTrie, mempoolTrie),
    ).pipe(
      Effect.catchAll((error) =>
        error._tag === "LocalFinalizationPendingError"
          ? Effect.succeed({
              type: "SubmittedAwaitingLocalFinalizationOutput",
              submittedTxHash: error.submittedTxHash,
              txSize: error.txSize,
              mempoolTxsCount: error.mempoolTxsCount,
              sizeOfBlocksTxs: error.sizeOfBlocksTxs,
              error: formatUnknownError(error.cause),
            } as WorkerOutput)
          : Effect.fail(error),
      ),
    );
    if (result) {
      return result;
    } else {
      return undefined;
    }
  });

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(MidgardContracts.Default),
  Effect.provide(Database.layer),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
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
