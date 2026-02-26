import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Either, Schedule, pipe } from "effect";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  SubmittedButUnconfirmedOutput,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import {
  deserializeStateQueueUTxO,
  serializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";
import { fromHex, LucidEvolution } from "@lucid-evolution/lucid";
import { TxConfirmError, TxSubmitError } from "@/transactions/utils.js";
import { UnsubmittedBlocksDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

const inputData = workerData as WorkerInput;

const fetchLatestBlock = (
  lucid: LucidEvolution,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError | SDK.LucidError,
  AlwaysSucceedsContract | NodeConfig
> =>
  Effect.gen(function* () {
    const { stateQueue: stateQueueAuthValidator } =
      yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    return yield* SDK.fetchLatestCommittedBlockProgram(lucid, fetchConfig);
  });

const syncUnsubmittedBlocksWithLatestOnchainBlock = (
  latestBlock: SDK.StateQueueUTxO,
): Effect.Effect<
  void,
  SDK.DataCoercionError | SDK.HashingError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (latestBlock.datum.key === "Empty") {
      return;
    }
    const latestHeader = yield* SDK.getHeaderFromStateQueueDatum(
      latestBlock.datum,
    );
    const latestHeaderHashHex = yield* SDK.hashBlockHeader(latestHeader);
    const latestHeaderHash = Buffer.from(fromHex(latestHeaderHashHex));
    yield* UnsubmittedBlocksDB.deleteUpToAndIncludingBlock(latestHeaderHash);
  });

const awaitConfirmation = (
  lucid: LucidEvolution,
  txHash: string,
): Effect.Effect<boolean, TxConfirmError> =>
  Effect.retry(
    Effect.tryPromise({
      try: () => lucid.awaitTx(txHash),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash: txHash,
          cause: e,
        }),
    }),
    Schedule.recurs(4),
  );

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.CmlUnexpectedError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxConfirmError,
  AlwaysSucceedsContract | Lucid | NodeConfig | Database
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;

    if (workerInput.data.availableConfirmedBlock === "") {
      yield* Effect.logInfo(
        "🔍 No cached on-chain block available. Fetching and synchronizing...",
      );
      const latestBlock = yield* fetchLatestBlock(lucid.api);
      yield* syncUnsubmittedBlocksWithLatestOnchainBlock(latestBlock);
      const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
      return {
        type: "SuccessfulConfirmationOutput",
        blocksUTxO: serializedUTxO,
      };
    }

    const latestAvailableBlock = yield* deserializeStateQueueUTxO(
      workerInput.data.availableConfirmedBlock,
    );
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(latestAvailableBlock);

    const unsubmittedBlocks = yield* UnsubmittedBlocksDB.retrieve;
    if (unsubmittedBlocks.length <= 0) {
      return {
        type: "NoUnsubmittedBlocksOutput",
      };
    }
    const nextUnsubmittedBlock = unsubmittedBlocks[0];

    yield* Effect.logInfo(
      `🔍 Submitting next queued unsubmitted block transaction...`,
    );
    const newWalletUTxOs =
      yield* UnsubmittedBlocksDB.deserializeUTxOsFromStorage(
        nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS],
      );
    yield* lucid.switchToOperatorsMainWallet;
    yield* Effect.sync(() => lucid.api.overrideUTxOs([...newWalletUTxOs]));

    const txCborHex =
      nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.L1_CBOR].toString("hex");
    const submitResult = yield* Effect.either(
      Effect.tryPromise({
        try: () => lucid.api.wallet().submitTx(txCborHex),
        catch: (e) =>
          new TxSubmitError({
            message: `Failed to submit queued unsubmitted block transaction`,
            txHash: "<unknown>",
            cause: e,
          }),
      }),
    );
    if (Either.isLeft(submitResult)) {
      return {
        type: "FailedConfirmationOutput",
        error: `Failed to submit queued transaction: ${submitResult.left}`,
      };
    }

    const confirmationResult = yield* Effect.either(
      awaitConfirmation(lucid.api, submitResult.right),
    );
    if (Either.isLeft(confirmationResult)) {
      const output: SubmittedButUnconfirmedOutput = {
        type: "SubmittedButUnconfirmedOutput",
        error: `Queued transaction was submitted, but confirmation failed for ${submitResult.right}: ${confirmationResult.left}`,
      };
      return output;
    }

    yield* UnsubmittedBlocksDB.deleteByBlocks([
      nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.BLOCK],
    ]);

    yield* Effect.logInfo(
      "🔍 Queued transactions confirmed. Fetching latest on-chain block...",
    );
    const latestBlock = yield* fetchLatestBlock(lucid.api);
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(latestBlock);
    const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
    return {
      type: "SuccessfulConfirmationOutput",
      blocksUTxO: serializedUTxO,
    };
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(Database.layer),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailedConfirmationOutput",
        error: `Tx confirmation worker failure: ${Cause.pretty(cause)}`,
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `🔍 Confirmation work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
