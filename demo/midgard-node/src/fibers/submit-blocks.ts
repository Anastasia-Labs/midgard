import * as SDK from "@al-ft/midgard-sdk";
import { UnsubmittedBlocksDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
} from "@/services/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { fromHex, LucidEvolution, TxHash } from "@lucid-evolution/lucid";
import { Effect, Ref, Schedule } from "effect";
import {
  SerializedStateQueueUTxO,
  deserializeStateQueueUTxO,
  serializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";

type AwaitConfirmationOutput =
  | {
      type: "Confirmed";
      latestBlock: SDK.StateQueueUTxO;
      serializedLatestBlock: SerializedStateQueueUTxO;
    }
  | {
      type: "AwaitTxFailure";
      error: string;
      latestBlock: SDK.StateQueueUTxO;
      serializedLatestBlock: SerializedStateQueueUTxO;
      latestConfirmedTxHash: string;
    };

const fetchLatestBlock = (
  lucid: LucidEvolution,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError | SDK.LucidError,
  AlwaysSucceedsContract
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
): Effect.Effect<void, SDK.DataCoercionError | DatabaseError, Database> =>
  Effect.gen(function* () {
    const headerHashHex = yield* SDK.headerHashFromStateQueueUTxO(latestBlock);
    const latestHeaderHash = Buffer.from(fromHex(headerHashHex));
    yield* UnsubmittedBlocksDB.deleteUpToAndIncludingBlock(latestHeaderHash);
  });

const awaitConfirmation = (
  lucid: LucidEvolution,
  targetTxHash: TxHash,
): Effect.Effect<
  AwaitConfirmationOutput,
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.LucidError
  | SDK.StateQueueError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`🔗 Confirming tx: ${targetTxHash}`);

    yield* Effect.retry(
      Effect.tryPromise({
        try: () => lucid.awaitTx(targetTxHash),
        catch: (cause) => cause,
      }),
      Schedule.recurs(4),
    ).pipe(Effect.catchAllCause(Effect.logInfo));

    const latestBlock = yield* fetchLatestBlock(lucid);
    const serializedLatestBlock = yield* serializeStateQueueUTxO(latestBlock);

    if (latestBlock.utxo.txHash === targetTxHash) {
      return {
        type: "Confirmed",
        latestBlock,
        serializedLatestBlock,
      };
    } else {
      return {
        type: "AwaitTxFailure",
        error: `Failed to confirm transaction: ${targetTxHash}`,
        latestBlock,
        serializedLatestBlock,
        latestConfirmedTxHash: latestBlock.utxo.txHash,
      };
    }
  });

const syncLatestConfirmedBlock = (
  lucid: LucidEvolution,
  globals: Globals,
): Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError,
  AlwaysSucceedsContract | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔗 No cached confirmed block available. Fetching latest on-chain block...",
    );
    const latestBlock = yield* fetchLatestBlock(lucid);
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(latestBlock);
    const serializedLatestBlock = yield* serializeStateQueueUTxO(latestBlock);
    yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, serializedLatestBlock);
    yield* Effect.logInfo("🔗 Updated cached confirmed block from chain tip.");
  });

const handleUnconfirmedSubmission = (
  lucid: LucidEvolution,
  globals: Globals,
  submittedTxHash: TxHash,
): Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError,
  AlwaysSucceedsContract | Database
> =>
  Effect.gen(function* () {
    const confirmationOutput = yield* awaitConfirmation(lucid, submittedTxHash);
    switch (confirmationOutput.type) {
      case "Confirmed": {
        yield* syncUnsubmittedBlocksWithLatestOnchainBlock(
          confirmationOutput.latestBlock,
        );
        yield* Ref.set(
          globals.AVAILABLE_CONFIRMED_BLOCK,
          confirmationOutput.serializedLatestBlock,
        );
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Effect.logInfo(
          `🔗 ☑️  Confirmed block submission tx: ${submittedTxHash}`,
        );
        break;
      }
      case "AwaitTxFailure": {
        const cachedConfirmedBlock = yield* Ref.get(
          globals.AVAILABLE_CONFIRMED_BLOCK,
        );
        if (cachedConfirmedBlock !== "") {
          const deserializedCachedBlock =
            yield* deserializeStateQueueUTxO(cachedConfirmedBlock);
          if (
            deserializedCachedBlock.utxo.txHash !==
            confirmationOutput.latestConfirmedTxHash
          ) {
            yield* syncUnsubmittedBlocksWithLatestOnchainBlock(
              confirmationOutput.latestBlock,
            );
            yield* Ref.set(
              globals.AVAILABLE_CONFIRMED_BLOCK,
              confirmationOutput.serializedLatestBlock,
            );
            yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
            yield* Effect.logWarning(
              `🔗 ⚠️  Submitted tx ${submittedTxHash} did not confirm and chain advanced to ${confirmationOutput.latestConfirmedTxHash}. Cache has been resynchronized.`,
            );
            break;
          }
        }
        yield* Effect.logWarning(
          `🔗 ⚠️  Submitted block tx is still unconfirmed: ${submittedTxHash}. ${confirmationOutput.error}. Latest on-chain tx hash is ${confirmationOutput.latestConfirmedTxHash}.`,
        );
        break;
      }
    }
  });

export const submitBlocks: Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxSignError
  | TxSubmitError
  | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const lucid = yield* Lucid;

  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (resetInProgress) {
    return;
  }

  const availableConfirmedBlock = yield* Ref.get(
    globals.AVAILABLE_CONFIRMED_BLOCK,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );

  if (unconfirmedSubmittedBlockTxHash !== "") {
    yield* handleUnconfirmedSubmission(
      lucid.api,
      globals,
      unconfirmedSubmittedBlockTxHash,
    );
    return;
  }

  if (availableConfirmedBlock === "") {
    yield* syncLatestConfirmedBlock(lucid.api, globals);
    return;
  }

  const unsubmittedBlocks = yield* UnsubmittedBlocksDB.retrieve;
  if (unsubmittedBlocks.length <= 0) {
    return;
  }

  const nextUnsubmittedBlock = unsubmittedBlocks[0];
  const blockHashHex =
    nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.HEADER_HASH].toString(
      "hex",
    );
  const txCborHex =
    nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.L1_CBOR].toString("hex");

  yield* Effect.logInfo(
    `🔗 Submitting next unsubmitted block transaction for block ${blockHashHex}`,
  );
  const submittedTxHash = yield* handleSignSubmitNoConfirmation(
    lucid.api,
    lucid.api.fromTx(txCborHex),
  );
  yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, submittedTxHash);

  yield* handleUnconfirmedSubmission(lucid.api, globals, submittedTxHash);
});

export const submitBlocksFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Database | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Submit blocks fiber started.");
    const action = submitBlocks.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
