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
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { fromHex, LucidEvolution, TxHash } from "@lucid-evolution/lucid";
import { Effect, Ref, Schedule } from "effect";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";

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

const confirmSubmittedBlock = (
  lucid: LucidEvolution,
  targetTxHash: TxHash,
): Effect.Effect<
  SDK.StateQueueUTxO,
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError,
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

    if (latestBlock.utxo.txHash === targetTxHash) {
      return latestBlock;
    } else {
      return yield* new TxConfirmError({
        message:
          "After multiple attempts, the block available on-chain has not updated yet.",
        cause: "Unknown",
        txHash: targetTxHash,
      });
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
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxConfirmError,
  AlwaysSucceedsContract | Database
> =>
  Effect.gen(function* () {
    const confirmedBlock = yield* confirmSubmittedBlock(lucid, submittedTxHash);
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(confirmedBlock);
    yield* Ref.set(
      globals.AVAILABLE_CONFIRMED_BLOCK,
      yield* serializeStateQueueUTxO(confirmedBlock),
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Effect.logInfo(
      `🔗 ☑️  Confirmed block submission tx: ${submittedTxHash}`,
    );
  });

export const submitBlocks: Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError
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
