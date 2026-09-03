import type * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Cause, Data, Effect, Option, pipe } from "effect";
import { parentPort, workerData } from "worker_threads";

import { ConfigError, NodeConfig } from "../services/config.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.js";
import { serializeStateQueueUTxO } from "./utils/commit-block-header.js";
import {
  decideUnsubmittedPendingBlockRecovery,
  fetchSortedCommittedStateQueueBlocks,
  findCommittedStateQueueBlockByHeaderHash,
  latestCommittedStateQueueBlockFromSorted,
  pendingBlockHasSubmittedTx,
  resolveStateQueueBlockEndTimeMs,
  serializeCanonicalCommittedHeaders,
  shouldRunFullStateQueueConfirmationScan,
  WorkerInput,
  WorkerOutput,
} from "./utils/confirm-block-commitments.js";

type StateQueueAuthValidator = SDK.MidgardValidators["stateQueue"];

class TxConfirmAwaitError extends Data.TaggedError("TxConfirmAwaitError")<{
  readonly message: string;
  readonly headerHash: string;
  readonly cause: string;
}> {}

const TARGETED_CONFIRMATION_PROBE_TIMEOUT_MS = 1_500;
const TARGETED_CONFIRMATION_PROBE_POLL_MS = 250;
export const probeSubmittedTx = (
  lucid: Pick<LucidEvolution, "awaitTxConfirmation">,
  txHash: string,
): Effect.Effect<boolean, never> =>
  Effect.promise(
    () =>
      new Promise<boolean>((resolve) => {
        const timeout = setTimeout(
          () => resolve(false),
          TARGETED_CONFIRMATION_PROBE_TIMEOUT_MS,
        );
        lucid
          .awaitTxConfirmation(txHash, {
            timeout: TARGETED_CONFIRMATION_PROBE_TIMEOUT_MS,
            checkInterval: TARGETED_CONFIRMATION_PROBE_POLL_MS,
          })
          .then(() => {
            clearTimeout(timeout);
            resolve(true);
          })
          .catch(() => {
            clearTimeout(timeout);
            resolve(false);
          });
      }),
  );

const awaitPendingBlockResolution = (
  lucid: LucidEvolution,
  stateQueueAuthValidator: StateQueueAuthValidator,
  pendingBlock: NonNullable<WorkerInput["data"]["pendingBlock"]>,
  timeoutMs: number,
) =>
  Effect.gen(function* () {
    const startedAt = Date.now();
    const pollIntervalMs = 2_000;
    while (Date.now() - startedAt < timeoutMs) {
      const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
        lucid,
        stateQueueAuthValidator,
      );
      const latestBlock =
        yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
      const latestEndTimeMs =
        yield* resolveStateQueueBlockEndTimeMs(latestBlock);
      const matchedPendingBlock =
        yield* findCommittedStateQueueBlockByHeaderHash(
          sortedBlocks,
          pendingBlock.expectedHeaderHash,
        );
      if (Option.isSome(matchedPendingBlock)) {
        return {
          latestBlock,
          matchedPendingBlock: matchedPendingBlock.value,
          sortedBlocks,
        };
      }
      if (latestEndTimeMs >= pendingBlock.blockEndTimeMs) {
        return {
          latestBlock,
          matchedPendingBlock: null,
          sortedBlocks,
        };
      }
      yield* Effect.sleep(`${pollIntervalMs} millis`);
    }
    return yield* Effect.fail(
      new TxConfirmAwaitError({
        message:
          "Timed out waiting for canonical state_queue resolution of pending block header",
        headerHash: pendingBlock.expectedHeaderHash,
        cause: `timeout_ms=${timeoutMs}`,
      }),
    );
  });

const resolveStateQueueAuthValidator = (): Effect.Effect<
  StateQueueAuthValidator,
  never,
  MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    // Effect.Service DTS generation currently widens this worker entry; keep
    // the runtime contract shape explicit at the boundary we actually need.
    return (
      contracts as unknown as {
        readonly stateQueue: StateQueueAuthValidator;
      }
    ).stateQueue;
  });

const provideConfirmationWorkerServices = <A, E>(
  effect: Effect.Effect<A, E, MidgardContracts | Lucid | NodeConfig>,
): Effect.Effect<A, E | ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContracts.Default),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

export const runConfirmBlockCommitmentsWorkerProgram = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const stateQueueAuthValidator = yield* resolveStateQueueAuthValidator();

    /**
     * Recovers confirmation processing by replaying from the latest known block.
     */
    const recoverWithLatestBlock = (
      stalePendingHeaderHash: string,
      staleSubmittedTxHash: "" | string,
    ) =>
      Effect.gen(function* () {
        const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
          lucid.api,
          stateQueueAuthValidator,
        );
        const latestBlock =
          yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
        const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
        return {
          type: "StaleUnconfirmedRecoveryOutput",
          stalePendingHeaderHash,
          staleSubmittedTxHash,
          latestBlocksUTxO: serializedUTxO,
          canonicalHeaders:
            yield* serializeCanonicalCommittedHeaders(sortedBlocks),
        } satisfies WorkerOutput;
      });

    if (workerInput.data.firstRun) {
      yield* Effect.logInfo("🔍 First run. Fetching the latest block...");
      const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
        lucid.api,
        stateQueueAuthValidator,
      );
      const latestBlock =
        yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
      const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
      return {
        type: "SuccessfulConfirmationOutput",
        latestBlocksUTxO: serializedUTxO,
        matchedPendingBlocksUTxO: null,
        canonicalHeaders:
          yield* serializeCanonicalCommittedHeaders(sortedBlocks),
      } satisfies WorkerOutput;
    }

    if (workerInput.data.pendingBlock === null) {
      yield* Effect.logInfo(
        "🔍 No active pending block. Refreshing canonical state_queue snapshot...",
      );
      const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
        lucid.api,
        stateQueueAuthValidator,
      );
      const latestBlock =
        yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
      const serializedUTxO = yield* serializeStateQueueUTxO(latestBlock);
      return {
        type: "SuccessfulConfirmationOutput",
        latestBlocksUTxO: serializedUTxO,
        matchedPendingBlocksUTxO: null,
        canonicalHeaders:
          yield* serializeCanonicalCommittedHeaders(sortedBlocks),
      } satisfies WorkerOutput;
    }

    const pendingBlock = workerInput.data.pendingBlock;
    const pendingAgeMs = Math.max(0, Date.now() - pendingBlock.updatedAtMs);
    yield* Effect.logInfo(
      `🔍 Resolving pending block header ${pendingBlock.expectedHeaderHash} (submitted_tx=${pendingBlock.submittedTxHash || "unknown"}, age_ms=${pendingAgeMs}).`,
    );
    if (!pendingBlockHasSubmittedTx(pendingBlock)) {
      const sortedBlocks = yield* fetchSortedCommittedStateQueueBlocks(
        lucid.api,
        stateQueueAuthValidator,
      );
      const latestBlock =
        yield* latestCommittedStateQueueBlockFromSorted(sortedBlocks);
      const matchedPendingBlock =
        yield* findCommittedStateQueueBlockByHeaderHash(
          sortedBlocks,
          pendingBlock.expectedHeaderHash,
        );
      const recoveryGraceMs = Math.max(
        nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS,
        30_000,
      );
      const recoveryDecision = decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: Option.isSome(matchedPendingBlock),
        pendingBlock,
        nowMs: Date.now(),
        recoveryGraceMs,
      });
      if (recoveryDecision === "recover_canonical") {
        yield* Effect.logWarning(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} has no submitted tx hash locally but is already present in canonical state_queue; recovering confirmation state instead of abandoning.`,
        );
        if (Option.isNone(matchedPendingBlock)) {
          return yield* Effect.fail(
            new TxConfirmAwaitError({
              message:
                "Unsubmitted pending recovery chose canonical recovery without a matched block",
              headerHash: pendingBlock.expectedHeaderHash,
              cause: "missing_matched_pending_block",
            }),
          );
        }
        const latestSerializedUTxO =
          yield* serializeStateQueueUTxO(latestBlock);
        const matchedSerializedUTxO = yield* serializeStateQueueUTxO(
          matchedPendingBlock.value,
        );
        return {
          type: "SuccessfulConfirmationOutput",
          latestBlocksUTxO: latestSerializedUTxO,
          matchedPendingBlocksUTxO: matchedSerializedUTxO,
          canonicalHeaders:
            yield* serializeCanonicalCommittedHeaders(sortedBlocks),
        } satisfies WorkerOutput;
      }
      if (recoveryDecision === "defer") {
        yield* Effect.logWarning(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} has no submitted tx hash and is not canonical yet; deferring stale recovery until the block validity window plus grace has elapsed (age_ms=${pendingAgeMs}, block_end_ms=${pendingBlock.blockEndTimeMs.toString()}, grace_ms=${recoveryGraceMs.toString()}).`,
        );
        return {
          type: "NoTxForConfirmationOutput",
        } satisfies WorkerOutput;
      }
      yield* Effect.logWarning(
        `🔍 Pending block header ${pendingBlock.expectedHeaderHash} has no submitted tx hash and is still absent from canonical state_queue after its recovery grace; abandoning unsubmitted journal and recovering canonical state_queue tip.`,
      );
      return yield* recoverWithLatestBlock(
        pendingBlock.expectedHeaderHash,
        pendingBlock.submittedTxHash,
      );
    }
    const targetedTxConfirmed = yield* probeSubmittedTx(
      lucid.api,
      pendingBlock.submittedTxHash,
    );
    const expiryRecoveryGraceMs = Math.max(
      nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS,
      30_000,
    );
    const validityExpired =
      Date.now() > pendingBlock.blockEndTimeMs + expiryRecoveryGraceMs;
    if (
      !shouldRunFullStateQueueConfirmationScan({
        targetedTxConfirmed,
        pendingAgeMs,
        unconfirmedBlockMaxAgeMs: nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS,
        validityExpired,
      })
    ) {
      yield* Effect.logDebug(
        `🔍 Targeted submitted-tx probe has not observed ${pendingBlock.submittedTxHash}; deferring the periodic full state_queue scan.`,
      );
      return {
        type: "NoTxForConfirmationOutput",
      } satisfies WorkerOutput;
    }
    const confirmationResult = yield* Effect.either(
      awaitPendingBlockResolution(
        lucid.api,
        stateQueueAuthValidator,
        pendingBlock,
        nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS,
      ),
    );

    if (confirmationResult._tag === "Left") {
      yield* Effect.logWarning(
        `🔍 Pending block header ${pendingBlock.expectedHeaderHash} not resolved yet (submitted_tx=${pendingBlock.submittedTxHash || "unknown"}, age_ms=${pendingAgeMs}, timeout_ms=${nodeConfig.BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS}).`,
      );
      if (Date.now() > pendingBlock.blockEndTimeMs + expiryRecoveryGraceMs) {
        yield* Effect.logWarning(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} passed its validity upper bound without confirmation; abandoning expired submission and recovering canonical state_queue tip.`,
        );
        return yield* recoverWithLatestBlock(
          pendingBlock.expectedHeaderHash,
          pendingBlock.submittedTxHash,
        );
      }
      if (pendingAgeMs >= nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS) {
        yield* Effect.logWarning(
          `🔍 Pending block header ${pendingBlock.expectedHeaderHash} exceeded warning age (${nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS}ms) without deterministic chain resolution.`,
        );
      }
      return {
        type: "NoTxForConfirmationOutput",
      } satisfies WorkerOutput;
    }

    const resolved = confirmationResult.right;
    if (resolved.matchedPendingBlock !== null) {
      yield* Effect.logInfo(
        `🔍 Pending block header ${pendingBlock.expectedHeaderHash} is present in canonical state_queue.`,
      );
      const latestSerializedUTxO = yield* serializeStateQueueUTxO(
        resolved.latestBlock,
      );
      const matchedSerializedUTxO = yield* serializeStateQueueUTxO(
        resolved.matchedPendingBlock,
      );
      yield* Effect.logInfo("🔍 Done.");
      return {
        type: "SuccessfulConfirmationOutput",
        latestBlocksUTxO: latestSerializedUTxO,
        matchedPendingBlocksUTxO: matchedSerializedUTxO,
        canonicalHeaders: yield* serializeCanonicalCommittedHeaders(
          resolved.sortedBlocks,
        ),
      } satisfies WorkerOutput;
    }

    yield* Effect.logWarning(
      `🔍 Canonical state_queue advanced past pending block header ${pendingBlock.expectedHeaderHash} without including it; abandoning that pending submission.`,
    );
    return yield* recoverWithLatestBlock(
      pendingBlock.expectedHeaderHash,
      pendingBlock.submittedTxHash,
    );
  });

if (parentPort !== null) {
  const inputData = workerData as WorkerInput;
  const program = pipe(
    runConfirmBlockCommitmentsWorkerProgram(inputData),
    provideConfirmationWorkerServices,
  );

  void Effect.runPromise(
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
}
