import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, TxHash } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { SerializedStateQueueUTxO } from "./commit-block-header.js";

export type PendingBlockConfirmation = {
  expectedHeaderHash: string;
  submittedTxHash: "" | TxHash;
  blockEndTimeMs: number;
  updatedAtMs: number;
};

export type WorkerInput = {
  data: {
    firstRun: boolean;
    pendingBlock: PendingBlockConfirmation | null;
  };
};

export type SuccessfulConfirmationOutput = {
  type: "SuccessfulConfirmationOutput";
  latestBlocksUTxO: SerializedStateQueueUTxO;
  matchedPendingBlocksUTxO: SerializedStateQueueUTxO | null;
};

export type NoTxForConfirmationOutput = {
  type: "NoTxForConfirmationOutput";
};

export type StaleUnconfirmedRecoveryOutput = {
  type: "StaleUnconfirmedRecoveryOutput";
  stalePendingHeaderHash: string;
  staleSubmittedTxHash: "" | TxHash;
  latestBlocksUTxO: SerializedStateQueueUTxO;
};

export type FailedConfirmationOutput = {
  type: "FailedConfirmationOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulConfirmationOutput
  | NoTxForConfirmationOutput
  | StaleUnconfirmedRecoveryOutput
  | FailedConfirmationOutput;

export const fetchLatestCommittedStateQueueBlock = (
  lucid: LucidEvolution,
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.StateQueueUTxO, SDK.StateQueueError | SDK.LucidError> =>
  SDK.fetchLatestCommittedBlockProgram(lucid, {
    stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  });

export const fetchSortedCommittedStateQueueBlocks = (
  lucid: LucidEvolution,
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
): Effect.Effect<
  readonly SDK.StateQueueUTxO[],
  SDK.LucidError | SDK.LinkedListError
> =>
  SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
    stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  });

export const latestCommittedStateQueueBlockFromSorted = (
  blocks: readonly SDK.StateQueueUTxO[],
): Effect.Effect<SDK.StateQueueUTxO, SDK.StateQueueError> => {
  const latestBlock = blocks.at(-1);
  if (latestBlock !== undefined) {
    return Effect.succeed(latestBlock);
  }
  return Effect.fail(
    new SDK.StateQueueError({
      message: "Failed to determine latest committed block",
      cause: "State queue is empty",
    }),
  );
};

export const resolveStateQueueBlockEndTimeMs = (
  block: SDK.StateQueueUTxO,
): Effect.Effect<number, SDK.DataCoercionError> =>
  Effect.gen(function* () {
    if (block.datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(block.datum);
      return Number(data.endTime);
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
    return Number(header.endTime);
  });

export const findCommittedStateQueueBlockByHeaderHash = (
  blocks: readonly SDK.StateQueueUTxO[],
  expectedHeaderHash: string,
): Effect.Effect<
  Option.Option<SDK.StateQueueUTxO>,
  SDK.DataCoercionError | SDK.HashingError
> =>
  Effect.gen(function* () {
    for (const block of blocks) {
      if (block.datum.key === "Empty") {
        continue;
      }
      const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
      const headerHash = yield* SDK.hashBlockHeader(header);
      if (headerHash === expectedHeaderHash) {
        return Option.some(block);
      }
    }
    return Option.none();
  });
