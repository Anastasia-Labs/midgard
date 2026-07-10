import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, TxHash } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  SerializedStateQueueUTxO,
  serializeStateQueueUTxO,
} from "./commit-block-header.js";

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
  canonicalHeaders: readonly SerializedCanonicalCommittedHeader[];
};

export type NoTxForConfirmationOutput = {
  type: "NoTxForConfirmationOutput";
};

export type StaleUnconfirmedRecoveryOutput = {
  type: "StaleUnconfirmedRecoveryOutput";
  stalePendingHeaderHash: string;
  staleSubmittedTxHash: "" | TxHash;
  latestBlocksUTxO: SerializedStateQueueUTxO;
  canonicalHeaders: readonly SerializedCanonicalCommittedHeader[];
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

export type SerializedCanonicalCommittedHeader = {
  readonly headerHash: string;
  readonly endTimeMs: number;
  readonly blockUTxO: SerializedStateQueueUTxO;
};

export const pendingBlockHasSubmittedTx = (
  pendingBlock: PendingBlockConfirmation,
): boolean => pendingBlock.submittedTxHash.length > 0;

export const shouldDeferUnsubmittedPendingBlockRecovery = ({
  pendingBlock,
  nowMs,
  recoveryGraceMs,
}: {
  readonly pendingBlock: PendingBlockConfirmation;
  readonly nowMs: number;
  readonly recoveryGraceMs: number;
}): boolean => nowMs <= pendingBlock.blockEndTimeMs + recoveryGraceMs;

export type UnsubmittedPendingBlockRecoveryDecision =
  | "recover_canonical"
  | "defer"
  | "recover_stale";

export const decideUnsubmittedPendingBlockRecovery = ({
  canonicalMatchFound,
  pendingBlock,
  nowMs,
  recoveryGraceMs,
}: {
  readonly canonicalMatchFound: boolean;
  readonly pendingBlock: PendingBlockConfirmation;
  readonly nowMs: number;
  readonly recoveryGraceMs: number;
}): UnsubmittedPendingBlockRecoveryDecision => {
  if (canonicalMatchFound) {
    return "recover_canonical";
  }
  return shouldDeferUnsubmittedPendingBlockRecovery({
    pendingBlock,
    nowMs,
    recoveryGraceMs,
  })
    ? "defer"
    : "recover_stale";
};

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
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
        block.datum,
      );
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

export const serializeCanonicalCommittedHeaders = (
  blocks: readonly SDK.StateQueueUTxO[],
): Effect.Effect<
  readonly SerializedCanonicalCommittedHeader[],
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
> =>
  Effect.gen(function* () {
    const headers: SerializedCanonicalCommittedHeader[] = [];
    for (const block of blocks) {
      if (block.datum.key === "Empty") {
        continue;
      }
      const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
      headers.push({
        headerHash: yield* SDK.hashBlockHeader(header),
        endTimeMs: Number(header.endTime),
        blockUTxO: yield* serializeStateQueueUTxO(block),
      });
    }
    return headers;
  });
