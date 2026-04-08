import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, TxHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { SerializedStateQueueUTxO } from "./commit-block-header.js";

export type WorkerInput = {
  data: {
    firstRun: boolean;
    unconfirmedSubmittedBlock: "" | TxHash;
    unconfirmedSubmittedBlockSinceMs: number;
  };
};

export type SuccessfulConfirmationOutput = {
  type: "SuccessfulConfirmationOutput";
  blocksUTxO: SerializedStateQueueUTxO;
};

export type NoTxForConfirmationOutput = {
  type: "NoTxForConfirmationOutput";
};

export type StaleUnconfirmedRecoveryOutput = {
  type: "StaleUnconfirmedRecoveryOutput";
  staleTxHash: TxHash;
  blocksUTxO: SerializedStateQueueUTxO;
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
