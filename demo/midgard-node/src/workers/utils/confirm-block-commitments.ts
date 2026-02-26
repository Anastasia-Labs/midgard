import { SerializedStateQueueUTxO } from "./commit-block-header.js";

export type WorkerInput = {
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  };
};

export type SuccessfulConfirmationOutput = {
  type: "SuccessfulConfirmationOutput";
  blocksUTxO: SerializedStateQueueUTxO;
};

export type NoUnsubmittedBlocksOutput = {
  type: "NoUnsubmittedBlocksOutput";
};

export type FailedConfirmationOutput = {
  type: "FailedConfirmationOutput";
  error: string;
};

export type SubmittedButUnconfirmedOutput = {
  type: "SubmittedButUnconfirmedOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulConfirmationOutput
  | NoUnsubmittedBlocksOutput
  | SubmittedButUnconfirmedOutput
  | FailedConfirmationOutput;
