import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "./commit-block-header.js";

export type WorkerInput = {
  data: {
    firstRun: boolean;
    unconfirmedSubmittedBlock: "" | TxHash;
  };
};

export type SuccessfulConfirmationOutput = {
  type: "SuccessfulConfirmationOutput";
  blocksUTxO: SerializedStateQueueUTxO;
};

export type SuccessfulRootCalculationOutput = {
  type: "SuccessfulRootCalculationOutput";
  mptRoot: string;
};

export type FailedRootCalculationOutput = {
  type: "FailedRootCalculationOutput";
  error: string;
};

export type NoTxForConfirmationOutput = {
  type: "NoTxForConfirmationOutput";
};

export type FailedConfirmationOutput = {
  type: "FailedConfirmationOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulConfirmationOutput
  | NoTxForConfirmationOutput
  | FailedConfirmationOutput
  | SuccessfulRootCalculationOutput
  | FailedRootCalculationOutput;
