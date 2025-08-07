import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToUtxo, utxoToCore } from "@lucid-evolution/lucid";

export type WorkerInput = {
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
    mempoolTxsCountSoFar: number;
    sizeOfProcessedTxsSoFar: number;
  };
};

export type SuccessfulSubmissionOutput = {
  type: "SuccessfulSubmissionOutput";
  submittedTxHash: string;
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
};

export type SkippedSubmissionOutput = {
  type: "SkippedSubmissionOutput";
  mempoolTxsCount: number;
  sizeOfProcessedTxs: number;
};

export type EmptyMempoolOutput = {
  type: "EmptyMempoolOutput";
};

export type FailureOutput = {
  type: "FailureOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulSubmissionOutput
  | SkippedSubmissionOutput
  | EmptyMempoolOutput
  | FailureOutput;

// Datatype to use CBOR hex of state queue UTxOs instead of `UTxO` from LE for
// transferability.
export type SerializedStateQueueUTxO = Omit<
  SDK.TxBuilder.StateQueue.StateQueueUTxO,
  "utxo"
> & { utxo: string };

export const serializeStateQueueUTxO = (
  stateQueueUTxO: SDK.TxBuilder.StateQueue.StateQueueUTxO,
): Effect.Effect<SerializedStateQueueUTxO, Error> =>
  Effect.gen(function* () {
    const core = yield* Effect.try({
      try: () => utxoToCore(stateQueueUTxO.utxo),
      catch: (e) => new Error(`${e}`),
    });
    return {
      ...stateQueueUTxO,
      utxo: core.to_cbor_hex(),
    };
  });

export const deserializeStateQueueUTxO = (
  stateQueueUTxO: SerializedStateQueueUTxO,
): Effect.Effect<SDK.TxBuilder.StateQueue.StateQueueUTxO, Error> =>
  Effect.gen(function* () {
    const u = yield* Effect.try({
      try: () =>
        coreToUtxo(
          CML.TransactionUnspentOutput.from_cbor_hex(stateQueueUTxO.utxo),
        ),
      catch: (e) => new Error(`${e}`),
    });
    return {
      ...stateQueueUTxO,
      utxo: u,
    };
  });
