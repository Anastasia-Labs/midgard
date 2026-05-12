import { parentPort, workerData } from "worker_threads";
import { CML } from "@lucid-evolution/lucid";
import * as chalk_ from "chalk";
import { Data, Effect, pipe } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  decodeTransaction,
  encodeTransactionOutput,
  midgardAddressToText,
  transactionId,
  type OutputReference,
} from "@/midgard-tx-codec/index.js";

export type ProcessedTx = {
  txId: Buffer;
  txCbor: Buffer;
  spent: Buffer[];
  produced: Ledger.Entry[];
};

// For some reason importing these directly into the new confirmation worker
// failed. This is probably a temporary workaround.
export const reexportedParentPort = parentPort;
export const reexportedWorkerData = workerData;

export const chalk = new chalk_.Chalk();

export type ProviderName = "Blockfrost" | "Koios" | "Kupmios" | "Maestro";

/**
 * Logs a success message to the console.
 */
export const logSuccess = (msg: string) => {
  Effect.runSync(Effect.logInfo(`🎉 ${msg}`));
};

/**
 * Logs a warning message to the console.
 */
export const logWarning = (msg: string) => {
  Effect.runSync(Effect.logWarning(`⚠️  ${msg}`));
};

/**
 * Logs an abort message to the console.
 */
export const logAbort = (msg: string) => {
  Effect.runSync(Effect.logError(msg));
};

/**
 * Logs an informational message to the console.
 */
export const logInfo = (msg: string) => {
  Effect.runSync(Effect.logInfo(`ℹ️  ${msg}`));
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

/** CBOR bytes of a Cardano `TransactionInput` for a Midgard output reference. */
const outRefToCardanoInputCbor = (ref: OutputReference): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(ref.tx_id),
      BigInt(ref.index),
    ).to_cbor_bytes(),
  );

export const findSpentAndProducedUTxOs = (
  txCBOR: Buffer,
  txHash?: Buffer,
): Effect.Effect<
  { spent: Buffer[]; produced: Ledger.MinimalEntry[] },
  SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const tx = yield* Effect.try({
      try: () => decodeTransaction(txCBOR),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode Midgard tx payload`,
          cause: e,
        }),
    });

    const spent = yield* Effect.try({
      try: () => tx.body.inputs.map(outRefToCardanoInputCbor),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `An error occurred on Midgard input CBOR serialization`,
          cause: e,
        }),
    });

    const produced: Ledger.MinimalEntry[] = [];
    const finalTxHash =
      txHash === undefined ? Buffer.from(transactionId(tx)) : txHash;
    const txHashObj = CML.TransactionHash.from_raw_bytes(finalTxHash);
    for (let i = 0; i < tx.body.outputs.length; i++) {
      produced.push({
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHashObj, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(
          encodeTransactionOutput(tx.body.outputs[i]),
        ),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const tx = yield* Effect.try({
      try: () => decodeTransaction(txCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to deserialize Midgard transaction`,
          cause: e,
        }),
    });

    const txHashBytes = Buffer.from(transactionId(tx));
    const txHash = CML.TransactionHash.from_raw_bytes(txHashBytes);
    const spent = yield* Effect.try({
      try: () => tx.body.inputs.map(outRefToCardanoInputCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to encode Midgard spend inputs`,
          cause: e,
        }),
    });
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < tx.body.outputs.length; i++) {
      const output = tx.body.outputs[i];
      const outputBytes = encodeTransactionOutput(output);
      produced.push({
        [Ledger.Columns.TX_ID]: txHashBytes,
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(outputBytes),
        [Ledger.Columns.ADDRESS]: midgardAddressToText(output.address),
      });
    }
    return {
      txId: txHashBytes,
      txCbor: Buffer.from(txCbor),
      spent,
      produced,
    };
  });

/**
 * Given a batch size and a total count, the required continuation will be
 * provided with start and end indices.
 *
 * @param batchSize - Size of each batch
 * @param totalCount - Total count of the iterable meant to be batched
 * @param opName - A name to make logs more readable (doesn't affect the logic)
 * @param effectMaker - A continuation that is provided with starting and ending indices for each batch.
 */
export const batchProgram = <A, E, C>(
  batchSize: number,
  totalCount: number,
  opName: string,
  effectMaker: (startIndex: number, endIndex: number) => Effect.Effect<A, E, C>,
  concurrencyOverride?: number,
) => {
  const batchIndices = Array.from(
    { length: Math.ceil(totalCount / batchSize) },
    (_, i) => i * batchSize,
  );
  return Effect.forEach(
    batchIndices,
    (startIndex) => {
      const endIndex = startIndex + batchSize;
      return pipe(
        effectMaker(startIndex, endIndex),
        Effect.withSpan(`batch-${opName}-${startIndex}-${endIndex}`),
      );
    },
    { concurrency: concurrencyOverride ?? "unbounded" },
  );
};

export const ENV_VARS_GUIDE = `
Make sure you first have set the environment variable for your seed phrase:

\u0009${chalk.bold("SEED_PHRASE")}\u0009 Your wallet's seed phrase

Depending on which provider you'll be using, other environment variables may also be needed:

Blockfrost or Maestro:
\u0009${chalk.bold("API_KEY")}    \u0009 Your provider's API key

Kupmios:
\u0009${chalk.bold("KUPO_URL")}   \u0009 URL of your Kupo instance
\u0009${chalk.bold("OGMIOS_URL")} \u0009 URL of your Ogmios instance
`;

export class FileSystemError extends Data.TaggedError(
  "FileSystemError",
)<SDK.GenericErrorFields> {}
