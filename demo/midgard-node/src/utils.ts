import { parentPort, workerData } from "worker_threads";
import { CML } from "@lucid-evolution/lucid";
import * as chalk_ from "chalk";
import { Data, Effect, pipe } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
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

export const logSuccess = (msg: string) => {
  Effect.runSync(Effect.logInfo(`🎉 ${msg}`));
};

export const logWarning = (msg: string) => {
  Effect.runSync(Effect.logWarning(`⚠️  ${msg}`));
};

export const logAbort = (msg: string) => {
  Effect.runSync(Effect.logError(msg));
};

export const logInfo = (msg: string) => {
  Effect.runSync(Effect.logInfo(`ℹ️  ${msg}`));
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

export const findSpentAndProducedUTxOs = (
  txCBOR: Buffer,
  txHash?: Buffer,
): Effect.Effect<
  { spent: Buffer[]; produced: Ledger.MinimalEntry[] },
  SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const nativeTx = yield* Effect.try({
      try: () => decodeMidgardNativeTxFull(txCBOR),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode Midgard-native tx payload`,
          cause: e,
        }),
    });

    const nativeSpent = yield* Effect.try({
      try: () =>
        decodeMidgardNativeByteListPreimage(
          nativeTx.body.spendInputsPreimageCbor,
          "native.spend_inputs",
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode native spend inputs`,
          cause: e,
        }),
    });

    const nativeOutputs = yield* Effect.try({
      try: () =>
        decodeMidgardNativeByteListPreimage(
          nativeTx.body.outputsPreimageCbor,
          "native.outputs",
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode native outputs`,
          cause: e,
        }),
    });

    const spent = yield* Effect.try({
      try: () =>
        nativeSpent.map((outRef) =>
          Buffer.from(CML.TransactionInput.from_cbor_bytes(outRef).to_cbor_bytes()),
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `An error occurred on native input CBOR serialization`,
          cause: e,
        }),
    });

    const produced: Ledger.MinimalEntry[] = [];
    const finalTxHash =
      txHash === undefined ? computeMidgardNativeTxIdFromFull(nativeTx) : txHash;
    const txHashObj = CML.TransactionHash.from_raw_bytes(finalTxHash);
    for (let i = 0; i < nativeOutputs.length; i++) {
      const output = nativeOutputs[i];
      produced.push({
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHashObj, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(output),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const nativeTx = yield* Effect.try({
      try: () => decodeMidgardNativeTxFull(txCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to deserialize Midgard-native transaction`,
          cause: e,
        }),
    });

    const txHashBytes = computeMidgardNativeTxIdFromFull(nativeTx);
    const txHash = CML.TransactionHash.from_raw_bytes(txHashBytes);
    const spent = yield* Effect.try({
      try: () =>
        decodeMidgardNativeByteListPreimage(
          nativeTx.body.spendInputsPreimageCbor,
          "native.spend_inputs",
        ).map((outRef) =>
          Buffer.from(CML.TransactionInput.from_cbor_bytes(outRef).to_cbor_bytes()),
        ),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to decode native spend inputs`,
          cause: e,
        }),
    });
    const outputBytes = yield* Effect.try({
      try: () =>
        decodeMidgardNativeByteListPreimage(
          nativeTx.body.outputsPreimageCbor,
          "native.outputs",
        ),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to decode native outputs`,
          cause: e,
        }),
    });
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < outputBytes.length; i++) {
      const output = CML.TransactionOutput.from_cbor_bytes(outputBytes[i]);
      produced.push({
        [Ledger.Columns.TX_ID]: txHashBytes,
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
        [Ledger.Columns.ADDRESS]: output.address().to_bech32(),
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
