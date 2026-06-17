import { encodeMidgardAddressText } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  ledgerOutputToCbor,
  midgardOutRefToCbor,
} from "@al-ft/midgard-validation";
import * as chalk_ from "chalk";
import { Data, Effect, pipe } from "effect";

import * as Ledger from "@/database/utils/ledger.js";

export type ProcessedTx = {
  txId: Buffer;
  txCbor: Buffer;
  spent: Buffer[];
  produced: Ledger.Entry[];
};

export const chalk = new chalk_.Chalk();

export const findSpentAndProducedUTxOs = (
  txCBOR: Buffer,
  txHash?: Buffer,
): Effect.Effect<
  { spent: Buffer[]; produced: Ledger.MinimalEntry[] },
  SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const submittedTx = yield* Effect.try({
      try: () => decodeMidgardSubmittedTxFromCanonicalCbor(txCBOR),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode Midgard-native tx payload`,
          cause: e,
        }),
    });

    const spent = yield* Effect.try({
      try: () =>
        submittedTx.ledgerTx.spendInputs.map((outRef) =>
          midgardOutRefToCbor(outRef),
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to encode Midgard spend inputs`,
          cause: e,
        }),
    });

    const produced: Ledger.MinimalEntry[] = [];
    const finalTxHash = txHash ?? submittedTx.ledgerTx.txId;
    for (let i = 0; i < submittedTx.ledgerTx.outputs.length; i++) {
      const output = submittedTx.ledgerTx.outputs[i];
      produced.push({
        [Ledger.Columns.OUTREF]: midgardOutRefToCbor({
          txId: finalTxHash,
          index: BigInt(i),
        }),
        [Ledger.Columns.OUTPUT]: ledgerOutputToCbor(output),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const submittedTx = yield* Effect.try({
      try: () => decodeMidgardSubmittedTxFromCanonicalCbor(txCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to deserialize Midgard-native transaction`,
          cause: e,
        }),
    });

    const txHashBytes = Buffer.from(submittedTx.ledgerTx.txId);
    const spent = yield* Effect.try({
      try: () =>
        submittedTx.ledgerTx.spendInputs.map((outRef) =>
          midgardOutRefToCbor(outRef),
        ),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to encode Midgard spend inputs`,
          cause: e,
        }),
    });
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < submittedTx.ledgerTx.outputs.length; i++) {
      const output = submittedTx.ledgerTx.outputs[i];
      produced.push({
        [Ledger.Columns.TX_ID]: txHashBytes,
        [Ledger.Columns.OUTREF]: midgardOutRefToCbor({
          txId: txHashBytes,
          index: BigInt(i),
        }),
        [Ledger.Columns.OUTPUT]: ledgerOutputToCbor(output),
        [Ledger.Columns.ADDRESS]: encodeMidgardAddressText(output.address),
      });
    }
    return {
      txId: txHashBytes,
      txCbor: submittedTx.txCbor,
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
