import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalBinary,
  encodeMidgardAddressText,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
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

const outRefToCanonicalCbor = (
  ref: { readonly txId: Buffer; readonly index: number },
): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(ref.txId),
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
    const nativeTx = yield* Effect.try({
      try: () => decodeMidgardNativeTxFullFromCanonicalBinary(txCBOR),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to decode Midgard-native tx payload`,
          cause: e,
        }),
    });

    const spent = yield* Effect.try({
      try: () => nativeTx.body.spendInputs.map(outRefToCanonicalCbor),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `An error occurred on native input CBOR serialization`,
          cause: e,
        }),
    });

    const produced: Ledger.MinimalEntry[] = [];
    const finalTxHash =
      txHash === undefined ? computeMidgardNativeTxId(nativeTx) : txHash;
    const txHashObj = CML.TransactionHash.from_raw_bytes(finalTxHash);
    for (let i = 0; i < nativeTx.body.outputs.length; i++) {
      produced.push({
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHashObj, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: encodeMidgardTxOutput(nativeTx.body.outputs[i]),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const nativeTx = yield* Effect.try({
      try: () => decodeMidgardNativeTxFullFromCanonicalBinary(txCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to deserialize Midgard-native transaction`,
          cause: e,
        }),
    });

    const txHashBytes = computeMidgardNativeTxId(nativeTx);
    const txHash = CML.TransactionHash.from_raw_bytes(txHashBytes);
    const spent = nativeTx.body.spendInputs.map(outRefToCanonicalCbor);
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < nativeTx.body.outputs.length; i++) {
      const output = nativeTx.body.outputs[i];
      produced.push({
        [Ledger.Columns.TX_ID]: txHashBytes,
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: encodeMidgardTxOutput(output),
        [Ledger.Columns.ADDRESS]: encodeMidgardAddressText(output.address),
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

	${chalk.bold("SEED_PHRASE")}	 Your wallet's seed phrase

Depending on which provider you'll be using, other environment variables may also be needed:

Blockfrost or Maestro:
	${chalk.bold("API_KEY")}    	 Your provider's API key

Kupmios:
	${chalk.bold("KUPO_URL")}   	 URL of your Kupo instance
	${chalk.bold("OGMIOS_URL")} 	 URL of your Ogmios instance
`;

export class FileSystemError extends Data.TaggedError(
  "FileSystemError",
)<SDK.GenericErrorFields> {}
