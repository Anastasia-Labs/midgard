import { parentPort, workerData } from "worker_threads";
import {
  Blockfrost,
  CML,
  Koios,
  Kupmios,
  Lucid,
  LucidEvolution,
  Maestro,
  Network,
  Provider,
} from "@lucid-evolution/lucid";
import * as chalk_ from "chalk";
import { Data, Effect, pipe } from "effect";
import * as Ledger from "@/database/utils/ledger.js";

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

export const setupLucid = async (
  network: Network,
  providerName: ProviderName,
): Promise<LucidEvolution> => {
  const seedPhrase = process.env.SEED_PHRASE;
  if (!seedPhrase) {
    logAbort("No wallet seed phrase found (SEED_PHRASE)");
    process.exit(1);
  }
  const networkStr = `${network}`.toLowerCase();
  let provider: Provider;
  if (providerName === "Blockfrost" || providerName === "Maestro") {
    const apiKey = process.env.API_KEY;
    if (!apiKey) {
      logAbort("No API key was found (API_KEY)");
      process.exit(1);
    }
    if (providerName === "Blockfrost") {
      provider = new Blockfrost(
        `https://cardano-${networkStr}.blockfrost.io/api/v0`,
        apiKey,
      );
    } else {
      provider = new Maestro({
        network: network === "Custom" ? "Mainnet" : network,
        apiKey,
      });
    }
  } else if (providerName === "Koios") {
    provider = new Koios(
      `https://${network === "Mainnet" ? "api" : networkStr}.koios.rest/api/v1`,
    );
  } else {
    const kupoURL = process.env.KUPO_URL;
    const ogmiosURL = process.env.OGMIOS_URL;
    if (!kupoURL || !ogmiosURL) {
      logAbort(
        "Make sure to set both KUPO_URL and OGMIOS_URL environment variables",
      );
      process.exit(1);
    }
    provider = new Kupmios(kupoURL, ogmiosURL);
  }
  try {
    const lucid = await Lucid(provider, network);
    lucid.selectWallet.fromSeed(seedPhrase);
    return lucid;
  } catch (e) {
    logAbort(`${e}`);
    process.exit(1);
  }
};

export const findSpentAndProducedUTxOs = (
  txCBOR: Buffer,
  txHash?: Buffer,
): Effect.Effect<
  { spent: Buffer[]; produced: Ledger.MinimalEntry[] },
  CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const spent: Buffer[] = [];
    const produced: Ledger.MinimalEntry[] = [];
    const tx = CML.Transaction.from_cbor_bytes(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const outputs = txBody.outputs();
    const inputsCount = inputs.len();
    const outputsCount = outputs.len();
    for (let i = 0; i < inputsCount; i++) {
      yield* Effect.try({
        try: () => spent.push(Buffer.from(inputs.get(i).to_cbor_bytes())),
        catch: (e) =>
          new CmlUnexpectedError({
            message: `An error occurred on input CBOR serialization`,
            cause: e,
          }),
      });
    }
    const finalTxHash =
      txHash === undefined
        ? CML.hash_transaction(txBody).to_raw_bytes()
        : txHash;
    for (let i = 0; i < outputsCount; i++) {
      produced.push({
        [Ledger.Columns.OUTREF]: Buffer.from(finalTxHash),
        [Ledger.Columns.OUTPUT]: Buffer.from(outputs.get(i).to_cbor_bytes()),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, CmlDeserializationError> =>
  Effect.gen(function* () {
    const deserializedTx = yield* Effect.try({
      try: () => CML.Transaction.from_cbor_bytes(txCbor),
      catch: (e) =>
        new CmlDeserializationError({
          message: `Failed to deserialize transaction: ${txCbor}`,
          cause: e,
        }),
    });
    const txBody = deserializedTx.body();
    const txHash = CML.hash_transaction(txBody);
    const txHashBytes = Buffer.from(txHash.to_raw_bytes());
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    const spent: Buffer[] = [];
    for (let i = 0; i < inputsCount; i++) {
      spent.push(Buffer.from(inputs.get(i).to_cbor_bytes()));
    }
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
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
      spent: spent,
      produced: produced,
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
    { concurrency: "unbounded" },
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

export type GenericErrorFields = {
  readonly message: string;
  readonly cause?: unknown;
};

// General errors that don't have specific domains

export class CmlUnexpectedError extends Data.TaggedError(
  "CmlUnexpectedError",
)<GenericErrorFields> {}

export class CmlDeserializationError extends Data.TaggedError(
  "CmlDeserializationError",
)<GenericErrorFields> {}

export class CborSerializationError extends Data.TaggedError(
  "CborSerializationError",
)<GenericErrorFields> {}

export class CborDeserializationError extends Data.TaggedError(
  "CborDeserializationError",
)<GenericErrorFields> {}

export class LucidError extends Data.TaggedError(
  "LucidError",
)<GenericErrorFields> {}
