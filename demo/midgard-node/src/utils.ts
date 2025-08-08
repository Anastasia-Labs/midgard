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
import { Effect } from "effect";
import * as LedgerUtils from "@/database/utils/ledger.js";

export interface WorkerInput {
  data: {
    command: string;
  };
}

export interface WorkerOutput {
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
}

export type ProcessedTx = {
  txId: Buffer;
  txCbor: Buffer;
  spent: Buffer[];
  produced: LedgerUtils.Entry[];
};

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

export const bufferToHex = (buf: Buffer): string => {
  try {
    return buf.toString("hex");
  } catch (_) {
    return "<no hex for undefined>";
  }
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
): Effect.Effect<{ spent: Buffer[]; produced: LedgerUtils.MinimalEntry[] }, Error> =>
  Effect.gen(function* () {
    const spent: Buffer[] = [];
    const produced: LedgerUtils.MinimalEntry[] = [];
    const tx = CML.Transaction.from_cbor_bytes(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const outputs = txBody.outputs();
    const inputsCount = inputs.len();
    const outputsCount = outputs.len();
    for (let i = 0; i < inputsCount; i++) {
      yield* Effect.try({
        try: () => spent.push(Buffer.from(inputs.get(i).to_cbor_bytes())),
        catch: (e) => new Error(`${e}`),
      });
    }
    const finalTxHash =
      txHash === undefined
        ? CML.hash_transaction(txBody).to_raw_bytes()
        : txHash;
    for (let i = 0; i < outputsCount; i++) {
      produced.push({
        [LedgerUtils.Columns.OUTREF]: Buffer.from(finalTxHash),
        [LedgerUtils.Columns.OUTPUT]: Buffer.from(outputs.get(i).to_cbor_bytes()),
      });
    }
    return { spent, produced };
  });

export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, Error> =>
  Effect.gen(function* () {
    const deserializedTx = yield* Effect.try({
      try: () => CML.Transaction.from_cbor_bytes(txCbor),
      catch: (e) => new Error(`${e}`),
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
    const produced: LedgerUtils.Entry[] = [];
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      produced.push({
        [LedgerUtils.Columns.TX_ID]: txHashBytes,
        [LedgerUtils.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [LedgerUtils.Columns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
        [LedgerUtils.Columns.ADDRESS]: output.address().to_bech32(),
      });
    }
    return {
      txId: txHashBytes,
      txCbor: Buffer.from(txCbor),
      spent: spent,
      produced: produced,
    };
  });

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
