import {
  Blockfrost,
  CML,
  coreToOutRef,
  coreToTxOutput,
  Koios,
  Kupmios,
  Lucid,
  LucidEvolution,
  Maestro,
  Network,
  OutRef,
  Provider,
  UTxO,
} from "@lucid-evolution/lucid";
import * as chalk_ from "chalk";
import { Effect } from "effect";

export const chalk = new chalk_.Chalk();

export type ProviderName = "Blockfrost" | "Koios" | "Kupmios" | "Maestro";

export const errorToString = (error: any): string => {
  return error.message ?? JSON.stringify(error);
};

export const showTime = (d: Date): string => {
  return d
    .toLocaleString("en-US", {
      month: "2-digit",
      day: "2-digit",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    })
    .replace(/\//g, ".");
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
    Effect.logError("No wallet seed phrase found (SEED_PHRASE)");
    process.exit(1);
  }
  const networkStr = `${network}`.toLowerCase();
  let provider: Provider;
  if (providerName === "Blockfrost" || providerName === "Maestro") {
    const apiKey = process.env.API_KEY;
    if (!apiKey) {
      Effect.logError("No API key was found (API_KEY)");
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
      Effect.logError(
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
    Effect.logError(errorToString(e));
    process.exit(1);
  }
};

export const findSpentAndProducedUTxOs = (
  txCBOR: string,
): Effect.Effect<{ spent: OutRef[]; produced: UTxO[] }, Error> => {
  try {
    const spent: OutRef[] = [];
    const produced: UTxO[] = [];
    const tx = CML.Transaction.from_cbor_hex(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const outputs = txBody.outputs();
    for (let i = 0; i < inputs.len(); i++) {
      try {
        spent.push(coreToOutRef(inputs.get(i)));
      } catch (e) {
        Effect.logInfo(`Catched in findSpentAndProducedUTxOs ${e}`);
      }
    }
    const txHash = CML.hash_transaction(txBody).to_hex();
    for (let i = 0; i < outputs.len(); i++) {
      try {
        const utxo: UTxO = {
          txHash: txHash,
          outputIndex: i,
          ...coreToTxOutput(outputs.get(i)),
        };
        produced.push(utxo);
      } catch (e) {
        Effect.logInfo(`Catched in findSpentAndProducedUTxOs ${e}`);
      }
    }
    return Effect.succeed({ spent, produced });
  } catch (_e) {
    return Effect.fail(
      new Error(`Something went wrong decoding the transaction: ${_e}`),
    );
  }
};

export const findAllSpentAndProducedUTxOs = (
  txCBORs: string[],
): Effect.Effect<{ spent: OutRef[]; produced: UTxO[] }, Error> =>
  Effect.gen(function* () {
    const allEffects = Effect.validateAll(findSpentAndProducedUTxOs)(txCBORs);
    const allSpentsAndProduces = yield* Effect.mapError(
      allEffects,
      (errors: [Error, ...Error[]]) => {
        return new Error(errors.map((e) => `${e}`).join("\n"));
      },
    );
    return allSpentsAndProduces.reduce(
      (
        { spent: spentAcc, produced: producedAcc },
        { spent: currSpent, produced: currProduced },
      ) => {
        return {
          spent: [...spentAcc, ...currSpent],
          produced: [...producedAcc, ...currProduced],
        };
      },
    );
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
