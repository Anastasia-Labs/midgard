import { Effect, Schedule } from "effect";
import { ConfigError, NodeConfig } from "./config.js";
import * as LE from "@lucid-evolution/lucid";

const toAdditionalScript = (
  scriptRef: LE.Script | null | undefined,
): Record<string, unknown> | undefined => {
  if (scriptRef === undefined || scriptRef === null) {
    return undefined;
  }
  switch (scriptRef.type) {
    case "PlutusV1":
      return {
        "plutus:v1": LE.applySingleCborEncoding(scriptRef.script),
      };
    case "PlutusV2":
      return {
        "plutus:v2": LE.applySingleCborEncoding(scriptRef.script),
      };
    case "PlutusV3":
      return {
        "plutus:v3": LE.applySingleCborEncoding(scriptRef.script),
      };
    default:
      return undefined;
  }
};

export const toBlockfrostAdditionalValue = (
  assets: LE.Assets,
): Record<string, unknown> => {
  const value: Record<string, unknown> = {
    coins: Number(assets.lovelace ?? 0n),
  };
  for (const [unit, amount] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    const policyAssets =
      (value[policyId] as Record<string, number> | undefined) ?? {};
    policyAssets[assetName] = Number(amount);
    value[policyId] = policyAssets;
  }
  return value;
};

type BlockfrostEvalRedeemerData = {
  memory: number | string;
  steps: number | string;
};

type BlockfrostEvalResponse =
  | {
      result: {
        EvaluationResult: Record<string, BlockfrostEvalRedeemerData>;
      };
    }
  | {
      result: Record<string, unknown>;
    }
  | {
      status_code?: number;
      message?: string;
      fault: unknown;
    };

const evaluateTxViaBlockfrostUtxoEndpoint = async (
  provider: LE.Blockfrost,
  tx: string,
  additionalUTxOs: readonly LE.UTxO[],
): Promise<LE.EvalRedeemer[]> => {
  const blockfrostProvider = provider as unknown as {
    url: string;
    projectId: string;
  };
  const payload = {
    cbor: tx,
    additionalUtxoSet: additionalUTxOs.map((utxo) => [
      {
        txId: utxo.txHash,
        index: utxo.outputIndex,
      },
      {
        address: utxo.address,
        value: toBlockfrostAdditionalValue(utxo.assets),
        datumHash: utxo.datumHash,
        datum: utxo.datum,
        script: toAdditionalScript(utxo.scriptRef),
      },
    ]),
  };
  const res = (await fetch(
    `${blockfrostProvider.url}/utils/txs/evaluate/utxos`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        project_id: blockfrostProvider.projectId,
      },
      body: JSON.stringify(payload),
    },
  ).then((response) => response.json())) as BlockfrostEvalResponse;

  if ("fault" in res) {
    const message =
      res.status_code === 400
        ? res.message
        : `Could not evaluate the transaction: ${JSON.stringify(res)}. Transaction: ${tx}`;
    throw new Error(message);
  }
  if (!("EvaluationResult" in res.result)) {
    throw new Error(`EvaluateTransaction fails: ${JSON.stringify(res.result)}`);
  }
  const evaluationResult = res.result.EvaluationResult as Record<
    string,
    BlockfrostEvalRedeemerData
  >;

  const evalRedeemers: LE.EvalRedeemer[] = [];
  for (const [redeemerPointer, data] of Object.entries(evaluationResult)) {
    const [pTag, pIndex] = redeemerPointer.split(":");
    evalRedeemers.push({
      redeemer_tag:
        pTag === "certificate"
          ? "publish"
          : pTag === "withdrawal"
            ? "withdraw"
            : (pTag as LE.RedeemerTag),
      redeemer_index: Number(pIndex),
      ex_units: {
        mem: Number(data.memory),
        steps: Number(data.steps),
      },
    });
  }
  return evalRedeemers;
};

const patchBlockfrostEvaluateTx = (
  provider: LE.Blockfrost,
): LE.Blockfrost => {
  provider.evaluateTx = async (tx, additionalUTxOs) => {
    return evaluateTxViaBlockfrostUtxoEndpoint(
      provider,
      tx,
      additionalUTxOs ?? [],
    );
  };
  return provider;
};

const makeLucid: Effect.Effect<
  {
    api: LE.LucidEvolution;
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  yield* Effect.logInfo("Initializing Lucid...");
  const lucid: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => {
      switch (nodeConfig.L1_PROVIDER) {
        case "Kupmios":
          return LE.Lucid(
            new LE.Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "Blockfrost":
          return LE.Lucid(
            patchBlockfrostEvaluateTx(
              new LE.Blockfrost(
                nodeConfig.L1_BLOCKFROST_API_URL,
                nodeConfig.L1_BLOCKFROST_KEY,
              ),
            ),
            nodeConfig.NETWORK,
          );
      }
    },
    catch: (e) =>
      new ConfigError({
        message: `An error occurred on lucid initialization`,
        cause: e,
        fieldsAndValues: [
          ["L1_PROVIDER", nodeConfig.L1_PROVIDER],
          ["NETWORK", nodeConfig.NETWORK],
        ],
      }),
  }).pipe(
    Effect.tapError(Effect.logInfo),
    Effect.retry(Schedule.fixed("1000 millis")),
  );
  yield* Effect.logInfo("Lucid built successfully.");
  return {
    api: lucid,
    switchToOperatorsMainWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      ),
    ),
  };
});

export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  effect: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}
