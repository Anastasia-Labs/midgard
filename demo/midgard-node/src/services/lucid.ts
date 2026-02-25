import { Effect, Schedule } from "effect";
import { ConfigError, NodeConfig } from "./config.js";
import * as LE from "@lucid-evolution/lucid";

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
            new LE.Blockfrost(
              nodeConfig.L1_BLOCKFROST_API_URL,
              nodeConfig.L1_BLOCKFROST_KEY,
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
