import {
  Lucid,
  Blockfrost,
  Network,
  Kupmios,
  Koios,
} from "@lucid-evolution/lucid";
import { Effect, pipe, Context, Layer, Config } from "effect";

const SUPPORTED_PROVIDERS = ["kupmios", "blockfrost"] as const;
type Provider = (typeof SUPPORTED_PROVIDERS)[number];

const isValidProvider = (provider: string): provider is Provider => {
  return SUPPORTED_PROVIDERS.includes(provider.toLowerCase() as Provider);
};

export type NodeConfigDep = {
  L1_PROVIDER: Provider;
  L1_BLOCKFROST_API_URL: string;
  L1_BLOCKFROST_KEY: string;
  L1_OGMIOS_KEY: string;
  L1_KUPO_KEY: string;
  L1_OPERATOR_SEED_PHRASE: string;
  NETWORK: Network;
  DATABASE_PATH: string;
  PORT: number;
  POLLING_INTERVAL: number;
  CONFIRMED_STATE_POLLING_INTERVAL: number;
  PROM_METRICS_PORT: number;
  OTLP_PORT: number;
};

export const makeUserFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* ($) {
    const user = yield* Effect.tryPromise(() => {
      switch (nodeConfig.L1_PROVIDER) {
        case "kupmios":
          return Lucid(
            new Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "blockfrost":
          return Lucid(
            new Blockfrost(
              nodeConfig.L1_BLOCKFROST_API_URL,
              nodeConfig.L1_BLOCKFROST_KEY,
            ),
            nodeConfig.NETWORK,
          );
      }
    });
    user.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
    yield* pipe(
      Effect.promise(() => user.wallet().address()),
      Effect.flatMap((address) => Effect.log(`Wallet : ${address}`)),
    );
    yield* pipe(
      Effect.promise(() => user.wallet().getUtxos()),
      Effect.flatMap((utxos) =>
        Effect.log(`Total Wallet UTxOs: ${utxos.length}`),
      ),
    );
    return {
      user,
    };
  });

const makeUser = Effect.gen(function* ($) {
  const nodeConfig = yield* NodeConfig;
  return yield* makeUserFn(nodeConfig);
}).pipe(Effect.orDie);

export class User extends Context.Tag("User")<
  User,
  Effect.Effect.Success<typeof makeUser>
>() {
  static readonly layer = Layer.effect(User, makeUser);
}

export const NETWORK: Network = "Preprod";

export const makeConfig = Effect.gen(function* ($) {
  const config = yield* Config.all([
    Config.string("L1_PROVIDER"),
    Config.string("L1_BLOCKFROST_API_URL"),
    Config.string("L1_BLOCKFROST_KEY"),
    Config.string("L1_OGMIOS_KEY"),
    Config.string("L1_KUPO_KEY"),
    Config.string("L1_OPERATOR_SEED_PHRASE"),
    Config.string("NETWORK"),
    Config.string("DATABASE_PATH"),
    Config.integer("PORT").pipe(Config.withDefault(3000)),
    Config.integer("POLLING_INTERVAL").pipe(Config.withDefault(10000)),
    Config.integer("CONFIRMED_STATE_POLLING_INTERVAL").pipe(
      Config.withDefault(60000),
    ),
    Config.integer("PROM_METRICS_PORT").pipe(Config.withDefault(9464)),
    Config.integer("OTLP_PORT").pipe(Config.withDefault(4318)),
  ]);

  const provider = config[0].toLowerCase();
  if (!isValidProvider(provider)) {
    throw new Error(
      `Invalid L1_PROVIDER: ${provider}. Supported providers: ${SUPPORTED_PROVIDERS.join(", ")}`,
    );
  }
  return {
    L1_PROVIDER: provider,
    L1_BLOCKFROST_API_URL: config[1],
    L1_BLOCKFROST_KEY: config[2],
    L1_OGMIOS_KEY: config[3],
    L1_KUPO_KEY: config[4],
    L1_OPERATOR_SEED_PHRASE: config[5],
    NETWORK: config[6] as Network,
    DATABASE_PATH: config[7],
    PORT: config[8],
    POLLING_INTERVAL: config[9],
    CONFIRMED_STATE_POLLING_INTERVAL: config[10],
    PROM_METRICS_PORT: config[11],
    OTLP_PORT: config[12],
  };
}).pipe(Effect.orDie);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}
