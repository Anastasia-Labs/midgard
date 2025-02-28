import { Lucid, Blockfrost, Network } from "@lucid-evolution/lucid";
import { Effect, pipe, Context, Layer, Config } from "effect";

const makeUser = Effect.gen(function* ($) {
  const nodeConfig = yield* NodeConfig;
  const user = yield* Effect.tryPromise(() =>
    Lucid(
      new Blockfrost(nodeConfig.BLOCKFROST_API_URL, nodeConfig.BLOCKFROST_KEY),
      nodeConfig.NETWORK,
    ),
  );
  user.selectWallet.fromSeed(nodeConfig.SEED_PHRASE);
  yield* pipe(
    Effect.promise(() => user.wallet().address()),
    Effect.flatMap((address) => Effect.logInfo(`Wallet : ${address}`)),
  );
  yield* pipe(
    Effect.promise(() => user.wallet().getUtxos()),
    Effect.flatMap((utxos) =>
      Effect.logInfo(`Total Wallet UTxOs: ${utxos.length}`),
    ),
  );
  return {
    user,
  };
}).pipe(Effect.orDie);

export class User extends Context.Tag("User")<
  User,
  Effect.Effect.Success<typeof makeUser>
>() {
  static readonly layer = Layer.effect(User, makeUser);
}

export const NETWORK: Network = "Preprod";

const makeConfig = Effect.gen(function* ($) {
  const config = yield* Config.all([
    Config.string("BLOCKFROST_API_URL"),
    Config.string("BLOCKFROST_KEY"),
    Config.string("SEED_PHRASE"),
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
  return {
    BLOCKFROST_API_URL: config[0],
    BLOCKFROST_KEY: config[1],
    SEED_PHRASE: config[2],
    NETWORK: config[3] as Network,
    DATABASE_PATH: config[4],
    PORT: config[5],
    POLLING_INTERVAL: config[6],
    CONFIRMED_STATE_POLLING_INTERVAL: config[7],
    PROM_METRICS_PORT: config[8],
    OTLP_PORT: config[9],
  };
}).pipe(Effect.orDie);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  {
    BLOCKFROST_API_URL: string;
    BLOCKFROST_KEY: string;
    SEED_PHRASE: string;
    NETWORK: Network;
    DATABASE_PATH: string;
    PORT: number;
    POLLING_INTERVAL: number;
    CONFIRMED_STATE_POLLING_INTERVAL: number;
    PROM_METRICS_PORT: number;
    OTLP_PORT: number;
  }
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}
