import { Blockfrost, Kupmios, Lucid, Network } from "@lucid-evolution/lucid";
import { Config, Context, Effect, Layer } from "effect";

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
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  NETWORK: Network;
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENT: number;
  WAIT_BETWEEN_BLOCK_CONFIRMATION: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  LEDGER_MPT_DB_PATH: string;
  MEMPOOL_MPT_DB_PATH: string;
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A: string;
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B: string;
  TESTNET_GENESIS_WALLET_SEED_PHRASE_C: string;
};

export const makeUserFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* () {
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
    return {
      user,
    };
  });

const makeUser = Effect.gen(function* () {
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

export const makeConfig = Effect.gen(function* () {
  const config = yield* Config.all([
    Config.string("L1_PROVIDER"),
    Config.string("L1_BLOCKFROST_API_URL"),
    Config.string("L1_BLOCKFROST_KEY"),
    Config.string("L1_OGMIOS_KEY"),
    Config.string("L1_KUPO_KEY"),
    Config.string("L1_OPERATOR_SEED_PHRASE"),
    Config.string("L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX"),
    Config.string("NETWORK"),
    Config.integer("PORT").pipe(Config.withDefault(3000)),
    Config.integer("WAIT_BETWEEN_BLOCK_COMMITMENT").pipe(
      Config.withDefault(1000),
    ),
    Config.integer("WAIT_BETWEEN_BLOCK_CONFIRMATION").pipe(
      Config.withDefault(10000),
    ),
    Config.integer("WAIT_BETWEEN_MERGE_TXS").pipe(Config.withDefault(10000)),
    Config.integer("PROM_METRICS_PORT").pipe(Config.withDefault(9464)),
    Config.string("OLTP_EXPORTER_URL").pipe(
      Config.withDefault("http://0.0.0.0:4318/v1/traces"),
    ),
    Config.string("POSTGRES_HOST").pipe(Config.withDefault("postgres")), // service name
    Config.string("POSTGRES_PASSWORD").pipe(Config.withDefault("postgres")),
    Config.string("POSTGRES_DB").pipe(Config.withDefault("midgard")),
    Config.string("POSTGRES_USER").pipe(Config.withDefault("postgres")),
    Config.string("LEDGER_MPT_DB").pipe(
      Config.withDefault("./midgard-ledger-mpt-db"),
    ),
    Config.string("MEMPOOL_MPT_DB").pipe(
      Config.withDefault("./midgard-mempool-mpt-db"),
    ),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A"),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B"),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C"),
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
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: config[6],
    NETWORK: config[7] as Network,
    PORT: config[8],
    WAIT_BETWEEN_BLOCK_COMMITMENT: config[9],
    WAIT_BETWEEN_BLOCK_CONFIRMATION: config[10],
    WAIT_BETWEEN_MERGE_TXS: config[11],
    PROM_METRICS_PORT: config[12],
    OLTP_EXPORTER_URL: config[13],
    POSTGRES_HOST: config[14],
    POSTGRES_PASSWORD: config[15],
    POSTGRES_DB: config[16],
    POSTGRES_USER: config[17],
    LEDGER_MPT_DB_PATH: config[18],
    MEMPOOL_MPT_DB_PATH: config[19],
    TESTNET_GENESIS_WALLET_SEED_PHRASE_A: config[20],
    TESTNET_GENESIS_WALLET_SEED_PHRASE_B: config[21],
    TESTNET_GENESIS_WALLET_SEED_PHRASE_C: config[22],
  };
}).pipe(Effect.orDie);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}
