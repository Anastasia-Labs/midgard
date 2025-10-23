import { Network, UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Config, Context, Data, Effect, Layer } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

type Provider = "Kupmios" | "Blockfrost";

type NodeConfigDep = {
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
  WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: number;
  WAIT_BETWEEN_TX_ORDER_UTXO_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  LEDGER_MPT_DB_PATH: string;
  MEMPOOL_MPT_DB_PATH: string;
  GENESIS_UTXOS: UTxO[];
};

const makeConfig = Effect.gen(function* () {
  const config = yield* Config.all([
    Config.literal("Kupmios", "Blockfrost")("L1_PROVIDER"),
    Config.string("L1_BLOCKFROST_API_URL"),
    Config.string("L1_BLOCKFROST_KEY"),
    Config.string("L1_OGMIOS_KEY"),
    Config.string("L1_KUPO_KEY"),
    Config.string("L1_OPERATOR_SEED_PHRASE"),
    Config.string("L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX"),
    Config.literal("Mainnet", "Preprod", "Preview", "Custom")("NETWORK"),
    Config.integer("PORT").pipe(Config.withDefault(3000)),
    Config.integer("WAIT_BETWEEN_BLOCK_COMMITMENT").pipe(
      Config.withDefault(1000),
    ),
    Config.integer("WAIT_BETWEEN_BLOCK_CONFIRMATION").pipe(
      Config.withDefault(10000),
    ),
    Config.integer("WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES").pipe(
      Config.withDefault(10000),
    ),
    Config.integer("WAIT_BETWEEN_TX_ORDER_UTXO_FETCHES").pipe(
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
    Config.string("LEDGER_MPT_DB_PATH").pipe(
      Config.withDefault("midgard-ledger-mpt-db"),
    ),
    Config.string("MEMPOOL_MPT_DB_PATH").pipe(
      Config.withDefault("midgard-mempool-mpt-db"),
    ),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A"),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B"),
    Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C"),
  ]);

  const provider: Provider = config[0];
  const network: Network = config[7];
  const seedA = config[21];
  const seedB = config[22];
  const seedC = config[23];

  const genesisUtxos: UTxO[] = [
    {
      txHash:
        "bb217abaca60fc0ca68c1555eca6a96d2478547818ae76ce6836133f3cc546e0",
      outputIndex: 1,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("4027026465"),
        "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
          BigInt("10000"),
      },
    },
    {
      txHash:
        "c7c0973c6bbf1a04a9f306da7814b4fa564db649bf48b0bd93c273bd03143547",
      outputIndex: 0,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("3289566"),
        "5c677ba4dd295d9286e0e22786fea9ed735a6ae9c07e7a45ae4d95c84372696d696e616c50756e6b73204c6f6f74":
          BigInt("1"),
      },
    },
    {
      txHash:
        "d1a25b8e9c3b985d9d2f0a5f2e6ca7efa1c43b10f2c0b61f29e4a2cd8142b09e",
      outputIndex: 0,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("200"),
      },
    },
    {
      txHash:
        "ea0f3c47bf18b02e9deb4e3a1239d8b263d765c4f7a3d12a9f62e8775e8c6141",
      outputIndex: 1,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("1500"),
      },
    },
    {
      txHash:
        "f40b9f6a507af50aad4ccf6c15157b6d05c7affe23ec55cf4109cc2549c97a37",
      outputIndex: 2,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("125243"),
      },
    },
    {
      txHash:
        "8e32d18c07cba2b65577bc829a9875e2fc3cdb554d5b0abbb3d4e3a71a3e3e3d",
      outputIndex: 0,
      address: walletFromSeed(seedC, { network }).address,
      assets: {
        lovelace: BigInt("300"),
        "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
          BigInt("15"),
      },
    },
  ];

  return {
    L1_PROVIDER: provider,
    L1_BLOCKFROST_API_URL: config[1],
    L1_BLOCKFROST_KEY: config[2],
    L1_OGMIOS_KEY: config[3],
    L1_KUPO_KEY: config[4],
    L1_OPERATOR_SEED_PHRASE: config[5],
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: config[6],
    NETWORK: network,
    PORT: config[8],
    WAIT_BETWEEN_BLOCK_COMMITMENT: config[9],
    WAIT_BETWEEN_BLOCK_CONFIRMATION: config[10],
    WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: config[11],
    WAIT_BETWEEN_TX_ORDER_UTXO_FETCHES: config[12],
    WAIT_BETWEEN_MERGE_TXS: config[13],
    PROM_METRICS_PORT: config[14],
    OLTP_EXPORTER_URL: config[15],
    POSTGRES_HOST: config[16],
    POSTGRES_PASSWORD: config[17],
    POSTGRES_DB: config[18],
    POSTGRES_USER: config[19],
    LEDGER_MPT_DB_PATH: config[20],
    MEMPOOL_MPT_DB_PATH: config[21],
    GENESIS_UTXOS: network === "Mainnet" ? [] : genesisUtxos,
  };
}).pipe(Effect.orDie);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}

export class ConfigError extends Data.TaggedError("ConfigError")<
  SDK.Utils.GenericErrorFields & {
    readonly fieldsAndValues: [string, string][];
  }
> {}
