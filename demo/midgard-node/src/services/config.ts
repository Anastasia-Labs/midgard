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
  PROTOCOL_PARAMETERS: SDK.ProtocolParameters;
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
  const provider = yield* Config.literal(
    "Kupmios",
    "Blockfrost",
  )("L1_PROVIDER");
  const blockfrostApiUrl = yield* Config.string("L1_BLOCKFROST_API_URL");
  const blockfrostKey = yield* Config.string("L1_BLOCKFROST_KEY");
  const ogmiosKey = yield* Config.string("L1_OGMIOS_KEY");
  const kupoKey = yield* Config.string("L1_KUPO_KEY");
  const operatorSeedPhrase = yield* Config.string("L1_OPERATOR_SEED_PHRASE");
  const operatorSeedPhraseForMergeTx = yield* Config.string(
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  );
  const network = yield* Config.literal(
    "Mainnet",
    "Preprod",
    "Preview",
    "Custom",
  )("NETWORK");
  const port = yield* Config.integer("PORT").pipe(Config.withDefault(3000));
  const waitBetweenBlockCommitment = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_COMMITMENT",
  ).pipe(Config.withDefault(1000));
  const waitBetweenBlockConfirmation = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_CONFIRMATION",
  ).pipe(Config.withDefault(10000));
  const waitBetweenMergeTxs = yield* Config.integer(
    "WAIT_BETWEEN_MERGE_TXS",
  ).pipe(Config.withDefault(10000));
  const waitBetweenDepositUTxOFetches = yield* Config.integer(
    "WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES",
  ).pipe(Config.withDefault(10000));
  const waitBetweenTxOrderUTxOFetches = yield* Config.integer(
    "WAIT_BETWEEN_TX_ORDER_UTXO_FETCHES",
  ).pipe(Config.withDefault(10000));
  const promMetricsPort = yield* Config.integer("PROM_METRICS_PORT").pipe(
    Config.withDefault(9464),
  );
  const oltpExporterUrl = yield* Config.string("OLTP_EXPORTER_URL").pipe(
    Config.withDefault("http://0.0.0.0:4318/v1/traces"),
  );
  const postgresHost = yield* Config.string("POSTGRES_HOST").pipe(
    Config.withDefault("postgres"),
  ); // service name
  const postgresPassword = yield* Config.string("POSTGRES_PASSWORD").pipe(
    Config.withDefault("postgres"),
  );
  const postgresDb = yield* Config.string("POSTGRES_DB").pipe(
    Config.withDefault("midgard"),
  );
  const postgresUser = yield* Config.string("POSTGRES_USER").pipe(
    Config.withDefault("postgres"),
  );
  const ledgerMptDbPath = yield* Config.string("LEDGER_MPT_DB_PATH").pipe(
    Config.withDefault("midgard-ledger-mpt-db"),
  );
  const mempoolMptDbPath = yield* Config.string("MEMPOOL_MPT_DB_PATH").pipe(
    Config.withDefault("midgard-mempool-mpt-db"),
  );
  const seedA = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A");
  const seedB = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B");
  const seedC = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C");

  const genesisUtxos: UTxO[] = [
    {
      txHash:
        "bb217abaca60fc0ca68c1555eca6a96d2478547818ae76ce6836133f3cc546e0",
      outputIndex: 1,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("4027026465"),
        // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        //   BigInt("10000"),
      },
    },
    {
      txHash:
        "c7c0973c6bbf1a04a9f306da7814b4fa564db649bf48b0bd93c273bd03143547",
      outputIndex: 0,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("3289566"),
        // "5c677ba4dd295d9286e0e22786fea9ed735a6ae9c07e7a45ae4d95c84372696d696e616c50756e6b73204c6f6f74":
        //   BigInt("1"),
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
        // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        //   BigInt("15"),
      },
    },
  ];

  return {
    L1_PROVIDER: provider,
    L1_BLOCKFROST_API_URL: blockfrostApiUrl,
    L1_BLOCKFROST_KEY: blockfrostKey,
    L1_OGMIOS_KEY: ogmiosKey,
    L1_KUPO_KEY: kupoKey,
    L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: operatorSeedPhraseForMergeTx,
    NETWORK: network,
    PROTOCOL_PARAMETERS: SDK.getProtocolParameters(network),
    PORT: port,
    WAIT_BETWEEN_BLOCK_COMMITMENT: waitBetweenBlockCommitment,
    WAIT_BETWEEN_BLOCK_CONFIRMATION: waitBetweenBlockConfirmation,
    WAIT_BETWEEN_MERGE_TXS: waitBetweenMergeTxs,
    WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: waitBetweenDepositUTxOFetches,
    WAIT_BETWEEN_TX_ORDER_UTXO_FETCHES: waitBetweenTxOrderUTxOFetches,
    PROM_METRICS_PORT: promMetricsPort,
    OLTP_EXPORTER_URL: oltpExporterUrl,
    POSTGRES_HOST: postgresHost,
    POSTGRES_PASSWORD: postgresPassword,
    POSTGRES_DB: postgresDb,
    POSTGRES_USER: postgresUser,
    LEDGER_MPT_DB_PATH: ledgerMptDbPath,
    MEMPOOL_MPT_DB_PATH: mempoolMptDbPath,
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
