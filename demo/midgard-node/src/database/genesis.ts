import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { LedgerColumns } from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import {
  Network,
  UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { NodeConfig } from "@/config.js";
import { makeMpts } from "@/workers/utils/mpt.js";

const makeGenesisUTxOs = (
  network: Network,
  seedA: string,
  seedB: string,
  seedC: string,
): UTxO[] => [
  {
    txHash: "bb217abaca60fc0ca68c1555eca6a96d2478547818ae76ce6836133f3cc546e0",
    outputIndex: 1,
    address: walletFromSeed(seedA, { network }).address,
    assets: {
      lovelace: BigInt("4027026465"),
      "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        BigInt("10000"),
    },
  },
  {
    txHash: "c7c0973c6bbf1a04a9f306da7814b4fa564db649bf48b0bd93c273bd03143547",
    outputIndex: 0,
    address: walletFromSeed(seedA, { network }).address,
    assets: {
      lovelace: BigInt("3289566"),
      "5c677ba4dd295d9286e0e22786fea9ed735a6ae9c07e7a45ae4d95c84372696d696e616c50756e6b73204c6f6f74":
        BigInt("1"),
    },
  },
  {
    txHash: "d1a25b8e9c3b985d9d2f0a5f2e6ca7efa1c43b10f2c0b61f29e4a2cd8142b09e",
    outputIndex: 0,
    address: walletFromSeed(seedB, { network }).address,
    assets: {
      lovelace: BigInt("200"),
    },
  },
  {
    txHash: "ea0f3c47bf18b02e9deb4e3a1239d8b263d765c4f7a3d12a9f62e8775e8c6141",
    outputIndex: 1,
    address: walletFromSeed(seedB, { network }).address,
    assets: {
      lovelace: BigInt("1500"),
    },
  },
  {
    txHash: "f40b9f6a507af50aad4ccf6c15157b6d05c7affe23ec55cf4109cc2549c97a37",
    outputIndex: 2,
    address: walletFromSeed(seedB, { network }).address,
    assets: {
      lovelace: BigInt("125243"),
    },
  },
  {
    txHash: "8e32d18c07cba2b65577bc829a9875e2fc3cdb554d5b0abbb3d4e3a71a3e3e3d",
    outputIndex: 0,
    address: walletFromSeed(seedC, { network }).address,
    assets: {
      lovelace: BigInt("300"),
      "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        BigInt("15"),
    },
  },
];

/**
 * Inserts genesis UTXOs from the imported TypeScript module into the MPT database
 * @returns Effect that succeeds if UTXOs were inserted, or fails on errors
 */
export const insertGenesisUtxos: Effect.Effect<
  void,
  Error,
  NodeConfig | Database
> = Effect.gen(function* () {
  const config = yield* NodeConfig;

  if (config.NETWORK === "Mainnet") {
    yield* Effect.logInfo(`🟣 On mainnet—No genesis UTxOs will be inserted.`);
    return;
  }

  const genesisUTxOs = makeGenesisUTxOs(
    config.NETWORK,
    config.TESTNET_GENESIS_WALLET_SEED_PHRASE_A,
    config.TESTNET_GENESIS_WALLET_SEED_PHRASE_B,
    config.TESTNET_GENESIS_WALLET_SEED_PHRASE_C,
  );

  yield* Effect.logInfo(
    `🟣 On testnet—Inserting ${genesisUTxOs.length} genesis UTxOs...`,
  );

  // Convert genesis UTxOs to LedgerEntry format and insert into MPT
  const ledgerEntries = genesisUTxOs.map((utxo: UTxO) => {
    const input = utxoToTransactionInput(utxo);
    const output = utxoToTransactionOutput(utxo);

    return {
      [LedgerColumns.TX_ID]: Buffer.from(utxo.txHash, "hex"),
      [LedgerColumns.OUTREF]: Buffer.from(input.to_cbor_bytes()),
      [LedgerColumns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
      [LedgerColumns.ADDRESS]: utxo.address,
    };
  });

  yield* Effect.logInfo(
    `🟣 Debug: Inserting ${ledgerEntries.length} UTxOs into MempoolLedgerDB...`,
  );

  yield* MempoolLedgerDB.insert(ledgerEntries);

  yield* Effect.logInfo(
    `🟣 Debug: Inserting ${ledgerEntries.length} UTxOs into trie...`,
  );

  const { ledgerTrie } = yield* makeMpts();

  yield* Effect.tryPromise({
    try: () =>
      ledgerTrie.batch(
        ledgerEntries.map((le) => ({
          type: "put",
          key: le[LedgerColumns.OUTREF],
          value: le[LedgerColumns.OUTPUT],
        })),
      ),
    catch: (e) => new Error(`${e}`),
  });

  yield* Effect.logInfo(
    `🟣 Successfully inserted ${ledgerEntries.length} genesis UTxOs into MPT database. Funded addresses are:
${Array.from(new Set(genesisUTxOs.map((u) => u.address))).join("\n")}`,
  );
});
