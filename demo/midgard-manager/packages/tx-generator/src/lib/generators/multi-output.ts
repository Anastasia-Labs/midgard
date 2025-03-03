import {
  Data,
  Emulator,
  EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  LucidEvolution,
  Network,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import {
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
} from '../../utils/common.js';
import { MidgardNodeClient } from '../client/node-client.js';
import { SerializedMidgardTransaction } from '../client/types.js';

/**
 * Configuration for generating multi-output transactions.
 * These transactions simulate complex transfers with multiple outputs.
 */
export interface MultiOutputTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  utxosCount: number;
  finalUtxosCount: number;
  walletSeedOrPrivateKey: string;
  nodeClient?: MidgardNodeClient;
}

// Constants
const TOTAL_ACCOUNT_COUNT = 100;
const OUTPUT_UTXOS_CHUNK = 20;
const GC_PAUSE_INTERVAL = 250; // number of transactions before GC pause
const ACCOUNT_GENERATION_CONCURRENCY = 10;
const MIN_LOVELACE_OUTPUT = 1_000_000n; // Minimum lovelace per output

/**
 * Validates the configuration parameters
 * @throws Error if configuration is invalid
 */
const validateConfig = (config: MultiOutputTransactionConfig): void => {
  const { initialUTxO, walletSeedOrPrivateKey, utxosCount, finalUtxosCount } = config;

  // Validate wallet key
  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);
  const publicKeyHash = getPublicKeyHashFromPrivateKey(privateKey);
  const initialUTxOAddressPubKeyHash = paymentCredentialOf(initialUTxO.address).hash;

  if (publicKeyHash !== initialUTxOAddressPubKeyHash) {
    throw new Error('Payment Key is not valid to spend Initial UTxO');
  }

  // Validate UTxO amount
  if (initialUTxO.assets.lovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error('Initial UTxO must have at least 1 ADA');
  }

  // Calculate output lovelace and validate
  const outputLovelace = calculateOutputLovelace(initialUTxO.assets.lovelace, utxosCount);
  if (outputLovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error('Not enough Lovelace to distribute');
  }

  // Validate UTxO counts
  if (utxosCount < 1) {
    throw new Error('UTxO count must be at least 1');
  }

  if (finalUtxosCount < 1) {
    throw new Error('Final UTxO count must be at least 1');
  }
  if (finalUtxosCount > utxosCount / OUTPUT_UTXOS_CHUNK) {
    throw new Error(
      `Final UTxO Count can be ${Math.floor(utxosCount / OUTPUT_UTXOS_CHUNK)} at maximum`
    );
  }
};

const initializeLucid = async (emulator: Emulator, network: Network): Promise<LucidEvolution> => {
  return await Lucid(emulator, network, {
    presetProtocolParameters: {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      minFeeA: 0,
      minFeeB: 0,
      priceMem: 0,
      priceStep: 0,
      coinsPerUtxoByte: 0n,
    },
  });
};

const generateTestAccounts = async (count: number): Promise<EmulatorAccount[]> => {
  const limit = pLimit(ACCOUNT_GENERATION_CONCURRENCY);
  return Promise.all(
    Array.from({ length: count }, () => limit(() => generateEmulatorAccountFromPrivateKey({})))
  );
};

const calculateOutputLovelace = (totalLovelace: bigint, utxosCount: number): bigint => {
  return (totalLovelace - MIN_LOVELACE_OUTPUT) / BigInt(utxosCount);
};

/**
 * Generate complex multi-output transactions for testing.
 * Each transaction has one input and multiple outputs.
 */
export const generateMultiOutputTransactions = async (
  config: MultiOutputTransactionConfig
): Promise<SerializedMidgardTransaction[]> => {
  const { network, initialUTxO, utxosCount, finalUtxosCount, walletSeedOrPrivateKey, nodeClient } =
    config;

  validateConfig(config);
  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);

  // Setup emulator env
  const mainAccount: EmulatorAccount = {
    seedPhrase: '',
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey,
  };

  const emulator = new Emulator([mainAccount]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

  const lucid = await initializeLucid(emulator, network);
  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // Generate test accounts for distribution
  const accounts = await generateTestAccounts(TOTAL_ACCOUNT_COUNT);

  // State tracking
  let currentUtxosCount = 1;
  let currentTxsCount = 0;
  let outputCounter = 0;
  const MAX_SAFE_COUNTER = Number.MAX_SAFE_INTEGER - OUTPUT_UTXOS_CHUNK; // Prevent overflow
  const rollBackers: { utxos: UTxO[]; privateKey: string }[] = [];
  const outputLovelace = calculateOutputLovelace(initialUTxO.assets.lovelace, utxosCount);

  // Store all generated transactions
  const transactions: SerializedMidgardTransaction[] = [];

  try {
    // Skip first transaction if it's a genesis UTxO
    if (!initialUTxO.txHash.startsWith('genesis')) {
      currentTxsCount++;
    }

    // Distribution phase: Generate transactions with multiple outputs
    while (currentUtxosCount < utxosCount) {
      // Check counter safety
      if (outputCounter >= MAX_SAFE_COUNTER) {
        throw new Error('Output counter limit reached. Please start a new generation batch.');
      }

      const randomAccount = accounts[Math.floor(Math.random() * TOTAL_ACCOUNT_COUNT)];
      const txBuilder = lucid.newTx();

      // create multiple outputs in single transaction
      Array.from({ length: OUTPUT_UTXOS_CHUNK }).forEach(() => {
        outputCounter++; // Increment for each output
        txBuilder.pay.ToAddressWithData(
          randomAccount.address,
          { kind: 'inline', value: Data.to(BigInt(outputCounter)) },
          {
            lovelace: outputLovelace,
          }
        );
      });

      const [newWalletUTxOs, derivedOutputs, txSignBuilder] = await txBuilder.chain();
      const txSigned = await txSignBuilder.sign.withPrivateKey(privateKey).complete();

      // Create serialized transaction in Midgard format
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `Multi-Output Distribution (1-to-${OUTPUT_UTXOS_CHUNK})`,
        txId: txHash,
        type: 'Midgard L2 User Transaction',
      };

      transactions.push(tx);

      // Update UTxO state
      lucid.overrideUTxOs(newWalletUTxOs);

      // Store outputs for collection phase
      derivedOutputs.pop(); // Remove own output
      rollBackers.unshift({
        utxos: derivedOutputs,
        privateKey: randomAccount.privateKey,
      });

      // Cleanup resource
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause
      if (currentTxsCount % GC_PAUSE_INTERVAL === 0) {
        await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
      }

      currentUtxosCount += OUTPUT_UTXOS_CHUNK;
      currentTxsCount += 1;
    }

    // Collection phase: Gather UTxOs back
    for (let i = 0; i < rollBackers.length; i += finalUtxosCount) {
      const chunk = rollBackers.slice(i, i + finalUtxosCount);
      const txBuilder = lucid.newTx();

      // Add inputs from each rollbacker
      chunk.forEach(({ utxos }) => utxos.forEach((utxo) => txBuilder.collectFrom([utxo])));

      // Add single output back to main account
      txBuilder.pay.ToAddress(mainAccount.address, {
        lovelace: outputLovelace * BigInt(OUTPUT_UTXOS_CHUNK * chunk.length),
      });

      const [newWalletUTxOs, , txSignBuilder] = await txBuilder.chain();

      // Sign with main key and all rollbacker keys
      const txSigned = await txSignBuilder.sign.withPrivateKey(privateKey).complete();

      // Create serialized transaction
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `Multi-Output Collection (${OUTPUT_UTXOS_CHUNK}-to-1)`,
        txId: txHash,
        type: 'Midgard L2 User Transaction',
      };

      transactions.push(tx);

      // Update UTxO state
      lucid.overrideUTxOs(newWalletUTxOs);

      // Cleanup resources
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause
      if (i % GC_PAUSE_INTERVAL === 0) {
        await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
      }
    }
  } catch (error) {
    console.error('Error generating transactions:', error);
    throw error;
  }

  return transactions;
};
