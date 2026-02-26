import { Writable } from 'node:stream';

import {
  Emulator,
  EmulatorAccount,
  Lucid,
  LucidEvolution,
  Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';

import { waitWritable } from '../../utils/common.js';
import { MidgardNodeClient } from '../client/node-client.js';
import { SerializedMidgardTransaction } from '../client/types.js';

/**
 * Configuration for generating one-to-one transactions.
 * These transactions simulate simple transfers between addresses.
 */
export interface OneToOneTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  txsCount: number;
  walletSeedOrPrivateKey: string;
  writable?: Writable;
  nodeClient?: MidgardNodeClient;
  nodeConfig?: {
    retryAttempts?: number;
    retryDelay?: number;
    enableLogs?: boolean;
  };
}

// Constants for transaction generation
const GC_PAUSE_INTERVAL = 1000; // Number of transactions before GC pause
const MIN_LOVELACE_OUTPUT = 1_000_000n; // Minimum lovelace per output

/**
 * Validates the configuration parameters
 * @throws Error if configuration is invalid
 */
const validateConfig = (config: OneToOneTransactionConfig): void => {
  const { initialUTxO, walletSeedOrPrivateKey } = config;

  // Validate wallet key format
  if (typeof walletSeedOrPrivateKey !== 'string' || walletSeedOrPrivateKey.trim().length === 0) {
    throw new Error('Invalid private key format. Expected a non-empty private key string.');
  }

  // Validate UTxO amount
  if (initialUTxO.assets.lovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error('Initial UTxO must have at least 1 ADA');
  }

  // Validate transaction count
  if (config.txsCount < 1) {
    throw new Error('Transaction count must be at least 1');
  }
};

/**
 * Initializes the Lucid instance with test configuration
 * @param emulator - Emulator instance
 * @param network - Network configuration
 * @returns Configured Lucid instance
 */
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

/**
 * Generate simple one-to-one transactions for testing.
 * Each transaction has one input and one output with the same value.
 */
const generateOneToOneTransactions = async (
  config: OneToOneTransactionConfig
): Promise<SerializedMidgardTransaction[]> => {
  const { network, initialUTxO, txsCount, writable, walletSeedOrPrivateKey } = config;

  // Validate configuration
  validateConfig(config);

  // Setup emulator environment
  const account: EmulatorAccount = {
    seedPhrase: '',
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey: walletSeedOrPrivateKey,
  };

  const emulator = new Emulator([account]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

  const lucid = await initializeLucid(emulator, network);
  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // Generate mock transactions
  const transactions: SerializedMidgardTransaction[] = [];

  try {
    // Generate transactions directly from the provided initial UTxO so the
    // first generated tx can be applied to the current node ledger state.
    for (let i = 0; i < txsCount; i++) {
      const txBuilder = lucid.newTx();

      const [newWalletUTxOs, derivedOutputs, txSignBuilder] = await txBuilder.pay
        .ToAddress(initialUTxO.address, initialUTxO.assets)
        .chain();

      const txSigned = await txSignBuilder.sign.withPrivateKey(walletSeedOrPrivateKey).complete();

      // Create serialized transaction in Midgard format
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `One-to-One Self Transfer ()`,
        txId: txHash,
        type: 'Midgard L2 User Transaction',
      };

      // Add to transactions array
      transactions.push(tx);

      // Write to test output if writable provided
      if (writable) {
        await waitWritable(writable);
        writable.write(JSON.stringify([tx], null, 2) + '\n');
      }

      // Update wallet state for next transaction
      lucid.overrideUTxOs(newWalletUTxOs);

      // Cleanup resources
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause to prevent memory pressure
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

export { generateOneToOneTransactions };
