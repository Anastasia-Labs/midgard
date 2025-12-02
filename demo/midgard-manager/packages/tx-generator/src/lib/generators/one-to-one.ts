import { Writable } from 'node:stream';

import {
  Data,
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
import { Int } from 'effect/Schema';

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
 * Generates a unique hex string for transaction datum
 * Combines timestamp and random values into a valid hex string
 */
const generateUniqueHexDatum = (counter: number): string => {
  const timestamp = Date.now().toString(16).padStart(12, '0');
  const random = Math.floor(Math.random() * 16777215)
    .toString(16)
    .padStart(6, '0');
  const count = counter.toString(16).padStart(6, '0');
  return timestamp + random + count;
};

/**
 * Validates the configuration parameters
 * @throws Error if configuration is invalid
 */
const validateConfig = (config: OneToOneTransactionConfig): void => {
  const { initialUTxO, walletSeedOrPrivateKey } = config;

  // Validate wallet key format
  if (!walletSeedOrPrivateKey.startsWith('ed25519_sk')) {
    throw new Error('Invalid private key format. Expected Lucid emulator account private key.');
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
  const { network, initialUTxO, txsCount, writable, nodeClient, walletSeedOrPrivateKey } = config;

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
    // First transaction to move away from genesis UTxO
    const initialTxBuilder = lucid.newTx();
    const [initialNewWalletUTxOs, initialDerivedOutputs, initialTxSignBuilder] =
      await initialTxBuilder.pay.ToAddress(initialUTxO.address, initialUTxO.assets).chain();

    const initialTxSigned = await initialTxSignBuilder.sign
      .withPrivateKey(walletSeedOrPrivateKey)
      .complete();

    // Use the output of initial transaction for subsequent transactions
    const firstUtxo = {
      txHash: initialTxSigned.toHash(),
      outputIndex: initialUTxO.outputIndex,
      address: initialUTxO.address,
      assets: initialUTxO.assets,
    };

    lucid.selectWallet.fromAddress(firstUtxo.address, [firstUtxo]);

    // Generate the actual transactions with unique data
    for (let i = 0; i < txsCount; i++) {
      const txBuilder = lucid.newTx();

      const [newWalletUTxOs, derivedOutputs, txSignBuilder] = await txBuilder.pay
        .ToAddressWithData(
          initialUTxO.address,
          { kind: 'inline', value: Data.to(generateUniqueHexDatum(i)) },
          initialUTxO.assets
        )
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

    // Cleanup initial transaction resources
    initialTxBuilder.rawConfig().txBuilder.free();
    initialTxSignBuilder.toTransaction().free();
    initialTxSigned.toTransaction().free();
  } catch (error) {
    console.error('Error generating transactions:', error);
    throw error;
  }

  return transactions;
};

export { generateOneToOneTransactions };
