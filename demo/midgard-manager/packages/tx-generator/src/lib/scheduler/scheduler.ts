import { Network, UTxO } from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import { MidgardNodeClient } from '../client/node-client.js';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../generators/index.js';
import { DEFAULT_CONFIG, TransactionGeneratorConfig, validateGeneratorConfig } from '../types.js';

// Internal constants for multi-output transactions
const OUTPUTS_PER_DISTRIBUTION = 20;
const FINAL_UTXO_COUNT = 1;

/**
 * Generator State Manager
 * Encapsulates all state for the transaction generator
 */
class TxGeneratorState {
  private static instance: TxGeneratorState;

  private _shouldStop = false;
  private _currentPromise: Promise<void> | null = null;
  private _stats = {
    transactionsGenerated: 0,
    transactionsSubmitted: 0,
    lastError: null as string | null,
    startTime: null as Date | null,
  };

  private constructor() {}

  static getInstance(): TxGeneratorState {
    if (!TxGeneratorState.instance) {
      TxGeneratorState.instance = new TxGeneratorState();
    }
    return TxGeneratorState.instance;
  }

  get shouldStop(): boolean {
    return this._shouldStop;
  }

  set shouldStop(value: boolean) {
    this._shouldStop = value;
  }

  get currentPromise(): Promise<void> | null {
    return this._currentPromise;
  }

  set currentPromise(value: Promise<void> | null) {
    this._currentPromise = value;
  }

  get stats() {
    return this._stats;
  }

  resetStats() {
    this._stats = {
      transactionsGenerated: 0,
      transactionsSubmitted: 0,
      lastError: null,
      startTime: new Date(),
    };
  }

  isRunning(): boolean {
    return !this._shouldStop && this._currentPromise !== null;
  }
}

// Get the shared instance
const state = TxGeneratorState.getInstance();

/**
 * Starts a transaction generator with the given configuration
 */
export const startGenerator = (config: Partial<TransactionGeneratorConfig> = {}): Promise<void> => {
  // Stop any existing generator
  if (state.currentPromise) {
    stopGenerator();
  }

  // Reset the stop flag
  state.shouldStop = false;

  // Merge with default config
  const fullConfig: TransactionGeneratorConfig = {
    ...DEFAULT_CONFIG,
    ...config,
  };

  // Validate the configuration
  validateGeneratorConfig(fullConfig);

  // Set up node client with the new configuration structure
  const nodeClient = new MidgardNodeClient({
    baseUrl: fullConfig.nodeEndpoint,
    retryAttempts: fullConfig.nodeRetryAttempts,
    retryDelay: fullConfig.nodeRetryDelay,
    enableLogs: fullConfig.nodeEnableLogs,
  });

  // Create a limiter for concurrent transaction generation
  const concurrencyLimiter = pLimit(fullConfig.concurrency);

  // Reset stats
  state.resetStats();

  // We need a dummy UTxO for now - this should be improved later
  const dummyUTxO: UTxO = {
    txHash: '0000000000000000000000000000000000000000000000000000000000000000',
    outputIndex: 0,
    assets: { lovelace: 100000000000n },
    address:
      'addr_test1qzq0nckg3ekgzuqtzw8aze3m7c4v8jxk8lm5s37lgjl5jfvrx5uv9rrnz6hd54l2l0ch6xvgwcnku6x9v736xcqnx3qvvmp2j', // Dummy testnet address
    datum: null,
    datumHash: null,
    scriptRef: null,
  };

  // Log start with more configuration details
  console.log('\nStarting transaction generator with configuration:');
  console.log(`• Type: ${fullConfig.transactionType}`);
  if (fullConfig.transactionType === 'mixed') {
    console.log(`• One-to-One Ratio: ${fullConfig.oneToOneRatio}%`);
  }
  console.log(`• Batch Size: ${fullConfig.batchSize}`);
  console.log(`• Interval: ${fullConfig.interval}s`);
  console.log(`• Concurrency: ${fullConfig.concurrency}`);
  console.log(`• Node Endpoint: ${fullConfig.nodeEndpoint}`);
  if (fullConfig.autoStopAfterBatch) {
    console.log('• Auto-stop: Enabled (will stop after one batch)');
  }
  console.log();

  // Define the transaction generation function
  const generateTransactions = async () => {
    try {
      // Generate transactions concurrently up to the concurrency limit
      const tasks = Array(fullConfig.batchSize)
        .fill(null)
        .map(async () => {
          return concurrencyLimiter(async () => {
            // For mixed type, determine which type to use based on ratio
            const useOneToOne =
              fullConfig.transactionType === 'one-to-one' ||
              (fullConfig.transactionType === 'mixed' &&
                Math.random() * 100 < (fullConfig.oneToOneRatio ?? 70));

            try {
              // Generate the appropriate transaction type
              const txs = useOneToOne
                ? await generateOneToOneTransactions({
                    network: 'Testnet' as Network,
                    initialUTxO: dummyUTxO,
                    txsCount: 1, // Generate one transaction at a time
                    walletSeedOrPrivateKey: fullConfig.walletPrivateKey,
                    nodeClient,
                  })
                : await generateMultiOutputTransactions({
                    network: 'Testnet' as Network,
                    initialUTxO: dummyUTxO,
                    utxosCount: OUTPUTS_PER_DISTRIBUTION,
                    finalUtxosCount: FINAL_UTXO_COUNT,
                    walletSeedOrPrivateKey: fullConfig.walletPrivateKey,
                    nodeClient,
                  });

              return txs;
            } catch (txError) {
              const errorMessage = txError instanceof Error ? txError.message : String(txError);

              console.error(
                `Error generating ${
                  useOneToOne ? 'one-to-one' : 'multi-output'
                } transactions: ${errorMessage}`
              );
              state.stats.lastError = errorMessage;

              // For testing purposes, create mock transactions when there's an error
              if (process.env.NODE_ENV === 'development' || process.env.MOCK_TXS === 'true') {
                return [
                  {
                    type: useOneToOne ? 'one-to-one' : 'multi-output',
                    description: `Test transaction`,
                    cborHex: 'mock_cbor_for_testing_only',
                    txId: `mock_tx_${Date.now()}`,
                  },
                ];
              } else {
                // In production, rethrow the error
                throw txError;
              }
            }
          });
        });

      // Wait for all transactions to be generated
      const results = await Promise.all(tasks);
      const txs = results.flat();

      // Update stats
      if (txs && Array.isArray(txs)) {
        state.stats.transactionsGenerated += txs.length;
        state.stats.transactionsSubmitted += txs.length;
        console.log(`Generated ${txs.length} transactions`);
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      state.stats.lastError = errorMessage;
      console.error('Error in transaction generation loop:', errorMessage);

      // Ensure we don't swallow errors in development
      if (process.env.NODE_ENV === 'development') {
        console.error(error);
      }
    }
  };

  // Create a function to run the generator loop
  const runGenerator = async () => {
    // Generate at least one batch
    await generateTransactions();

    // If auto-stop is enabled, stop here (for scheduled jobs)
    if (fullConfig.autoStopAfterBatch) {
      console.log('Auto-stop enabled - stopping after one batch');
      state.shouldStop = true;
      return;
    }

    // Otherwise, continue in a loop until stopped manually
    while (!state.shouldStop) {
      // Wait for the specified interval
      await new Promise((resolve) => setTimeout(resolve, fullConfig.interval * 1000));

      // Check if we should stop before generating more transactions
      if (state.shouldStop) break;

      await generateTransactions();
    }
    console.log('Transaction generator stopped');
  };

  // Start the generator
  state.currentPromise = runGenerator().catch((error) => {
    const errorMessage = error instanceof Error ? error.message : String(error);
    state.stats.lastError = errorMessage;
    console.error('Generator failed:', errorMessage);
    state.currentPromise = null;
  });

  return Promise.resolve();
};

/**
 * Stops the currently running transaction generator
 */
export const stopGenerator = (): Promise<void> => {
  state.shouldStop = true;
  return Promise.resolve();
};

/**
 * Gets the current status of the transaction generator
 */
export const getGeneratorStatus = (): {
  running: boolean;
  transactionsGenerated: number;
  transactionsSubmitted: number;
  lastError: string | null;
  uptime: number | null;
} => {
  return {
    running: state.isRunning(),
    transactionsGenerated: state.stats.transactionsGenerated,
    transactionsSubmitted: state.stats.transactionsSubmitted,
    lastError: state.stats.lastError,
    uptime: state.stats.startTime
      ? Math.floor((new Date().getTime() - state.stats.startTime.getTime()) / 1000)
      : null,
  };
};
