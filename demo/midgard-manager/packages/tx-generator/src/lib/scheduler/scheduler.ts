import { mkdir, writeFile } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { randomBytes } from 'node:crypto';

import {
  CML,
  Network,
  UTxO,
  walletFromSeed,
} from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import { MidgardNodeClient } from '../client/node-client.js';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../generators/index.js';
import { parseUnknownKeytoBech32PrivateKey } from '../../utils/common.js';
import {
  DEFAULT_CONFIG,
  TRANSACTION_CONSTANTS,
  TransactionGeneratorConfig,
  validateGeneratorConfig,
} from '../types.js';

// Get the directory path for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

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
export const startGenerator = async (
  config: Partial<TransactionGeneratorConfig> = {}
): Promise<void> => {
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
    initialUTxO: config.initialUTxO ?? DEFAULT_CONFIG.initialUTxO,
  };

  // Validate the configuration
  validateGeneratorConfig(fullConfig);
  const canonicalWalletPrivateKey = parseUnknownKeytoBech32PrivateKey(
    fullConfig.walletSeedOrPrivateKey
  );

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

  // Create output directory if needed
  if (fullConfig.outputDir) {
    const projectRoot = join(__dirname, '../../../..');
    const outputPath = join(projectRoot, fullConfig.outputDir);
    await mkdir(outputPath, { recursive: true });
  }

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

  const generateUniqueUTxOs = (baseUTxO: UTxO, count: number) =>
    Array.from({ length: count }, () => ({
      ...baseUTxO,
      txHash: randomBytes(32).toString('hex').toUpperCase(),
      outputIndex: Math.floor(Math.random() * 1001), // Random outputIndex 0 -> 1000
    }));

  const resolveWalletAddress = async (): Promise<string> => {
    if (fullConfig.initialUTxO?.address && fullConfig.initialUTxO.address.length > 0) {
      return fullConfig.initialUTxO.address;
    }

    const rawWalletKey = fullConfig.walletSeedOrPrivateKey.trim();
    if (rawWalletKey.includes(' ')) {
      return walletFromSeed(rawWalletKey, { network: fullConfig.network }).address;
    }

    const networkId = fullConfig.network === 'Mainnet' ? 1 : 0;
    return CML.EnterpriseAddress.new(
      networkId,
      CML.Credential.new_pub_key(CML.PrivateKey.from_bech32(canonicalWalletPrivateKey).to_public().hash())
    )
      .to_address()
      .to_bech32();
  };

  // Define the transaction generation function
  const generateTransactions = async () => {
    try {
      const nodeAvailable = await nodeClient.isAvailable();
      let taskUTxOs: UTxO[] = [];
      let forceOneToOne = false;

      if (nodeAvailable) {
        const walletAddress = await resolveWalletAddress();
        const spendableUtxos = (await nodeClient.getSpendableUtxos(walletAddress)).filter(
          (utxo) => {
            const lovelace = utxo.assets?.lovelace;
            const lovelaceAmount =
              typeof lovelace === 'bigint'
                ? lovelace
                : BigInt(typeof lovelace === 'string' ? lovelace : 0);
            return lovelaceAmount >= TRANSACTION_CONSTANTS.MIN_LOVELACE_OUTPUT;
          }
        );
        if (spendableUtxos.length === 0) {
          console.warn(
            `No spendable UTxOs found for ${walletAddress}. Waiting for funds in mempool_ledger...`
          );
          return;
        }

        taskUTxOs = spendableUtxos.slice(0, fullConfig.batchSize);
        forceOneToOne = true;
        if (fullConfig.transactionType !== 'one-to-one') {
          console.log(
            'Node-connected mode uses one-to-one spends to ensure generated txs match live ledger UTxOs.'
          );
        }
      } else {
        taskUTxOs = generateUniqueUTxOs(fullConfig.initialUTxO, fullConfig.batchSize);
      }

      const tasks = taskUTxOs.map((taskUTxO) =>
        concurrencyLimiter(async () => {
          const useOneToOne =
            forceOneToOne ||
            fullConfig.transactionType === 'one-to-one' ||
            (fullConfig.transactionType === 'mixed' &&
              Math.random() * 100 < (fullConfig.oneToOneRatio ?? 70));

          const txs = useOneToOne
            ? await generateOneToOneTransactions({
                network: fullConfig.network,
                initialUTxO: taskUTxO,
                txsCount: 1,
                walletSeedOrPrivateKey: canonicalWalletPrivateKey,
                nodeClient,
              })
            : await generateMultiOutputTransactions({
                network: fullConfig.network,
                initialUTxO: taskUTxO,
                utxosCount: TRANSACTION_CONSTANTS.OUTPUTS_PER_DISTRIBUTION,
                finalUtxosCount: 1,
                walletSeedOrPrivateKey: canonicalWalletPrivateKey,
                nodeClient,
              });

          if (!txs || !Array.isArray(txs)) {
            throw new Error('Failed to generate transactions');
          }

          const timestamp = new Date().toISOString().replace(/[:.]/g, '-');

          if (!nodeAvailable && fullConfig.outputDir) {
            const projectRoot = join(__dirname, '../../../..');
            const filename = `${useOneToOne ? 'one-to-one' : 'multi-output'}-${timestamp}.json`;
            const filepath = join(projectRoot, fullConfig.outputDir, filename);
            await writeFile(filepath, JSON.stringify(txs, null, 2));
            console.log(`Node unavailable - transactions written to ${filepath}`);
            state.stats.transactionsGenerated += txs.length;
          } else {
            try {
              const submissionStart = Date.now();
              let submittedCount = 0;
              for (const tx of txs) {
                const result = (await nodeClient.submitTransaction(tx.cborHex)) as {
                  status?: string;
                  error?: string;
                };

                // Handle node unavailability gracefully
                if (result && result.status === 'NODE_UNAVAILABLE') {
                  if (fullConfig.outputDir) {
                    const projectRoot = join(__dirname, '../../../..');
                    const filename = `${useOneToOne ? 'one-to-one' : 'multi-output'}-${timestamp}.json`;
                    const filepath = join(projectRoot, fullConfig.outputDir, filename);
                    await writeFile(filepath, JSON.stringify(txs, null, 2));
                    console.log(`Node unavailable - transactions written to ${filepath}`);
                  }
                  state.stats.transactionsGenerated += txs.length;
                  break; // Exit the loop since node is unavailable
                }
                if (result && result.status === 'ERROR') {
                  throw new Error(result.error ?? 'submit failed');
                }
                submittedCount++;
              }
              const submissionEnd = Date.now();

              state.stats.transactionsGenerated += txs.length;
              state.stats.transactionsSubmitted += submittedCount;
              console.log(
                `Submitted ${submittedCount}/${txs.length} transactions in ${submissionEnd - submissionStart}ms`
              );
            } catch (submitError) {
              console.error('Failed to submit transactions:', submitError);

              if (fullConfig.outputDir) {
                const projectRoot = join(__dirname, '../../../..');
                const filename = `${useOneToOne ? 'one-to-one' : 'multi-output'}-${timestamp}.json`;
                const filepath = join(projectRoot, fullConfig.outputDir, filename);
                await writeFile(filepath, JSON.stringify(txs, null, 2));
                console.log(`Failed submission - transactions written to ${filepath}`);
              }

              state.stats.transactionsGenerated += txs.length;
            }
          }

          return txs;
        })
      );

      await Promise.all(tasks);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      state.stats.lastError = errorMessage;
      console.error('Error in transaction generation loop:', errorMessage);
      throw error;
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
