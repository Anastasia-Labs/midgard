import { Network, UTxO } from '@lucid-evolution/lucid';

// Transaction Types
export type TransactionType = 'one-to-one' | 'multi-output' | 'mixed';

// Node Client Configuration
export interface MidgardNodeConfig {
  baseUrl: string;
  retryAttempts?: number;
  retryDelay?: number;
  enableLogs?: boolean;
}

// Transaction Generator Configuration
export interface TransactionGeneratorConfig {
  // Node settings
  nodeEndpoint: string;
  nodeRetryAttempts?: number;
  nodeRetryDelay?: number;
  nodeEnableLogs?: boolean;

  // Network settings
  network: Network;
  initialUTxO: UTxO;

  // Wallet settings
  walletSeedOrPrivateKey: string;

  // Transaction settings
  transactionType: TransactionType;
  oneToOneRatio?: number;

  // Batch settings
  batchSize: number;
  interval: number;
  concurrency: number;
  autoStopAfterBatch?: boolean;

  // Output settings
  outputDir?: string;
}

// Serialized Transaction Format
export interface SerializedMidgardTransaction {
  cborHex: string;
  description: string;
  txId: string;
  type: string;
}

// Default configuration values
export const DEFAULT_CONFIG: TransactionGeneratorConfig = {
  // Node defaults
  nodeEndpoint: 'http://localhost:3000',
  nodeRetryAttempts: 3,
  nodeRetryDelay: 1000,
  nodeEnableLogs: true,

  // Network defaults
  network: 'Preview' as Network,
  initialUTxO: {
    txHash: '0'.repeat(64),
    outputIndex: 0,
    assets: { lovelace: 10_000_000_000n },
    address: '', // Will be derived from wallet
    datum: null,
    datumHash: null,
    scriptRef: null,
  },

  // Wallet defaults - must be provided
  walletSeedOrPrivateKey: '',

  // Transaction defaults
  transactionType: 'mixed',
  oneToOneRatio: 70,

  // Batch defaults
  batchSize: 10,
  interval: 5,
  concurrency: 5,
  autoStopAfterBatch: false,

  // Output defaults
  outputDir: 'generated-transactions',
};

// Constants
export const TRANSACTION_CONSTANTS = {
  MIN_LOVELACE_OUTPUT: 1_000_000n,
  OUTPUTS_PER_DISTRIBUTION: 20,
  GC_PAUSE_INTERVAL: {
    ONE_TO_ONE: 1000,
    MULTI_OUTPUT: 250,
  },
  NODE_DEFAULTS: {
    RETRY_ATTEMPTS: 3,
    RETRY_DELAY: 1000,
    AVAILABILITY_TIMEOUT: 5000,
  },
} as const;

/**
 * Validates the transaction generator configuration
 * @throws Error if configuration is invalid
 */
export const validateGeneratorConfig = (config: TransactionGeneratorConfig): void => {
  // Node validation
  if (!config.nodeEndpoint) {
    throw new Error('Node endpoint is required');
  }
  if (!config.nodeEndpoint.startsWith('http')) {
    throw new Error('Node endpoint must start with http:// or https://');
  }
  if (config.nodeRetryAttempts !== undefined && config.nodeRetryAttempts < 0) {
    throw new Error('Node retry attempts must be non-negative');
  }
  if (config.nodeRetryDelay !== undefined && config.nodeRetryDelay < 0) {
    throw new Error('Node retry delay must be non-negative');
  }

  // Wallet validation
  if (!config.walletSeedOrPrivateKey) {
    throw new Error('Wallet private key or seed phrase is required');
  }

  // Transaction validation
  if (config.oneToOneRatio !== undefined) {
    if (config.oneToOneRatio < 0 || config.oneToOneRatio > 100) {
      throw new Error('One-to-one ratio must be between 0 and 100');
    }
  }

  // Batch validation
  if (config.batchSize < 1) {
    throw new Error('Batch size must be at least 1');
  }
  if (config.interval < 0) {
    throw new Error('Interval must be non-negative');
  }
  if (config.concurrency < 1) {
    throw new Error('Concurrency must be at least 1');
  }
  if (config.concurrency > 20) {
    throw new Error('Concurrency must not exceed 20');
  }
};
