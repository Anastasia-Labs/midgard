/* eslint-disable simple-import-sort/exports */
/**
 * Midgard Transaction Generator
 * Generates and submits test transactions to the Midgard L2 network
 */

// Re-export transaction generator functions
export {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from './lib/generators/index.js';

// Export configuration types
export type { TransactionGeneratorConfig } from './lib/types.js';
export { DEFAULT_CONFIG } from './lib/types.js';

// Export transaction types
export type {
  MultiOutputTransactionConfig,
  OneToOneTransactionConfig,
} from './lib/generators/index.js';

export type { SerializedMidgardTransaction } from './lib/client/types.js';
export * from './lib/client/types.js';
export { MidgardNodeClient } from './lib/client/node-client.js';

// Utils for transaction handling
export { serializeAssets } from './utils/common.js';

// Export the simplified API
export { startGenerator, stopGenerator, getGeneratorStatus } from './lib/scheduler/scheduler.js';
