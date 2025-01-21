/* eslint-disable simple-import-sort/exports */
/**
 * Midgard Transaction Generator
 *
 * This package provides tools for generating transactions
 * that simulate user interactions with the Midgard L2 network.
 *
 * The generated transactions are in CBOR format and will be used
 * by the Midgard node for testing transaction deserialization
 * and validation functionality.
 */

export {
  getPrivateKeyCborHex,
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  serializeAssets,
} from './core/utils.js';

export type {
  MidgardTransaction,
  SerializedMidgardTransaction,
} from './core/types.js';

export {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from './generators/index.js';

export type {
  MultiOutputTransactionConfig,
  OneToOneTransactionConfig,
} from './generators/index.js';
