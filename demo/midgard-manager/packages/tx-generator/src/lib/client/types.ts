import { UTxO } from '@lucid-evolution/lucid';

/**
 * Core types for Midgard transaction generation and submission.
 */

/**
 * Serialized transaction payload emitted by the tx generator.
 */
export interface SerializedMidgardTransaction {
  type: 'Midgard L2 User Transaction';
  description: string;
  cborHex: string;
  txId: string;
}

/**
 * Configuration for generation flows that interact with a live Midgard node.
 */
export interface NodeTransactionConfig {
  network: string;
  initialUTxO: UTxO;
  nodeConfig: {
    baseUrl: string;
    retryAttempts?: number;
    retryDelay?: number;
    enableLogs?: boolean;
  };
}

/**
 * Generic success/error result shape returned by generator helpers.
 */
export interface GeneratorResult {
  success: boolean;
  transaction?: SerializedMidgardTransaction;
  error?: string;
}

/**
 * Response returned by the node's transaction-submission endpoint.
 */
export interface NodeTxResponse {
  message: string;
  status?: string;
}

/**
 * Response returned by the node's UTxO lookup endpoint.
 */
export interface NodeUtxoResponse {
  utxos: UTxO[];
}

/**
 * Response returned by block-query endpoints.
 */
export interface NodeBlockResponse {
  hashes: string[];
}
