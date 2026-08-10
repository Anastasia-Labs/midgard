import { decodeMidgardUtxo } from '@al-ft/lucid-midgard';
import { UTxO } from '@lucid-evolution/lucid';
import { Effect } from 'effect';

import { formatError } from '../../utils/common.js';
import { logFailedTransaction, logSubmittedTransaction } from '../../utils/logging.js';
import { MidgardNodeConfig, TRANSACTION_CONSTANTS } from '../types.js';

/**
 * Normalized error variants returned by node-client read paths.
 */
export type SubmitTxError =
  | { _tag: 'NetworkError'; error: string }
  | { _tag: 'ValidationError'; error: string }
  | { _tag: 'UnknownError'; error: string };

/**
 * Thin HTTP client for Midgard node endpoints used by the tx generator.
 */
export class MidgardNodeClient {
  private readonly baseUrl: string;
  private readonly retryAttempts: number;
  private readonly retryDelay: number;
  private readonly enableLogs: boolean;

  constructor(config: MidgardNodeConfig) {
    this.baseUrl = config.baseUrl;
    this.retryAttempts = config.retryAttempts ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_ATTEMPTS;
    if (!Number.isSafeInteger(this.retryAttempts) || this.retryAttempts < 1) {
      throw new Error('Node retry attempts must be a positive integer');
    }
    this.retryDelay = config.retryDelay ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_DELAY;
    this.enableLogs = config.enableLogs ?? true;
  }

  /**
   * Decodes one raw UTxO payload returned by the node into Lucid's core UTxO
   * shape.
   */
  private decodeNodeUtxo(raw: { outref: string; outputCbor: string }): UTxO | undefined {
    try {
      // The node's `outref` bytes are the §5.3 field-0/1 item encoding —
      // `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes — which is what
      // on-chain `ledger_outref_key` derives through `encode_midgard_tx_input`,
      // not CML's minimal-index `TransactionInput` CBOR. `decodeMidgardUtxo`
      // owns that decode (and bounds the index to a CBOR uint16), so the shape
      // never gets a second spelling here.
      const utxo = decodeMidgardUtxo({
        outRefCbor: Buffer.from(raw.outref, 'hex'),
        outputCbor: Buffer.from(raw.outputCbor, 'hex'),
      });
      const output = utxo.output;
      return {
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        address: output.address,
        assets: output.assets,
        ...(output.datum == null ? {} : { datum: output.datum.cbor }),
      };
    } catch {
      return undefined;
    }
  }

  /**
   * Checks whether the node endpoint is reachable within the provided timeout.
   *
   * Any HTTP response counts as "available" because bootstrap state may change
   * the exact status code even when the node is healthy.
   */
  async isAvailable(
    timeoutMs: number = TRANSACTION_CONSTANTS.NODE_DEFAULTS.AVAILABILITY_TIMEOUT
  ): Promise<boolean> {
    const controller = new AbortController();
    const timeoutId = timeoutMs > 0 ? setTimeout(() => controller.abort(), timeoutMs) : undefined;

    try {
      const response = await fetch(`${this.baseUrl}/tx?tx_hash=${'0'.repeat(64)}`, {
        signal: controller.signal,
      });
      // Any HTTP response means the node is reachable; status code semantics
      // can vary depending on DB/bootstrap state.
      return response.status >= 100;
    } catch {
      return false;
    } finally {
      if (timeoutId !== undefined) clearTimeout(timeoutId);
    }
  }

  /**
   * Submits a transaction to the node and falls back to a structured status
   * result when the node is unavailable.
   */
  submitTransaction(cborHex: string, txType: string = 'Transaction') {
    return Effect.tryPromise((signal: AbortSignal) =>
      (async () => {
        // First check if node is available
        const isNodeAvailable = await this.isAvailable();
        if (!isNodeAvailable) {
          return {
            status: 'NODE_UNAVAILABLE',
            message: 'Node is not available - transaction will be stored locally',
          };
        }

        for (let attempt = 1; ; attempt++) {
          try {
            const response = await fetch(`${this.baseUrl}/submit`, {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
              },
              body: JSON.stringify({ tx_cbor: cborHex }),
              signal,
            });

            if (!response.ok) {
              const error = await response
                .json()
                .catch(() => ({ error: `Unexpected status: ${response.status}` }));
              throw new Error(
                error.error || error.message || `Unexpected status: ${response.status}`
              );
            }

            const result = await response.json();
            // Log the successful transaction submission
            if (this.enableLogs && result && result.txId) {
              logSubmittedTransaction(result.txId, txType);
            }
            // For successful submissions, return the raw result
            return result;
          } catch (error) {
            if (attempt >= this.retryAttempts) {
              // Log the failed transaction if we've exhausted all attempts
              if (this.enableLogs) {
                logFailedTransaction('unknown', txType, formatError(error));
              }
              throw error; // Let the Effect.catchAll handle it
            }
            await new Promise((resolve) => setTimeout(resolve, this.retryDelay));
          }
        }
      })()
    ).pipe(
      // Handle errors by converting them to our status format
      Effect.catchAll((error) =>
        Effect.succeed(
          error instanceof TypeError
            ? {
                status: 'NODE_UNAVAILABLE',
                message: 'Node is not available - transaction will be stored locally',
              }
            : {
                status: 'ERROR',
                error: formatError(error),
              }
        )
      ),
      Effect.runPromise
    );
  }

  /**
   * Retrieves the node's status for a previously submitted transaction.
   */
  getTransactionStatus(txHash: string) {
    return Effect.tryPromise({
      try: async () => {
        const response = await fetch(`${this.baseUrl}/tx?tx_hash=${txHash}`);
        if (!response.ok) {
          throw new Error(`Unexpected status: ${response.status}`);
        }
        const data = await response.json();
        return data;
      },
      catch: (error: unknown): SubmitTxError => {
        if (error instanceof TypeError) {
          return { _tag: 'NetworkError', error: error.message };
        }
        if (error instanceof Error) {
          return { _tag: 'ValidationError', error: error.message };
        }
        return { _tag: 'UnknownError', error: formatError(error) };
      },
    });
  }

  /**
   * Retrieve spendable UTxOs from the node for a specific address.
   * Invalid/non-Cardano entries are ignored.
   */
  async getSpendableUtxos(address: string): Promise<UTxO[]> {
    const response = await fetch(`${this.baseUrl}/utxos?address=${encodeURIComponent(address)}`);
    if (!response.ok) {
      return [];
    }

    const data = (await response.json()) as {
      utxos?: Array<{ outref: string; outputCbor: string }>;
    };
    if (!Array.isArray(data.utxos)) {
      return [];
    }

    return data.utxos
      .map((raw) => this.decodeNodeUtxo(raw))
      .filter((utxo): utxo is UTxO => utxo !== undefined);
  }
}
