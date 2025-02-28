import { Effect } from 'effect';

import { logFailedTransaction, logSubmittedTransaction } from '../../utils/logging.js';
import { MidgardNodeConfig, TRANSACTION_CONSTANTS } from '../types.js';

// Simple error types for better error handling
export type SubmitTxError =
  | { _tag: 'NetworkError'; error: string }
  | { _tag: 'ValidationError'; error: string }
  | { _tag: 'UnknownError'; error: string };

export class MidgardNodeClient {
  private readonly baseUrl: string;
  private readonly retryAttempts: number;
  private readonly retryDelay: number;
  private readonly enableLogs: boolean;

  constructor(config: MidgardNodeConfig) {
    this.baseUrl = config.baseUrl;
    this.retryAttempts = config.retryAttempts ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_ATTEMPTS;
    this.retryDelay = config.retryDelay ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_DELAY;
    this.enableLogs = config.enableLogs ?? true;
  }

  /**
   * Check node availability by making a dummy request
   */
  async isAvailable(
    timeoutMs: number = TRANSACTION_CONSTANTS.NODE_DEFAULTS.AVAILABILITY_TIMEOUT
  ): Promise<boolean> {
    try {
      // Create an AbortController to allow for timeouts
      const controller = new AbortController();
      const signal = controller.signal;

      // Set a timeout if provided
      let timeoutId: NodeJS.Timeout | undefined;
      if (timeoutMs) {
        timeoutId = setTimeout(() => controller.abort(), timeoutMs);
      }

      try {
        // Try to fetch a dummy transaction status - if the node is up, it will return 404
        // If the node is down, it will throw a connection error
        const response = await fetch(`${this.baseUrl}/tx?tx_hash=${'0'.repeat(64)}`, { signal });

        // Clear timeout
        if (timeoutId) clearTimeout(timeoutId);

        return response.status === 404;
      } catch (error) {
        // Clear timeout to prevent memory leaks
        if (timeoutId) clearTimeout(timeoutId);

        // Check if this was a timeout abort or a different error
        if (error instanceof DOMException && error.name === 'AbortError') {
          return false;
        }

        throw error; // Re-throw other errors to be caught by the outer try/catch
      }
    } catch (error) {
      return false;
    }
  }

  /**
   * Submit a transaction to the node with retries
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

        let attempts = 0;
        while (attempts < this.retryAttempts) {
          try {
            const response = await fetch(
              `${this.baseUrl}/submit?tx_cbor=${encodeURIComponent(cborHex)}`,
              {
                method: 'POST',
                headers: {
                  'Content-Type': 'text/plain',
                },
                signal,
              }
            );

            if (!response.ok) {
              const error = await response.json();
              throw new Error(error.message || `Unexpected status: ${response.status}`);
            }

            const result = await response.json();

            // Log the successful transaction submission
            if (this.enableLogs && result && result.txId) {
              logSubmittedTransaction(result.txId, txType);
            }

            // For successful submissions, return the raw result
            return result;
          } catch (error) {
            attempts++;
            if (attempts === this.retryAttempts) {
              // Log the failed transaction if we've exhausted all attempts
              if (this.enableLogs) {
                const errorMsg = error instanceof Error ? error.message : String(error);
                logFailedTransaction('unknown', txType, errorMsg);
              }
              throw error; // Let the Effect.catchAll handle it
            }
            await new Promise((resolve) => setTimeout(resolve, this.retryDelay));
          }
        }

        throw new Error('All retry attempts failed');
      })()
    ).pipe(
      // Handle errors by converting them to our status format
      Effect.catchAll((error) =>
        Effect.succeed(
          error instanceof TypeError ||
            (error instanceof Error && error.message === 'Node is not available')
            ? {
                status: 'NODE_UNAVAILABLE',
                message: 'Node is not available - transaction will be stored locally',
              }
            : {
                status: 'ERROR',
                error: error instanceof Error ? error.message : String(error),
              }
        )
      ),
      Effect.runPromise
    );
  }

  /**
   * Get transaction status from the node
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
        return { _tag: 'UnknownError', error: String(error) };
      },
    });
  }
}
