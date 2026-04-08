import { Data } from 'effect';

/**
 * Small tagged error type used by the CLI to group failures by operational
 * area.
 */
export class MidgardError extends Data.TaggedError('MidgardError')<{
  readonly message: string;
  readonly operation: 'config' | 'transaction' | 'node';
}> {
  /**
   * Creates a configuration-scoped CLI error.
   */
  static config(message: string) {
    return new MidgardError({ message, operation: 'config' });
  }

  /**
   * Creates a transaction-scoped CLI error.
   */
  static transaction(message: string) {
    return new MidgardError({ message, operation: 'transaction' });
  }

  /**
   * Creates a node-scoped CLI error.
   */
  static node(message: string) {
    return new MidgardError({ message, operation: 'node' });
  }
}

/**
 * Extracts a human-readable error string from unknown thrown values.
 */
export const formatError = (error: unknown): string => {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
};
