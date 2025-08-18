import { Data } from 'effect';

// Single error type for all operations
export class MidgardError extends Data.TaggedError('MidgardError')<{
  readonly message: string;
  readonly operation: 'config' | 'transaction' | 'node';
}> {
  static config(message: string) {
    return new MidgardError({ message, operation: 'config' });
  }

  static transaction(message: string) {
    return new MidgardError({ message, operation: 'transaction' });
  }

  static node(message: string) {
    return new MidgardError({ message, operation: 'node' });
  }
}

// Helper to create error messages with consistent formatting
export const formatError = (error: unknown): string => {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
};
