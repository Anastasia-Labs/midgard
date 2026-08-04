const transactionTypeDescriptions: Record<string, string> = {
  'one-to-one': 'Simple single-output transactions',
  'multi-output': 'Complex multi-recipient transactions',
  mixed: 'Combination of simple and complex transactions',
};

/**
 * Maps generator transaction types to short operator-facing descriptions.
 */
export const getTransactionTypeDescription = (type: string): string =>
  transactionTypeDescriptions[type] ?? 'Unknown transaction type';
