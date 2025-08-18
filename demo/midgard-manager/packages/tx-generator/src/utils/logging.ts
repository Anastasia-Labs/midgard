import chalk from 'chalk';

/**
 * Interface for a transaction log entry
 */
export interface TransactionLogEntry {
  txId: string;
  timestamp: Date;
  type: string;
  status: 'submitted' | 'failed';
  details?: string;
}

/**
 * In-memory transaction log storage
 * This allows us to keep track of recent transactions and their status
 */
class TransactionLogger {
  private static instance: TransactionLogger;
  private logs: TransactionLogEntry[] = [];
  private maxLogEntries = 100; // Limit the number of entries to prevent memory bloat

  private constructor() {}

  static getInstance(): TransactionLogger {
    if (!TransactionLogger.instance) {
      TransactionLogger.instance = new TransactionLogger();
    }
    return TransactionLogger.instance;
  }

  /**
   * Add a transaction to the log
   */
  addLog(entry: Omit<TransactionLogEntry, 'timestamp'>): TransactionLogEntry {
    const logEntry: TransactionLogEntry = {
      ...entry,
      timestamp: new Date(),
    };

    // Add to beginning of array for most recent first
    this.logs.unshift(logEntry);

    // Truncate logs if they exceed the maximum size
    if (this.logs.length > this.maxLogEntries) {
      this.logs = this.logs.slice(0, this.maxLogEntries);
    }

    // Display the log entry immediately
    this.displayLogEntry(logEntry);

    return logEntry;
  }

  /**
   * Display a formatted log entry in the console
   */
  private displayLogEntry(entry: TransactionLogEntry): void {
    const timestamp = entry.timestamp.toISOString().replace('T', ' ').substring(0, 19);
    const shortTxId = `${entry.txId.substring(0, 8)}...${entry.txId.substring(
      entry.txId.length - 8
    )}`;

    let statusSymbol = '';
    let statusColor = chalk.white;

    switch (entry.status) {
      case 'submitted':
        statusSymbol = '✓';
        statusColor = chalk.green;
        break;
      case 'failed':
        statusSymbol = '✗';
        statusColor = chalk.red;
        break;
    }

    console.log(
      `${chalk.gray(timestamp)} ${statusColor(statusSymbol)} ${chalk.cyan(
        entry.type
      )} ${chalk.white(shortTxId)} ${statusColor(entry.status)}${
        entry.details ? ` (${entry.details})` : ''
      }`
    );
  }

  /**
   * Get the recent transaction logs
   */
  getLogs(): TransactionLogEntry[] {
    return [...this.logs];
  }

  /**
   * Clear all logs
   */
  clearLogs(): void {
    this.logs = [];
  }
}

// Expose the singleton instance
export const txLogger = TransactionLogger.getInstance();

/**
 * Log a submitted transaction
 */
export const logSubmittedTransaction = (txId: string, type: string, details?: string): void => {
  txLogger.addLog({
    txId,
    type,
    status: 'submitted',
    details,
  });
};

/**
 * Log a failed transaction
 */
export const logFailedTransaction = (txId: string, type: string, details?: string): void => {
  txLogger.addLog({
    txId,
    type,
    status: 'failed',
    details,
  });
};
