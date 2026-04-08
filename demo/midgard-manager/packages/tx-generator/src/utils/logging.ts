import chalk from 'chalk';

/**
 * Structured representation of one tx-generator log event.
 */
export interface TransactionLogEntry {
  txId: string;
  timestamp: Date;
  type: string;
  status: 'submitted' | 'failed';
  details?: string;
}

/**
 * In-memory log sink for recently submitted or failed transactions.
 */
class TransactionLogger {
  private static instance: TransactionLogger;
  private logs: TransactionLogEntry[] = [];
  private maxLogEntries = 100; // Limit the number of entries to prevent memory bloat

  private constructor() {}

  /**
   * Returns the shared transaction logger instance.
   */
  static getInstance(): TransactionLogger {
    if (!TransactionLogger.instance) {
      TransactionLogger.instance = new TransactionLogger();
    }
    return TransactionLogger.instance;
  }

  /**
   * Adds a transaction event to the in-memory log and echoes it to stdout.
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
   * Renders a single transaction log entry using the shared console format.
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
   * Returns a copy of the most recent transaction logs.
   */
  getLogs(): TransactionLogEntry[] {
    return [...this.logs];
  }

  /**
   * Clears the in-memory transaction log buffer.
   */
  clearLogs(): void {
    this.logs = [];
  }
}

/**
 * Shared logger instance used by tx submission paths.
 */
export const txLogger = TransactionLogger.getInstance();

/**
 * Records a successful transaction submission.
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
 * Records a failed transaction submission.
 */
export const logFailedTransaction = (txId: string, type: string, details?: string): void => {
  txLogger.addLog({
    txId,
    type,
    status: 'failed',
    details,
  });
};
