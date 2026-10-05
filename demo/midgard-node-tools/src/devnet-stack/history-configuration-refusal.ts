/** Intrinsic recorded configuration/retained-admission contradictions only. */
export class HistoryConfigurationRefusal extends Error {
  constructor(message: string) {
    super(message);
    this.name = "HistoryConfigurationRefusal";
  }
}

/** A positively observed raw/path contradiction, independent of decoder time. */
export class HistoryEvidenceContradiction extends HistoryConfigurationRefusal {}

/** History command consumers keep transport, cancellation and I/O transient. */
export const historyCommandExitCode = (error: unknown): 70 | 78 =>
  error instanceof HistoryConfigurationRefusal ? 78 : 70;
