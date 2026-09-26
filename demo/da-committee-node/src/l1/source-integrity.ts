/**
 * The L1 source contradicted itself, or contradicted state this committee
 * node already made durable, in a way that re-reading cannot repair: a
 * rollback past a persisted decision, surfaces disagreeing at one proven chain
 * point, replayed state-queue history that fails verification, or a corrupt
 * durable chain-sync journal.
 *
 * The committee quarantines its L1 source only on this error. Every other
 * failure to observe L1 (timeouts, surfaces at different points, a chain that
 * moved during a read, history not yet final) fails the current tick and is
 * retried on the next; a sustained outage ends in the L1-view fatal exit.
 */
export class L1SourceIntegrityError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "L1SourceIntegrityError";
  }
}

/**
 * Authenticated state-queue history does not lead from the replay anchor to
 * the current queue: an anchor output the chain does not know, an anchor no
 * transaction advances, or checkpoints the SDK replay does not accept from it
 * (history that does not extend it, or checkpoint content that is not
 * canonical). From a durable anchor, which is final, that is an integrity
 * failure like any other. From a not-yet-final bootstrap candidate it is how
 * a rollback of the candidate shows, and the scanner discards the candidate
 * instead. That also discards, rather than quarantines on, non-canonical
 * history replayed from a candidate, which is harmless: no decision is made
 * before a durable anchor exists.
 */
export class StateQueueHistoryNotExtendingAnchorError extends L1SourceIntegrityError {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "StateQueueHistoryNotExtendingAnchorError";
  }
}
