/**
 * Bounds the L1 reads spent on superseded attempts. A superseded attempt is
 * read again only to adopt a late landing or to record its retirement past k,
 * so each one backs off after an unresolved read, and one pass reads at most
 * `perPass` of them, those due longest first. The cost of a pass therefore
 * stays bounded however many attempts accumulate; retirement or adoption
 * drops an attempt from the set.
 */
export type SupersededAttemptReadSchedule = Readonly<{
  /** The attempts to read in this pass, from those still superseded. */
  due(transactionHashes: readonly string[], nowMs: number): readonly string[];
  /** Record an unresolved read: the attempt backs off. */
  unresolved(transactionHash: string, nowMs: number): void;
  /** Retired or adopted: the attempt leaves the schedule. */
  forget(transactionHash: string): void;
}>;

export const SUPERSEDED_ATTEMPT_READ_BASE_DELAY_MS = 20_000;
export const SUPERSEDED_ATTEMPT_READ_MAX_DELAY_MS = 600_000;
export const SUPERSEDED_ATTEMPT_READS_PER_PASS = 2;
/** Entries kept for attempts no caller has named for a while. */
const MAXIMUM_TRACKED_ATTEMPTS = 4096;

export const createSupersededAttemptReadSchedule = ({
  baseDelayMs = SUPERSEDED_ATTEMPT_READ_BASE_DELAY_MS,
  maxDelayMs = SUPERSEDED_ATTEMPT_READ_MAX_DELAY_MS,
  perPass = SUPERSEDED_ATTEMPT_READS_PER_PASS,
}: {
  readonly baseDelayMs?: number;
  readonly maxDelayMs?: number;
  readonly perPass?: number;
} = {}): SupersededAttemptReadSchedule => {
  if (
    !Number.isSafeInteger(perPass) ||
    perPass < 1 ||
    !(baseDelayMs > 0) ||
    !(maxDelayMs >= baseDelayMs)
  )
    throw new Error("superseded attempt read schedule is out of range");
  // Insertion order is least recently touched first, for eviction.
  const entries = new Map<string, { dueAtMs: number; delayMs: number }>();
  return Object.freeze({
    due: (transactionHashes, nowMs) =>
      [...new Set(transactionHashes)]
        .map((hash, order) => ({
          hash,
          order,
          // A first read is due at once.
          dueAtMs: entries.get(hash)?.dueAtMs ?? Number.NEGATIVE_INFINITY,
        }))
        .filter(({ dueAtMs }) => dueAtMs <= nowMs)
        .sort((left, right) =>
          left.dueAtMs === right.dueAtMs
            ? left.order - right.order
            : left.dueAtMs < right.dueAtMs
              ? -1
              : 1,
        )
        .slice(0, perPass)
        .map(({ hash }) => hash),
    unresolved: (transactionHash, nowMs) => {
      const previous = entries.get(transactionHash);
      const delayMs =
        previous === undefined
          ? baseDelayMs
          : Math.min(previous.delayMs * 2, maxDelayMs);
      entries.delete(transactionHash);
      entries.set(transactionHash, { dueAtMs: nowMs + delayMs, delayMs });
      while (entries.size > MAXIMUM_TRACKED_ATTEMPTS)
        entries.delete(entries.keys().next().value!);
    },
    forget: (transactionHash) => {
      entries.delete(transactionHash);
    },
  });
};

/** Process-wide default: transaction hashes identify attempts uniquely. */
export const supersededAttemptReadSchedule =
  createSupersededAttemptReadSchedule();
