/**
 * A local ledger divergence: ledger entries this node holds do not derive
 * the root it stores for them, or a processed landed delta does not apply
 * to the ledger it follows. Both come from this node's own stores
 * (`confirmed_ledger` with the processed landed deltas, and the frontier's
 * or a landed row's root), so the divergence is this node's fault, never a
 * block's. Each check that compares them fails with
 * `LocalLedgerDivergenceError`, which names the check. The main thread
 * counts each divergence in `midgard_local_ledger_divergence_total`, the
 * series the Prometheus exporter reads and the alert rules select: a check
 * it runs counts as it fails, and a commit worker's check counts when the
 * worker's output reaches it. A worker thread's metrics stay in that
 * thread, so a worker never counts its own.
 */
import { Data, Effect, Metric } from "effect";

/**
 * Where a divergence was found:
 * - `commit_base_landed`, `commit_base_confirmed_ledger`: the commit base;
 *   no check raises them, since the commit base reads the ledger store,
 *   whose reads this node trusts;
 * - `confirmed_ledger`: `confirmed_ledger` against the frontier's root, in
 *   the full verification the main thread runs off the commit path
 *   (`confirmed-ledger-verification.ts`);
 * - `confirmed_ledger_tail`: in that verification, `confirmed_ledger` with
 *   the processed landed deltas to the queue tail against the tail's root,
 *   or one of those deltas that does not apply to the ledger before it.
 */
export const LEDGER_DIVERGENCE_CHECKS = [
  "commit_base_landed",
  "commit_base_confirmed_ledger",
  "confirmed_ledger",
  "confirmed_ledger_tail",
] as const;

export type LedgerDivergenceCheck = (typeof LEDGER_DIVERGENCE_CHECKS)[number];

/**
 * The checks whose divergence holds commits: the commit base's two and
 * the two of the full verification of the landed ledger the landed commit
 * base builds on.
 */
export const COMMIT_BASE_DIVERGENCE_CHECKS = [
  "commit_base_landed",
  "commit_base_confirmed_ledger",
  "confirmed_ledger",
  "confirmed_ledger_tail",
] as const satisfies readonly LedgerDivergenceCheck[];

/**
 * The ledger at `headerHash` does not match the root `root` this node's
 * stores give that header: its entries derive `derivedRoot`, or, where the
 * check applies deltas instead of deriving a root, `fault` names the delta
 * that does not apply.
 */
export type LedgerDivergence = Readonly<{
  check: LedgerDivergenceCheck;
  headerHash: string;
  root: string;
  derivedRoot?: string;
  fault?: string;
}>;

export class LocalLedgerDivergenceError extends Data.TaggedError(
  "LocalLedgerDivergenceError",
)<LedgerDivergence> {}

/** One line naming the divergence: its check, header, stored root and what
 * the ledger gave instead. */
export const ledgerDivergenceDetail = (divergence: LedgerDivergence): string =>
  `local ledger divergence (${divergence.check}): the ledger at ${divergence.headerHash} ${
    divergence.fault ?? `derives root ${divergence.derivedRoot ?? "none"}`
  }, its stored root is ${divergence.root}`;

export const localLedgerDivergenceCounter = Metric.counter(
  "midgard_local_ledger_divergence_total",
  {
    description:
      "Checks that found this node's ledger entries do not derive the root it stores for them, by check",
    incremental: true,
  },
);

const divergenceSeries = (check: LedgerDivergenceCheck) =>
  Metric.tagged(localLedgerDivergenceCounter, "check", check);

/**
 * Exports every check's series at 0, which the node does at startup: an
 * alert over the counter's increase then sees its first divergence, where
 * a series first exported at 1 would show no increase.
 */
export const exportLedgerDivergenceSeries: Effect.Effect<void> = Effect.forEach(
  LEDGER_DIVERGENCE_CHECKS,
  (check) => Metric.incrementBy(divergenceSeries(check), 0),
  { discard: true },
);

/** Counts `divergence` under its check, in the main thread only (above). */
export const countLedgerDivergence = (
  divergence: Pick<LedgerDivergence, "check">,
): Effect.Effect<void> => Metric.increment(divergenceSeries(divergence.check));

/** Fails with the divergence unless `derivedRoot` is `root`; counts nothing. */
export const checkLedgerRoot = (
  divergence: LedgerDivergence & Readonly<{ derivedRoot: string }>,
): Effect.Effect<void, LocalLedgerDivergenceError> =>
  divergence.derivedRoot === divergence.root
    ? Effect.void
    : Effect.fail(new LocalLedgerDivergenceError(divergence));
