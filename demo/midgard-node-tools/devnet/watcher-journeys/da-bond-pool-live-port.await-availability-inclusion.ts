import * as SDK from "@al-ft/midgard-sdk";

import {
  availabilityEndingError,
  AvailabilityIntentLapsedError,
  type AvailabilityJournalView,
  isTransientCanonicalError,
  ledgerValidityRefusal,
  unsettledReconciliationError,
} from "./da-bond-pool-live-port.summarize-da-bond-pool-timeout.js";
import { describeErrorChain } from "./error-chain.js";

/**
 * Waits until the journal holds `txId` as included or confirmed. A
 * reconciliation that met an `unsettledReconciliationError` (a rebroadcast
 * refused with spent inputs or outside its validity interval, or a canonical
 * view catching up) is retried until `timeoutMs`; every other error, an
 * `expired` or `conflict` outcome (see `availabilityEndingError`), or the
 * timeout fails the wait.
 */
export const awaitAvailabilityInclusion = async ({
  txId,
  reconcile,
  journalRecord,
  timeoutMs,
  pollMs,
  wait,
  now = Date.now,
  log,
}: Readonly<{
  txId: string;
  reconcile: () => Promise<readonly SDK.DaAvailabilityOperationResult[]>;
  journalRecord: (txId: string) => AvailabilityJournalView | undefined;
  timeoutMs: number;
  pollMs: number;
  wait: (ms: number) => Promise<unknown>;
  now?: () => number;
  log: (line: string) => void;
}>): Promise<void> => {
  const deadline = now() + timeoutMs;
  let refusals = 0;
  let lastRefusal = "";
  const logged = new Set<string>();
  for (;;) {
    let results: readonly SDK.DaAvailabilityOperationResult[] = [];
    try {
      results = await reconcile();
    } catch (error) {
      const unsettled = unsettledReconciliationError(error);
      if (unsettled === undefined) throw error;
      refusals += 1;
      lastRefusal = describeErrorChain(error);
      if (!logged.has(unsettled)) {
        logged.add(unsettled);
        log(
          `availability ${txId}: ${unsettled}; reconciling until the canonical view settles it: ${lastRefusal}`,
        );
      }
    }
    const status =
      results.find((result) => result.txHash === txId)?.status ??
      journalRecord(txId)?.state;
    if (status === "included" || status === "confirmed") return;
    if (status === "expired" || status === "conflict")
      throw availabilityEndingError(txId, status, journalRecord(txId));
    if (now() > deadline)
      throw new Error(
        `Availability transaction ${txId} was not included in time (${status ?? "unknown"})` +
          (refusals > 0
            ? `; ${refusals} unsettled reconciliation(s), last: ${lastRefusal}`
            : ""),
      );
    await wait(pollMs);
  }
};

const JOURNAL_FINAL_STATES = new Set(["included", "confirmed", "expired"]);

/**
 * Reconciles the availability journal until every intent is final (included,
 * confirmed or expired). A reconciliation that met an
 * `unsettledReconciliationError` (a rebroadcast refused with spent inputs or
 * outside its validity interval, or a canonical view catching up) is retried
 * until `timeoutMs`, and then its error is rethrown; every other error, a
 * conflicting intent, or an intent still open at the timeout fails the wait.
 */
export const awaitQuietJournal = async ({
  reconcile,
  timeoutMs,
  pollMs,
  wait,
  now = Date.now,
}: Readonly<{
  reconcile: () => Promise<readonly SDK.DaAvailabilityOperationResult[]>;
  timeoutMs: number;
  pollMs: number;
  wait: (ms: number) => Promise<unknown>;
  now?: () => number;
}>): Promise<void> => {
  const deadline = now() + timeoutMs;
  for (;;) {
    let results: readonly SDK.DaAvailabilityOperationResult[];
    try {
      results = await reconcile();
    } catch (error) {
      // See unsettledReconciliationError: not settled yet.
      if (unsettledReconciliationError(error) === undefined) throw error;
      if (now() > deadline) throw error;
      await wait(pollMs);
      continue;
    }
    const open = results.filter(
      (result) => !JOURNAL_FINAL_STATES.has(result.status),
    );
    if (open.length === 0) return;
    if (open.some((result) => result.status === "conflict"))
      throw new Error(
        `Availability journal holds a conflicting intent: ${JSON.stringify(open)}`,
      );
    if (now() > deadline)
      throw new Error(
        `Availability journal did not settle: ${JSON.stringify(open)}`,
      );
    await wait(pollMs);
  }
};

/**
 * The built transaction to keep waiting on after the availability executor
 * threw, or undefined to rethrow. The executor journals an intent as pending
 * before its first broadcast, so a first broadcast the ledger refused outside
 * its validity interval, or a canonical read that raced a block, leaves a
 * journaled transaction whose own reconciliation settles it: included, or
 * lapsed and re-planned. Re-planning over it instead could plan the next
 * action while it is still in flight. Every other error, and any error before
 * the intent was journaled, is rethrown.
 */
export const availabilitySubmissionToAwait = (
  error: unknown,
  builtTxId: string | undefined,
  journalState: (txId: string) => string | undefined,
): string | undefined =>
  builtTxId !== undefined &&
  journalState(builtTxId) === "pending" &&
  (ledgerValidityRefusal(error) !== undefined ||
    isTransientCanonicalError(error))
    ? builtTxId
    : undefined;

/**
 * Runs one availability action through the executor (`execute`) and waits
 * until its transaction is included (`awaitIncluded`). When the executor built
 * nothing, because it reconciled an earlier intent instead, its result is
 * returned as `reconciled`. When it threw after journaling the built
 * transaction as pending, on an error `availabilitySubmissionToAwait` names,
 * that transaction is awaited; every other error is rethrown. An executor
 * result for another transaction, and an `expired` or `conflict` result, fail
 * the action (see `availabilityEndingError`).
 */
export const landAvailabilitySubmission = async ({
  label,
  execute,
  builtTxId,
  journalRecord,
  awaitIncluded,
  log,
}: Readonly<{
  label: string;
  execute: () => Promise<SDK.DaAvailabilityOperationResult>;
  builtTxId: () => string | undefined;
  journalRecord: (txId: string) => AvailabilityJournalView | undefined;
  awaitIncluded: (txId: string) => Promise<void>;
  log: (line: string) => void;
}>): Promise<
  | Readonly<{ kind: "included"; txId: string }>
  | Readonly<{ kind: "reconciled"; result: SDK.DaAvailabilityOperationResult }>
> => {
  let result: SDK.DaAvailabilityOperationResult | undefined;
  try {
    result = await execute();
  } catch (error) {
    const awaited = availabilitySubmissionToAwait(
      error,
      builtTxId(),
      (id) => journalRecord(id)?.state,
    );
    if (awaited === undefined) throw error;
    log(
      `${label}: journaled ${awaited}, but its first broadcast failed (${describeErrorChain(error)}); awaiting it`,
    );
  }
  const txId = builtTxId();
  if (txId === undefined) {
    if (result === undefined)
      throw new Error(`${label}: the executor neither built nor returned`);
    return { kind: "reconciled", result };
  }
  if (result !== undefined) {
    if (result.txHash !== txId)
      throw new Error(
        `Availability executor returned ${result.txHash} for the transaction built as ${txId}`,
      );
    if (result.status === "expired" || result.status === "conflict")
      throw availabilityEndingError(txId, result.status, journalRecord(txId));
  }
  log(`${label}: submitted ${txId}`);
  await awaitIncluded(txId);
  return { kind: "included", txId };
};

/**
 * What an availability attempt does before it plans: settle the journal, wait
 * for a ledger tip fresh enough for the CLI builder's interval (it opens
 * sixty seconds before the wall clock, and the ledger checks it against its
 * tip), then read the canonical boundary the action is planned against.
 */
export const prepareAvailabilityAttempt = async <B>({
  quietJournal,
  awaitFreshTip,
  readBoundary,
}: Readonly<{
  quietJournal: () => Promise<void>;
  awaitFreshTip: () => Promise<void>;
  readBoundary: () => Promise<B>;
}>): Promise<B> => {
  await quietJournal();
  await awaitFreshTip();
  return readBoundary();
};

/** How many lapsed availability transactions one action may re-plan. */
export const MAX_LAPSED_REPLANS = 3;

/**
 * What `landAvailability` does after an attempt failed: re-plan after a
 * lapsed transaction (`AvailabilityIntentLapsedError`) while fewer than
 * `MAX_LAPSED_REPLANS` have lapsed; retry the whole flow after a transient
 * canonical error while nothing was journaled (a transaction that was never
 * journaled was never broadcast) and fewer than `maxTransientAttempts`
 * attempts ran; throw otherwise. A conflict, any other expiry, a script or
 * validator refusal and an unexpected planned action always throw.
 */
export const availabilityAttemptRecovery = (
  error: unknown,
  state: Readonly<{
    journaled: boolean;
    lapses: number;
    attempt: number;
    maxTransientAttempts: number;
  }>,
): "replan" | "retry" | "throw" => {
  if (error instanceof AvailabilityIntentLapsedError)
    return state.lapses < MAX_LAPSED_REPLANS ? "replan" : "throw";
  return !state.journaled &&
    isTransientCanonicalError(error) &&
    state.attempt < state.maxTransientAttempts
    ? "retry"
    : "throw";
};

/**
 * What one read of an expired header commit decides, from reads bracketed by
 * one canonical boundary (`stable` when the boundary did not move across
 * them): `adopt` when the commit's own transaction spent its anchor, whoever
 * holds the header output now (a DA Apply spends and recreates it); `absent`
 * when the anchor is unspent and no output holds the header, so the commit
 * never landed and never can; `reread` when the boundary moved, until `read`
 * reaches `maxReads`, then `unsettled`; `conflict` for everything else (the
 * anchor spent by another transaction, a header without its anchor spent, or
 * more than one header output).
 */
export const settleExpiredCommitReads = ({
  txId,
  stable,
  anchorSpentBy,
  headerHolders,
  read,
  maxReads,
}: Readonly<{
  txId: string;
  stable: boolean;
  /** The transaction that spent the anchor; null while it is unspent. */
  anchorSpentBy: string | null;
  /** The transactions whose unspent outputs hold the header's unit. */
  headerHolders: readonly string[];
  read: number;
  maxReads: number;
}>): "adopt" | "absent" | "conflict" | "reread" | "unsettled" => {
  if (!stable) return read >= maxReads ? "unsettled" : "reread";
  if (anchorSpentBy === txId)
    return headerHolders.length <= 1 ? "adopt" : "conflict";
  return anchorSpentBy === null && headerHolders.length === 0
    ? "absent"
    : "conflict";
};
