/**
 * The sweep of our own computation threads whose header is gone: each
 * admitted observation is followed by one pass, run by the supervisor's
 * pump after the jobs it queued. A pass reads one page of our threads from
 * the follower (`readWatcherOwnThreadPage`) and cancels at most
 * `cancelsPerPass` of those due for reclaim, each built with no header and
 * no workflow behind it (`createOwnThreadCanceller`).
 *
 * The sweep keeps no durable state and reads no stored workflow record:
 * what is left to cancel is what the follower still holds. Its process
 * memory is a page cursor and the threads it could not reclaim; both are
 * rebuilt from the chain within one cycle of pages. The canceller reads
 * the ledger with the node's mempool applied, so a cancel of ours still in
 * the mempool shows its thread spent: a later attempt, before or after a
 * restart, meets the thread spent and sends nothing. A cancel that meets
 * the thread already spent, by a workflow's own reclaim or an earlier
 * cancel of ours, settles quietly.
 *
 * Operators see two degradations, status and metrics only:
 * `fault_proof_own_thread_unreclaimed` (a thread due for reclaim whose
 * last two cancels in a row could not be built, funded or sent, with its
 * out-ref and lovelace) and `fault_proof_own_thread_uncancellable` (a thread of ours
 * the chain offers no cancel for, never attempted).
 */
import type {
  OwnThreadCanceller,
  OwnThreadCancelOutcome,
} from "@al-ft/midgard-fault-proofs/own-thread-cancel";
import type { FactStore } from "@al-ft/midgard-l1-follower";

import {
  compareOwnThreadOutRefs,
  readWatcherOwnThread,
  readWatcherOwnThreadPage,
  type WatcherOwnThread,
} from "../l1-follower/own-thread-reads.js";
import type { WatcherL1Degradation } from "../l1-follower/tx-inputs.js";
import type { WatcherFaultProofApplication } from "./fault-proof-application.js";

/** A thread of ours due for reclaim whose latest cancel did not go out. */
export const FAULT_PROOF_OWN_THREAD_UNRECLAIMED =
  "fault_proof_own_thread_unreclaimed" as const;
/** A thread of ours at a step the chain offers no cancel for. */
export const FAULT_PROOF_OWN_THREAD_UNCANCELLABLE =
  "fault_proof_own_thread_uncancellable" as const;

/** The live outputs under the computation-thread policy one pass reads. */
export const OWN_THREAD_PAGE_LIMIT = 64;
/** The cancels one pass sends at most. */
export const OWN_THREAD_CANCELS_PER_PASS = 2;
/** The threads a degradation's detail names before it counts the rest. */
const DETAIL_THREADS = 8;
/**
 * The failed attempts in a row after which a thread is reported
 * unreclaimed. One failure is not: a cancel that meets the thread already
 * being spent by another of ours (a workflow's reclaim still pending) fails
 * once, and the next pass finds the thread gone.
 */
const FAILURES_REPORTED = 2;

type Held = Readonly<{ lovelace: bigint; reason: string; failures: number }>;

export type WatcherOwnThreadSweep = Readonly<{
  /** One pass: read a page of our threads and cancel those due, bounded. */
  pass(): Promise<void>;
  degradations(): readonly WatcherL1Degradation[];
}>;

export type WatcherOwnThreadCancellerSource = () => Promise<
  | Readonly<{ kind: "cancelling"; canceller: OwnThreadCanceller }>
  | Readonly<{ kind: "observing" }>
>;

const message = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

const inRange = (
  outRef: string,
  after: string | null,
  through: string | null,
): boolean =>
  (after === null || compareOwnThreadOutRefs(outRef, after) > 0) &&
  (through === null || compareOwnThreadOutRefs(outRef, through) <= 0);

const degradation = (
  reason: string,
  held: ReadonlyMap<string, Held>,
): WatcherL1Degradation[] => {
  const entries = [...held]
    .filter(([, { failures }]) => failures >= FAILURES_REPORTED)
    .sort(([left], [right]) => compareOwnThreadOutRefs(left, right));
  const named = entries
    .slice(0, DETAIL_THREADS)
    .map(
      ([outRef, { lovelace, reason: why }]) =>
        `${outRef} lovelace=${lovelace.toString()} ${why}`,
    );
  const rest = entries.length - named.length;
  if (entries.length === 0) return [];
  return [
    Object.freeze({
      reason,
      count: entries.length,
      detail: [
        ...named,
        ...(rest > 0 ? [`and ${rest.toString()} more`] : []),
      ].join("; "),
    }),
  ];
};

export const createWatcherOwnThreadSweep = ({
  store,
  policyId,
  finalDepth,
  openCanceller,
  log = () => undefined,
  pageLimit = OWN_THREAD_PAGE_LIMIT,
  cancelsPerPass = OWN_THREAD_CANCELS_PER_PASS,
}: {
  readonly store: Pick<FactStore, "transaction">;
  /** The computation-thread minting policy. */
  readonly policyId: string;
  /** The depth a header's removal must reach before its threads are reclaimed. */
  readonly finalDepth: number;
  /** Opens the canceller, kept once open; a refusal or an observing
   * application is asked again at the next pass. */
  readonly openCanceller: WatcherOwnThreadCancellerSource;
  readonly log?: (line: string) => void;
  readonly pageLimit?: number;
  readonly cancelsPerPass?: number;
}): WatcherOwnThreadSweep => {
  let opened: ReturnType<WatcherOwnThreadCancellerSource> | null = null;
  let after: string | null = null;
  const unreclaimed = new Map<string, Held>();
  const uncancellable = new Map<string, Held>();

  const forget = (outRef: string): void => {
    unreclaimed.delete(outRef);
    uncancellable.delete(outRef);
  };

  const settle = (
    thread: WatcherOwnThread,
    outcome: OwnThreadCancelOutcome,
  ): void => {
    const { outRef, lovelace } = thread;
    switch (outcome.kind) {
      case "submitted":
        forget(outRef);
        log(
          `own thread ${outRef}: cancel ${outcome.txHash} sent, reclaiming ${outcome.reclaimedLovelace.toString()} lovelace`,
        );
        return;
      case "spent_elsewhere":
        forget(outRef);
        log(`own thread ${outRef}: already spent; nothing to reclaim`);
        return;
      case "withdrawn":
        forget(outRef);
        return;
      case "uncancellable":
        unreclaimed.delete(outRef);
        uncancellable.set(outRef, {
          lovelace,
          reason: outcome.reason,
          failures: FAILURES_REPORTED,
        });
        return;
      case "unfunded":
      case "unavailable":
      case "failed":
        uncancellable.delete(outRef);
        unreclaimed.set(outRef, {
          lovelace,
          reason: `${outcome.kind}: ${outcome.reason}`,
          failures: (unreclaimed.get(outRef)?.failures ?? 0) + 1,
        });
        log(
          `own thread ${outRef}: cancel ${outcome.kind}, retried at a later pass: ${outcome.reason}`,
        );
    }
  };

  const canceller = async (): Promise<OwnThreadCanceller | null> => {
    opened ??= openCanceller();
    try {
      const source = await opened;
      if (source.kind === "cancelling") return source.canceller;
      // An application admitted later cancels from then on.
      opened = null;
      return null;
    } catch (cause) {
      opened = null;
      log(`own threads: no canceller this pass: ${message(cause)}`);
      return null;
    }
  };

  const pass = async (): Promise<void> => {
    const active = await canceller();
    if (active === null) return;
    const scope = {
      policyId,
      proverKeyHash: active.proverKeyHash,
      finalDepth,
    };
    let page: Awaited<ReturnType<typeof readWatcherOwnThreadPage>>;
    try {
      page = await readWatcherOwnThreadPage(store, {
        ...scope,
        after,
        limit: pageLimit,
      });
    } catch (cause) {
      log(`own threads: the follower read failed this pass: ${message(cause)}`);
      return;
    }
    if (page.kind === "unready") return;
    const ours = new Set(page.threads.map(({ outRef }) => outRef));
    for (const held of [unreclaimed, uncancellable])
      for (const outRef of [...held.keys()])
        if (inRange(outRef, page.after, page.through) && !ours.has(outRef))
          held.delete(outRef);
    let attempts = 0;
    let stoppedAt: string | null = null;
    for (const thread of page.threads) {
      if (thread.disposition !== "reclaim") {
        forget(thread.outRef);
        continue;
      }
      const subject = {
        threadOutRef: thread.outRef,
        categoryId: thread.categoryId,
        paymentScriptHash: thread.paymentScriptHash,
      };
      const plan = active.plan(subject);
      if (plan.kind !== "cancellable") {
        settle(thread, plan);
        continue;
      }
      if (attempts === cancelsPerPass) break;
      attempts += 1;
      stoppedAt = thread.outRef;
      let outcome: OwnThreadCancelOutcome;
      try {
        outcome = await active.cancel({
          subject,
          stillDue: async () =>
            (
              await readWatcherOwnThread(store, {
                ...scope,
                outRef: thread.outRef,
              })
            )?.disposition === "reclaim",
        });
      } catch (cause) {
        outcome = { kind: "failed", reason: message(cause) };
      }
      settle(thread, outcome);
    }
    // A page with due threads left past the cap is read again from the last
    // one attempted, so every due thread is reached within a few passes.
    const capped =
      attempts === cancelsPerPass &&
      page.threads.some(
        ({ outRef, disposition }) =>
          disposition === "reclaim" &&
          stoppedAt !== null &&
          compareOwnThreadOutRefs(outRef, stoppedAt) > 0,
      );
    after = capped ? stoppedAt : page.through;
  };

  return Object.freeze({
    pass,
    degradations: () => [
      ...degradation(FAULT_PROOF_OWN_THREAD_UNRECLAIMED, unreclaimed),
      ...degradation(FAULT_PROOF_OWN_THREAD_UNCANCELLABLE, uncancellable),
    ],
  });
};

/** The sweep over the application's canceller. */
export const watcherOwnThreadCancellerSource =
  (
    application: Pick<WatcherFaultProofApplication, "openOwnThreadCanceller">,
  ): WatcherOwnThreadCancellerSource =>
  async () =>
    await application.openOwnThreadCanceller();
