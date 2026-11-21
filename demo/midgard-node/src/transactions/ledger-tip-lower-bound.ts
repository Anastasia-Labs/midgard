/**
 * Lower bounds judged at the observed ledger tip, the way submit judges them.
 *
 * The ledger tip moves only when a block lands, so the time until it reaches
 * a lower bound cannot be computed from the slot difference: a block may land
 * at once or after many slot lengths. A transaction with a lower bound the tip
 * has not reached is not admissible, and submit refuses it as built too early.
 * So a caller with such a bound reads the observed tip before it builds:
 *
 * - a one-shot command or operator verb waits for the tip to reach it
 *   (`awaitLedgerTipAtLowerBound`), re-reading the tip as the L1 clock does;
 * - a long-running node loop never waits inline: it checks
 *   (`lowerBoundAtLedgerTip`) and defers through its own due work.
 *
 * Either way the tip must pass the bound by the submit margin, the same slot
 * the submit check waits for.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  ogmiosSlotEvidenceUnavailableCause,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { L1_TIP_REFRESH_MS } from "../l1-access.js";
import { isRetryableProviderError } from "../provider-retry.js";
import { L1LedgerBehindError } from "../services/l1-ledger-tip.js";
import {
  planSubmitTiming,
  type SubmitTimingNotDuePlan,
  type SubmitTimingReadyPlan,
} from "./submit-timing.js";
import { EARLY_VALIDITY_RETRY_SLOT_BUFFER } from "./utils.parse-structured-outside-validity-interval-details.js";
import {
  resolvePreSubmitSlotSnapshot,
  submitRecoverySleep,
} from "./utils.submit-recovery-options.js";

/** Whether the tip has reached a lower bound, judged as submit judges it. */
export type LowerBoundAtLedgerTip =
  | (SubmitTimingReadyPlan & { readonly snapshot: SubmitSlotSnapshot })
  | (SubmitTimingNotDuePlan & { readonly snapshot: SubmitSlotSnapshot });

/**
 * The observed ledger tip: the client's L1 access snapshot, or the
 * emulator's chain. Fails with the tip source's own error, including its
 * staleness refusal when the tip trails the wall clock too far.
 */
export const observedLedgerTip = (
  lucid: LucidEvolution,
): Effect.Effect<SubmitSlotSnapshot, unknown> =>
  resolvePreSubmitSlotSnapshot(lucid);

/** Judges `validFromMs` at `snapshot`'s tip with the submit margin. */
export const judgeLowerBoundAtLedgerTip = (
  lucid: LucidEvolution,
  snapshot: SubmitSlotSnapshot,
  validFromMs: number | bigint,
  callerLabel: string,
): LowerBoundAtLedgerTip => {
  const plan = planSubmitTiming({
    callerLabel,
    invalidBeforeSlot: lucid.unixTimeToSlot(Number(validFromMs)),
    slotSnapshot: snapshot,
    submitSlotBuffer: EARLY_VALIDITY_RETRY_SLOT_BUFFER,
    inlineWaitPolicy: "defer_positive_wait",
  });
  if (plan.status === "ready" || plan.status === "not_due") {
    return { ...plan, snapshot };
  }
  // Without an upper bound the planner answers only ready or not due.
  throw new Error(
    `${callerLabel}: unexpected lower-bound judgement ${plan.status}`,
  );
};

/** Reads the observed tip and judges `validFromMs` at it. */
export const lowerBoundAtLedgerTip = (
  lucid: LucidEvolution,
  validFromMs: number | bigint,
  callerLabel: string,
): Effect.Effect<LowerBoundAtLedgerTip, unknown> =>
  Effect.map(observedLedgerTip(lucid), (snapshot) =>
    judgeLowerBoundAtLedgerTip(lucid, snapshot, validFromMs, callerLabel),
  );

/**
 * The tip source's staleness refusal: its ledger tip further behind wall
 * time than the source's own bound (the local node's or follower's
 * `l1_node_behind`, the local Ogmios's `ogmios_tip_stale`). It ends a wait.
 */
const isTipStalenessRefusal = (error: unknown): boolean => {
  if (ogmiosSlotEvidenceUnavailableCause(error)?.reason === "ogmios_tip_stale")
    return true;
  let current: unknown = error;
  for (let depth = 0; depth < 16 && current instanceof Error; depth += 1) {
    if (current instanceof L1LedgerBehindError) return true;
    current = current.cause;
  }
  return false;
};

/**
 * For one-shot commands and operator verbs: waits until the observed ledger
 * tip reaches `validFromMs` by the submit margin, re-reading it every
 * `L1_TIP_REFRESH_MS`, and returns the snapshot that reached it. A failed
 * read the provider classifies as transient is read again on the same
 * cadence, as the L1 clock keeps its last good read through one. The wait
 * ends only at the bound, on the tip source's staleness refusal, or on a
 * failure that is not transient; it has no timeout of its own.
 */
export const awaitLedgerTipAtLowerBound = (
  lucid: LucidEvolution,
  validFromMs: number | bigint,
  callerLabel: string,
): Effect.Effect<SubmitSlotSnapshot, unknown> =>
  Effect.gen(function* () {
    const sleep = submitRecoverySleep(lucid);
    let logged = false;
    let readFailing = false;
    for (;;) {
      const read = yield* Effect.either(
        lowerBoundAtLedgerTip(lucid, validFromMs, callerLabel),
      );
      if (read._tag === "Left") {
        if (
          !isRetryableProviderError(read.left) ||
          isTipStalenessRefusal(read.left)
        )
          return yield* Effect.fail(read.left);
        if (!readFailing) {
          readFailing = true;
          yield* Effect.logWarning(
            `${callerLabel}: the L1 ledger tip read failed; reading it again every ${L1_TIP_REFRESH_MS.toString()}ms: ${formatUnknownError(read.left)}`,
          );
        }
        yield* sleep(L1_TIP_REFRESH_MS);
        continue;
      }
      readFailing = false;
      const judged = read.right;
      if (judged.status === "ready") return judged.snapshot;
      if (!logged) {
        logged = true;
        yield* Effect.logInfo(
          `${callerLabel}: waiting for the L1 ledger tip to reach the lower bound (tip_slot=${judged.currentSlot.toString()},lower_bound_slot=${judged.invalidBeforeSlot.toString()},due_slot=${judged.dueSlot.toString()})`,
        );
      }
      yield* sleep(L1_TIP_REFRESH_MS);
    }
  });
