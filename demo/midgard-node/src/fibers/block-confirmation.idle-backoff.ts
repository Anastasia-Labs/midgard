import { Effect, Option, Ref } from "effect";

import type { Globals } from "../services/globals.globals.js";
import {
  idleBackoffActive,
  recordIdleTick,
  resetIdleBackoff,
} from "../services/globals.idle-backoff.js";
import { HaltSource } from "../services/liveness-halt.js";

export const CONFIRMATION_IDLE_BACKOFF_KEY = "block_confirmation";
/** The longest a provably idle confirmation fiber waits between refreshes. */
export const CONFIRMATION_IDLE_BACKOFF_MAX_MS = 30_000;

type ConfirmationIdleGlobals = Pick<
  Globals,
  | "AVAILABLE_CONFIRMED_BLOCK"
  | "UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH"
  | "LOCAL_FINALIZATION_PENDING"
  | "COMMIT_PIPELINE_IDLE"
  | "IDLE_BACKOFF"
  | "LIVENESS_REASONS"
>;

/**
 * The confirmation fiber has nothing to confirm: no pending-finalization
 * journal, no submitted block awaiting its tx, no local finalization to
 * recover, a confirmed tip already published, and a commitment fiber whose
 * last tick found no work. Only then may a refresh be skipped; the commitment
 * takes its own fresh state-queue snapshot before it builds, so a skipped
 * refresh never becomes a commit base. Never while this fiber's own hold
 * (`signed_intent_undecided`) is raised: only a refresh re-derives it, so a
 * held node refreshes at the configured cadence and clears it on the first
 * tick that decides.
 */
export const confirmationProvablyIdle = (
  globals: ConfirmationIdleGlobals,
  pending: Option.Option<unknown>,
): Effect.Effect<boolean> =>
  Effect.gen(function* () {
    if (Option.isSome(pending)) return false;
    if (
      (yield* Ref.get(globals.LIVENESS_REASONS)).has(
        HaltSource.blockConfirmationSignedIntent,
      )
    )
      return false;
    if ((yield* Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK)) === "")
      return false;
    if ((yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH)) !== "")
      return false;
    if (yield* Ref.get(globals.LOCAL_FINALIZATION_PENDING)) return false;
    return yield* Ref.get(globals.COMMIT_PIPELINE_IDLE);
  });

/**
 * True when this tick should be skipped: the fiber is provably idle and still
 * inside its backoff. Any sign of work resets the backoff first.
 */
export const skipIdleConfirmationTick = (
  globals: ConfirmationIdleGlobals,
  pending: Option.Option<unknown>,
): Effect.Effect<boolean> =>
  Effect.gen(function* () {
    if (!(yield* confirmationProvablyIdle(globals, pending))) {
      yield* resetIdleBackoff(globals, CONFIRMATION_IDLE_BACKOFF_KEY);
      return false;
    }
    return yield* idleBackoffActive(globals, CONFIRMATION_IDLE_BACKOFF_KEY);
  });

/**
 * After a refresh: backs off further while still provably idle, otherwise
 * resets, so the next tick runs at the configured cadence.
 */
export const recordConfirmationTickIdleness = (
  globals: ConfirmationIdleGlobals,
  pending: Option.Option<unknown>,
  baseMs: number,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    if (yield* confirmationProvablyIdle(globals, pending)) {
      yield* recordIdleTick(globals, CONFIRMATION_IDLE_BACKOFF_KEY, {
        baseMs,
        maxMs: CONFIRMATION_IDLE_BACKOFF_MAX_MS,
      });
    } else {
      yield* resetIdleBackoff(globals, CONFIRMATION_IDLE_BACKOFF_KEY);
    }
  });
