/**
 * Intent journals (§8.2) for tests whose subject is not the journal:
 *
 * - `withoutFollowerJournal` provides the journal of a process with no
 *   follower, as the CLI commands and protocol init run: every submission
 *   goes out unjournaled. It keeps each journaled intent it is handed, so
 *   a real flow's intents can be replayed onto a follower
 *   (`replayJournaledOnFollower`, `helpers/intent-journal-replay.ts`).
 * - `TEST_INTENT` is the intent a submission-mechanics test passes, under
 *   `TEST_PLAN`, the plan of a view at generation 0.
 * - `recordingIntentJournal` remembers what it was asked to record, in
 *   order, records nothing, and runs the caller's gate with an insert that
 *   writes nothing.
 */
import { Effect, Layer } from "effect";

import {
  IntentJournal,
  IntentJournalWithoutFollower,
  type IntentPlan,
  journaledIntent,
  type RecordOutcome,
  type RecordPurpose,
  type SubmissionIntent,
} from "../../src/services/intent-journal.js";

export const TEST_PLAN: IntentPlan = { kind: "view", generation: 0 };

export const TEST_INTENT: SubmissionIntent = journaledIntent(
  "commit",
  "test",
  TEST_PLAN,
);

/** A record whose bytes S6 sends, not the caller. */
export const RECORD_ONLY: RecordPurpose = { kind: "record_only" };

/**
 * A send whose follower view is never behind wall-clock time: its slot clock
 * reads every slot as now, so the node-behind hold never applies.
 */
export const SEND_AT_TIP: RecordPurpose = {
  kind: "send",
  slotTime: () => Date.now(),
};

export type RecordedIntent = Readonly<{
  intent: SubmissionIntent;
  signedTxCbor: string;
  txHash: string;
}>;

const journaledWithoutFollower: RecordedIntent[] = [];

/** The journaled intents `withoutFollowerJournal` was handed since the last drain, in order. */
export const drainJournaledWithoutFollower = (): RecordedIntent[] =>
  journaledWithoutFollower.splice(0);

const noFollower = Effect.runSync(
  Effect.provide(IntentJournal, IntentJournalWithoutFollower),
);

const keepingJournaled = Layer.succeed(IntentJournal, {
  ...noFollower,
  record: (intent, signedTxCbor, txHash, purpose, gate) =>
    Effect.suspend(() => {
      if (intent.kind === "journaled")
        journaledWithoutFollower.push({ intent, signedTxCbor, txHash });
      return noFollower.record(intent, signedTxCbor, txHash, purpose, gate);
    }),
});

export const withoutFollowerJournal = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E, Exclude<R, IntentJournal>> =>
  Effect.provide(effect, keepingJournaled);

export const recordingIntentJournal = () => {
  const recorded: RecordedIntent[] = [];
  const layer = Layer.succeed(IntentJournal, {
    openPlan: Effect.succeed(TEST_PLAN),
    record: (intent, signedTxCbor, txHash, _purpose, gate) =>
      Effect.sync((): RecordOutcome => {
        recorded.push({ intent, signedTxCbor, txHash });
        return { kind: "recorded" };
      }).pipe(
        // No follower here: the gate runs with an insert that writes nothing.
        Effect.zipLeft(gate === undefined ? Effect.void : gate(Effect.void)),
      ),
    holds: () => [],
    handOff: () => [],
    adopt: () => undefined,
    refresh: () => Effect.void,
    walletView: noFollower.walletView,
  });
  return { recorded, layer };
};

/** `Effect.runPromise` under the journal of a process with no follower. */
export const runWithoutFollower = <A, E>(
  effect: Effect.Effect<A, E, IntentJournal>,
): Promise<A> => Effect.runPromise(withoutFollowerJournal(effect));
