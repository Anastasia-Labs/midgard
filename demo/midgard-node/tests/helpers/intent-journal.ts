/**
 * Intent journals (§8.2) for tests whose subject is not the journal:
 *
 * - `withoutFollowerJournal` provides the journal of a process with no
 *   follower, as the CLI commands and protocol init run: every submission
 *   goes out unjournaled.
 * - `TEST_INTENT` is the intent a submission-mechanics test passes.
 * - `recordingIntentJournal` remembers what it was asked to record, in
 *   order, and records nothing.
 */
import { Effect, Layer } from "effect";

import {
  IntentJournal,
  IntentJournalWithoutFollower,
  journaledIntent,
  type RecordOutcome,
  type SubmissionIntent,
} from "../../src/services/intent-journal.js";

export const TEST_INTENT: SubmissionIntent = journaledIntent("commit", "test");

export const withoutFollowerJournal = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E, Exclude<R, IntentJournal>> =>
  Effect.provide(effect, IntentJournalWithoutFollower);

export type RecordedIntent = Readonly<{
  intent: SubmissionIntent;
  signedTxCbor: string;
  txHash: string;
}>;

export const recordingIntentJournal = () => {
  const recorded: RecordedIntent[] = [];
  const layer = Layer.succeed(IntentJournal, {
    record: (intent, signedTxCbor, txHash) =>
      Effect.sync((): RecordOutcome => {
        recorded.push({ intent, signedTxCbor, txHash });
        return { kind: "recorded" };
      }),
    holds: () => [],
  });
  return { recorded, layer };
};

/** `Effect.runPromise` under the journal of a process with no follower. */
export const runWithoutFollower = <A, E>(
  effect: Effect.Effect<A, E, IntentJournal>,
): Promise<A> => Effect.runPromise(withoutFollowerJournal(effect));
