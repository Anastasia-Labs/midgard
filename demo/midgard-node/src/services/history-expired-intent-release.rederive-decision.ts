import { Effect } from "effect";

import { HistoryRecoverySuperseded } from "./event-history-recovery.js";
import { decide } from "./history-expired-intent-release.decide.js";
import { effective } from "./history-expired-intent-release.open-retained-native-owner.js";
import {
  type Decision,
  journalIdentity,
  journalIdentityBeforeSubmissionAck,
  type ReleaseEvidence,
  replaceableJournal,
} from "./history-expired-intent-release.signed-commit-node.js";

/** Re-derives the decision and the unchanged journal inside the caller's
 * transaction. Its inputs were read outside that transaction, so another
 * fiber may have moved the journal since (recorded its submission, or
 * abandoned it) or the correction observer may have changed what it decides.
 * Such a lost race is no fault: the transaction rolls back with nothing
 * written and the owner reconnects and decides again from fresh state, where
 * a journal that really is broken fails the first derivation as before. */
export const rederiveDecision = (input: {
  readonly headerHash: Buffer;
  readonly manifestId: string;
  readonly identity: string;
  readonly evidence: ReleaseEvidence;
  readonly retainedPlan: boolean;
  readonly expected: Decision["kind"];
}) =>
  Effect.gen(function* () {
    const { headerHash, expected } = input;
    const header = headerHash.toString("hex");
    const superseded = (message: string) =>
      Effect.fail(new HistoryRecoverySuperseded({ message }));
    const journal = yield* replaceableJournal(
      headerHash,
      input.manifestId,
    ).pipe(
      Effect.catchAll((error) =>
        superseded(
          `Signed-intent journal ${header} stopped being replaceable since its decision: ${error.message}`,
        ),
      ),
    );
    if (
      journalIdentity(journal.record) !== input.identity &&
      journalIdentityBeforeSubmissionAck(journal.record) !== input.identity
    )
      return yield* superseded(
        `Signed-intent journal ${header} identity changed`,
      );
    const decision = effective(
      yield* decide(journal.record, input.evidence),
      input.retainedPlan,
    );
    if (decision.kind !== expected)
      return yield* superseded(
        `Signed-intent decision for ${header} changed from ${expected} to ${decision.kind}`,
      );
    return { ...journal, decision };
  });
