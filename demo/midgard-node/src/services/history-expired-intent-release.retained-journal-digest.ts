import { Data, Effect } from "effect";

import { retainedPreparedRecoveryPlan } from "../database/eventHistoryRecoveryPlans.js";
import type * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { journalIdentityBeforeSubmissionAck } from "./history-expired-intent-release.signed-commit-node.js";
import { C } from "./history-expired-intent-release.table.js";

/** This intent's retained replacement plan binds a journal other than the
 * active one, beyond the one evolution an honest node makes (see
 * `retainedJournalDigest`). Its native CAS may already have run, so neither
 * resuming nor discarding it is safe: it is held (see
 * `heldOnIntegrityFailure`), with nothing written. */
export class RetainedReplacementPlanChanged extends Data.TaggedError(
  "RetainedReplacementPlanChanged",
)<{ readonly message: string }> {}

/** The native durable root is not where this intent's release plan moves it
 * from or to (see `prepareRetainedNativeHistoryRecoveryPlan`): outside the
 * journal's base and candidate roots, or, under a retained plan, not where
 * that plan's CAS left it or moves it from. The plan never executes against
 * it, and a restart reads the same native store again, so it is held (see
 * `heldOnIntegrityFailure`), with nothing written, until a later evaluation
 * reads a durable root the plan accepts. */
export class NativeRecoveryRootRefused extends Data.TaggedError(
  "NativeRecoveryRootRefused",
)<{ readonly message: string }> {}

/** The plan preparation's refusals of the observed native durable root. */
const ROOT_REFUSALS: readonly string[] = [
  "Native recovery root is outside the authenticated journal",
];

/** A refusal of the observed durable root `durableRoot` by the release plan
 * of block `header` (base `targetRoot`, candidate `candidateRoot`) as the
 * held `NativeRecoveryRootRefused`; any other failure is returned as is. */
export const heldRootRefusal =
  (
    header: string,
    native: Readonly<{
      durableRoot: string;
      targetRoot: string;
      candidateRoot: string;
    }>,
  ) =>
  <E>(
    error: E,
  ): E | NativeRecoveryRootRefused | RetainedReplacementPlanChanged => {
    if (!(error instanceof DatabaseError)) return error;
    const refusal =
      error.cause instanceof Object && "nativeRecoveryRefusal" in error.cause
        ? error.cause.nativeRecoveryRefusal
        : undefined;
    if (refusal === "identity")
      return new RetainedReplacementPlanChanged({
        message: `${error.message}: the retained recovery identity for block ${header} changed; no SQL repair is authorized by that plan.`,
      });
    return ROOT_REFUSALS.includes(error.message) || refusal === "root"
      ? new NativeRecoveryRootRefused({
          message: `${error.message}: the native durable root ${native.durableRoot} is not where the release plan of block ${header} (base root ${native.targetRoot}, candidate root ${native.candidateRoot}) moves it from or to; the plan does not execute while it is.`,
        })
      : error;
  };

/**
 * The journal digest the release plan of `record` (re-derived in the caller's
 * recovery transaction, with identity `identity`) binds. With no retained
 * plan for it, its identity. With one, the digest that plan was prepared
 * with, which must be this journal's: either its identity, or its identity
 * before its submission was acknowledged. The acknowledgement (setting the
 * submitted hash to the signed hash, the only value it is ever set to) can
 * land between the plan's native CAS and its SQL repair; it changes nothing
 * the plan restores (the roots, the signed bytes and the lease are the
 * same), so the plan is resumed under the digest it was prepared with
 * rather than refused for good. Anything else is
 * `RetainedReplacementPlanChanged`.
 */
export const retainedJournalDigest = (
  bindingDigest: string,
  record: Pending.Record,
  identity: string,
) =>
  Effect.gen(function* () {
    const retained = yield* retainedPreparedRecoveryPlan(bindingDigest);
    const header = record[C.HEADER_HASH].toString("hex");
    if (
      retained === undefined ||
      retained.kind !== "signed_intent_release" ||
      retained.headerHash !== header ||
      retained.journalDigest === identity
    )
      // Another plan is refused by the plan's own preparation.
      return identity;
    const unacknowledged = journalIdentityBeforeSubmissionAck(record);
    if (
      unacknowledged !== undefined &&
      retained.journalDigest === unacknowledged
    )
      return unacknowledged;
    return yield* Effect.fail(
      new RetainedReplacementPlanChanged({
        message: `The retained replacement plan of block ${header} binds journal ${retained.journalDigest ?? "(none)"}, not the active journal ${identity} or that journal before its submission was acknowledged; its native restoration may already have run, so it is neither resumed nor discarded.`,
      }),
    );
  });
