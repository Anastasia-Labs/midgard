import { Effect, Option, Ref } from "effect";

import * as HistoryAuthority from "../database/eventHistoryAuthority.js";
import {
  HistoryProducer,
  runHistoryProducer,
} from "../services/event-history-producer.js";
import {
  applyForeignBaseVerificationOutcome,
  foreignBaseVerificationForAuthority,
  type ForeignBaseVerificationOutcome,
} from "../services/foreign-base-verification.js";
import { Globals, MidgardContracts } from "../services/index.js";
import { landedStateQueueSnapshot } from "../services/landed-state-queue.js";
import { verifyForeignCommitBase } from "../workers/commit-block-header.verify-foreign-base.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import {
  beginCommitForeignVerification,
  notifyForeignNativeAdoptionRequested,
  prepareForeignBaseForCommitment,
} from "./block-commitment.prepare-foreign-base.js";

/**
 * `/readyz` holds a node unready until its current Ready history authority has
 * verified foreign-base evidence, and only a commitment tick produces it. A
 * tick with no tx or user-event work stops before the commit worker, so an
 * idle node (a fresh deployment before any deposit, or any node after a
 * restart or a new history generation) would never become ready.
 *
 * On such a tick this runs the commit path's own base check against the live
 * state-queue tail: the parent's foreign-tip preflight and adoption request,
 * then the worker's `verifyForeignCommitBase`, recorded through the same
 * begin/apply transitions. Its outcome is evidence only; it never relaxes the
 * readiness rule. It takes no L1 control plane, no state-queue mutation lease
 * and no worker, and holds the history producer permit the check itself
 * requires only while the check runs. Evidence already verified for the
 * current authority makes it a no-op, so steady idle ticks add no L1 queries;
 * held or failed evidence is retried on the next scheduled tick, exactly as a
 * commit tick would retry it.
 */
export const verifyForeignBaseOnIdleTick = Effect.gen(function* () {
  const globals = yield* Globals;
  // Set by this tick's idle preflight; false whenever local finalization is
  // pending, which the commit path itself resolves.
  if (!(yield* Ref.get(globals.COMMIT_PIPELINE_IDLE))) return;
  // Without both owners the check cannot run, and the node stays unready on
  // its unverified base until startup installs them.
  const owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
  if (
    owner === undefined ||
    (yield* Ref.get(globals.EVENT_HISTORY_OWNER)) === undefined
  )
    return;
  const authority = yield* HistoryAuthority.retrieve;
  if (Option.isNone(authority) || authority.value.state !== "ready") return;
  const evidence = foreignBaseVerificationForAuthority(
    yield* Ref.get(globals.FOREIGN_BASE_VERIFICATION),
    HistoryAuthority.tokenFromRow(authority.value),
  );
  if (evidence.status === "verified") return;
  let adoptionRequested = false;
  yield* Effect.gen(function* () {
    const history = yield* HistoryProducer;
    const contracts = yield* MidgardContracts;
    const snapshot = yield* landedStateQueueSnapshot(
      contracts.stateQueue,
      "commit_preflight",
    );
    const scope = yield* beginCommitForeignVerification(globals, {
      ...history.token,
      baseHeaderHash: snapshot.tailCommitBase.headerHash,
    });
    const held = yield* prepareForeignBaseForCommitment({
      localFinalizationPending: false,
      availableConfirmedBlock: snapshot.tailCommitBase.utxo,
      owner,
      globals,
      scope,
    });
    if (held !== undefined) {
      adoptionRequested = held.adoptionRequested;
      return;
    }
    const outcome: ForeignBaseVerificationOutcome =
      yield* deserializeStateQueueUTxO(snapshot.tailCommitBase.utxo).pipe(
        Effect.flatMap((tail) => verifyForeignCommitBase(tail)),
        Effect.map((base) => base.verification),
        // The commit worker's mapping of the same refusal.
        Effect.catchTag("ForeignBlockVerificationError", (error) =>
          Effect.succeed({
            status:
              error.reason === "missing"
                ? ("missing" as const)
                : ("refused" as const),
            foreignHeaderHash: error.foreignHeaderHash,
            reason: error.detail,
          }),
        ),
      );
    yield* Ref.update(globals.FOREIGN_BASE_VERIFICATION, (current) =>
      applyForeignBaseVerificationOutcome(current, scope, outcome),
    );
  }).pipe(
    runHistoryProducer,
    Effect.tap(() => notifyForeignNativeAdoptionRequested(adoptionRequested)),
  );
});
