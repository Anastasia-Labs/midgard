import { Effect, Ref } from "effect";

import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  discardPreparedHistoryRecoveryPlan,
  retainedPreparedRecoveryPlan,
  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
} from "../database/eventHistoryRecoveryPlans.js";
import type * as Pending from "../database/pendingBlockFinalizations.js";
import { recordConfirmedPendingBlock } from "../fibers/block-confirmation.js";
import { serializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import type { Globals } from "./globals.js";
import { persistedReplay } from "./history-expired-intent-release.open-retained-native-owner.js";
import { ownedBy } from "./history-expired-intent-release.owned.js";
import type { rederiveDecision } from "./history-expired-intent-release.rederive-decision.js";
import type { Decision } from "./history-expired-intent-release.signed-commit-node.js";
import {
  C,
  failure,
  reportOnce,
} from "./history-expired-intent-release.table.js";
import type { NativeMpfOwnerService } from "./mpf-native-owner/index.js";

/** The `landed` arm of `prepareExpiredIntentRelease`: the signed commit
 * itself is on the queue, so its journal is recorded confirmed (after any
 * retained plan's native replay) and the retained plan, if any, discarded. */
export const recordLandedRelease = <EO, RO>(input: {
  readonly decision: Extract<Decision, { kind: "landed" }>;
  readonly record: Pending.Record;
  readonly derived: {
    readonly retainedPlan: boolean;
    readonly replayRetained: boolean;
  };
  readonly bindingDigest: string;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly globals: Globals;
  readonly openOwner: Effect.Effect<NativeMpfOwnerService | undefined, EO, RO>;
  readonly current: (
    expected: Decision["kind"],
  ) => ReturnType<typeof rederiveDecision>;
  readonly reportKey: string;
  readonly context: string;
}) =>
  Effect.gen(function* () {
    const {
      decision,
      record,
      derived,
      checkpoint,
      preparation,
      globals,
      openOwner,
      current,
      reportKey,
      context,
    } = input;
    const owned = ownedBy(preparation);
    const header = record[C.HEADER_HASH].toString("hex");
    const serialized = yield* serializeStateQueueUTxO(decision.node.node);
    if (derived.replayRetained) {
      // A replacement prepared from the candidate root before the block was
      // seen to land may already have restored the base root natively (its
      // CAS ran, its SQL repair did not), so the journal is intact but the
      // native root may be at its base. Replay it to the candidate first (a
      // no-op when the CAS never ran): a locally finalized journal is not
      // replayed again at local finalization. The native root stays within
      // the retained plan's two roots, so the plan can still be resumed if
      // this attempt stops before it is discarded. A plan prepared from the
      // base root (a journal never promoted) is base to base: the native
      // root never left the base, local finalization replays the journal,
      // and replaying here would strand the plan outside its roots.
      const owner = yield* openOwner;
      if (owner === undefined) return;
      yield* preparation.assertCurrent;
      yield* Effect.tryPromise({
        try: () => owner.recover(persistedReplay(record.nativeMpfReplay!)),
        catch: (cause) =>
          failure(
            `Native replay of landed block ${header} over its discarded replacement failed`,
            cause,
          ),
      }).pipe(Effect.uninterruptible);
      yield* preparation.assertCurrent;
    }
    const requiresLocalFinalization = yield* owned(
      current("landed").pipe(
        Effect.tap(() =>
          // The native root is where the journal's status says it is again;
          // the replacement is discarded, not resumed. The plan must still
          // be the one the replay choice was made for.
          derived.retainedPlan
            ? retainedPreparedRecoveryPlan(input.bindingDigest)
                .pipe(
                  Effect.flatMap((retained) =>
                    retained?.kind === "signed_intent_release" &&
                    retained.headerHash === header &&
                    (retained.expectedRoot ===
                      record[C.EXPECTED_UTXOS_ROOT]) ===
                      derived.replayRetained
                      ? discardPreparedHistoryRecoveryPlan(
                          checkpoint,
                          SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
                          header,
                        )
                      : Effect.fail(
                          failure(
                            `The retained replacement plan of landed block ${header} changed`,
                          ),
                        ),
                  ),
                )
                .pipe(
                  Effect.zipRight(
                    Effect.logWarning(
                      `Discarded the prepared replacement of block ${header}: it landed.`,
                    ),
                  ),
                )
            : Effect.void,
        ),
        Effect.flatMap((journal) =>
          recordConfirmedPendingBlock(
            journal.record,
            Buffer.from(decision.node.node.utxo.txHash, "hex"),
          ),
        ),
      ),
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
    yield* Ref.set(
      globals.LOCAL_FINALIZATION_PENDING,
      requiresLocalFinalization,
    );
    yield* Ref.set(
      globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      requiresLocalFinalization ? serialized : "",
    );
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logInfo(
      `Recorded the L1 observation of ${context}: ${decision.evidence}.`,
    );
  });
