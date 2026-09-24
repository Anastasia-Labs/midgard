import { Effect } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  applyHistoryRecoveryPlan,
  type HistoryRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import { DatabaseError } from "../database/utils/common.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import type { NativeMpfOwnerService } from "./mpf-native-owner/protocol.js";

/** Production ordering for a source-authorized dependent rollback. The plan was
 * committed under recovery authority before this call; native mutation holds no
 * SQL transaction. SQL disposition and its applied receipt are one transaction.
 * Cache reload and Ready remain exclusively the source owner's responsibility.
 * Repeating after interruption uses the retained native operation ID, including
 * when its marker changed but its acknowledgement or SQL transaction was lost.
 */
export const executeHistoryDependentRecovery = <E, R>(input: {
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly plan: HistoryRecoveryPlan;
  readonly owner: NativeMpfOwnerService;
  /** Must recheck exact journal/preimages as part of this bounded SQL work. */
  readonly repair: Effect.Effect<void, E, R>;
  /** Bounded in-memory publication after SQL COMMIT, before cancellation can
   * return control. No IO or fallible work; process restart rebuilds these refs. */
  readonly afterSqlCommit: Effect.Effect<void>;
}) =>
  Effect.gen(function* () {
    yield* input.preparation.assertCurrent;
    yield* Effect.tryPromise({
      try: () => input.owner.restoreCanonicalRoot(input.plan.native),
      catch: (cause) =>
        new DatabaseError({
          table: "event_history_recovery_plans",
          message: "Native dependent rollback requires resumable recovery",
          cause,
        }),
    });
    yield* input.preparation.assertCurrent;
    yield* Effect.uninterruptible(
      Authority.withRecovery(
        input.preparation.token,
        input.preparation.assertCurrent.pipe(
          Effect.zipRight(
            applyHistoryRecoveryPlan(
              input.checkpoint,
              input.plan,
              input.repair,
            ),
          ),
          Effect.tap(() => input.preparation.assertCurrent),
        ),
      ).pipe(Effect.zipRight(input.afterSqlCommit)),
    );
    yield* input.preparation.assertCurrent;
  });
