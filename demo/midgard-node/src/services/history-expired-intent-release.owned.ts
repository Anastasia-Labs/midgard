import { Effect } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";

/** Runs `work` in the preparation's owned recovery transaction, checking the
 * preparation is still current before and after it. */
export const ownedBy =
  (preparation: HistoryRecoveryPreparation) =>
  <A, E, R>(work: Effect.Effect<A, E, R>) =>
    Authority.withRecovery(
      preparation.token,
      preparation.assertCurrent.pipe(
        Effect.zipRight(work),
        Effect.tap(() => preparation.assertCurrent),
      ),
    );
