/**
 * The merge wallet's view and the inputs a merge is funded from. A view that
 * cannot be read, or holds nothing to fund the merge, is a merge failure:
 * both count on `mergeFailureCounter`, which `MidgardMergeFailures` watches.
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";

import type { IntentJournal } from "../../services/intent-journal.js";
import type { NodeWalletView } from "../../services/intent-journal.wallet-view.js";
import { readSelectedWalletView } from "../utils.wallet-view.js";
import { mergeFailureCounter } from "./merge-to-confirmed-state.landed-unfinalized-merges.js";

/** The view of the merge wallet selected on `lucid`. */
export const readMergeWalletView = (
  lucid: LucidEvolution,
): Effect.Effect<NodeWalletView, SDK.StateQueueError, IntentJournal> =>
  readSelectedWalletView(lucid).pipe(
    Effect.mapError(
      (cause) =>
        new SDK.StateQueueError({
          message: `Failed to read the merge wallet view: ${cause.message}`,
          cause,
        }),
    ),
    Effect.tapError(() => Metric.increment(mergeFailureCounter)),
  );

/** The merge wallet view's UTxOs as the merge's preset wallet inputs. */
export const requireMergeWalletInputs = (
  utxos: readonly UTxO[],
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  SDK.requireOperatorWalletInputs(utxos, "state_queue merge tx").pipe(
    Effect.tapError(() => Metric.increment(mergeFailureCounter)),
  );
