import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { Database, Globals } from "../../services/index.js";
import {
  finalizeConfirmedMergeProgram,
  landedMergeOf,
} from "./merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import { mergeLocalFinalizationFailureCounter } from "./merge-to-confirmed-state.landed-unfinalized-merges.js";

/**
 * The local finalization of a merge this node submitted, once L1 confirmed
 * it. Merging is permissionless, so the block may be another operator's: its
 * header names another operator key and this node holds no journal for it.
 * Such a block has nothing to finalize here; landed-block processing folds
 * it into `confirmed_ledger` in queue order, as it folds every merged block
 * (`landed-blocks/fold.ts`). Every other block, including one of this
 * node's own whose journal is missing, runs the confirmed-merge
 * finalization (finalizeConfirmedMergeProgram). A failure counts in
 * `merge_local_finalization_failure_count` and is logged; the next merge
 * attempt's landed-merge catch-up retries it.
 */
export const finalizeSubmittedMerge = ({
  headerHash,
  header,
  txIds,
  ownOperatorKeyHash,
}: {
  readonly headerHash: Buffer;
  readonly header: SDK.Header;
  /** The merged block's transaction ids, for the failure log. */
  readonly txIds: readonly Buffer[];
  /** This node's operator key hash, which its own headers carry. */
  readonly ownOperatorKeyHash: string;
}): Effect.Effect<void, DatabaseError, Database | Globals> =>
  Effect.gen(function* () {
    if (header.operatorVkey !== ownOperatorKeyHash) {
      const journal =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
      if (Option.isNone(journal)) {
        yield* Effect.logInfo(
          `🔸 Merged another operator's block (header=${headerHash.toString(
            "hex",
          )},operator=${header.operatorVkey}); this node holds no journal for it, so landed-block processing folds it into the confirmed ledger.`,
        );
        return;
      }
    }
    yield* finalizeConfirmedMergeProgram(landedMergeOf(headerHash, header));
  }).pipe(
    Effect.tapError((error) =>
      Effect.gen(function* () {
        yield* Metric.increment(mergeLocalFinalizationFailureCounter);
        yield* Effect.logError(
          `🔸 Merge local finalization failed after on-chain submit (header=${headerHash.toString(
            "hex",
          )},tx_count=${txIds.length},sample_tx_ids=${JSON.stringify(
            txIds.slice(0, 10).map((txId) => txId.toString("hex")),
          )},error=${formatUnknownError(error)})`,
        );
      }),
    ),
  );
