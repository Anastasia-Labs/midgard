import "./listen-startup.ensure-protocol-initialized-on-startup.js";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Option, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { setUnresolvedBlockSubmission } from "../fibers/unresolved-block-submission.js";
import { Globals } from "../services/globals.globals.js";
import {
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";

/**
 * The local block boundary startup seeds: the latest of the state-queue
 * tip's end time and the end time of the tip's journal, when this node
 * built the tip.
 */
const seededLocalBlockBoundary = (
  latestHeaderHash: Option.Option<Buffer>,
  latestEndTimeMs: number,
) =>
  Effect.gen(function* () {
    if (Option.isNone(latestHeaderHash)) return latestEndTimeMs;
    const latestJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        latestHeaderHash.value,
      );
    if (Option.isNone(latestJournal)) return latestEndTimeMs;
    const seededBoundaryMs = Math.max(
      latestJournal.value[
        PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
      ].getTime(),
      latestEndTimeMs,
    );
    yield* Effect.logInfo(
      `Seeded latest local block boundary from pending-finalization journal for header ${latestHeaderHash.value.toString("hex")}: ${new Date(seededBoundaryMs).toISOString()}`,
    );
    return seededBoundaryMs;
  });

/**
 * Seeds the in-memory local block-boundary cache from the current state-queue
 * tip during startup.
 */
export const seedLatestLocalBlockBoundaryOnStartup = Effect.gen(function* () {
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

  const snapshot = yield* landedStateQueueSnapshot(
    contracts.stateQueue,
    "startup",
  );
  yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
  const latestBlock = yield* deserializeStateQueueUTxO(
    snapshot.tailCommitBase.utxo,
  );
  const latestEndTimeMs = snapshot.tailCommitBase.blockEndTimeMs;
  yield* Effect.logInfo(
    `Startup state-queue snapshot hydrated: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
  );
  const latestHeaderHash =
    latestBlock.datum.key === "Empty"
      ? Option.none<Buffer>()
      : Option.some(
          Buffer.from(
            yield* SDK.hashBlockHeader(
              yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
            ),
            "hex",
          ),
        );
  const seededBoundaryMs = yield* seededLocalBlockBoundary(
    latestHeaderHash,
    latestEndTimeMs,
  );
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, seededBoundaryMs);
  yield* Effect.logInfo(
    `Seeded latest local block boundary from startup state: ${new Date(seededBoundaryMs).toISOString()}`,
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed latest local block boundary on startup: ${formatUnknownError(e)}`,
    ),
  ),
);

export const hydratePendingBlockFinalizationOnStartup = Effect.gen(
  function* () {
    const globals = yield* Globals;
    yield* PendingBlockFinalizationsDB.assertLiveJournalPayloadsComplete;
    const live = yield* PendingBlockFinalizationsDB.retrieveLive();
    if (Option.isNone(live)) {
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      return;
    }

    const record = live.value;
    const intendedTxHash =
      record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH];
    yield* setUnresolvedBlockSubmission(globals, record);
    yield* Effect.logInfo(
      `Hydrated the live block journal on startup for header ${record[
        PendingBlockFinalizationsDB.Columns.HEADER_HASH
      ].toString("hex")} (intended_tx=${
        intendedTxHash == null ? "unsigned" : intendedTxHash.toString("hex")
      }).`,
    );
  },
).pipe(
  Effect.tapError((error) =>
    Effect.logError(
      `Failed to hydrate pending block-finalization journal on startup: ${formatUnknownError(error)}`,
    ),
  ),
  Effect.orDie,
);
