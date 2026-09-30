import { randomUUID } from "node:crypto";

import { Duration, Effect, Option, Ref } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { canonicalSlotConfigForLucid } from "../lucid-time.js";
import { Database, Globals, Lucid, NodeConfig } from "../services/index.js";
import { WorkerError } from "../workers/utils/common.js";
import { nativeMpfWorkerInput } from "./native-mpf-worker-input.js";
import {
  acquirePipelinePhase,
  hasActiveSpeculativeCommitSession,
  releasePipelinePhase,
  spawnSpeculativeSession,
} from "./speculative-commit-builder.apply-speculative-submission-output.js";
import { speculativeBuildDuration } from "./speculative-commit-builder.persist-authenticated-foreign-tip-mismatch.js";
import { invalidateSpeculativeCommitCandidate } from "./speculative-commit-builder.spawn-speculative-session-with-worker.js";
import {
  barrierWatermarksAreFresh,
  reduceSpeculativeCommitState,
  type SpeculativeCommitState,
} from "./speculative-commit-state.js";

export const runSpeculativeCommitBuilderOnce = (
  requestedBaseHeaderHash: string,
): Effect.Effect<
  void,
  WorkerError | DatabaseError,
  Globals | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    const lucid = yield* Lucid;
    if (
      !config.SPECULATIVE_COMMIT_BUILD ||
      hasActiveSpeculativeCommitSession()
    ) {
      return;
    }
    const state = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
    if (
      (state._tag !== "Building" && state._tag !== "Invalidated") ||
      state.baseHeaderHash !== requestedBaseHeaderHash
    ) {
      return;
    }
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T5");
      return;
    }
    const watermarks = yield* Ref.get(globals.USER_EVENT_BARRIER_WATERMARKS);
    if (
      !barrierWatermarksAreFresh({
        watermarks,
        nowMs: Date.now(),
        maxStalenessMs: config.USER_EVENT_BARRIER_MAX_STALENESS_MS,
      })
    ) {
      yield* Effect.logWarning(
        `Speculative commit build deferred because user-event barriers are stale base_header_hash=${requestedBaseHeaderHash}`,
      );
      return;
    }
    const acquired = yield* acquirePipelinePhase(globals, "speculative_build");
    if (!acquired) {
      return;
    }
    const startedAtMs = Date.now();
    yield* Ref.set(globals.SPECULATIVE_COMMIT_SESSION_ACTIVE, true);
    if (state._tag === "Invalidated") {
      yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
        reduceSpeculativeCommitState(
          current,
          { _tag: "RebuildStarted", atMs: startedAtMs },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
      );
    }
    const program = Effect.gen(function* () {
      const pending = yield* PendingBlockFinalizationsDB.retrieveActive();
      if (Option.isNone(pending)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Cannot speculate without an active submitted journal",
            cause: requestedBaseHeaderHash,
          }),
        );
      }
      const record = pending.value;
      const journalHeaderHash =
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
      const submittedTxHash =
        record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        );
      if (
        journalHeaderHash !== requestedBaseHeaderHash ||
        submittedTxHash === undefined
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Active journal does not match the speculative base",
            cause: `requested=${requestedBaseHeaderHash},journal=${journalHeaderHash},submitted=${submittedTxHash ?? "missing"}`,
          }),
        );
      }
      const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
      if (nativeMpfOwner === undefined) {
        return yield* Effect.fail(
          new WorkerError({
            worker: "speculative-commit-builder",
            message: "Architecture G native owner is not initialized",
            cause: requestedBaseHeaderHash,
          }),
        );
      }
      const nativeMpfInput = yield* nativeMpfWorkerInput(
        nativeMpfOwner,
        "speculative-commit-builder",
        config.MPF_NATIVE_OWNER_BINARY_SHA256,
      );
      const candidate = yield* spawnSpeculativeSession(globals, config, {
        nativeMpf: nativeMpfInput,
        data: {
          availableConfirmedBlock: "",
          availableLocalFinalizationBlock: "",
          currentBlockStartTimeMs:
            record[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime(),
          forcedValidationSlotConfig: canonicalSlotConfigForLucid(lucid.api),
          ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
          localFinalizationPending: false,
          mempoolTxsCountSoFar: 0,
          sizeOfProcessedTxsSoFar: 0,
          baseSnapshotId: `speculative:${journalHeaderHash}`,
          stateQueueHasUnmergedTail: true,
          speculativeBuild: {
            base: {
              headerHash: journalHeaderHash,
              utxosRoot:
                record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
              blockEndTimeMs:
                record[
                  PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
                ].getTime(),
              submittedTxHash,
            },
            watermarks,
            excludedMempoolTxIds: record.mempoolTxIds.map((txId) =>
              txId.toString("hex"),
            ),
            excludedDepositEventIds: record.depositEventIds.map((eventId) =>
              eventId.toString("hex"),
            ),
            excludedForcedTransactionEventIds:
              record.forcedTransactionEventIds.map((eventId) =>
                eventId.toString("hex"),
              ),
            excludedWithdrawalEventIds: record.withdrawalEventIds.map(
              (eventId) => eventId.toString("hex"),
            ),
          },
        },
      });
      yield* Ref.update(globals.SPECULATIVE_COMMIT_STATE, (current) =>
        reduceSpeculativeCommitState(
          current,
          { _tag: "CandidateReady", candidate },
          config.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
        ),
      );
      yield* speculativeBuildDuration(
        Effect.succeed(Duration.millis(Date.now() - startedAtMs)),
      );
      yield* Effect.logInfo(
        `pipeline_trace phase=candidate_ready candidate_id=${candidate.candidateId} base_header_hash=${candidate.baseHeaderHash}`,
      );
    });
    yield* program.pipe(
      Effect.catchAll((error) =>
        invalidateSpeculativeCommitCandidate(globals, config, "T7").pipe(
          Effect.zipRight(Effect.fail(error)),
        ),
      ),
      Effect.ensuring(releasePipelinePhase(globals)),
    );
  });

export const waitForCandidate = (
  globals: Globals,
  confirmedHeaderHash: string,
): Effect.Effect<SpeculativeCommitState> =>
  Effect.gen(function* () {
    const deadline = Date.now() + 12_000;
    while (Date.now() < deadline) {
      const state = yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
      if (state._tag !== "Building") return state;
      if (state.baseHeaderHash !== confirmedHeaderHash) return state;
      yield* Effect.sleep("50 millis");
    }
    return yield* Ref.get(globals.SPECULATIVE_COMMIT_STATE);
  });
