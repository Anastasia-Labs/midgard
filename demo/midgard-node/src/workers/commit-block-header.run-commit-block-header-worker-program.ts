import { Cause, Effect } from "effect";
import { parentPort, workerData } from "worker_threads";

import {
  MpfEngineStateDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { MidgardMpf, type NativeMpfBuildContext } from "../mpf/index.js";
import { HistoryProducer } from "../services/event-history-producer.js";
import {
  ContractDeploymentIdentity,
  Database,
  MidgardContracts,
  NativeMpfWorkerPortClient,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { findUndecidedBatchMember } from "../services/working-ledger-recompute.js";
import {
  type NotifyCommitWorkerParent,
  shouldPreserveCommitMpfRoots,
} from "./commit-block-header.commit-explicit-block-header-program.js";
import { databaseOperationsProgram } from "./commit-block-header.database-operations-program.js";
import {
  type CommitLucidFactory,
  CommitWorkerInvariantError,
  defaultCommitLucidFactory,
  provideCommitBlockWorkerServices,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
import {
  COMMIT_STAGE_BATCH_UNDECIDED,
  WorkerInput,
  WorkerOutput,
} from "./utils/commit-block-header.js";

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
  notifyParent?: NotifyCommitWorkerParent,
  commitLucidFactory: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  WorkerOutput,
  unknown,
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | NodeConfig
  | IntentJournal
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const stateQueueLeaseToken = workerInput.data.stateQueueLeaseToken;
    if (stateQueueLeaseToken !== undefined) {
      // Production lock order is state-queue mutation lease, then MPF store
      // lease. The parent owns/renews the former while this worker runs.
      yield* StateQueueMutationLeasesDB.revalidate(stateQueueLeaseToken);
    }
    const leaseOwner = workerInput.data.ledgerStoreLeaseOwner;
    if (
      !/^(?:node-)?commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/.test(
        leaseOwner,
      )
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Commit worker requires a unique parent-generated ledger MPF lease owner",
        }),
      );
    }
    const leasedResult = yield* MpfEngineStateDB.tryWithLedgerStoreLease(
      leaseOwner,
      (activeLeaseOwner) =>
        Effect.gen(function* () {
          const nativeMpfClient =
            workerInput.nativeMpf === undefined
              ? undefined
              : new NativeMpfWorkerPortClient(workerInput.nativeMpf.port);
          const nativeMpfState: {
            context?: NativeMpfBuildContext;
            preserve: boolean;
          } = { preserve: false };
          const transactionsMpf = yield* MidgardMpf.createScratch(
            "architecture-g-transactions",
            { mode: "overlay" },
          );
          const closeMpfs = transactionsMpf
            .close()
            .pipe(Effect.catchAll(() => Effect.void));

          const runProgram = () =>
            databaseOperationsProgram(
              workerInput,
              transactionsMpf,
              notifyParent,
              nativeMpfClient,
              nativeMpfState,
              commitLucidFactory,
            ).pipe(
              Effect.tap((output) =>
                shouldPreserveCommitMpfRoots(output)
                  ? Effect.gen(function* () {
                      if (stateQueueLeaseToken !== undefined) {
                        yield* StateQueueMutationLeasesDB.revalidate(
                          stateQueueLeaseToken,
                        );
                      }
                      yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                        activeLeaseOwner,
                      );
                    })
                  : Effect.void,
              ),
            );
          const program = Effect.gen(function* () {
            yield* transactionsMpf.beginBlockOverlay();
            const result = yield* Effect.either(runProgram());
            yield* transactionsMpf.discardBlockOverlayIfActive();
            if (result._tag === "Left") {
              return yield* Effect.fail(result.left);
            }
            return result.right;
          });
          const finalizeNativeGenerationLease = Effect.suspend(() => {
            const context = nativeMpfState.context;
            if (context === undefined || nativeMpfClient === undefined) {
              return Effect.void;
            }
            if (nativeMpfState.preserve) {
              return Effect.promise(() =>
                nativeMpfClient.retainForJournal(context.handle),
              ).pipe(Effect.orDie);
            }
            return Effect.promise(() =>
              nativeMpfClient.discard(context.handle),
            ).pipe(Effect.catchAll(() => Effect.void));
          });
          return yield* program.pipe(
            Effect.ensuring(finalizeNativeGenerationLease),
            Effect.ensuring(closeMpfs),
            Effect.ensuring(Effect.sync(() => nativeMpfClient?.close())),
          );
        }),
    );
    if (leasedResult._tag === "Busy") {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message: "Ledger MPF store is busy with an audit or another commit",
        }),
      );
    }
    const result = leasedResult.value;
    if (result === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Block commitment worker completed without producing a worker output",
        }),
      );
    }
    return result;
  }).pipe(
    workerInput.history === undefined
      ? (effect) => effect
      : Effect.provideService(HistoryProducer, workerInput.history),
  );

/** The worker's outgoing failure boundary, shared with in-process
 * acceptance. A failure that carries an `UndecidedBatchMember` names
 * `commit_stage_batch_undecided`. */
export const captureCommitWorkerFailure = <E, R>(
  program: Effect.Effect<WorkerOutput, E, R>,
) =>
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailureOutput",
        error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
        ...(findUndecidedBatchMember(cause) === undefined
          ? {}
          : { reason: COMMIT_STAGE_BATCH_UNDECIDED }),
      } satisfies WorkerOutput),
    ),
  );

if (parentPort !== null) {
  const workerParentPort = parentPort;
  const inputData = workerData as WorkerInput;

  const notifyParent: NotifyCommitWorkerParent = (message) =>
    Effect.sync(() => workerParentPort.postMessage(message));

  const program = provideCommitBlockWorkerServices(
    runCommitBlockHeaderWorkerProgram(inputData, notifyParent),
  );

  void Effect.runPromise(captureCommitWorkerFailure(program)).then((output) => {
    Effect.runSync(
      Effect.logInfo(
        `👷 Block commitment work completed (${JSON.stringify(output)}).`,
      ),
    );
    parentPort?.postMessage(output);
  });
}
