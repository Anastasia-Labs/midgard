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
import {
  type AwaitSpeculativeCommitInstruction,
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
  type SpeculativeCandidateReadyOutput,
  type SpeculativeCommitWorkerInstruction,
  WorkerInput,
  WorkerOutput,
} from "./utils/commit-block-header.js";

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
  awaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
  notifyParent?: NotifyCommitWorkerParent,
  commitLucidFactory: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
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

          const runProgram = (
            activeAwaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
          ) =>
            databaseOperationsProgram(
              workerInput,
              transactionsMpf,
              activeAwaitSpeculativeInstruction,
              notifyParent,
              activeLeaseOwner,
              transactionsMpf,
              () => ({ transactionsMpf }),
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
            const awaitInstructionWithRetainedNativeHandle =
              workerInput.data.speculativeBuild === undefined ||
              awaitSpeculativeInstruction === undefined
                ? undefined
                : (candidate: SpeculativeCandidateReadyOutput["candidate"]) =>
                    Effect.gen(function* () {
                      yield* MpfEngineStateDB.releaseLedgerStoreLease(
                        activeLeaseOwner,
                      );
                      const instruction =
                        yield* awaitSpeculativeInstruction(candidate);
                      if (instruction.type !== "SubmitSpeculativeCandidate") {
                        return instruction;
                      }
                      yield* StateQueueMutationLeasesDB.revalidate(
                        instruction.stateQueueLeaseToken,
                      );
                      let reacquired = false;
                      for (let attempt = 0; attempt < 200; attempt += 1) {
                        reacquired =
                          yield* MpfEngineStateDB.acquireLedgerStoreLease({
                            owner: activeLeaseOwner,
                            ttlMs: 10 * 60 * 1000,
                          });
                        if (reacquired) break;
                        yield* Effect.sleep("50 millis");
                      }
                      if (!reacquired) {
                        return yield* Effect.fail(
                          new CommitWorkerInvariantError({
                            message:
                              "Timed out reacquiring the logical MPF lease for Architecture G speculative submission",
                          }),
                        );
                      }
                      return instruction;
                    });
            const result = yield* Effect.either(
              runProgram(awaitInstructionWithRetainedNativeHandle),
            );
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

/**
 * Runs the exact production speculative commit-candidate path and stops at the
 * candidate-ready boundary. This is the only benchmark-safe build-only entry:
 * it never advances to source revalidation, journal preparation, signing, or
 * L1 submission, and the ordinary worker cleanup discards the native
 * generation after the typed invalidation instruction.
 */
export const runCommitBlockHeaderCandidateBuildProgram = (
  workerInput: WorkerInput,
  commitLucidFactory: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  SpeculativeCandidateReadyOutput["candidate"],
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    if (workerInput.data.speculativeBuild === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Commit-candidate build-only program requires speculativeBuild input",
        }),
      );
    }
    let captured: SpeculativeCandidateReadyOutput["candidate"] | undefined;
    const output = yield* runCommitBlockHeaderWorkerProgram(
      workerInput,
      (candidate) =>
        Effect.sync(() => {
          captured = candidate;
          return {
            type: "InvalidateSpeculativeCandidate",
            reason: "T1",
          } satisfies SpeculativeCommitWorkerInstruction;
        }),
      undefined,
      commitLucidFactory,
    );
    if (
      captured === undefined ||
      output.type !== "SpeculativeCandidateInvalidatedOutput" ||
      output.candidateId !== captured.candidateId
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message: `Commit-candidate build-only program did not stop at the candidate-ready boundary (output=${output.type})`,
        }),
      );
    }
    return captured;
  });

/** The worker's outgoing failure boundary, shared with in-process acceptance. */
export const captureCommitWorkerFailure = <E, R>(
  program: Effect.Effect<WorkerOutput, E, R>,
) =>
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailureOutput",
        error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
      } satisfies WorkerOutput),
    ),
  );

if (parentPort !== null) {
  const workerParentPort = parentPort;
  const inputData = workerData as WorkerInput;

  const awaitSpeculativeInstruction: AwaitSpeculativeCommitInstruction = (
    candidate,
  ) =>
    Effect.async((resume) => {
      const onInstruction = (
        instruction: SpeculativeCommitWorkerInstruction,
      ) => {
        workerParentPort.off("message", onInstruction);
        resume(Effect.succeed(instruction));
      };
      workerParentPort.on("message", onInstruction);
      workerParentPort.postMessage({
        type: "SpeculativeCandidateReadyOutput",
        candidate,
      } satisfies SpeculativeCandidateReadyOutput);
      return Effect.sync(() => workerParentPort.off("message", onInstruction));
    });

  const notifyParent: NotifyCommitWorkerParent = (message) =>
    Effect.sync(() => workerParentPort.postMessage(message));

  const program = provideCommitBlockWorkerServices(
    runCommitBlockHeaderWorkerProgram(
      inputData,
      inputData.data.speculativeBuild === undefined
        ? undefined
        : awaitSpeculativeInstruction,
      notifyParent,
    ),
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
