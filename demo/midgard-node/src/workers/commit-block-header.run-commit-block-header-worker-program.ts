import {
  findLedgerSignatureVerifierUnavailable,
  ledgerEd25519SignatureReady,
  LedgerSignatureVerifierUnavailableError,
} from "@al-ft/midgard-core";
import { Cause, Duration, Effect } from "effect";
import { parentPort, workerData } from "worker_threads";

import { LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS } from "../commands/listen-startup.await-ledger-signature-verifier.js";
import {
  MpfEngineStateDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { MidgardMpf, type NativeMpfBuildContext } from "../mpf/index.js";
import { FollowerWrite } from "../services/follower-write-gate.js";
import {
  ContractDeploymentIdentity,
  Database,
  MidgardContracts,
  NativeMpfWorkerPortClient,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { FollowerL1AdapterLive } from "../services/l1-adapter.js";
import {
  type NotifyCommitWorkerParent,
  shouldPreserveCommitMpfRoots,
} from "./commit-block-header.commit-explicit-block-header-program.js";
import { databaseOperationsProgram } from "./commit-block-header.database-operations-program.js";
import { handOffUnwrittenRefusalHolds } from "./commit-block-header.hand-off-refusal-holds.js";
import {
  type CommitLucidFactory,
  CommitWorkerInvariantError,
  followerCommitLucidFactory,
  provideCommitBlockWorkerServices,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
import {
  type LocalFaultMark,
  VERIFIER_UNAVAILABLE_FAULT,
  WITHDRAWAL_UNCLASSIFIABLE_FAULT,
  WorkerInput,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import { findWithdrawalUnclassifiable } from "./utils/mpf/withdrawal-classification.js";

/**
 * The worker's wait for its Ed25519 verifier, bounded as one startup attempt
 * of the ledger signature verifier is
 * (`LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.attemptTimeout`). An
 * initialisation that fails, or does not settle within the bound, fails the
 * run with `LedgerSignatureVerifierUnavailableError`, which the worker's
 * failure boundary (`captureCommitWorkerFailure`) reports as a local fault
 * and the parent bounds.
 */
const awaitCommitWorkerVerifier = (): Effect.Effect<
  void,
  LedgerSignatureVerifierUnavailableError
> => {
  const attemptTimeout = Duration.decode(
    LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.attemptTimeout,
  );
  return Effect.tryPromise({
    try: () => ledgerEd25519SignatureReady(),
    catch: (cause) =>
      findLedgerSignatureVerifierUnavailable(cause) ??
      new LedgerSignatureVerifierUnavailableError(
        "The commit worker's Ed25519 verifier failed to initialise",
        { cause },
      ),
  }).pipe(
    Effect.timeoutFail({
      duration: attemptTimeout,
      onTimeout: () =>
        new LedgerSignatureVerifierUnavailableError(
          `The commit worker's Ed25519 verifier did not initialise within ${Duration.format(attemptTimeout)}`,
        ),
    }),
  );
};

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
  notifyParent: NotifyCommitWorkerParent | undefined,
  commitLucidFactory: CommitLucidFactory,
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
    // Phase A, withdrawal classification and the trace builder verify
    // signatures with libsodium, which initialises asynchronously.
    yield* awaitCommitWorkerVerifier();
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
    Effect.ensuring(handOffUnwrittenRefusalHolds(notifyParent)),
    workerInput.history === undefined
      ? (effect) => effect
      : Effect.provideService(FollowerWrite, workerInput.history),
  );

/** The local fault a failure's errors carry, if any: this process's Ed25519
 * verifier fault, else a due withdrawal row this node cannot classify. */
const localFaultOf = (errors: readonly unknown[]): LocalFaultMark => {
  if (
    errors.some(
      (error) => findLedgerSignatureVerifierUnavailable(error) !== undefined,
    )
  )
    return { localFault: VERIFIER_UNAVAILABLE_FAULT };
  const row = errors
    .map(findWithdrawalUnclassifiable)
    .find((found) => found !== undefined);
  return row === undefined
    ? {}
    : {
        localFault: WITHDRAWAL_UNCLASSIFIABLE_FAULT,
        localFaultSubject: row.eventId,
      };
};

/** The worker's outgoing failure boundary, shared with in-process
 * acceptance. A failure carrying a local fault (this process's Ed25519
 * verifier fault, or a withdrawal row this node cannot classify, named in
 * `localFaultSubject`) is marked `localFault`, which the parent bounds. */
export const captureCommitWorkerFailure = <E, R>(
  program: Effect.Effect<WorkerOutput, E, R>,
) =>
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailureOutput",
        error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
        ...localFaultOf([...Cause.failures(cause), ...Cause.defects(cause)]),
      } satisfies WorkerOutput),
    ),
  );

if (parentPort !== null) {
  const workerParentPort = parentPort;
  const inputData = workerData as WorkerInput;

  const notifyParent: NotifyCommitWorkerParent = (message) =>
    Effect.sync(() => workerParentPort.postMessage(message));

  const program = Effect.scoped(
    Effect.flatMap(followerCommitLucidFactory, (commitLucidFactory) =>
      provideCommitBlockWorkerServices(
        runCommitBlockHeaderWorkerProgram(
          inputData,
          notifyParent,
          commitLucidFactory,
        ),
        FollowerL1AdapterLive,
      ),
    ),
  );

  void Effect.runPromise(captureCommitWorkerFailure(program)).then((output) => {
    // The parent reports a failure as `commit_worker_failed` (one warning, an
    // error once escalated, its first line on /readyz), so a failure that
    // repeats every tick logs here at debug.
    const log =
      output.type === "FailureOutput" ? Effect.logDebug : Effect.logInfo;
    Effect.runSync(
      log(`👷 Block commitment work completed (${JSON.stringify(output)}).`),
    );
    parentPort?.postMessage(output);
  });
}
