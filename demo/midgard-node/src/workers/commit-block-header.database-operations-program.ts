import { Effect } from "effect";

import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { assertFollowerWrite } from "../services/follower-write-gate.js";
import { buildOnVerifiedCommitBaseProgram } from "./commit-block-header.build-on-verified-base-program.js";
import { CommitWorkerInvariantError } from "./commit-block-header.pending-user-event-counts-up-to.js";
import { buildEvictingDaFrameRefusedNormalTx } from "./commit-block-header/commit-da-oversize-eviction.js";
import { WorkerOutput } from "./utils/commit-block-header.js";

/** Builds on the commit base under the worker's producer permit. A foreign
 * base the landed-block processing has not applied yet waits for it. A
 * normal transaction whose lone block the DA frame refuses is rejected and
 * the block rebuilt without it. */
export const databaseOperationsProgram = (
  ...args: Parameters<typeof buildOnVerifiedCommitBaseProgram>
): ReturnType<typeof buildOnVerifiedCommitBaseProgram> =>
  Effect.gen(function* () {
    yield* assertFollowerWrite(args[0].history);
    const [
      workerInput,
      transactionsMpf,
      notifyParent,
      nativeMpfClient,
      nativeMpfState,
      acquireCommitLucid,
    ] = args;
    return yield* buildEvictingDaFrameRefusedNormalTx({
      build: (evictions) =>
        buildOnVerifiedCommitBaseProgram(
          workerInput,
          transactionsMpf,
          notifyParent,
          nativeMpfClient,
          nativeMpfState,
          acquireCommitLucid,
          evictions,
        ),
      // The refused build's ledger fork never reaches commit; the rebuild
      // forks again from the same base.
      discardFailedBuild: Effect.suspend(() => {
        const context = nativeMpfState?.context;
        if (context === undefined || nativeMpfState === undefined)
          return Effect.void;
        nativeMpfState.context = undefined;
        return Effect.tryPromise({
          try: () => context.client.discard(context.handle),
          catch: (cause) =>
            new CommitWorkerInvariantError({
              message: `Architecture G discard failed: ${String(cause)}`,
            }),
        });
      }),
    });
  }).pipe(
    Effect.catchAll((cause) => {
      if (!(cause instanceof ForeignBlockVerificationError))
        return Effect.fail(cause);
      return cause.reason === "missing"
        ? Effect.succeed<WorkerOutput>({
            type: "AwaitingCommitBaseOutput",
            baseHeaderHash: cause.foreignHeaderHash,
            detail: cause.detail,
          })
        : Effect.succeed<WorkerOutput>({
            type: "FailureOutput",
            error: cause.detail,
          });
    }),
  );
