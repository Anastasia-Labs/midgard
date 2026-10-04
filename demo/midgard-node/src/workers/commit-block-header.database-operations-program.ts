import { Effect } from "effect";

import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { assertHistoryProducer } from "../services/event-history-producer.js";
import { Lucid, MidgardContracts } from "../services/index.js";
import { buildOnVerifiedCommitBaseProgram } from "./commit-block-header.build-on-verified-base-program.js";
import { defaultCommitLucidFactory } from "./commit-block-header.pending-user-event-counts-up-to.js";
import {
  revalidateForeignCommitBase,
  VerifiedForeignBase,
  verifyForeignCommitBase,
} from "./commit-block-header.verify-foreign-base.js";
import {
  deserializeStateQueueUTxO,
  WorkerOutput,
} from "./utils/commit-block-header.js";

/** Verify before overdue reconciliation, idle returns, and every foreign root
 * shortcut. Network acquisition stays outside source/Ready SQL transactions. */
export const databaseOperationsProgram = (
  ...args: Parameters<typeof buildOnVerifiedCommitBaseProgram>
): ReturnType<typeof buildOnVerifiedCommitBaseProgram> =>
  Effect.gen(function* () {
    const workerInput = args[0];
    yield* assertHistoryProducer(workerInput.history);
    // Explicit unowned model fixtures cannot issue readiness evidence. Runtime
    // workers carry the source owner's serializable permit.
    if (
      workerInput.history === undefined ||
      workerInput.data.availableConfirmedBlock === ""
    )
      return yield* buildOnVerifiedCommitBaseProgram(...args);
    const acquireLucid = args[9] ?? defaultCommitLucidFactory;
    const lucid = yield* acquireLucid();
    const contracts = yield* MidgardContracts;
    const latest = yield* deserializeStateQueueUTxO(
      workerInput.data.availableConfirmedBlock,
    );
    const base = yield* verifyForeignCommitBase(latest).pipe(
      Effect.provideService(Lucid, lucid),
    );
    const assertCurrent = revalidateForeignCommitBase(base).pipe(
      Effect.provideService(Lucid, lucid),
      Effect.provideService(MidgardContracts, contracts),
    );
    const output = yield* buildOnVerifiedCommitBaseProgram(
      args[0],
      args[1],
      args[2],
      args[3],
      args[4],
      args[5],
      args[6],
      args[7],
      args[8],
      () => Effect.succeed(lucid),
    ).pipe(Effect.provideService(VerifiedForeignBase, { base, assertCurrent }));
    return {
      ...output,
      foreignBaseVerification:
        output.type === "AwaitingForeignDaOutput"
          ? {
              status: "missing" as const,
              foreignHeaderHash: output.foreignHeaderHash,
              reason: output.reason,
            }
          : base.verification,
    };
  }).pipe(
    Effect.catchAll((cause) => {
      if (!(cause instanceof ForeignBlockVerificationError))
        return Effect.fail(cause);
      const metadata = {
        status:
          cause.reason === "missing"
            ? ("missing" as const)
            : ("refused" as const),
        foreignHeaderHash: cause.foreignHeaderHash,
        reason: cause.detail,
      };
      return cause.reason === "missing"
        ? Effect.succeed<WorkerOutput>({
            type: "AwaitingForeignDaOutput",
            foreignHeaderHash: cause.foreignHeaderHash,
            reason: cause.detail,
            foreignBaseVerification: metadata,
          })
        : Effect.succeed<WorkerOutput>({
            type: "FailureOutput",
            error: cause.detail,
            foreignBaseVerification: metadata,
          });
    }),
  );
