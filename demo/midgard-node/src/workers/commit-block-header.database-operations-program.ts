import { Effect } from "effect";

import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { assertFollowerWrite } from "../services/follower-write-gate.js";
import { buildOnVerifiedCommitBaseProgram } from "./commit-block-header.build-on-verified-base-program.js";
import { WorkerOutput } from "./utils/commit-block-header.js";

/** Builds on the commit base under the worker's producer permit. A foreign
 * base the landed-block processing has not applied yet waits for it. */
export const databaseOperationsProgram = (
  ...args: Parameters<typeof buildOnVerifiedCommitBaseProgram>
): ReturnType<typeof buildOnVerifiedCommitBaseProgram> =>
  Effect.gen(function* () {
    yield* assertFollowerWrite(args[0].history);
    return yield* buildOnVerifiedCommitBaseProgram(...args);
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
