import { Effect } from "effect";

import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { assertHistoryProducer } from "../services/event-history-producer.js";
import { buildOnVerifiedCommitBaseProgram } from "./commit-block-header.build-on-verified-base-program.js";
import { WorkerOutput } from "./utils/commit-block-header.js";

/** Builds on the commit base under the worker's producer permit. A foreign
 * base the landed-block processing has not applied yet waits for it. */
export const databaseOperationsProgram = (
  ...args: Parameters<typeof buildOnVerifiedCommitBaseProgram>
): ReturnType<typeof buildOnVerifiedCommitBaseProgram> =>
  Effect.gen(function* () {
    yield* assertHistoryProducer(args[0].history);
    return yield* buildOnVerifiedCommitBaseProgram(...args);
  }).pipe(
    Effect.catchAll((cause) => {
      if (!(cause instanceof ForeignBlockVerificationError))
        return Effect.fail(cause);
      return cause.reason === "missing"
        ? Effect.succeed<WorkerOutput>({
            type: "AwaitingForeignDaOutput",
            foreignHeaderHash: cause.foreignHeaderHash,
            reason: cause.detail,
          })
        : Effect.succeed<WorkerOutput>({
            type: "FailureOutput",
            error: cause.detail,
          });
    }),
  );
