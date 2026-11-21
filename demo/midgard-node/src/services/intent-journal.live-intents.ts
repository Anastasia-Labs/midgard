/**
 * The live intents of one workflow (`IntentJournalService.liveIntents`): a
 * read-only journal query over the follower's facts, so a workflow builds
 * nothing for a key whose journaled transaction can still land. Each
 * intent's status is derived (`deriveIntentStatusIn`), never stored, in one
 * follower snapshot.
 */
import {
  deriveIntentStatusIn,
  postgresDialect,
  readIntentsByWorkflowIn,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Data, Effect } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import type { NodeIntentFamily } from "./intent-journal.intent.js";

/** A journaled transaction of a workflow that is live now. */
export type LiveIntent = Readonly<{
  txHash: string;
  /** Every input, reference input and collateral is a live fact now. */
  inputsAvailable: boolean;
}>;

/** The journal could not be read (the database); nothing is held. */
export class IntentJournalReadFailed extends Data.TaggedError(
  "IntentJournalReadFailed",
)<{
  readonly message: string;
  readonly cause: unknown;
}> {}

/** The live intents of `family` under `workflowKey`, read now. */
export const readLiveIntents = (
  family: NodeIntentFamily,
  workflowKey: string,
): Effect.Effect<
  readonly LiveIntent[],
  IntentJournalReadFailed,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise(async () => {
          const live: LiveIntent[] = [];
          for (const intent of await readIntentsByWorkflowIn(
            tx,
            postgresDialect,
            workflowKey,
          )) {
            if (intent.family !== family) continue;
            const { state } = await deriveIntentStatusIn(
              tx,
              postgresDialect,
              intent.txHash,
            );
            if (state?.status.kind === "live")
              live.push({
                txHash: intent.txHash.toString("hex"),
                inputsAvailable: state.status.inputsAvailable,
              });
          }
          return live;
        }),
      ),
    );
  }).pipe(
    Effect.mapError(
      (cause) =>
        new IntentJournalReadFailed({
          message: `the live ${family} intents of ${workflowKey} could not be read`,
          cause,
        }),
    ),
  );
