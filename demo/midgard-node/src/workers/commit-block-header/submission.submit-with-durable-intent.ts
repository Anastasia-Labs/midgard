import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../../database/index.js";
import { Database } from "../../services/index.js";
import { BeforeSignedTransactionSubmission } from "../../transactions/utils.js";

export const retainedIntentFailure = (headerHash: Buffer, error: unknown) =>
  PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash).pipe(
    Effect.map((record) =>
      Option.isSome(record) &&
      record.value[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] != null
        ? {
            type: "FailureOutput" as const,
            error: `Signed commit intent retained for canonical reconciliation: ${formatUnknownError(error)}`,
          }
        : undefined,
    ),
  );

export const submitWithDurableIntent = <A, E>(
  headerHash: Buffer,
  program: Effect.Effect<A, E>,
) =>
  Effect.gen(function* () {
    const context = yield* Effect.context<Database>();
    return yield* program.pipe(
      Effect.provideService(BeforeSignedTransactionSubmission, {
        persist: ({ txHash, signedTxCbor }) =>
          PendingBlockFinalizationsDB.recordSignedIntent(
            headerHash,
            Buffer.from(txHash, "hex"),
            Buffer.from(signedTxCbor, "hex"),
          ).pipe(Effect.provide(context)),
      }),
    );
  });
