import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../../database/index.js";
import { Database } from "../../services/index.js";
import { BeforeSignedTransactionSubmission } from "../../transactions/utils.js";
import type { WorkerOutput } from "../utils/commit-block-header.js";

/** Why a prepared journal makes the commit wait for the next window. */
export const HELD_HEADER_DETAIL =
  "an abandoned journal whose commit was signed holds this header hash; its signed bytes can still land, so the journal is kept";

/** The commit's output when a signed abandoned journal holds its header
 * (`preparePendingSubmission` returned `held`, writing nothing): it waits for
 * the next scheduler window, whose block end time gives it another header. */
export const awaitNextCommitWindow = (heldHeaderHash: Buffer) =>
  Effect.logWarning(
    `🔹 Commit awaits the next window header_hash=${heldHeaderHash.toString("hex")}: ${HELD_HEADER_DETAIL}`,
  ).pipe(
    Effect.as({
      type: "AwaitingNextCommitWindowOutput",
      heldHeaderHash: heldHeaderHash.toString("hex"),
      detail: HELD_HEADER_DETAIL,
    } satisfies WorkerOutput),
  );

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
        // The gate's history write owns the transaction; the journal's
        // insert runs inside it (`PreBroadcastGate`).
        persist: ({ txHash, signedTxCbor, journal }) =>
          PendingBlockFinalizationsDB.recordSignedIntent(
            headerHash,
            Buffer.from(txHash, "hex"),
            Buffer.from(signedTxCbor, "hex"),
            journal,
          ).pipe(Effect.provide(context)),
      }),
    );
  });
