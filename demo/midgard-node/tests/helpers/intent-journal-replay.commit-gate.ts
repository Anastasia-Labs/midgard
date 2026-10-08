/**
 * A replayed commit's record through its production pre-broadcast gate
 * (`replayJournaledOnFollower`, I1-fix2 B2): the commit worker's
 * `submitWithDurableIntent` seam, whose gate's history write owns the
 * transaction the journal's insert runs in.
 */
import { SqlClient } from "@effect/sql";
import { Effect, type ManagedRuntime, Option } from "effect";

import { FollowerWriteFixture } from "../../src/services/follower-write-gate.js";
import type { IntentJournalService } from "../../src/services/intent-journal.js";
import { BeforeSignedTransactionSubmission } from "../../src/transactions/utils.js";
import { submitWithDurableIntent } from "../../src/workers/commit-block-header/submission.submit-with-durable-intent.js";
import { type RecordedIntent, SEND_AT_TIP } from "./intent-journal.js";

/**
 * Records a commit through its production pre-broadcast gate: the
 * worker's `submitWithDurableIntent` seam, whose gate's history write
 * owns the transaction the journal's insert runs in. Its pending block
 * is put back as it stood when the worker signed it (prepared, nothing
 * signed or submitted), then restored as the flow left it once the gate
 * has run, so S6 reads the flow's rows.
 */
export const recordCommitThroughGate = async ({
  runtime,
  sql,
  journal,
  entry,
  header,
}: {
  readonly runtime: ManagedRuntime.ManagedRuntime<SqlClient.SqlClient, unknown>;
  readonly sql: SqlClient.SqlClient;
  readonly journal: IntentJournalService;
  readonly entry: RecordedIntent;
  readonly header: Buffer;
}) => {
  type Signed = {
    readonly status: string;
    readonly submitted_tx_hash: Buffer | null;
    readonly intended_tx_hash: Buffer | null;
    readonly signed_tx_cbor: Buffer | null;
  };
  const signedOf = (pending: SqlClient.SqlClient) =>
    pending<Signed>`SELECT status, submitted_tx_hash, intended_tx_hash,
        signed_tx_cbor FROM pending_block_finalizations
      WHERE header_hash = ${header}`.pipe(Effect.map((rows) => rows[0]));
  const flowLeft = await runtime.runPromise(signedOf(sql));
  if (flowLeft === undefined)
    throw new Error(
      `commit ${entry.txHash} has no pending block ${header.toString("hex")}`,
    );
  await runtime.runPromise(sql`UPDATE pending_block_finalizations
    SET status = 'pending_submission', submitted_tx_hash = NULL,
      intended_tx_hash = NULL, signed_tx_cbor = NULL
    WHERE header_hash = ${header}`);
  const outcome = await runtime.runPromise(
    Effect.either(
      submitWithDurableIntent(
        header,
        // As `submitSignedTxWithRecovery` hands it to the journal.
        Effect.flatMap(
          Effect.serviceOption(BeforeSignedTransactionSubmission),
          (before) =>
            Option.isNone(before)
              ? Effect.dieMessage("The commit seam provides no gate")
              : journal.record(
                  entry.intent,
                  entry.signedTxCbor,
                  entry.txHash,
                  SEND_AT_TIP,
                  (insert) =>
                    before.value.persist({
                      txHash: entry.txHash,
                      signedTxCbor: entry.signedTxCbor,
                      journal: insert,
                    }),
                ),
        ),
      ).pipe(Effect.provideService(FollowerWriteFixture, true)),
    ),
  );
  const written = await runtime.runPromise(signedOf(sql));
  await runtime.runPromise(sql`UPDATE pending_block_finalizations
    SET status = ${flowLeft.status},
      submitted_tx_hash = ${flowLeft.submitted_tx_hash},
      intended_tx_hash = ${flowLeft.intended_tx_hash},
      signed_tx_cbor = ${flowLeft.signed_tx_cbor}
    WHERE header_hash = ${header}`);
  return {
    outcome,
    gated:
      written?.intended_tx_hash?.toString("hex") === entry.txHash &&
      written.signed_tx_cbor?.toString("hex") === entry.signedTxCbor,
  };
};
