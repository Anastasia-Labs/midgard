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
import {
  type AnchorEmulatorHeight,
  rebindCommitAnchor,
} from "./intent-journal-replay.anchor.js";

/**
 * Records a commit through its production pre-broadcast gate: the
 * worker's `submitWithDurableIntent` seam, whose gate's history write
 * owns the transaction the journal's insert runs in. Its pending block
 * is put back as it stood when the worker signed it (prepared, no
 * intended commit, its state-queue mutation lease held), then restored as
 * the flow left it once the gate has run, so S6 reads the flow's rows.
 * Its anchor is first rebound to the replay's chain when
 * `emulatorHeightOf` is given.
 */
export const recordCommitThroughGate = async ({
  runtime,
  sql,
  journal,
  entry,
  header,
  emulatorHeightOf,
}: {
  readonly runtime: ManagedRuntime.ManagedRuntime<SqlClient.SqlClient, unknown>;
  readonly sql: SqlClient.SqlClient;
  readonly journal: IntentJournalService;
  readonly entry: RecordedIntent;
  readonly header: Buffer;
  readonly emulatorHeightOf?: AnchorEmulatorHeight;
}) => {
  if (emulatorHeightOf !== undefined)
    await rebindCommitAnchor({ runtime, sql, header, emulatorHeightOf });
  type Signed = {
    readonly intended_tx_hash: Buffer | null;
    readonly intent_tx_cbor: Buffer | null;
    readonly state_queue_lease_token: string;
  };
  const signedOf = (pending: SqlClient.SqlClient) =>
    pending<Signed>`SELECT p.intended_tx_hash, intent.tx_cbor AS intent_tx_cbor,
        p.state_queue_lease_token
      FROM pending_block_finalizations p
      LEFT JOIN l1_intents intent ON intent.tx_hash = p.intended_tx_hash
      WHERE p.header_hash = ${header}`.pipe(Effect.map((rows) => rows[0]));
  const flowLeft = await runtime.runPromise(signedOf(sql));
  if (flowLeft === undefined)
    throw new Error(
      `commit ${entry.txHash} has no pending block ${header.toString("hex")}`,
    );
  await runtime.runPromise(sql`UPDATE pending_block_finalizations
    SET intended_tx_hash = NULL WHERE header_hash = ${header}`);
  const lease = flowLeft.state_queue_lease_token;
  await runtime.runPromise(sql`INSERT INTO state_queue_mutation_leases
      (token, scope, holder, status, expires_at)
    VALUES (${lease}, 'state_queue', 'commit-replay', 'active',
      NOW() + INTERVAL '10 minutes')`);
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
  await runtime.runPromise(
    sql`DELETE FROM state_queue_mutation_leases WHERE token = ${lease}`,
  );
  await runtime.runPromise(sql`UPDATE pending_block_finalizations
    SET intended_tx_hash = ${flowLeft.intended_tx_hash}
    WHERE header_hash = ${header}`);
  return {
    outcome,
    gated:
      written?.intended_tx_hash?.toString("hex") === entry.txHash &&
      written.intent_tx_cbor?.toString("hex") === entry.signedTxCbor,
  };
};
