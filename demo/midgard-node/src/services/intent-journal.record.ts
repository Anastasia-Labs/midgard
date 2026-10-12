/**
 * The intent journal's record (S5, §8.1) and its send decision (S6), in one
 * node database transaction: the record under the family's plan
 * (`recordAtPlanIn`), and for a role's own send the decision taken with the
 * view check (`decideSendIn`). `openIntentPlan` opens a family's plan.
 * Used by `intent-journal.ts`.
 */
import {
  currentViewIn,
  decideSubmitIn,
  FOLLOWER_NODE_BEHIND,
  type OutputSummary,
  postgresDialect,
  recordIntentIn,
  type RecordIntentResult,
  type SqlTx,
  type SubmitDecision,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import {
  CONTENT_REF_FAMILIES,
  type IntentPlan,
  type RecordOutcome,
  type SubmissionIntent,
} from "./intent-journal.intent.js";
import {
  causeText,
  INTENT_BYTES_MISMATCH,
  INTENT_CONTENT_REF_MISSING,
  INTENT_JOURNAL_NO_VIEW,
  INTENT_JOURNAL_UNAVAILABLE,
  INTENT_UNDECODABLE,
  IntentJournalRefused,
  IntentSubmitHeld,
  refusal,
  refusalOf,
} from "./intent-journal.refusals.js";

/** A record's outcome, with S6's hold when the send decision held it. */
export type InsertOutcome = RecordOutcome &
  Readonly<{ held?: IntentSubmitHeld }>;

/** The send decision's hold, as the journal reports it. */
const heldBy = (
  decision: Extract<SubmitDecision, { kind: "hold" }>,
  txHash: string,
): IntentSubmitHeld =>
  new IntentSubmitHeld({
    reason: `intent_${decision.reason}`,
    txHash,
    message: `tx ${txHash} not sent now: ${decision.detail}`,
  });

/**
 * S6's decision for a role's own send (§8.1), in the record's transaction:
 * held while the follower's view (its cursor) is more than `nodeBehindMs`
 * behind wall-clock time, since the cursor is never past the node tip, so a
 * node behind by more holds it too; otherwise `decideSubmitIn` (the journal
 * row's view check, `stale_at_write`, abandoned).
 */
const decideSendIn = async (
  tx: SqlTx,
  txHash: string,
  slotTime: (slot: number) => number,
  nodeBehindMs: number,
): Promise<IntentSubmitHeld | undefined> => {
  const view = await currentViewIn(tx, postgresDialect);
  if (view !== null) {
    const lagMs = Date.now() - slotTime(view.point.slot);
    if (lagMs > nodeBehindMs)
      return new IntentSubmitHeld({
        reason: FOLLOWER_NODE_BEHIND,
        txHash,
        message: `tx ${txHash} not sent now: the L1 follower's view (slot ${view.point.slot.toString()}) is ${Math.round(lagMs / 1000).toString()} s behind wall-clock time (bound ${Math.round(nodeBehindMs / 1000).toString()} s); the node is behind or the follower is catching up`,
      });
  }
  const decision = await decideSubmitIn(
    tx,
    postgresDialect,
    Buffer.from(txHash, "hex"),
  );
  return decision.kind === "send" ? undefined : heldBy(decision, txHash);
};

/** The record's result, or `no_view` with no cursor to record under. */
const recordAtPlanIn = async (
  tx: SqlTx,
  intent: Extract<SubmissionIntent, { kind: "journaled" }>,
  generation: number,
  bytes: Buffer,
  txHash: string,
  isOwnOutput: (output: OutputSummary) => boolean,
): Promise<RecordIntentResult> => {
  const now = await currentViewIn(tx, postgresDialect);
  if (now === null)
    return { kind: "no_view", txHash: Buffer.from(txHash, "hex") };
  return recordIntentIn(tx, postgresDialect, {
    family: intent.family,
    workflowKey: intent.workflowKey,
    txCbor: bytes,
    isOwnOutput,
    contentRef: intent.contentRef ?? null,
    // With no rewind since the plan opened, every read it made is at or
    // below this cursor, on the chain: this view is the plan's.
    builtAt: now,
    ...(now.generation === generation
      ? {}
      : {
          staleBecause: `planned at generation ${generation.toString()}; the follower rewound to generation ${now.generation.toString()} before the record`,
        }),
  });
};

/**
 * Records `signedTxCbor` in the caller's database, refusing what §8.2
 * refuses (S5: under the intent's plan, `recordAtPlanIn`). `isOwnOutput`
 * marks the predicted change the wallet view reads. With `send`, the send
 * decision (S6, `decideSendIn`) is taken in the same transaction and
 * returned as `held` when it holds.
 */
export const recordSignedIntent = (
  intent: Extract<SubmissionIntent, { kind: "journaled" }>,
  signedTxCbor: string,
  txHash: string,
  isOwnOutput: (output: OutputSummary) => boolean,
  send?: Readonly<{
    slotTime: (slot: number) => number;
    nodeBehindMs: number;
  }>,
): Effect.Effect<InsertOutcome, IntentJournalRefused, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    if (
      CONTENT_REF_FAMILIES.has(intent.family) &&
      intent.contentRef === undefined
    )
      return yield* refusal(
        INTENT_CONTENT_REF_MISSING,
        txHash,
        `tx ${txHash} not submitted: a ${intent.family} intent must name its content (§8.2), and ${intent.workflowKey} names none`,
      );
    const plan = intent.plan;
    if (plan.kind === "none")
      return yield* refusal(
        plan.reason,
        txHash,
        `tx ${txHash} not submitted: its plan has no L1 follower view (${plan.detail})`,
      );
    const sql = yield* SqlClient.SqlClient;
    const bytes = Buffer.from(signedTxCbor, "hex");
    const { result, held } = yield* sql
      .withTransaction(
        Effect.flatMap(followerSqlTx, (tx) =>
          Effect.tryPromise({
            try: async () => {
              const result = await recordAtPlanIn(
                tx,
                intent,
                plan.generation,
                bytes,
                txHash,
                isOwnOutput,
              );
              const sendable =
                (result.kind === "recorded" ||
                  (result.kind === "already_recorded" && result.identical)) &&
                result.intent.txHash.toString("hex") === txHash;
              if (send === undefined || !sendable)
                return { result, held: undefined };
              return {
                result,
                held:
                  result.kind === "recorded" && result.stale
                    ? heldBy(
                        {
                          kind: "hold",
                          reason: "stale_at_write",
                          detail:
                            "a rewind since its plan opened; it is recorded stale_at_write and S6 decides it under the current view",
                        },
                        txHash,
                      )
                    : await decideSendIn(
                        tx,
                        txHash,
                        send.slotTime,
                        send.nodeBehindMs,
                      ),
              };
            },
            catch: (cause) => cause,
          }),
        ),
      )
      .pipe(
        Effect.mapError((cause) =>
          refusal(
            INTENT_JOURNAL_UNAVAILABLE,
            txHash,
            `tx ${txHash} not submitted: the intent journal write failed: ${causeText(cause)}`,
          ),
        ),
      );
    if (result.kind === "recorded" || result.kind === "already_recorded") {
      const journaledHash = result.intent.txHash.toString("hex");
      if (journaledHash !== txHash)
        return yield* refusal(
          INTENT_UNDECODABLE,
          txHash,
          `tx ${txHash} not submitted: its signed bytes hash to ${journaledHash}`,
        );
      if (result.kind === "already_recorded" && !result.identical)
        return yield* refusal(
          INTENT_BYTES_MISMATCH,
          txHash,
          `tx ${txHash} not submitted: it was journaled with other signed bytes; only those are ever sent (S6 resubmits them)`,
        );
      return held === undefined
        ? { kind: result.kind }
        : { kind: result.kind, held };
    }
    return yield* refusalOf(result, txHash);
  });

/** A family's plan at the follower's cursor now (S5). */
export const openIntentPlan = (
  sql: SqlClient.SqlClient,
): Effect.Effect<IntentPlan> =>
  sql
    .withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise({
          try: () => currentViewIn(tx, postgresDialect),
          catch: (cause) => cause,
        }),
      ),
    )
    .pipe(
      Effect.map(
        (view): IntentPlan =>
          view === null
            ? {
                kind: "none",
                reason: INTENT_JOURNAL_NO_VIEW,
                detail: "the L1 follower has no cursor yet",
              }
            : { kind: "view", generation: view.generation },
      ),
      Effect.catchAll((cause) =>
        Effect.succeed<IntentPlan>({
          kind: "none",
          reason: INTENT_JOURNAL_UNAVAILABLE,
          detail: `reading the follower cursor failed: ${causeText(cause)}`,
        }),
      ),
      Effect.provideService(SqlClient.SqlClient, sql),
    );
