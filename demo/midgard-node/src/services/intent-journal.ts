/**
 * The node's intent journal (plan §8.2, I1): every node L1 family journals
 * the exact signed bytes of a transaction in `l1_intents` before its first
 * submission, in the node database, through the follower's
 * `recordIntentIn`. Status is never written: it is derived from the
 * follower's facts (`deriveIntentStatusesIn`, or `deriveIntentStatusIn` for
 * one transaction), and S6 (the node follower's
 * intent reconciler, `l1-follower.intents.ts`) resubmits the journaled bytes
 * of live intents and never sends a dead one.
 *
 * - The submit seam (`submitSignedTxWithRecovery`) takes a `SubmissionIntent`
 *   and requires this service, so no node submission can skip it.
 * - S5 (§8.1): every journaled intent carries its family's plan, opened
 *   (`openPlan`) before the family's first L1 read. The record compares the
 *   plan's generation with the cursor in its own transaction: a rewind
 *   between the two records the row with `stale_at_write`, never sent from
 *   that write. Otherwise the view recorded is the cursor there, at or above
 *   every read the plan made.
 * - S6 (§8.1): a role's own send is decided in the record's transaction
 *   (`decideSubmitIn`, with the view check), and held while the follower's
 *   view is more than the node-behind bound behind wall-clock time. A held
 *   send fails with `IntentSubmitHeld`, not a refusal; S6's reconciler
 *   decides it under the current view. The network send follows the commit.
 * - A refusal (§8.2's tracked-input invariant, no follower view, bytes that
 *   differ from the journaled ones) stops that submission and is held as a
 *   named `/readyz` reason until the family's next recording succeeds, or a
 *   landed transaction spends the refused one's input. Holds live in the
 *   node database (`intent-journal.holds.ts`), so a worker thread's reach the
 *   main process. The process stays up.
 * - A phase or process with no running follower (protocol initialization
 *   before `startL1Follower`, a one-shot CLI command) provides
 *   `IntentJournalWithoutFollower`: journaled families are submitted
 *   unjournaled (`no_follower`) and await their own confirmation there. A
 *   path that runs only from a CLI command passes an `unjournaled` intent
 *   (`no_follower`) itself.
 */
import {
  DEFAULT_NODE_BEHIND_MS,
  deriveIntentStatusIn,
  type IntentStatus,
  type OutputSummary,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Context, Effect, Layer, Option } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import type { DriverHold } from "../l1-events/driver.js";
import { NodeConfig } from "./config.js";
import {
  refusalHoldsOver,
  type UnwrittenHold,
} from "./intent-journal.holds.js";
import type {
  IntentPlan,
  RecordOutcome,
  RecordPurpose,
  SubmissionIntent,
  UnjournaledReason,
} from "./intent-journal.intent.js";
import {
  type InsertOutcome,
  openIntentPlan,
  recordSignedIntent,
} from "./intent-journal.record.js";
import {
  INTENT_GATE_UNJOURNALED,
  INTENT_JOURNAL_NO_VIEW,
  IntentJournalRefused,
  IntentSubmitHeld,
  refusal,
  unavailable,
} from "./intent-journal.refusals.js";
import { nodeOwnWallets } from "./intent-journal.tracked-set.js";
import {
  type NodeWalletView,
  readFollowerWalletView,
  readProviderWalletView,
  type WalletViewUnavailable,
} from "./intent-journal.wallet-view.js";

export {
  CONTENT_REF_FAMILIES,
  intentLabel,
  type IntentPlan,
  intentPlanAt,
  journaledIntent,
  NODE_INTENT_FAMILIES,
  type NodeIntentFamily,
  type RecordOutcome,
  type RecordPurpose,
  type SubmissionIntent,
  type UnjournaledReason,
  unjournaledSubmission,
} from "./intent-journal.intent.js";
export { openIntentPlan, recordSignedIntent } from "./intent-journal.record.js";
export {
  INTENT_BYTES_MISMATCH,
  INTENT_CONTENT_REF_MISSING,
  INTENT_GATE_UNJOURNALED,
  INTENT_INPUT_UNTRACKED,
  INTENT_JOURNAL_NO_VIEW,
  INTENT_JOURNAL_UNAVAILABLE,
  INTENT_UNDECODABLE,
  IntentJournalRefused,
  IntentSubmitHeld,
} from "./intent-journal.refusals.js";

/**
 * The journal's insert of one intent, recorded in the caller's transaction:
 * it must run inside a SQL transaction the caller opened and owns, on the
 * node database, and commits or rolls back with it. Outside a transaction it
 * refuses (`INTENT_GATE_UNJOURNALED`) and writes nothing. It fails with the
 * journal's refusal (§8.2), which the caller must let roll its transaction
 * back. For an unjournaled submission it does nothing.
 */
export type JournalInsert = Effect.Effect<void, IntentJournalRefused>;

/**
 * A workflow's pre-broadcast gate: its checks and durable write (the
 * commit's pending-finalization row, the settlement attempt), run in one SQL
 * transaction the gate itself opens as the outermost one (a history write
 * must own it), with the journal's insert run inside that same transaction.
 * The single transaction is what makes the gate and the journal row one
 * fact: a gate that refuses, or a process that stops anywhere before the
 * transaction commits, leaves no journal row, so S6 never holds bytes whose
 * gate did not pass.
 */
export type PreBroadcastGate<E> = (
  journalInsert: JournalInsert,
) => Effect.Effect<void, E>;

export type IntentJournalService = Readonly<{
  /**
   * Journals the signed bytes before their first submission. Idempotent per
   * transaction: a second call with the same bytes is `already_recorded`.
   *
   * With no `gate`, the journal writes the row in its own transaction.
   *
   * With a `gate` (`PreBroadcastGate`), the gate owns the transaction: it is
   * handed the journal's insert and runs it inside its own outermost
   * transaction, together with its write ("record in the caller's
   * transaction"). The journal never opens a transaction around a gate. A
   * gate that fails returns its error as it is, unless the journal's insert
   * refused inside it, in which case the refusal is returned (and held). A
   * gate that passes without the insert having succeeded inside a
   * transaction is refused (`INTENT_GATE_UNJOURNALED`).
   *
   * With `purpose` `send`, the send decision (S6) is taken inside the same
   * transaction; a held one fails with `IntentSubmitHeld` once that
   * transaction committed (the row, and the gate's write, stand).
   */
  record: <E = never>(
    intent: SubmissionIntent,
    signedTxCbor: string,
    txHash: string,
    purpose: RecordPurpose,
    gate?: PreBroadcastGate<E>,
  ) => Effect.Effect<
    RecordOutcome,
    IntentJournalRefused | IntentSubmitHeld | E
  >;
  /**
   * Opens a family's plan (S5): the follower generation now. Call it
   * before the family's first L1 read. Never fails: no view is `none`.
   */
  openPlan: Effect.Effect<IntentPlan>;
  /** The refusals still standing, one per family: each fails `/readyz`. */
  holds: () => readonly DriverHold[];
  /**
   * Returns this journal's refusal holds whose write to the node database
   * has not landed yet, and forgets them. A worker thread's journal ends
   * with the thread, so the worker hands them to the main process (I1-H1),
   * which `adopt`s them.
   */
  handOff: () => readonly UnwrittenHold[];
  /**
   * Takes over a worker's unwritten refusal holds: they are holds of this
   * journal at once (so `/readyz` names them), and are written on its next
   * record or refresh until they land.
   */
  adopt: (holds: readonly UnwrittenHold[]) => void;
  /**
   * Re-reads the standing refusals from the node database, after clearing
   * those whose refused transaction lost an input to another landed one
   * (`intent-journal.holds.ts`). The main process runs it at every tip, so
   * refusals raised in worker threads reach its `/readyz`. Never fails: a
   * failed read keeps the last one.
   */
  refresh: () => Effect.Effect<void>;
  /**
   * The wallet view (§8.5) of one own address, read now
   * (`intent-journal.wallet-view.ts`): the facts and live intents under a
   * follower, the provider's UTxOs where none runs.
   */
  walletView: (
    lucid: LucidEvolution,
    address: string,
  ) => Effect.Effect<NodeWalletView, WalletViewUnavailable>;
}>;

export class IntentJournal extends Context.Tag("midgard/IntentJournal")<
  IntentJournal,
  IntentJournalService
>() {}

const submitUnjournaled = (
  reason: UnjournaledReason,
  workflowKey: string,
  txHash: string,
): Effect.Effect<RecordOutcome> =>
  Effect.logInfo(
    `Submitting ${txHash} unjournaled (${reason}): ${workflowKey}`,
  ).pipe(Effect.as({ kind: "unjournaled" as const, reason }));

/**
 * The derived status (§8.2) of one journaled transaction at the follower's
 * cursor, or null when it is not journaled (or was pruned). Reads only that
 * intent and its journaled dependencies, by key.
 */
export const readIntentStatus = (
  txHash: string,
): Effect.Effect<IntentStatus | null, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const { state } = yield* sql.withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise({
          try: () =>
            deriveIntentStatusIn(
              tx,
              postgresDialect,
              Buffer.from(txHash, "hex"),
            ),
          catch: (cause) => cause,
        }),
      ),
    );
    return state?.status ?? null;
  });

/** Opens a plan with the journal in context (`IntentJournalService.openPlan`). */
export const openPlan: Effect.Effect<IntentPlan, never, IntentJournal> =
  Effect.flatMap(IntentJournal, (journal) => journal.openPlan);

/**
 * The journal over one node SQL client. A refusal is held for its family
 * (`refusalHoldsOver`) until that family's next success clears it, in the
 * record's transaction. `nodeBehindMs` bounds how far behind wall-clock time
 * the follower's view may be for a send (default `DEFAULT_NODE_BEHIND_MS`).
 */
export const intentJournalOver = (
  sql: SqlClient.SqlClient,
  isOwnOutput: (output: OutputSummary) => boolean,
  options: Readonly<{ nodeBehindMs?: number }> = {},
): IntentJournalService => {
  const held = refusalHoldsOver(sql);
  const nodeBehindMs = options.nodeBehindMs ?? DEFAULT_NODE_BEHIND_MS;
  return {
    openPlan: openIntentPlan(sql),
    record: <E>(
      intent: SubmissionIntent,
      signedTxCbor: string,
      txHash: string,
      purpose: RecordPurpose,
      gate?: PreBroadcastGate<E>,
    ): Effect.Effect<
      RecordOutcome,
      IntentJournalRefused | IntentSubmitHeld | E
    > => {
      if (intent.kind === "unjournaled")
        return submitUnjournaled(
          intent.reason,
          intent.workflowKey,
          txHash,
        ).pipe(
          Effect.zipLeft(gate === undefined ? Effect.void : gate(Effect.void)),
        );
      const attempt = Effect.suspend(() => {
        // What the insert did, in the transaction that ran it last. A gate
        // may wrap the insert's refusal in its own error (a history write
        // maps every failure), so the journal keeps it here.
        let inserted: InsertOutcome | undefined;
        let refused: IntentJournalRefused | undefined;
        const insert: JournalInsert = Effect.gen(function* () {
          inserted = undefined;
          refused = undefined;
          if (
            gate !== undefined &&
            Option.isNone(
              yield* Effect.serviceOption(SqlClient.TransactionConnection),
            )
          )
            return yield* refusal(
              INTENT_GATE_UNJOURNALED,
              txHash,
              `tx ${txHash} not submitted: the ${intent.family} gate ran the journal insert outside its own transaction`,
            );
          const outcome = yield* recordSignedIntent(
            intent,
            signedTxCbor,
            txHash,
            isOwnOutput,
            purpose.kind === "send"
              ? { slotTime: purpose.slotTime, nodeBehindMs }
              : undefined,
          ).pipe(Effect.provideService(SqlClient.SqlClient, sql));
          yield* held
            .clearIn(intent.family)
            .pipe(Effect.mapError((cause) => unavailable(txHash, cause)));
          inserted = outcome;
        }).pipe(
          Effect.tapError((error) =>
            Effect.sync(() => {
              refused = error;
            }),
          ),
        );
        /** The insert's outcome once its transaction committed. */
        const outcome = (): Effect.Effect<
          InsertOutcome,
          IntentJournalRefused
        > =>
          inserted !== undefined
            ? Effect.succeed(inserted)
            : Effect.fail(
                refused ??
                  refusal(
                    INTENT_GATE_UNJOURNALED,
                    txHash,
                    `tx ${txHash} not submitted: the ${intent.family} gate passed without journaling it`,
                  ),
              );
        return gate === undefined
          ? sql.withTransaction(insert).pipe(
              Effect.catchTag("SqlError", (cause) =>
                Effect.fail(unavailable(txHash, cause)),
              ),
              Effect.flatMap(outcome),
            )
          : gate(insert).pipe(
              Effect.catchAll(
                (error): Effect.Effect<never, IntentJournalRefused | E> =>
                  Effect.fail(refused ?? error),
              ),
              Effect.flatMap(outcome),
            );
      });
      return held.flush.pipe(
        Effect.zipRight(attempt),
        Effect.tap(() => held.cleared(intent.family)),
        Effect.tapError((error) =>
          error instanceof IntentJournalRefused
            ? held.raise(
                intent.family,
                {
                  reason: error.reason,
                  detail: `${intent.family} ${intent.workflowKey}: ${error.message}`,
                },
                txHash,
                signedTxCbor,
              )
            : Effect.void,
        ),
        // A held send recorded the row (and the gate's write) and committed;
        // only the send is held.
        Effect.flatMap(({ held: hold, ...outcome }) =>
          hold === undefined
            ? Effect.succeed<RecordOutcome>(outcome)
            : Effect.fail(hold),
        ),
      );
    },
    holds: held.holds,
    handOff: held.handOff,
    adopt: held.adopt,
    refresh: held.refresh,
    walletView: (_lucid, address) =>
      readFollowerWalletView(address).pipe(
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
  };
};

/** The journal over the node database (`intentJournalOver`). */
export const makeIntentJournal = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const sql = yield* SqlClient.SqlClient;
  const own = new Set(nodeOwnWallets(config).map((a) => a.toString("hex")));
  return intentJournalOver(
    sql,
    (output) => own.has(output.address.toString("hex")),
    { nodeBehindMs: config.L1_NODE_BEHIND_MAX_MS },
  );
});

export const IntentJournalLive = Layer.effect(IntentJournal, makeIntentJournal);

/**
 * The journal where no follower runs (see the module doc): every submission
 * goes out unjournaled and nothing is held.
 */
export const IntentJournalWithoutFollower = Layer.succeed(IntentJournal, {
  openPlan: Effect.succeed<IntentPlan>({
    kind: "none",
    reason: INTENT_JOURNAL_NO_VIEW,
    detail: "no L1 follower runs in this phase or process",
  }),
  record: (intent, _signedTxCbor, txHash, _purpose, gate) =>
    submitUnjournaled(
      intent.kind === "unjournaled" ? intent.reason : "no_follower",
      intent.kind === "unjournaled"
        ? intent.workflowKey
        : `${intent.family} ${intent.workflowKey}`,
      txHash,
    ).pipe(
      Effect.zipLeft(gate === undefined ? Effect.void : gate(Effect.void)),
    ),
  holds: () => [],
  handOff: () => [],
  // No worker journals in a phase without a follower.
  adopt: () => undefined,
  refresh: () => Effect.void,
  walletView: readProviderWalletView,
});
