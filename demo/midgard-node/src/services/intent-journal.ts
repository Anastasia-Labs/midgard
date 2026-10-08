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
 * - A refusal (§8.2's tracked-input invariant, no follower view, bytes that
 *   differ from the journaled ones) stops that submission and is held as a
 *   named `/readyz` reason until the family's next recording succeeds, or a
 *   landed transaction spends the refused one's input. Holds live in the
 *   node database (`intent-journal.holds.ts`), so a worker thread's reach the
 *   main process. The process stays up.
 * - `unjournaled` submissions name why they cannot be journaled: protocol
 *   bootstrap runs before the follower starts, and user or committee wallets
 *   are not the node's tracked wallets.
 * - A phase or process with no running follower (protocol initialization
 *   before `startL1Follower`, a one-shot CLI command) provides
 *   `IntentJournalWithoutFollower`: journaled families are submitted
 *   unjournaled (`no_follower`) and await their own confirmation there.
 */
import { inspect } from "node:util";

import {
  deriveIntentStatusIn,
  type IntentStatus,
  type OutputSummary,
  postgresDialect,
  recordIntentIn,
  type RecordIntentResult,
} from "@al-ft/midgard-l1-follower";
import { SqlClient, type SqlError } from "@effect/sql";
import { Context, Data, Effect, Layer } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import type { DriverHold } from "../l1-events/driver.js";
import { NodeConfig } from "./config.js";
import { refusalHoldsOver } from "./intent-journal.holds.js";
import { nodeOwnWallets } from "./intent-journal.tracked-set.js";

/** The node's L1 families (§8.4, Node rows). */
export const NODE_INTENT_FAMILIES = [
  "commit",
  "scheduler_refresh",
  "merge",
  "attest",
  "correction",
  "register",
  "activate",
  "deregister",
  "exit",
  "takeover",
  "retire",
  "recover_bond",
  "reserve_payout",
  "settlement",
  "reference_publication",
  "reference_sweep",
  "reference_funding",
  "script_reward_registration",
  "phas_membership",
] as const;

export type NodeIntentFamily = (typeof NODE_INTENT_FAMILIES)[number];

/** The families whose intent names its content: the header, or the event settled. */
export const CONTENT_REF_FAMILIES: ReadonlySet<NodeIntentFamily> = new Set([
  "commit",
  "merge",
  "attest",
  "correction",
  "reserve_payout",
  "settlement",
]);

/** Why a submission is not journaled. */
export type UnjournaledReason =
  /** Protocol bootstrap: it runs before the node's follower starts. */
  | "bootstrap"
  /** A user's own wallet (deposit, withdrawal): not a node wallet. */
  | "user_wallet"
  /** A committee member's wallet (DA bond): the committee journals its own. */
  | "committee_wallet"
  /**
   * No follower runs in this phase or process (protocol initialization
   * before the follower starts, a one-shot CLI command): nothing would
   * reconcile the intent, so the submission awaits its own confirmation.
   */
  | "no_follower";

export type SubmissionIntent =
  | Readonly<{
      kind: "journaled";
      family: NodeIntentFamily;
      /** The workflow the tx belongs to, e.g. `commit:tail=<outref>`. */
      workflowKey: string;
      /** Class B content the tx commits to (an own block's header hash). */
      contentRef?: Buffer;
    }>
  | Readonly<{
      kind: "unjournaled";
      reason: UnjournaledReason;
      workflowKey: string;
    }>;

export const journaledIntent = (
  family: NodeIntentFamily,
  workflowKey: string,
  contentRef?: Buffer,
): SubmissionIntent => ({
  kind: "journaled",
  family,
  workflowKey,
  ...(contentRef === undefined ? {} : { contentRef }),
});

export const unjournaledSubmission = (
  reason: UnjournaledReason,
  workflowKey: string,
): SubmissionIntent => ({ kind: "unjournaled", reason, workflowKey });

/** A short label for logs. */
export const intentLabel = (intent: SubmissionIntent): string =>
  intent.kind === "journaled"
    ? `${intent.family} ${intent.workflowKey}`
    : `unjournaled (${intent.reason}) ${intent.workflowKey}`;

/** The follower store has no cursor: there is no view to journal under. */
export const INTENT_JOURNAL_NO_VIEW = "intent_journal_no_view";
/** §8.2: an input, reference input or collateral is not a tracked fact. */
export const INTENT_INPUT_UNTRACKED = "intent_input_untracked";
/** The same transaction was journaled with other bytes; only those are sent. */
export const INTENT_BYTES_MISMATCH = "intent_bytes_mismatch";
/** The signed bytes do not decode, or hash to another transaction. */
export const INTENT_UNDECODABLE = "intent_undecodable";
/** A family that names its content (§8.2) journaled no content reference. */
export const INTENT_CONTENT_REF_MISSING = "intent_content_ref_missing";
/** The journal write failed (the database). */
export const INTENT_JOURNAL_UNAVAILABLE = "intent_journal_unavailable";

/** The journal refused the transaction; it was not submitted. */
export class IntentJournalRefused extends Data.TaggedError(
  "IntentJournalRefused",
)<{
  readonly reason: string;
  readonly txHash: string;
  readonly message: string;
}> {}

export type RecordOutcome =
  | Readonly<{ kind: "recorded" | "already_recorded" }>
  | Readonly<{ kind: "unjournaled"; reason: UnjournaledReason }>;

export type IntentJournalService = Readonly<{
  /**
   * Journals the signed bytes before their first submission. Idempotent per
   * transaction: a second call with the same bytes is `already_recorded`.
   *
   * `gate` is the workflow's pre-broadcast check and durable write (the
   * commit's pending-finalization row, the settlement attempt). It runs in
   * the journal row's own SQL transaction, after the row is written: a gate
   * that fails, or a process that stops before the transaction commits,
   * leaves no journal row, so S6 never holds bytes whose gate did not pass.
   * The gate's error is returned as it is.
   */
  record: <E = never>(
    intent: SubmissionIntent,
    signedTxCbor: string,
    txHash: string,
    gate?: Effect.Effect<void, E>,
  ) => Effect.Effect<RecordOutcome, IntentJournalRefused | E>;
  /** The refusals still standing, one per family: each fails `/readyz`. */
  holds: () => readonly DriverHold[];
  /**
   * Re-reads the standing refusals from the node database, after clearing
   * those whose refused transaction lost an input to another landed one
   * (`intent-journal.holds.ts`). The main process runs it at every tip, so
   * refusals raised in worker threads reach its `/readyz`. Never fails: a
   * failed read keeps the last one.
   */
  refresh: () => Effect.Effect<void>;
}>;

export class IntentJournal extends Context.Tag("midgard/IntentJournal")<
  IntentJournal,
  IntentJournalService
>() {}

/** An error and the causes under it (a SQL error names its driver's). */
const causeText = (error: unknown): string => {
  const parts: string[] = [];
  for (
    let at: unknown = error, depth = 0;
    at !== undefined && at !== null && depth < 4;
    at = typeof at === "object" ? (at as { cause?: unknown }).cause : undefined,
      depth += 1
  )
    parts.push(
      at instanceof Error
        ? at.message
        : typeof at === "string"
          ? at
          : typeof at === "object" &&
              typeof (at as { message?: unknown }).message === "string"
            ? (at as { message: string }).message
            : inspect(at, { depth: 1 }),
    );
  return parts.join(": ");
};

const refusal = (
  reason: string,
  txHash: string,
  message: string,
): IntentJournalRefused =>
  new IntentJournalRefused({ reason, txHash, message });

const refusalOf = (
  result: Exclude<
    RecordIntentResult,
    { kind: "recorded" } | { kind: "already_recorded" }
  >,
  txHash: string,
): IntentJournalRefused => {
  switch (result.kind) {
    case "no_view":
      return refusal(
        INTENT_JOURNAL_NO_VIEW,
        txHash,
        `tx ${txHash} not submitted: the L1 follower has no view to journal it under`,
      );
    case "input_untracked":
      return refusal(
        INTENT_INPUT_UNTRACKED,
        txHash,
        `tx ${txHash} not submitted: ${result.untracked
          .map((o) => `${o.txHash.toString("hex")}#${o.index.toString()}`)
          .join(
            ", ",
          )} not a tracked fact or an output of a journaled intent (§8.2); the follower may be behind`,
      );
    case "undecodable":
      return refusal(
        INTENT_UNDECODABLE,
        txHash,
        `tx ${txHash} not submitted: ${result.detail}`,
      );
  }
};

/**
 * Records `signedTxCbor` in the caller's database, refusing what §8.2
 * refuses. `isOwnOutput` marks the predicted change the wallet view reads.
 */
export const recordSignedIntent = (
  intent: Extract<SubmissionIntent, { kind: "journaled" }>,
  signedTxCbor: string,
  txHash: string,
  isOwnOutput: (output: OutputSummary) => boolean,
): Effect.Effect<RecordOutcome, IntentJournalRefused, SqlClient.SqlClient> =>
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
    const sql = yield* SqlClient.SqlClient;
    const bytes = Buffer.from(signedTxCbor, "hex");
    const result = yield* sql
      .withTransaction(
        Effect.flatMap(followerSqlTx, (tx) =>
          Effect.tryPromise({
            try: () =>
              recordIntentIn(tx, postgresDialect, {
                family: intent.family,
                workflowKey: intent.workflowKey,
                txCbor: bytes,
                isOwnOutput,
                contentRef: intent.contentRef ?? null,
              }),
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
      return { kind: result.kind };
    }
    return yield* refusalOf(result, txHash);
  });

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

/** A gate failure carried through the journal's transaction unchanged. */
class GateFailed<E> {
  readonly _tag = "GateFailed";
  constructor(readonly error: E) {}
}

/**
 * The journal over one node SQL client: the row and the caller's gate in one
 * transaction. A refusal is held for its family (`refusalHoldsOver`) until
 * that family's next success clears it, in the record's transaction.
 */
export const intentJournalOver = (
  sql: SqlClient.SqlClient,
  isOwnOutput: (output: OutputSummary) => boolean,
): IntentJournalService => {
  const held = refusalHoldsOver(sql);
  return {
    record: <E>(
      intent: SubmissionIntent,
      signedTxCbor: string,
      txHash: string,
      gate?: Effect.Effect<void, E>,
    ): Effect.Effect<RecordOutcome, IntentJournalRefused | E> => {
      if (intent.kind === "unjournaled")
        return submitUnjournaled(
          intent.reason,
          intent.workflowKey,
          txHash,
        ).pipe(Effect.zipLeft(gate ?? Effect.void));
      const inTransaction: Effect.Effect<
        RecordOutcome,
        IntentJournalRefused | GateFailed<E> | SqlError.SqlError
      > = sql.withTransaction(
        recordSignedIntent(intent, signedTxCbor, txHash, isOwnOutput).pipe(
          Effect.provideService(SqlClient.SqlClient, sql),
          Effect.zipLeft(
            (gate ?? Effect.void).pipe(
              Effect.mapError((error) => new GateFailed(error)),
            ),
          ),
          Effect.zipLeft(held.clearIn(intent.family)),
        ),
      );
      return inTransaction.pipe(
        Effect.catchAll(
          (error): Effect.Effect<never, IntentJournalRefused | E> =>
            error instanceof GateFailed
              ? Effect.fail(error.error)
              : error instanceof IntentJournalRefused
                ? Effect.fail(error)
                : Effect.fail(
                    refusal(
                      INTENT_JOURNAL_UNAVAILABLE,
                      txHash,
                      `tx ${txHash} not submitted: the intent journal transaction failed: ${causeText(error)}`,
                    ),
                  ),
        ),
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
      );
    },
    holds: held.holds,
    refresh: held.refresh,
  };
};

/** The journal over the node database (`intentJournalOver`). */
export const makeIntentJournal = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const sql = yield* SqlClient.SqlClient;
  const own = new Set(nodeOwnWallets(config).map((a) => a.toString("hex")));
  return intentJournalOver(sql, (output) =>
    own.has(output.address.toString("hex")),
  );
});

export const IntentJournalLive = Layer.effect(IntentJournal, makeIntentJournal);

/**
 * The journal where no follower runs (see the module doc): every submission
 * goes out unjournaled and nothing is held.
 */
export const IntentJournalWithoutFollower = Layer.succeed(IntentJournal, {
  record: (intent, _signedTxCbor, txHash, gate) =>
    submitUnjournaled(
      intent.kind === "unjournaled" ? intent.reason : "no_follower",
      intent.kind === "unjournaled"
        ? intent.workflowKey
        : `${intent.family} ${intent.workflowKey}`,
      txHash,
    ).pipe(Effect.zipLeft(gate ?? Effect.void)),
  holds: () => [],
  refresh: () => Effect.void,
});
