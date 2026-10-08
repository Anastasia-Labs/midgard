import { isFinal } from "../heads.js";
import type { Dialect, SqlTx, TransactionMode } from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { Cursor } from "../types.js";
import {
  appendIntentEventIn,
  type Intent,
  type IntentHead,
  readIntentIn,
} from "./journal.js";
import { createIntentStatesView } from "./reconcile.view.js";
import type { IntentState, IntentStatus } from "./status.js";

/**
 * What S6 does with one intent (§8.3). Only `resubmit` sends anything, and
 * it sends the journaled bytes; only `abandon` writes a status-relevant
 * event. Every other outcome follows from the facts with no write.
 */
export type ReconcileAction =
  /** Landed within k: derivations that depend on it proceed. */
  | "follow"
  /** Landed deeper than k: terminal, retention prunes it. */
  | "terminal"
  /** Conflicted by a journaled intent's spend: superseded, nothing to do. */
  | "superseded"
  /** Dead (foreign conflict, expired, dependency dead, abandoned, failed): never resubmitted. */
  | "dead"
  /** Live and in the node's mempool. */
  | "wait_in_mempool"
  /**
   * Live, but an input is not a live fact now (its creator rolled back): the
   * family predicate is not read, so a rollback alone never abandons.
   */
  | "wait_inputs"
  /** Live, wanted, already sent at this tip. */
  | "wait_attempted"
  /** Live, wanted, not in the mempool: send the exact bytes again. */
  | "resubmit"
  /**
   * Live but the family predicate says it is no longer wanted, or the
   * ledger refused its resend at `abandonAfterRejections` tips.
   */
  | "abandon"
  /**
   * Live, the predicate said abandon, but the tip moved (a new block or a
   * rewind) between the status read and the abandon write: nothing is
   * written, and the next pass (at the new tip) decides again.
   */
  | "wait_tip_moved"
  /** Live; the mempool read, predicate or submission failed this pass (`error`). */
  | "wait_transient";

/** The settled outcome of a state, before any mempool or predicate read. */
export const settledAction = (
  status: IntentStatus,
  securityParameter: number,
): Exclude<
  ReconcileAction,
  | "wait_in_mempool"
  | "wait_inputs"
  | "wait_attempted"
  | "resubmit"
  | "abandon"
  | "wait_tip_moved"
  | "wait_transient"
> | null => {
  switch (status.kind) {
    case "landed":
      return isFinal(status.depth, { securityParameter })
        ? "terminal"
        : "follow";
    case "conflicted":
      return status.ownSpender ? "superseded" : "dead";
    case "failed_landed":
    case "expired":
    case "dependency_dead":
    case "abandoned":
      return "dead";
    case "live":
      return null;
  }
};

/**
 * The action for a live intent, given what S6 read for it: whether it was
 * already sent at this tip, whether the node's mempool holds it (§8.3:
 * waiting wins), whether its inputs are live facts now, and the family
 * predicate (read only when it is not in the mempool and its inputs are
 * live: a predicate read at a view that lacks the intent's inputs says
 * nothing about whether it is still wanted).
 */
export const liveAction = (
  observed: Readonly<{
    inputsAvailable: boolean;
    attemptedAtTip: boolean;
    inMempool: boolean;
    wanted: boolean;
  }>,
): Extract<
  ReconcileAction,
  "wait_in_mempool" | "wait_inputs" | "wait_attempted" | "resubmit" | "abandon"
> => {
  if (observed.inMempool) return "wait_in_mempool";
  if (!observed.inputsAvailable) return "wait_inputs";
  if (!observed.wanted) return "abandon";
  if (observed.attemptedAtTip) return "wait_attempted";
  return "resubmit";
};

export type SubmitOutcome =
  | Readonly<{ kind: "accepted" }>
  /** The node's ledger refused the bytes; recorded, the status stays derived. */
  | Readonly<{ kind: "rejected"; detail: string }>;

export type IntentReconcilerOptions = Readonly<{
  dialect: Dialect;
  /** A transaction on the store holding the facts and the journal. */
  transaction: <T>(
    mode: TransactionMode,
    run: (tx: SqlTx) => Promise<T>,
  ) => Promise<T>;
  /** k, for `landed` deeper than k (terminal). */
  securityParameter: number;
  /** LocalTxMonitor `HasTx`. A throw ends this pass; the next pass retries. */
  inMempool: (intent: IntentHead) => Promise<boolean>;
  /** The §8.4 family predicate, read over projections: can it still land and is it still wanted. */
  wanted: (state: IntentState) => Promise<boolean>;
  /** LocalTxSubmission of the exact journaled bytes. A throw is transient. */
  submit: (intent: Intent) => Promise<SubmitOutcome>;
  /**
   * The ledger refusals after which a live intent is abandoned
   * (`ledger_rejected`): each at a distinct tip (block hash) at or past the
   * intent's lower validity bound. Counted in memory: a restart counts
   * again. Never abandons for refusals when omitted.
   */
  abandonAfterRejections?: number;
}>;

export type ReconciledIntent = Readonly<{
  intent: IntentHead;
  status: IntentStatus;
  action: ReconcileAction;
  /** A transient failure of the mempool read, predicate or submission. */
  error?: string;
  /** The node's ledger refused this pass's resubmission (its detail). */
  rejection?: string;
}>;

export type ReconcileReport = Readonly<{
  cursor: Cursor | null;
  /**
   * This pass's entries: every live intent, every intent whose state the
   * pass derived (recorded since the last pass, touched by the slots the
   * cursor moved over or by a rewind, or a dependant or dependency of one),
   * and every landed intent that became final. The first pass, and a pass
   * after a reset, derive and list every retained intent.
   */
  intents: readonly ReconciledIntent[];
  /**
   * Any intent's entry as of this pass: this pass's entry, else its settled
   * action at this pass's cursor; undefined when it is not retained. Valid
   * until the next pass.
   */
  entry(txHash: Buffer): ReconciledIntent | undefined;
  /** Every retained intent's entry as of this pass (reads the whole view). Valid until the next pass. */
  entries(): readonly ReconciledIntent[];
}>;

export type IntentReconciler = Readonly<{
  /** One S6 pass: run it on every head change and every generation change. */
  reconcile(): Promise<ReconcileReport>;
}>;

const tipKey = (cursor: Cursor | null): string =>
  cursor === null
    ? "none"
    : `${cursor.generation}:${cursor.point.hash.toString("hex")}`;

/** The tip's block, whatever the generation: a rewind back onto it is not a new tip. */
const pointKey = (cursor: Cursor): string => cursor.point.hash.toString("hex");

/** Whether a refusal at `cursor` counts toward `abandonAfterRejections`. */
const refusalCounts = (intent: IntentHead, cursor: Cursor): boolean =>
  intent.validFromSlot === null || cursor.point.slot >= intent.validFromSlot;

const errorText = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * S6 (§8.3): derives the intents' statuses from the facts, then waits,
 * resubmits the exact bytes (at most once per tip), or abandons a live
 * intent whose predicate no longer holds or whose resend the ledger
 * refused at `abandonAfterRejections` distinct tips at or past its lower
 * validity bound. A dead intent is never sent.
 * The abandon write re-reads the cursor and writes only at the tip the
 * statuses were derived at; the signed bytes are read only to send them.
 * The statuses are kept between passes (`createIntentStatesView`): a pass
 * reads what changed since the last one, so its work is bounded by the live
 * intents and what the new blocks touched, not by the retained journal.
 */
export const createIntentReconciler = (
  options: IntentReconcilerOptions,
): IntentReconciler => {
  const { dialect } = options;
  /** The tip each intent was last sent at (in memory: a restart sends once more). */
  const attempted = new Map<string, string>();
  /** The tips (block hashes) each live intent's resend was refused at, counted per `refusalCounts`. */
  const refusedAt = new Map<string, Set<string>>();
  const refusalsToAbandon = options.abandonAfterRejections ?? Infinity;
  const refusedEnough = (key: string): boolean =>
    (refusedAt.get(key)?.size ?? 0) >= refusalsToAbandon;
  const view = createIntentStatesView(dialect, options.securityParameter);
  let pass = 0;
  const settledEntry = (state: IntentState): ReconciledIntent => ({
    intent: state.intent,
    status: state.status,
    action: settledAction(state.status, options.securityParameter) ?? "dead",
  });

  const liveEntry = async (
    state: IntentState,
    cursor: Cursor | null,
    tip: string,
  ): Promise<ReconciledIntent> => {
    const { intent, status } = state;
    if (status.kind !== "live") return settledEntry(state);
    const tipSlot = cursor?.point.slot ?? null;
    const key = intent.txHash.toString("hex");
    try {
      const inMempool = await options.inMempool(intent);
      const wanted =
        inMempool || !status.inputsAvailable
          ? true
          : await options.wanted(state);
      const action = liveAction({
        inputsAvailable: status.inputsAvailable,
        attemptedAtTip: attempted.get(key) === tip,
        inMempool,
        wanted,
      });
      /** The abandon event, written only while the cursor is still at `tip`. */
      const abandon = (detail: Readonly<Record<string, unknown>>) =>
        options.transaction("write", async (tx) => {
          if (tipKey(await readCursor(tx, dialect, "share")) !== tip)
            return false;
          await appendIntentEventIn(tx, dialect, intent.txHash, "abandoned", {
            detail,
            tipSlot,
          });
          return true;
        });
      const refusedAbandon = () =>
        abandon({
          reason: "ledger_rejected",
          tips: refusedAt.get(key)?.size ?? 0,
        });
      if (action === "abandon") {
        if (!(await abandon({ reason: "family_predicate_false" })))
          return { intent, status, action: "wait_tip_moved" };
      }
      if (action === "resubmit" && refusedEnough(key))
        return {
          intent,
          status,
          action: (await refusedAbandon()) ? "abandon" : "wait_tip_moved",
        };
      if (action === "resubmit") {
        attempted.set(key, tip);
        const signed = await options.transaction("write", async (tx) => {
          await appendIntentEventIn(
            tx,
            dialect,
            intent.txHash,
            "submit_attempt",
            {
              detail: { generation: cursor?.generation ?? null },
              tipSlot,
            },
          );
          return readIntentIn(tx, dialect, intent.txHash);
        });
        if (signed === null)
          throw new Error("the intent was pruned during the pass");
        const outcome = await options.submit(signed);
        if (outcome.kind === "rejected") {
          await options.transaction("write", (tx) =>
            appendIntentEventIn(tx, dialect, intent.txHash, "submit_rejected", {
              detail: { rejection: outcome.detail },
              tipSlot,
            }),
          );
          if (cursor !== null && refusalCounts(intent, cursor))
            refusedAt.set(
              key,
              new Set([...(refusedAt.get(key) ?? []), pointKey(cursor)]),
            );
          return {
            intent,
            status,
            action:
              refusedEnough(key) && (await refusedAbandon())
                ? "abandon"
                : action,
            rejection: outcome.detail,
          };
        }
      }
      return { intent, status, action };
    } catch (error) {
      return {
        intent,
        status,
        action: "wait_transient",
        error: errorText(error),
      };
    }
  };

  return {
    reconcile: async () => {
      const { cursor, changed } = await options.transaction("read", (tx) =>
        view.advance(tx),
      );
      pass += 1;
      const at = pass;
      const tip = tipKey(cursor);
      const live = view.liveKeys();
      for (const key of [...attempted.keys()])
        if (!live.has(key)) attempted.delete(key);
      for (const key of [...refusedAt.keys()])
        if (!live.has(key)) refusedAt.delete(key);
      const entries = new Map<string, ReconciledIntent>();
      for (const state of changed)
        if (state.status.kind !== "live")
          entries.set(state.intent.txHash.toString("hex"), settledEntry(state));
      for (const key of live) {
        const state = view.state(Buffer.from(key, "hex"));
        if (state !== undefined)
          entries.set(key, await liveEntry(state, cursor, tip));
      }
      const current = (): void => {
        if (at !== pass)
          throw new Error("a later reconcile pass replaced this report");
      };
      return {
        cursor,
        intents: [...entries.values()],
        entry: (txHash) => {
          current();
          const key = txHash.toString("hex");
          const listed = entries.get(key);
          if (listed !== undefined) return listed;
          const state = view.state(txHash);
          return state === undefined ? undefined : settledEntry(state);
        },
        entries: () => {
          current();
          return view
            .states()
            .map(
              (state) =>
                entries.get(state.intent.txHash.toString("hex")) ??
                settledEntry(state),
            );
        },
      };
    },
  };
};
