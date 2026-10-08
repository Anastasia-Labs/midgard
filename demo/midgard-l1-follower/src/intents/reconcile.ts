import { isFinal } from "../heads.js";
import type { Dialect, SqlTx, TransactionMode } from "../sql/backend.js";
import type { Cursor } from "../types.js";
import { appendIntentEventIn, type Intent } from "./journal.js";
import {
  deriveIntentStatusesIn,
  type IntentState,
  type IntentStatus,
} from "./status.js";

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
  /** Live but the family predicate says it is no longer wanted. */
  | "abandon"
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
  inMempool: (intent: Intent) => Promise<boolean>;
  /** The §8.4 family predicate, read over projections: can it still land and is it still wanted. */
  wanted: (state: IntentState) => Promise<boolean>;
  /** LocalTxSubmission of the exact journaled bytes. A throw is transient. */
  submit: (intent: Intent) => Promise<SubmitOutcome>;
}>;

export type ReconciledIntent = Readonly<{
  intent: Intent;
  status: IntentStatus;
  action: ReconcileAction;
  /** A transient failure of the mempool read, predicate or submission. */
  error?: string;
}>;

export type ReconcileReport = Readonly<{
  cursor: Cursor | null;
  intents: readonly ReconciledIntent[];
}>;

export type IntentReconciler = Readonly<{
  /** One S6 pass: run it on every head change and every generation change. */
  reconcile(): Promise<ReconcileReport>;
}>;

const tipKey = (cursor: Cursor | null): string =>
  cursor === null
    ? "none"
    : `${cursor.generation}:${cursor.point.hash.toString("hex")}`;

const errorText = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * S6 (§8.3): derives every intent's status from the facts, then waits,
 * resubmits the exact bytes (at most once per tip), or abandons a live
 * intent whose predicate no longer holds. A dead intent is never sent.
 */
export const createIntentReconciler = (
  options: IntentReconcilerOptions,
): IntentReconciler => {
  const { dialect } = options;
  /** The tip each intent was last sent at (in memory: a restart sends once more). */
  const attempted = new Map<string, string>();
  return {
    reconcile: async () => {
      const { cursor, states } = await options.transaction("read", (tx) =>
        deriveIntentStatusesIn(tx, dialect),
      );
      const tip = tipKey(cursor);
      const tipSlot = cursor?.point.slot ?? null;
      const live = new Set(
        states
          .filter((state) => state.status.kind === "live")
          .map((state) => state.intent.txHash.toString("hex")),
      );
      for (const key of [...attempted.keys()])
        if (!live.has(key)) attempted.delete(key);
      const report: ReconciledIntent[] = [];
      for (const state of states) {
        const { intent, status } = state;
        const settled = settledAction(status, options.securityParameter);
        if (settled !== null || status.kind !== "live") {
          report.push({ intent, status, action: settled ?? "dead" });
          continue;
        }
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
          if (action === "abandon")
            await options.transaction("write", (tx) =>
              appendIntentEventIn(tx, dialect, intent.txHash, "abandoned", {
                detail: { reason: "family_predicate_false" },
                tipSlot,
              }),
            );
          if (action === "resubmit") {
            attempted.set(key, tip);
            await options.transaction("write", (tx) =>
              appendIntentEventIn(
                tx,
                dialect,
                intent.txHash,
                "submit_attempt",
                {
                  detail: { generation: cursor?.generation ?? null },
                  tipSlot,
                },
              ),
            );
            const outcome = await options.submit(intent);
            if (outcome.kind === "rejected")
              await options.transaction("write", (tx) =>
                appendIntentEventIn(
                  tx,
                  dialect,
                  intent.txHash,
                  "submit_rejected",
                  { detail: { rejection: outcome.detail }, tipSlot },
                ),
              );
          }
          report.push({ intent, status, action });
        } catch (error) {
          report.push({
            intent,
            status,
            action: "wait_transient",
            error: errorText(error),
          });
        }
      }
      return { cursor, intents: report };
    },
  };
};
