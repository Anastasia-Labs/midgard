/**
 * What one call of a fault-proof builder returns.
 *
 * A step that reads published carriage (field chunks, a §8.6 certificate, a
 * field grammar certificate) cannot be built until that carriage is live. A
 * builder that finds some of it missing builds and signs the next
 * transaction that creates it, a {@link PrerequisiteTurn}, and stops there;
 * otherwise it builds the step itself. Each call builds and signs at most one
 * transaction. Once the provider shows that transaction's output, the next
 * call finds it by content and moves on.
 */
import type { LocallyEvaluatedTransaction } from "./workflow/transaction-boundary.js";

/** A transaction a step needs confirmed before the step can be built. */
export type PrerequisiteTurn = Readonly<{
  kind: "publication" | "certificate";
  transaction: LocallyEvaluatedTransaction;
  /** Names the transaction in a submission refusal. */
  label: string;
  /**
   * The carriage plan the transaction belongs to, by a key unique to that
   * plan, and how many transactions the plan needs in all.
   */
  plan: Readonly<{ key: string; transactions: number }>;
}>;

/** A builder's result: the next prerequisite transaction, or `Step`. */
export type FaultProofTurn<Step> = PrerequisiteTurn | Step;

/** Whether a builder's result is a prerequisite rather than its own result. */
export const isPrerequisiteTurn = <Other>(
  value: PrerequisiteTurn | Other,
): value is PrerequisiteTurn =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  (value.kind === "publication" || value.kind === "certificate") &&
  "plan" in value;

/**
 * A transaction that advances a step built over several transactions, such
 * as a router hop or a checkpoint of a computation thread. Once it is
 * confirmed, the same builder is called again with `next` merged into its
 * arguments and builds the following transaction.
 */
export type ContinuationTurn<Next> = Readonly<{
  kind: "continuation";
  transaction: LocallyEvaluatedTransaction;
  /** Names the transaction in a submission refusal. */
  label: string;
  next: Next;
}>;

/** Whether a builder's result advances its step rather than completing it. */
export const isContinuationTurn = <Next, Other>(
  value: ContinuationTurn<Next> | Other,
): value is ContinuationTurn<Next> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  value.kind === "continuation" &&
  "next" in value;
