/**
 * Drives one fault-proof step to completion with no journal, for the
 * diagnostic CLI: the builder is called, each prerequisite or continuation
 * transaction it returns is submitted and confirmed, and the builder is
 * called again until it returns the step itself, which is submitted last.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type ContinuationTurn,
  type FaultProofTurn,
  isContinuationTurn,
  isPrerequisiteTurn,
} from "./fault-proof-turn.js";
import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
  submitOutsideWorkflow,
} from "./workflow/transaction-boundary.js";

/**
 * Calls `build` with `args` until it returns something other than a turn,
 * submitting every turn it returns before then, and returns that result.
 *
 * Each call finds the carriage the earlier ones published by content. A
 * prerequisite is confirmed before the next call, since the CLI's provider
 * reads the ledger without the node's mempool. No carriage plan is given
 * more transactions than it declares: a builder that asks for another one
 * has not found its carriage live, and the loop stops with an error naming
 * the plan. A continuation is confirmed and its `next` arguments merged in.
 */
export const settleFaultProofTurns = async <
  Args extends { readonly lucid: LucidEvolution },
  Result,
>(
  build: (
    args: Args,
  ) => Promise<FaultProofTurn<Result | ContinuationTurn<Partial<Args>>>>,
  args: NoInfer<Args>,
): Promise<Exclude<Result, ContinuationTurn<unknown>>> => {
  let current: Args = args;
  const submittedByPlan = new Map<string, number>();
  for (;;) {
    const built = await build(current);
    if (isPrerequisiteTurn(built)) {
      const submitted = (submittedByPlan.get(built.plan.key) ?? 0) + 1;
      if (submitted > built.plan.transactions)
        throw new Error(
          `${built.label} would be transaction ${submitted.toString()} of carriage plan ${built.plan.key}, which declares ${built.plan.transactions.toString()}`,
        );
      submittedByPlan.set(built.plan.key, submitted);
      await submitOutsideWorkflow(args.lucid, built.transaction, {
        label: built.label,
        awaitConfirmation: true,
      });
      continue;
    }
    if (isContinuationTurn(built)) {
      await submitOutsideWorkflow(args.lucid, built.transaction, {
        label: built.label,
        awaitConfirmation: true,
      });
      current = { ...current, ...built.next };
      continue;
    }
    return built as Exclude<Result, ContinuationTurn<unknown>>;
  }
};

/**
 * Settles `build`'s turns (see {@link settleFaultProofTurns}) and submits the
 * step it then returns under `label`, waiting for confirmation only when
 * `awaitConfirmation` is set.
 */
export const submitFaultProofTurns = async <
  Args extends { readonly lucid: LucidEvolution },
  Step extends { readonly transaction: LocallyEvaluatedTransaction },
>(
  build: (
    args: Args,
  ) => Promise<FaultProofTurn<Step | ContinuationTurn<Partial<Args>>>>,
  args: NoInfer<Args>,
  {
    label,
    awaitConfirmation,
  }: { readonly label: string; readonly awaitConfirmation: boolean },
): Promise<
  OutsideWorkflowSubmission<Exclude<Step, ContinuationTurn<unknown>>>
> => {
  const { transaction, ...fields } = await settleFaultProofTurns(build, args);
  const txHash = await submitOutsideWorkflow(args.lucid, transaction, {
    label,
    awaitConfirmation,
  });
  return {
    txHash,
    ...fields,
    awaitedConfirmation: awaitConfirmation,
  } as unknown as OutsideWorkflowSubmission<
    Exclude<Step, ContinuationTurn<unknown>>
  >;
};
