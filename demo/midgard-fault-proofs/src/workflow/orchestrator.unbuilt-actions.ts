/** The actions of one run that did not build. A family's preflight that
 * cannot build an action, for a reason the run cannot repair by itself,
 * throws a `WorkflowActionUnbuiltError`. The run then observes again with
 * that action skipped, so a move the family ranks lower still goes in the
 * same run, and returns every such action as an alarm on its result, even
 * when another move landed. */
import { WorkflowActionChangedError } from "./action-changed.js";
import type {
  FraudProofWorkflowIdentity,
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.js";
import type { FraudProofWorkflowObservation } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  type FraudProofWorkflowAlarm,
  type FraudProofWorkflowRunResult,
  journalRepeatsStall,
} from "./orchestrator.fraud-proof-workflow-run-result.js";

/** An action of the run that does not build; `message` is the alarm. */
export class WorkflowActionUnbuiltError extends WorkflowActionChangedError {
  readonly _tag = "WorkflowActionUnbuiltError";

  constructor(
    readonly actionId: string,
    message: string,
  ) {
    super(message);
    this.name = "WorkflowActionUnbuiltError";
  }
}

/** Matched by tag: the package ships more than one entry bundle. */
export const isWorkflowActionUnbuiltError = (
  error: unknown,
): error is WorkflowActionUnbuiltError =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag ===
    "WorkflowActionUnbuiltError" &&
  typeof (error as { readonly actionId?: unknown }).actionId === "string";

export type UnbuiltWorkflowActions = Readonly<{
  /** The ids of the actions skipped for the rest of the run. */
  ids: ReadonlySet<string>;
  /** Records `cause` when it is an unbuilt action, and says whether it was. */
  skip: (cause: unknown) => boolean;
  /**
   * The alarm a run with unbuilt actions stops on once `observation` has
   * nothing else for it to do: a skipped action again, or a wait.
   */
  held: (observation: FraudProofWorkflowObservation) => string | undefined;
  /** The `alarms` field of a result, absent while no action was skipped. */
  field: () => Readonly<{ alarms?: readonly FraudProofWorkflowAlarm[] }>;
}>;

export const createUnbuiltWorkflowActions = (): UnbuiltWorkflowActions => {
  const ids = new Set<string>();
  const alarms: FraudProofWorkflowAlarm[] = [];
  return Object.freeze({
    ids,
    skip: (cause) => {
      if (!isWorkflowActionUnbuiltError(cause) || ids.has(cause.actionId))
        return false;
      ids.add(cause.actionId);
      alarms.push(
        Object.freeze({ actionId: cause.actionId, reason: cause.message }),
      );
      return true;
    },
    held: (observation) =>
      alarms.length > 0 &&
      (observation.kind === "pending" ||
        (observation.kind === "action_required" &&
          ids.has(observation.action.actionId)))
        ? alarms[0]!.reason
        : undefined,
    field: () =>
      alarms.length === 0 ? {} : { alarms: Object.freeze([...alarms]) },
  });
};

/**
 * The results a run hands back before it ends: a stall, journaled once per
 * cause; a pending wait; and a pending run that resumes on the next
 * observation. Each carries the run's alarms.
 */
export const workflowRunOutcomes = ({
  workflowId,
  identity,
  entries,
  append,
  unbuilt,
}: {
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly entries: () => readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<void>;
  readonly unbuilt: UnbuiltWorkflowActions;
}) => {
  const result = (kind: "pending" | "stalled", reason: string) => ({
    kind,
    workflowId,
    identity,
    reason,
    entries: entries(),
    ...unbuilt.field(),
  });
  return Object.freeze({
    stalled: async (
      reason: string,
      phase?: "preflight",
    ): Promise<FraudProofWorkflowRunResult> => {
      if (!journalRepeatsStall(entries(), reason))
        await append({ kind: "stalled", reason });
      return {
        ...result("stalled", reason),
        ...(phase === undefined ? {} : { phase }),
      };
    },
    pending: (reason: string): FraudProofWorkflowRunResult =>
      result("pending", reason),
    resumeOnObservation: (reason: string): FraudProofWorkflowRunResult => ({
      ...result("pending", reason),
      resumeOnObservation: true,
    }),
  });
};
