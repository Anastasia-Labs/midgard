/** The actions of one run that did not build. A family's preflight that
 * cannot build an action, for a reason the run cannot repair by itself,
 * throws a `WorkflowActionUnbuiltError`. The run then observes again with
 * that action skipped, so a move the family ranks lower still goes in the
 * same run, and returns every such action as an alarm on its result, even
 * when another move landed. A wallet-only action the restart wait holds is
 * skipped the same way, with no alarm: once nothing else is left the run
 * ends in the restart wait's named state. */
import { WorkflowActionChangedError } from "./action-changed.js";
import type { FraudProofWorkflowIdentity } from "./journal.js";
import type { FraudProofWorkflowObservation } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  type FraudProofWorkflowAlarm,
  type FraudProofWorkflowNamedState,
  type FraudProofWorkflowRunResult,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import { threadReclaimNamedState } from "./own-thread-reclaim.js";
import { faultProofRestartWaitNamedState } from "./wallet-only-send-gate.js";

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
   * Skips the wallet-only action `actionId` for the rest of the run while
   * the restart wait holds it. It raises no alarm: the wait opens by itself.
   */
  hold: (actionId: string) => void;
  /**
   * The first action the restart wait held, once `observation` has nothing
   * else for the run to do: a skipped or held action again, or a wait.
   */
  waiting: (observation: FraudProofWorkflowObservation) => string | undefined;
  /** Whether the restart wait holds an action of the run. */
  holding: () => boolean;
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
  const waits: string[] = [];
  const idle = (observation: FraudProofWorkflowObservation) =>
    observation.kind === "pending" ||
    (observation.kind === "action_required" &&
      ids.has(observation.action.actionId));
  return Object.freeze({
    ids,
    hold: (actionId) => {
      ids.add(actionId);
      waits.push(actionId);
    },
    waiting: (observation) =>
      waits.length > 0 && idle(observation) ? waits[0] : undefined,
    holding: () => waits.length > 0,
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
      alarms.length > 0 && idle(observation) ? alarms[0]!.reason : undefined,
    field: () =>
      alarms.length === 0 ? {} : { alarms: Object.freeze([...alarms]) },
  });
};

/**
 * The results a run hands back before it ends: a stall; a pending wait,
 * naming the action whose transaction it awaits when it awaits one; and a
 * pending run that resumes on the next observation. Each carries the run's alarms, and a pending one the restart
 * wait's named state while that holds an action of the run.
 */
export const workflowRunOutcomes = ({
  workflowId,
  identity,
  unbuilt,
}: {
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly unbuilt: UnbuiltWorkflowActions;
}) => {
  const result = (kind: "pending" | "stalled", reason: string) => ({
    kind,
    workflowId,
    identity,
    reason,
    ...unbuilt.field(),
  });
  // While the restart wait holds an action of the run, a pending result
  // names it, whatever else the run left in flight.
  const waiting = () =>
    unbuilt.holding() ? { namedState: faultProofRestartWaitNamedState } : {};
  // A thread of ours left standing on a removed header that the run cannot
  // reclaim yet: held under the operator's alarm, resumed on observation.
  const named = (reason: string): FraudProofWorkflowRunResult | undefined => {
    const namedState = threadReclaimNamedState(reason);
    return namedState === undefined
      ? undefined
      : {
          ...result("pending", reason),
          resumeOnObservation: true,
          namedState,
        };
  };
  return Object.freeze({
    stalled: (
      reason: string,
      phase?: "preflight",
    ): FraudProofWorkflowRunResult => ({
      ...result("stalled", reason),
      ...(phase === undefined ? {} : { phase }),
    }),
    pending: (
      reason: string,
      awaitingActionId?: string,
    ): FraudProofWorkflowRunResult =>
      named(reason) ?? {
        ...result("pending", reason),
        ...(awaitingActionId === undefined ? {} : { awaitingActionId }),
        ...waiting(),
      },
    resumeOnObservation: (
      reason: string,
      namedState?: FraudProofWorkflowNamedState,
    ): FraudProofWorkflowRunResult =>
      named(reason) ?? {
        ...result("pending", reason),
        resumeOnObservation: true,
        ...(namedState === undefined ? waiting() : { namedState }),
      },
  });
};
