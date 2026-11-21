import {
  assertWorkflowRunActuation,
  workflowRunIsReconciliationOnly,
} from "./actuation-permit.js";
import type { WorkflowAdapterRunnerInput } from "./adapters.js";
import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "./orchestrator.fraud-proof-workflow-run-result.js";
import type { WorkflowRun } from "./workflow-run.js";

/**
 * Longest a session may poll without progress before it hands its
 * turn back. An honest script-decode pending resolves within its validity
 * window (`SCRIPT_DECODE_VALIDITY_MS`, 15 min); this leaves 15 min over that
 * for the provider to show the outcome.
 */
export const WORKFLOW_CONTINUATION_QUIET_BOUND_MS = 30 * 60_000;

/** The same bound in 1 s polls, for a clock that does not advance. */
export const WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS = 1_800;

const POLL_MS = 1_000;

/**
 * The progress a pending result shows: the last action its run submitted. A
 * new one is progress; the same action again, however often its transaction
 * is rebuilt, is not.
 */
const awaitedAction = (result: object): string | undefined =>
  "awaitingActionId" in result && typeof result.awaitingActionId === "string"
    ? result.awaitingActionId
    : undefined;

/**
 * Keep one admitted runner session alive while its run is pending. A session
 * that submits no new action within the quiet bound returns its last pending
 * result to resume on the next observation, with a reason naming
 * `FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED`, so the caller can run other
 * work and raise an alarm; its later progress is reported through
 * `onProgress`, whose throws are ignored. It is a hold, never a refusal: an
 * action that is sent again and never lands, as under sustained congestion,
 * is held the same way and runs again on its next observation.
 */
export const continuePendingWorkflow = async <Result>({
  invocation,
  run,
  execute,
}: {
  readonly invocation: Pick<
    WorkflowAdapterRunnerInput,
    "deploymentFingerprint" | "category" | "headerHash" | "onProgress"
  >;
  readonly run: WorkflowRun;
  readonly execute: () => Promise<Result>;
}): Promise<Result> => {
  const awaited = new Set<string>();
  let quietSinceMs = Date.now();
  let quietPolls = 0;
  while (true) {
    assertWorkflowRunActuation({
      run,
      deploymentFingerprint: invocation.deploymentFingerprint,
      category: invocation.category,
      headerHash: invocation.headerHash,
      checkpoint: "workflow_resume",
    });
    const result = await execute();
    if (
      typeof result !== "object" ||
      result === null ||
      !("kind" in result) ||
      result.kind !== "pending" ||
      workflowRunIsReconciliationOnly(run) ||
      ("resumeOnObservation" in result && result.resumeOnObservation === true)
    ) {
      return result;
    }
    const action = awaitedAction(result);
    if (action !== undefined && !awaited.has(action)) {
      // The first awaited action only sets the session's baseline.
      if (awaited.size > 0) {
        try {
          invocation.onProgress?.();
        } catch {
          // A failing callback must not end the session.
        }
      }
      awaited.add(action);
      quietSinceMs = Date.now();
      quietPolls = 0;
    } else quietPolls += 1;
    const quietMs = Date.now() - quietSinceMs;
    if (
      quietMs >= WORKFLOW_CONTINUATION_QUIET_BOUND_MS ||
      quietPolls >= WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS
    ) {
      const last =
        "reason" in result && typeof result.reason === "string"
          ? result.reason
          : "unnamed";
      return {
        ...result,
        resumeOnObservation: true,
        reason: `${FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED}: no progress for ${quietMs.toString()} ms over ${quietPolls.toString()} polls; last pending: ${last}`,
      } as Result;
    }
    // Reuse the same bound run and permit.
    await new Promise<void>((resolve) => setTimeout(resolve, POLL_MS));
  }
};
