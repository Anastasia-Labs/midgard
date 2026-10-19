import {
  assertWorkflowJournalActuation,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import type { WorkflowAdapterReadinessInput } from "./adapters.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "./orchestrator.fraud-proof-workflow-run-result.js";

/**
 * Longest a session may poll without journaled progress before it hands its
 * turn back. An honest script-decode pending resolves within its validity
 * window (15 min) plus provider grace (10 min); this leaves 5 min over that.
 */
export const WORKFLOW_CONTINUATION_QUIET_BOUND_MS = 30 * 60_000;

/** The same bound in 1 s polls, for a clock that does not advance. */
export const WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS = 1_800;

const POLL_MS = 1_000;

/**
 * Journaled progress of a result: its entries other than the pending
 * reconciliation every unconfirmed poll appends. Absent entries are none.
 */
const journaledProgress = (result: object): number =>
  "entries" in result && Array.isArray(result.entries)
    ? result.entries.filter(
        (entry: unknown) =>
          !(
            typeof entry === "object" &&
            entry !== null &&
            "event" in entry &&
            typeof entry.event === "object" &&
            entry.event !== null &&
            "kind" in entry.event &&
            entry.event.kind === "reconciled" &&
            "outcome" in entry.event &&
            entry.event.outcome === "pending"
          ),
      ).length
    : -1;

/**
 * Keep one admitted runner session alive while its durable intent is pending.
 * A session that journals no progress within the quiet bound returns its last
 * pending result to resume on the next observation, with a reason naming
 * `FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED`, so the caller can run other
 * work and raise an alarm. It is a hold, never a refusal.
 */
export const continuePendingWorkflow = async <Result>({
  invocation,
  journal,
  execute,
}: {
  readonly invocation: Pick<
    WorkflowAdapterReadinessInput,
    "mode" | "deploymentFingerprint" | "category" | "headerHash"
  >;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly execute: (mode: "run" | "resume") => Promise<Result>;
}): Promise<Result> => {
  let mode = invocation.mode;
  let progress = -1;
  let quietSinceMs = Date.now();
  let quietPolls = 0;
  while (true) {
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint: invocation.deploymentFingerprint,
      category: invocation.category,
      headerHash: invocation.headerHash,
      checkpoint: "workflow_resume",
    });
    const result = await execute(mode);
    if (
      typeof result !== "object" ||
      result === null ||
      !("kind" in result) ||
      result.kind !== "pending" ||
      workflowJournalIsReconciliationOnly(journal) ||
      ("resumeOnObservation" in result && result.resumeOnObservation === true)
    ) {
      return result;
    }
    const journaled = journaledProgress(result);
    if (journaled > progress) {
      progress = journaled;
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
        reason: `${FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED}: no journaled progress for ${quietMs.toString()} ms over ${quietPolls.toString()} polls; last pending: ${last}`,
      } as Result;
    }
    // Reuse the same bound journal and permits. A fresh top-level invocation
    // would rebind funding authority and may try to resolve spent inputs.
    mode = "resume";
    await new Promise<void>((resolve) => setTimeout(resolve, POLL_MS));
  }
};
