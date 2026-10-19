import {
  assertWorkflowJournalActuation,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import type { WorkflowAdapterRunnerInput } from "./adapters.js";
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

type JournalEvent = Readonly<Record<string, unknown>>;

const eventOf = (entry: unknown): JournalEvent | undefined =>
  typeof entry === "object" &&
  entry !== null &&
  "event" in entry &&
  typeof entry.event === "object" &&
  entry.event !== null
    ? (entry.event as JournalEvent)
    : undefined;

/**
 * Journaled progress of a result: the number of distinct milestones its
 * entries reach. Every confirmation is one, and so is every entry that names
 * no action. Everything else an action journals (its preflight, intents,
 * submissions, pending or missing reconciliations and re-observations) counts
 * once for that action however often it repeats, and a retired attempt counts
 * for nothing: resubmitting an action whose last attempt never landed is not
 * progress. Absent entries are none.
 */
const journaledProgress = (result: object): number => {
  if (!("entries" in result) || !Array.isArray(result.entries)) return -1;
  const milestones = new Set<string>();
  result.entries.forEach((entry: unknown, index) => {
    const event = eventOf(entry);
    const kind = event?.kind;
    if (kind === "signed_attempt_retired") return;
    const confirmed =
      kind === "confirmed" ||
      (kind === "reconciled" && event?.outcome === "confirmed");
    milestones.add(
      typeof event?.actionId === "string" && !confirmed
        ? `action:${event.actionId}`
        : `entry:${index.toString()}`,
    );
  });
  return milestones.size;
};

/**
 * Keep one admitted runner session alive while its durable intent is pending.
 * A session that journals no progress within the quiet bound returns its last
 * pending result to resume on the next observation, with a reason naming
 * `FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED`, so the caller can run other
 * work and raise an alarm; its later progress is reported through
 * `onProgress`. It is a hold, never a refusal: an action that is
 * resubmitted every validity window and never lands, as under sustained
 * congestion, is held the same way and runs again on its next observation.
 */
export const continuePendingWorkflow = async <Result>({
  invocation,
  journal,
  execute,
}: {
  readonly invocation: Pick<
    WorkflowAdapterRunnerInput,
    "mode" | "deploymentFingerprint" | "category" | "headerHash" | "onProgress"
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
      // The first poll only sets the session's baseline.
      if (progress >= 0) invocation.onProgress?.();
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
