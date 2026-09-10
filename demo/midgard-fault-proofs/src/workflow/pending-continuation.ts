import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import type { WorkflowAdapterReadinessInput } from "./adapters.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";

/** Keep one admitted runner session alive while its durable intent is pending. */
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
      result.kind !== "pending"
    ) {
      return result;
    }
    // Reuse the same bound journal and permits. A fresh top-level invocation
    // would rebind funding authority and may try to resolve spent inputs.
    mode = "resume";
    await new Promise<void>((resolve) => setTimeout(resolve, 1_000));
  }
};
