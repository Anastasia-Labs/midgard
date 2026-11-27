/**
 * One move per run, for a test that steps a dispute through each state its
 * moves leave. A run reads our node's mempool, so the orchestrator carries on
 * over its own move at once; these tests look at the thread between moves,
 * once each lands. Once a run has asked for a submission, its next
 * observation is pending under {@link ONE_MOVE_PER_RUN}, naming the move,
 * and the move lands with the block the runner adds after the run.
 */
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";

export const ONE_MOVE_PER_RUN =
  "one-move-per-run: the move waits for its block";

/** `adapter`, ending each run at the observation after its first submission;
 * `startRun` begins a run. */
export const oneMovePerRun = (
  adapter: FraudProofFamilyWorkflowAdapter,
): Readonly<{
  adapter: FraudProofFamilyWorkflowAdapter;
  startRun: () => void;
}> => {
  let moved: string | undefined;
  return Object.freeze({
    adapter: {
      ...adapter,
      observe: async (context) =>
        moved === undefined
          ? await adapter.observe(context)
          : { kind: "pending", reason: `${ONE_MOVE_PER_RUN}: ${moved}` },
      submit: async (input) => {
        moved = input.action.actionId;
        return await adapter.submit(input);
      },
    },
    startRun: () => {
      moved = undefined;
    },
  });
};
