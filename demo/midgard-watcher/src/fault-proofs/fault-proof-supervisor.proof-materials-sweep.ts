import { formatUnknownError } from "@al-ft/midgard-core";
import type { WorkflowActuationPermit } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type {
  WorkflowInFlight,
  WorkflowSends,
} from "@al-ft/midgard-fault-proofs/workflow/in-flight";
import type { FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs/workflow/journal";
import type { WalletOnlySend } from "@al-ft/midgard-fault-proofs/workflow/wallet-only-send-gate";

import type { WatcherFaultProofJob } from "./fault-proof-supervisor.validate-job.js";

/** The objective a departure completed, with the chain's completion and the
 * reconciliation permit it was verified under. */
type Completion = Readonly<{
  job: WatcherFaultProofJob;
  actuationPermit: WorkflowActuationPermit;
  terminal: FraudProofWorkflowTerminal;
}>;

type Objective = Readonly<{ category: string; headerHash: string }>;

export type WatcherProofMaterialsSweep = (
  input: Completion &
    Readonly<{ sends: WorkflowSends; walletOnlySend: WalletOnlySend }>,
) => Promise<unknown>;

/**
 * One sweep-only run for each objective a departure completes from the
 * chain alone, after restart as before it: noted when the chain verifies
 * the completion and queued once the objective is completed, so its
 * proof-materials needs are released first. Sweeps run one at a time; a
 * failure is logged and never reopens the objective, and each run's part of
 * the in-flight set goes with it (a sweep spends no wallet coin, and the
 * materials a dropped sweep leaves are the next ending's).
 */
export const createWatcherProofMaterialsSweeps = (input: {
  readonly sweep: WatcherProofMaterialsSweep | undefined;
  readonly inFlight: Pick<WorkflowInFlight, "forObjective" | "forget">;
  readonly walletOnlySend: () => WalletOnlySend;
}) => {
  const verified = new Map<string, Completion>();
  const keyOf = ({ category, headerHash }: Objective) =>
    `${category}/${headerHash}`;
  let queue: Promise<void> = Promise.resolve();
  return Object.freeze({
    verified: (completion: Completion): void => {
      if (input.sweep !== undefined)
        verified.set(keyOf(completion.job), completion);
    },
    completed: (completed: Objective): void => {
      const completion = verified.get(keyOf(completed));
      verified.delete(keyOf(completed));
      if (completion === undefined || input.sweep === undefined) return;
      const { sweep } = input;
      const { category, headerHash } = completion.job;
      const objective = { category, headerHash };
      queue = queue.then(async () => {
        await new Promise((resolve) => setImmediate(resolve));
        try {
          await sweep({
            ...completion,
            sends: input.inFlight.forObjective(objective),
            walletOnlySend: input.walletOnlySend(),
          });
        } catch (cause) {
          process.stderr.write(
            `watcher proof materials: sweep of ${keyOf(objective)} failed: ${formatUnknownError(cause)}\n`,
          );
        } finally {
          input.inFlight.forget(objective);
        }
      });
    },
    idle: async () => await queue,
  });
};
