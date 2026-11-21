import { formatUnknownError } from "@al-ft/midgard-core";
import type { WorkflowActuationPermit } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { FraudProofWorkflowTerminal } from "@al-ft/midgard-fault-proofs/workflow/journal";

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
  input: Completion,
) => Promise<unknown>;

/** Whether the process's proof-materials views hold materials a sweep may
 * take now: `unknown` while no view can say (none open). */
export type WatcherProofMaterialsSweepable = () => Promise<
  "some" | "none" | "unknown"
>;

/**
 * One sweep-only run for each objective a departure completes from the
 * chain alone, after restart as before it: noted when the chain verifies
 * the completion and queued once the objective is completed, so its
 * proof-materials needs are released first. Sweeps run one at a time; a
 * failure is logged and never reopens the objective.
 *
 * A sweep that failed, or whose transaction was lost, is run again at a
 * completed pass at tip (`atTip`): the last completion swept is kept in
 * memory, and while no sweep is queued or running and the views report
 * sweepable materials, its sweep-only run is queued once more, one per
 * pass. A sweep in the node's mempool has taken its materials from the
 * views' answer, so it is not run twice. A pass that finds nothing
 * sweepable drops it; so does a sweep whose permit was revoked.
 */
export const createWatcherProofMaterialsSweeps = (input: {
  readonly sweep: WatcherProofMaterialsSweep | undefined;
  readonly sweepable?: WatcherProofMaterialsSweepable;
  readonly isRevoked?: (error: unknown) => boolean;
}) => {
  const verified = new Map<string, Completion>();
  const keyOf = ({ category, headerHash }: Objective) =>
    `${category}/${headerHash}`;
  // Objectives whose sweep is queued or running.
  const sweeping = new Set<string>();
  let last: Completion | undefined;
  let queue: Promise<void> = Promise.resolve();
  const enqueue = (
    sweep: WatcherProofMaterialsSweep,
    completion: Completion,
  ): void => {
    const { category, headerHash } = completion.job;
    const objective = { category, headerHash };
    sweeping.add(keyOf(objective));
    queue = queue.then(async () => {
      await new Promise((resolve) => setImmediate(resolve));
      try {
        await sweep(completion);
      } catch (cause) {
        if (input.isRevoked?.(cause) === true && last === completion)
          last = undefined;
        process.stderr.write(
          `watcher proof materials: sweep of ${keyOf(objective)} failed: ${formatUnknownError(cause)}\n`,
        );
      } finally {
        sweeping.delete(keyOf(objective));
      }
    });
  };
  return Object.freeze({
    verified: (completion: Completion): void => {
      if (input.sweep !== undefined)
        verified.set(keyOf(completion.job), completion);
    },
    completed: (completed: Objective): void => {
      const completion = verified.get(keyOf(completed));
      verified.delete(keyOf(completed));
      if (completion === undefined || input.sweep === undefined) return;
      last = completion;
      enqueue(input.sweep, completion);
    },
    /** At a completed pass at tip: the last sweep again, when it is due. */
    atTip: async (): Promise<void> => {
      const { sweep } = input;
      if (
        last === undefined ||
        sweep === undefined ||
        input.sweepable === undefined ||
        sweeping.size > 0
      )
        return;
      const retried = last;
      let found: Awaited<ReturnType<WatcherProofMaterialsSweepable>>;
      try {
        found = await input.sweepable();
      } catch {
        return; // read again at the next pass
      }
      if (last !== retried || sweeping.size > 0) return;
      if (found === "none") last = undefined;
      if (found === "some") enqueue(sweep, retried);
    },
    idle: async () => await queue,
  });
};
