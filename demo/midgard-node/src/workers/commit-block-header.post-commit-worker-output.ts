import { closeSharedL1NodeTransports } from "@al-ft/l1-node-transport";
import { Effect } from "effect";

import type { WorkerOutput } from "./utils/commit-block-header.js";

/**
 * Ends a commit worker run: logs its output and posts it to the parent, which
 * terminates the thread on it. The run's L1 node transport is closed first.
 * Its sidecar is a child of this thread, so nothing collects its exit once
 * the thread is gone: one left running would stay a defunct child of the
 * node for every run.
 */
export const postCommitWorkerOutput = async (
  port: { readonly postMessage: (output: WorkerOutput) => void },
  output: WorkerOutput,
): Promise<void> => {
  // The parent reports a failure as `commit_worker_failed` (one warning, an
  // error once escalated, its first line on /readyz), so a failure that
  // repeats every tick logs here at debug.
  const log =
    output.type === "FailureOutput" ? Effect.logDebug : Effect.logInfo;
  const printed = JSON.stringify(output, (_key, value: unknown) =>
    value instanceof Uint8Array ? Buffer.from(value).toString("hex") : value,
  );
  Effect.runSync(log(`👷 Block commitment work completed (${printed}).`));
  await closeSharedL1NodeTransports();
  port.postMessage(output);
};
