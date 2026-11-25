// One node worker run that opened the shared L1 node transport, ended by the
// compiled production code that ends that worker: the commit worker's
// output post, or the settlement worker's runner around a failing program.
import { parentPort, workerData } from "node:worker_threads";

import { sharedL1NodeTransport } from "@al-ft/l1-node-transport";

const transport = sharedL1NodeTransport(workerData.node);
// A run that ends at once ends while its sidecar is still starting.
if (workerData.ends === "after-ready") await transport.whenReady();

if (workerData.worker === "commit") {
  const { postCommitWorkerOutput } = await import(workerData.compiled);
  await postCommitWorkerOutput(parentPort, { type: "NothingToCommitOutput" });
} else {
  const { runSettlementWorker } = await import(workerData.compiled);
  const { Effect } = await import("effect");
  await runSettlementWorker(
    parentPort,
    Effect.fail(new Error("the settlement program failed")),
  );
}
