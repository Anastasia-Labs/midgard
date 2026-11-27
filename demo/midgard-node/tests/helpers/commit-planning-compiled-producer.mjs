import { parentPort, workerData } from "node:worker_threads";

import { Effect } from "effect";

const { notifyCommitPlanningSelection } = await import(workerData.notifierUrl);
const { deserializeStateQueueUTxO } = await import(workerData.parentCodecUrl);
const parent = await Effect.runPromise(
  deserializeStateQueueUTxO(workerData.input.parent),
);
await Effect.runPromise(
  notifyCommitPlanningSelection({
    ...workerData.input,
    parent,
    notify: (notice) => Effect.sync(() => parentPort.postMessage(notice)),
  }),
);
