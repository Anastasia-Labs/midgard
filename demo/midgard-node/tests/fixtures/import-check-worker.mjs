// A validation worker for the import checker's tests. It answers the pool's
// empty warm-up job like the production worker and handles every
// `cek_verdict` job by `workerData.mode`:
// - "hang": never answers;
// - "crash": the worker throws;
// - "crash-first": the first job across all instances throws, every later
//   one is accepted.
// `workerData.log` (an Int32Array over a SharedArrayBuffer) records, after a
// counter at index 0, the thread id of each `cek_verdict` job.
// `workerData.startDelayMs` delays the worker's start by busy-waiting.
import { parentPort, threadId, workerData } from "node:worker_threads";

const { mode, log, startDelayMs = 0 } = workerData;
const startedAt = Date.now();
while (Date.now() - startedAt < startDelayMs) {
  // Busy start, as a worker loading its evaluator.
}

const cache = { size: 0, maxEntries: 0, hits: 0, misses: 0, evictions: 0 };

parentPort.on("message", (request) => {
  if (request.kind === "phase_a") {
    parentPort.postMessage({
      kind: "phase_a",
      jobId: request.jobId,
      workerThreadId: threadId,
      publicKeyCache: cache,
      addressCache: cache,
      results: [],
    });
    return;
  }
  const order = Atomics.add(log, 0, 1);
  Atomics.store(log, order + 1, threadId);
  if (mode === "hang") {
    for (;;) {
      // Never answers.
    }
  }
  if (mode === "crash" || (mode === "crash-first" && order === 0))
    throw new Error("injected worker fault");
  parentPort.postMessage({
    kind: "cek_verdict",
    jobId: request.jobId,
    result: { kind: "accepted", budget: { cpu: 0n, memory: 0n } },
  });
});
