// A validation worker for the import checker's tests. It answers the pool's
// empty warm-up job like the production worker and handles each
// `cek_verdict` job by its context (`request.contextCbor`, hex), else by
// `workerData.mode`:
// - a context in `rejectContexts` is rejected, one in `acceptContexts`
//   accepted, and one in `slowContexts` accepted after `slowMs` of busy work;
// - "hang": never answers;
// - "crash": the worker throws;
// - "crash-even": the 1st, 3rd, 5th... job across all instances throws, every
//   other one is accepted;
// - "accept": accepted.
// With `workerData.faults`, "hang" and "crash" apply to the first `faults`
// jobs across all instances only; later jobs are accepted.
// `workerData.log` (an Int32Array over a SharedArrayBuffer) records, after a
// counter at index 0, the thread id of each `cek_verdict` job decided by
// `mode`. `workerData.startDelayMs` delays the worker's start by busy-waiting.
import { parentPort, threadId, workerData } from "node:worker_threads";

const {
  mode,
  log,
  faults,
  startDelayMs = 0,
  slowMs = 0,
  rejectContexts = [],
  acceptContexts = [],
  slowContexts = [],
} = workerData;
const busy = (ms) => {
  const startedAt = Date.now();
  while (Date.now() - startedAt < ms) {
    // Busy, as a worker loading its evaluator or running a script.
  }
};
busy(startDelayMs);

const cache = { size: 0, maxEntries: 0, hits: 0, misses: 0, evictions: 0 };

const answer = (jobId, result) =>
  parentPort.postMessage({ kind: "cek_verdict", jobId, result });
const accepted = { kind: "accepted", budget: { cpu: 0n, memory: 0n } };

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
  const context = Buffer.from(request.request.contextCbor).toString("hex");
  if (rejectContexts.includes(context))
    return answer(request.jobId, {
      kind: "script_invalid",
      detail: "injected rejection",
    });
  if (acceptContexts.includes(context)) return answer(request.jobId, accepted);
  if (slowContexts.includes(context)) {
    busy(slowMs);
    return answer(request.jobId, accepted);
  }
  const order = Atomics.add(log, 0, 1);
  Atomics.store(log, order + 1, threadId);
  const faulting = faults === undefined || order < faults;
  if (mode === "hang" && faulting) {
    for (;;) {
      // Never answers.
    }
  }
  if (
    (mode === "crash" && faulting) ||
    (mode === "crash-even" && order % 2 === 0)
  )
    throw new Error("injected worker fault");
  answer(request.jobId, accepted);
});
