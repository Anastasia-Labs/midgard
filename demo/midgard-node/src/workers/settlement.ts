import { parentPort } from "node:worker_threads";

import { Cause, Effect } from "effect";

import {
  settlementProgram,
  settlementWorkerLayer,
} from "../services/settlement.js";

if (parentPort !== null) {
  const port = parentPort;
  void Effect.runPromise(
    settlementProgram((health) => port.postMessage(health)).pipe(
      Effect.provide(settlementWorkerLayer),
      Effect.catchAllCause((cause) =>
        Effect.sync(() => {
          port.postMessage({
            observedAt: Date.now(),
            state: "error",
            detail: Cause.pretty(cause).slice(0, 2000),
          });
          process.exitCode = 1;
          port.close();
        }),
      ),
    ),
  );
}
