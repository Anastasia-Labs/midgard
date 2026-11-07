import { Effect } from "effect";

import type { SettlementHealth } from "../services/settlement.js";
import { settlementCauseDetail } from "../services/settlement-call.js";

/** Where a settlement worker run posts its health reports: the worker
 * thread's parent port. */
export type SettlementWorkerPort = {
  readonly postMessage: (health: SettlementHealth) => void;
  readonly close: () => void;
};

/** Runs one settlement worker program to its end. A failed program posts its
 * cause as a last 'error' report, naming each failing call and what the
 * provider answered, and exits the thread with code 1; the node's supervisor
 * reports the death with that error and counts it towards its failure
 * streak. */
export const runSettlementWorker = (
  port: SettlementWorkerPort,
  program: Effect.Effect<unknown, unknown>,
): Promise<void> =>
  Effect.runPromise(
    program.pipe(
      Effect.asVoid,
      Effect.catchAllCause((cause) =>
        Effect.sync(() => {
          port.postMessage({
            observedAt: Date.now(),
            state: "error",
            detail: settlementCauseDetail(cause),
          });
          process.exitCode = 1;
          port.close();
        }),
      ),
    ),
  );
