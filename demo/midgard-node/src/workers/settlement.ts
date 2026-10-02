import { parentPort, workerData } from "node:worker_threads";

import { Effect } from "effect";

import type { SettlementWorkerData } from "../fibers/settlement.js";
import {
  settlementProgram,
  settlementWorkerLayer,
} from "../services/settlement.js";
import { runSettlementWorker } from "./settlement.run-settlement-worker.js";

if (parentPort !== null) {
  const port = parentPort;
  void runSettlementWorker(
    port,
    settlementProgram(
      (health) => port.postMessage(health),
      (workerData as SettlementWorkerData | undefined)?.ownerToken,
    ).pipe(Effect.provide(settlementWorkerLayer)),
  );
}
