import { parentPort, workerData } from "node:worker_threads";

const { makeCommitPlanningNotice } = await import(workerData.producerUrl);
parentPort.postMessage(makeCommitPlanningNotice(workerData.selection));
