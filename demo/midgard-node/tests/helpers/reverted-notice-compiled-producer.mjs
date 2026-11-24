import { parentPort, workerData } from "node:worker_threads";

const { mempoolLedgerRevertedNotice } = await import(workerData.codecUrl);
parentPort.postMessage(mempoolLedgerRevertedNotice(workerData.outRefs));
// Payloads no current producer builds, posted as a foreign worker would.
for (const payload of workerData.malformed)
  parentPort.postMessage({ type: "MempoolLedgerRevertedNotice", ...payload });
