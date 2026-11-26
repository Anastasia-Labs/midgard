import os from "node:os";

import { counterDelta } from "./throughput-benchmark-utils.mjs";

export const readRuntimeMetadata = async () => ({
  nodeVersion: process.version,
  platform: process.platform,
  arch: process.arch,
  hostname: os.hostname(),
  cpuModel: os.cpus()[0]?.model ?? null,
  cpuCount: os.cpus().length,
  totalMemoryBytes: os.totalmem(),
  freeMemoryBytes: os.freemem(),
  loadAverage: os.loadavg(),
  pid: process.pid,
  argv: process.argv,
  env: {
    NODE_ENV: process.env.NODE_ENV ?? null,
  },
});

export const activeCursorCount = (cursors) =>
  cursors.filter(
    (cursor) =>
      !cursor.stopped &&
      cursor.nextIndex < (cursor.entry?.rowCount ?? cursor.chain.txs.length),
  ).length;

export const hasCounterActivity = (before, after) =>
  [
    "submit",
    "accept",
    "reject",
    "commitBlock",
    "commitBlockTx",
    "mergeBlock",
  ].some((key) => counterDelta(before, after, key) !== 0);
