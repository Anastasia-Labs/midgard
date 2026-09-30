import { createHash } from "node:crypto";
import os from "node:os";

import { counterDelta } from "./throughput-benchmark-utils.mjs";

const cursorPositionDigest = (positions) =>
  createHash("sha256")
    .update(
      positions
        .map(({ chainIndex, nextIndex }) => `${chainIndex}|${nextIndex}`)
        .join("\n"),
    )
    .digest("hex");

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

export const summarizeCursorContinuity = (stage, checkpoint) => {
  const start = stage.cursorPositionsAtStart;
  const middle = checkpoint?.cursorPositions;
  const end = stage.cursorPositionsAtEnd;
  if (!Array.isArray(start) || !Array.isArray(middle) || !Array.isArray(end)) {
    return {
      passed: false,
      reason: "cursor position snapshot missing",
    };
  }
  if (start.length !== middle.length || middle.length !== end.length) {
    return {
      passed: false,
      reason: `cursor count changed start=${start.length} checkpoint=${middle.length} end=${end.length}`,
    };
  }
  for (let index = 0; index < start.length; index += 1) {
    const initial = start[index];
    const observed = middle[index];
    const final = end[index];
    if (
      initial.chainIndex !== observed.chainIndex ||
      observed.chainIndex !== final.chainIndex
    ) {
      return {
        passed: false,
        reason: `cursor chain order changed at ordinal=${index}`,
      };
    }
    if (
      initial.nextIndex > observed.nextIndex ||
      observed.nextIndex > final.nextIndex
    ) {
      return {
        passed: false,
        reason: `cursor regressed chain=${initial.chainIndex} start=${initial.nextIndex} checkpoint=${observed.nextIndex} end=${final.nextIndex}`,
      };
    }
  }
  const total = (positions) =>
    positions.reduce((sum, position) => sum + position.nextIndex, 0);
  return {
    passed: true,
    reason: null,
    cursorCount: start.length,
    startConsumedRows: total(start),
    checkpointConsumedRows: total(middle),
    endConsumedRows: total(end),
    startPositionsSha256: cursorPositionDigest(start),
    checkpointPositionsSha256: cursorPositionDigest(middle),
    endPositionsSha256: cursorPositionDigest(end),
    checkpointMode: "observer_only_no_cursor_mutation",
  };
};

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
