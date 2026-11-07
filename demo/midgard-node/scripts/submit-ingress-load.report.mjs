// CPU readings and the report file for the submit-ingress harness.
//
// Node CPU comes from /proc: the main thread is the task whose id is the
// process id, so /proc/<pid>/task/<pid>/stat holds the event loop's own user
// and system time, apart from libuv, GC and worker threads. A node in a Docker
// container on the same Linux host is visible by its host pid
// (`docker inspect -f '{{.State.Pid}}' <container>`).
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

let clockTicksPerSecond;

/** The kernel clock tick (USER_HZ) /proc times are counted in. */
export const clockTicks = () => {
  if (clockTicksPerSecond === undefined) {
    try {
      clockTicksPerSecond = Number(
        execFileSync("getconf", ["CLK_TCK"], { encoding: "utf8" }).trim(),
      );
    } catch {
      clockTicksPerSecond = 100;
    }
    if (!(clockTicksPerSecond > 0)) clockTicksPerSecond = 100;
  }
  return clockTicksPerSecond;
};

/**
 * User and system ticks from a /proc stat line. The command name (field 2)
 * may hold spaces and parentheses, so fields are counted from its last `)`.
 */
export const parseProcStatTicks = (text) => {
  const fields = text
    .slice(text.lastIndexOf(")") + 2)
    .trim()
    .split(/\s+/u);
  // After the name: state is field 3, utime field 14, stime field 15.
  return { userTicks: Number(fields[11]), systemTicks: Number(fields[12]) };
};

const readTicks = (file) => {
  const { userTicks, systemTicks } = parseProcStatTicks(
    fs.readFileSync(file, "utf8"),
  );
  return userTicks + systemTicks;
};

/** One reading of a process's main-thread and whole-process CPU ticks. */
export const sampleProcessCpu = (pid, procRoot = "/proc") => ({
  atMs: Date.now(),
  mainThreadTicks: readTicks(
    path.join(procRoot, String(pid), "task", String(pid), "stat"),
  ),
  processTicks: readTicks(path.join(procRoot, String(pid), "stat")),
});

/**
 * CPU use between two readings as a share of one core: 1.0 is a core kept
 * busy for the whole interval.
 */
export const cpuShares = (before, after, ticksPerSecond = clockTicks()) => {
  const seconds = (after.atMs - before.atMs) / 1_000;
  const share = (ticks) => ticks / ticksPerSecond / seconds;
  return {
    seconds,
    mainThreadCores: share(after.mainThreadTicks - before.mainThreadTicks),
    processCores: share(after.processTicks - before.processTicks),
  };
};

/** Writes the run report as JSON and returns the path. */
export const writeReport = (file, report) => {
  fs.mkdirSync(path.dirname(path.resolve(file)), { recursive: true });
  fs.writeFileSync(
    file,
    `${JSON.stringify(
      {
        schema: "midgard-submit-ingress-load-report/v1",
        writtenAt: new Date().toISOString(),
        host: { cpus: os.cpus().length, platform: process.platform },
        ...report,
      },
      null,
      2,
    )}\n`,
  );
  return file;
};

/** A few lines a person reads after a run. */
export const summaryLines = (report) => {
  const load = report.load;
  const lines = [
    `offered ${load.offeredRatePerSecond}/s for ${(load.offeredMs / 1_000).toFixed(1)} s: sent ${load.sent}, completed ${load.completed}, not sent when due ${load.notSentWhenDue}`,
    `admitted ${load.admitted} (${load.admittedPerSecond.toFixed(1)}/s); latency p50 ${fmtMs(load.latencyMs.p50)} p99 ${fmtMs(load.latencyMs.p99)}; admitted p50 ${fmtMs(load.admittedLatencyMs.p50)} p99 ${fmtMs(load.admittedLatencyMs.p99)}`,
    `outcomes ${Object.entries(load.outcomes)
      .sort(([, left], [, right]) => right - left)
      .map(([label, count]) => `${label}=${count}`)
      .join(", ")}`,
  ];
  if (report.postgres !== undefined)
    lines.push(
      `postgres: admission rows ${report.postgres.admissionRowsPerSecond.toFixed(1)}/s, inserts ${report.postgres.tuplesInsertedPerSecond.toFixed(1)}/s, updates ${report.postgres.tuplesUpdatedPerSecond.toFixed(1)}/s, commits ${report.postgres.commitsPerSecond.toFixed(1)}/s`,
    );
  if (report.nodeCpu !== undefined)
    lines.push(
      `node cpu: main thread ${(report.nodeCpu.mainThreadCores * 100).toFixed(1)}% of a core, process ${(report.nodeCpu.processCores * 100).toFixed(1)}%`,
    );
  if (report.drainStub !== undefined)
    lines.push(
      `drain stub: marked ${report.drainStub.marked} terminal, ${report.drainStub.errors} errors`,
    );
  return lines;
};

const fmtMs = (value) => (value === null ? "-" : `${value.toFixed(1)} ms`);
