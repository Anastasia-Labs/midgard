import * as SDK from "@al-ft/midgard-sdk";

import {
  buildAuthenticatedMpfRootInWorker,
  closeMpfRootWorkers,
  configureMpfRootWorkers,
  mpfRootWorkerMetrics,
} from "./utils/mpf-root-pool.js";

const entryCount = Math.max(
  1,
  Number.parseInt(process.env.MPF_ROOT_PROBE_ENTRIES ?? "50000", 10),
);
const workerCount = Math.max(
  2,
  Number.parseInt(process.env.MPF_ROOT_PROBE_WORKERS ?? "2", 10),
);
const requestCount = Math.max(
  1,
  Number.parseInt(process.env.MPF_ROOT_PROBE_REQUESTS ?? "2", 10),
);
const sampleCount = Math.max(
  1,
  Number.parseInt(process.env.MPF_ROOT_PROBE_SAMPLES ?? "1", 10),
);
const entries = Array.from({ length: entryCount }, (_, index) => {
  const key = Buffer.alloc(32);
  key.writeUInt32BE(index, 28);
  const value = Buffer.alloc(64, index % 251);
  return { key, value };
});

configureMpfRootWorkers({
  enabled: true,
  workers: workerCount,
  minEntries: 1,
  timeoutMs: 300_000,
});

const run = async (): Promise<void> => {
try {
  const durationsMs: number[] = [];
  const domains = [
    SDK.ROOT_DOMAINS.transitionTrace,
    SDK.ROOT_DOMAINS.eventToStep,
  ] as const;
  let results: Awaited<ReturnType<typeof buildAuthenticatedMpfRootInWorker>>[] = [];
  for (let sample = 0; sample < sampleCount; sample += 1) {
    const startedAt = performance.now();
    results = await Promise.all(
      Array.from({ length: requestCount }, (_, index) =>
        buildAuthenticatedMpfRootInWorker(
          domains[index % domains.length]!,
          entries,
        ),
      ),
    );
    durationsMs.push(performance.now() - startedAt);
  }
  const sortedDurations = [...durationsMs].sort((left, right) => left - right);
  const p95Ms =
    sortedDurations[
      Math.max(0, Math.ceil(sortedDurations.length * 0.95) - 1)
    ]!;
  const metrics = mpfRootWorkerMetrics();
  if (requestCount >= 2 && metrics.maxActiveWorkers < 2) {
    throw new Error(
      `Expected at least two simultaneous MPF root workers, observed ${metrics.maxActiveWorkers.toString()}`,
    );
  }
  process.stdout.write(
    `${JSON.stringify({
      entryCount,
      workerCount,
      requestCount,
      sampleCount,
      durationsMs,
      p95Ms,
      roots: results.map((result) => result.rootHex),
      workerTimings: results.map((result) => result.timings),
      metrics,
    })}\n`,
  );
} finally {
  closeMpfRootWorkers();
}
};

void run().catch((error: unknown) => {
  process.stderr.write(`${error instanceof Error ? error.stack : String(error)}\n`);
  process.exitCode = 1;
});
