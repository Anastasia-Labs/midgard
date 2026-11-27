const mib = (bytes: number) => Math.round((bytes / 2 ** 20) * 10) / 10;

/** Peak RSS and heap, sampled while `run` runs. */
export const sampled = async <T>(run: () => Promise<T>) => {
  const baseline = process.memoryUsage();
  let rss = baseline.rss;
  let heap = baseline.heapUsed;
  const sample = () => {
    const usage = process.memoryUsage();
    rss = Math.max(rss, usage.rss);
    heap = Math.max(heap, usage.heapUsed);
  };
  const timer = setInterval(sample, 20);
  const started = performance.now();
  try {
    const result = await run();
    sample();
    return {
      result,
      milliseconds: performance.now() - started,
      baselineRssMiB: mib(baseline.rss),
      baselineHeapMiB: mib(baseline.heapUsed),
      peakRssMiB: mib(rss),
      peakHeapMiB: mib(heap),
    };
  } finally {
    clearInterval(timer);
  }
};
