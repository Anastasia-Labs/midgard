import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import { makeConfirmationWorkerRunner } from "../src/fibers/block-confirmation.run-confirmation-worker-in-thread.js";

const workerFromSource = (source: string) =>
  new URL(`data:text/javascript,${encodeURIComponent(source)}`);

// Builds a few MiB of garbage, like a confirmation's provider reads, then
// reports that there was nothing to confirm.
const confirmingWorker = workerFromSource(`
  import { parentPort } from "node:worker_threads";
  const rows = Array.from({ length: 50_000 }, (_, i) => ({ i, s: "x".repeat(64) }));
  parentPort.postMessage({ type: "NoTxForConfirmationOutput", rows: rows.length });
`);

// Allocates past any small heap cap.
const runawayWorker = workerFromSource(`
  const hoard = [];
  for (;;) hoard.push(new Array(1_000_000).fill(hoard.length));
`);

const input = { data: { firstRun: false, pendingBlock: null } } as never;
const MiB = 1024 * 1024;

// A harness check of the runner's spawn, message and terminate path only:
// the stub worker loads no Lucid, wasm or database pool, so this cannot see a
// per-spawn cost of the real confirmation worker's module graph.
describe("confirmation worker runner soak", () => {
  it("keeps parent RSS and heap bounded across many stub worker spawns", async () => {
    const runner = makeConfirmationWorkerRunner({
      workerEntry: confirmingWorker,
    });
    const runMany = (count: number) =>
      Effect.runPromise(
        Effect.forEach(Array.from({ length: count }), () => runner(input), {
          discard: true,
        }),
      );
    await runMany(10);
    global.gc?.();
    const before = process.memoryUsage();
    await runMany(80);
    global.gc?.();
    const after = process.memoryUsage();
    const rssGrowthMiB = (after.rss - before.rss) / MiB;
    const heapGrowthMiB = (after.heapUsed - before.heapUsed) / MiB;
    expect(rssGrowthMiB).toBeLessThan(96);
    expect(heapGrowthMiB).toBeLessThan(24);
  }, 240_000);

  it("fails a worker that outgrows its heap cap instead of growing the process", async () => {
    const exit = await Effect.runPromise(
      makeConfirmationWorkerRunner({
        workerEntry: runawayWorker,
        heapMb: 32,
      })(input).pipe(Effect.exit),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    if (Exit.isFailure(exit)) {
      expect(String(exit.cause)).toMatch(/ERR_WORKER_OUT_OF_MEMORY|memory/i);
    }
  }, 60_000);
});
