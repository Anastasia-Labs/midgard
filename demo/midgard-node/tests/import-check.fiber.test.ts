/**
 * The import checker's fiber: its sweep runs on its own, so an import pass
 * held by a run that never completes does not hold the sweep's alarms.
 */
import { Effect, Fiber, Metric, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import { importCheckAlarmCounter } from "../src/import-check/alarms.js";
import { importCheckFiber } from "../src/import-check/fiber.js";
import {
  ValidationPool,
  type ValidationPoolService,
} from "../src/services/validation-pool.js";
import {
  blockOf,
  db,
  processBlock,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";

useCheckerDatabase();

/** A pool whose import-check runs start and never complete. */
const hangingPool = {
  evaluateImportCheck: () => Effect.never,
  importCheckBacklogShare: 1,
} as unknown as ValidationPoolService;

const missedCount = Metric.value(
  Metric.tagged(importCheckAlarmCounter, "alarm", "unverified_deadline_missed"),
).pipe(Effect.map((state) => BigInt(state.count)));

describe("import checker fiber", () => {
  it("raises the missed alarm while the import pass waits on a run that never completes", async () => {
    const block = await blockOf(["accept"]);
    await processBlock(block, { deadlineMs: Date.now() + 2_000 });
    const raised = await db(
      Effect.gen(function* () {
        const before = yield* missedCount;
        const fiber = yield* Effect.fork(
          Effect.provideService(importCheckFiber, ValidationPool, hangingPool),
        );
        const after = yield* missedCount.pipe(
          Effect.filterOrFail((count) => count > before),
          Effect.retry(Schedule.spaced("100 millis")),
          Effect.timeoutTo({
            duration: "20 seconds",
            onSuccess: (count) => count,
            onTimeout: () => before,
          }),
        );
        yield* Fiber.interrupt(fiber);
        return after - before;
      }),
    );
    expect(raised).toBe(1n);
  }, 60_000);
});
