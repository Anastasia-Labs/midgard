/**
 * The validation pool's import-check lanes under saturated admission: the
 * backlog lane keeps its reserved share of workers, and the import lane
 * waits until no other job does.
 */
import { afterEach, describe, expect, it } from "vitest";

import { type ImportCheckLane } from "../src/services/validation-pool.js";
import {
  blockOf,
  closePools,
  firstRuns,
  injectedEvaluator,
} from "./import-check.checker.fixture.js";

afterEach(() => closePools());

describe("validation pool import-check lanes", () => {
  it("runs the backlog lane at its reserved share while admission saturates the pool, and the import lane only once admission is idle", async () => {
    const SLOW_MS = 300;
    const [slow, fast] = await firstRuns(await blockOf(["accept", "accept"]));
    const { pool } = await injectedEvaluator(
      { mode: "accept", slowContexts: [slow!.contextCbor], slowMs: SLOW_MS },
      { size: 2, backlogShare: 1 },
    );
    const job = () =>
      ({ kind: "cek_verdict", jobId: pool.allocateJobId() }) as const;
    // Admission: two running, eight waiting.
    const admission = Array.from({ length: 10 }, () =>
      pool.submit({ ...job(), request: slow! }),
    );
    const startedAt = Date.now();
    const background = (lane: ImportCheckLane, request = fast!) =>
      Array.from({ length: 5 }, () =>
        pool
          .submitBackground(
            { ...job(), request },
            { killMs: 5_000, fresh: false, lane },
          )
          .then(() => ({
            atMs: Date.now() - startedAt,
            waiting: pool.stats().queueDepth,
          })),
      );
    const backlog = background("backlog", slow!);
    const imports = background("import");
    const backlogDone = await Promise.all(backlog);
    // One worker of two, one job at a time: about one job per SLOW_MS.
    for (const done of backlogDone) expect(done.waiting).toBeGreaterThan(0);
    expect(Math.max(...backlogDone.map((done) => done.atMs))).toBeLessThan(
      (5 + 3) * SLOW_MS,
    );
    const importDone = await Promise.all(imports);
    for (const done of importDone) expect(done.waiting).toBe(0);
    await Promise.all(admission);
  }, 60_000);
});
