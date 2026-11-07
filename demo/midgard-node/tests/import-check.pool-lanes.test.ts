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
    const [slow, fast] = firstRuns(await blockOf(["accept", "accept"]));
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

  it("keeps a worker for admission while both lanes have a job (two workers, a backlog share of one)", async () => {
    const SLOW_MS = 1_000;
    const [slow, fast] = firstRuns(await blockOf(["accept", "accept"]));
    const { pool } = await injectedEvaluator(
      {
        mode: "hang",
        acceptContexts: [fast!.contextCbor],
        slowContexts: [slow!.contextCbor],
        slowMs: SLOW_MS,
      },
      { size: 2, backlogShare: 1 },
    );
    const job = () =>
      ({ kind: "cek_verdict", jobId: pool.allocateJobId() }) as const;
    const startedAt = Date.now();
    const background = (["import", "backlog"] as const).map((lane) =>
      pool
        .submitBackground(
          { ...job(), request: slow! },
          { killMs: 10_000, fresh: false, lane },
        )
        .then(() => Date.now() - startedAt),
    );
    await new Promise((resolve) => setTimeout(resolve, 50));
    const admittedAt = Date.now();
    await pool.submit({ ...job(), request: fast! });
    expect(Date.now() - admittedAt).toBeLessThan(SLOW_MS / 2);
    // One lane job at a time: the second ran after the first.
    const done = (await Promise.all(background)).sort((a, b) => a - b);
    expect(done[1]!).toBeGreaterThanOrEqual(2 * SLOW_MS);
  }, 60_000);

  it("counts a killed lane job's worker as its lane's until the replacement is free", async () => {
    const SLOW_MS = 1_000;
    const [hang, slow, fast] = firstRuns(
      await blockOf(["accept", "accept", "accept"]),
    );
    // A replacement worker takes longer to start than a slow job runs.
    const { pool } = await injectedEvaluator(
      {
        mode: "hang",
        startDelayMs: 1_500,
        acceptContexts: [fast!.contextCbor],
        slowContexts: [slow!.contextCbor],
        slowMs: SLOW_MS,
      },
      { size: 2, backlogShare: 1 },
    );
    const job = () =>
      ({ kind: "cek_verdict", jobId: pool.allocateJobId() }) as const;
    await expect(
      pool.submitBackground(
        { ...job(), request: hang! },
        { killMs: 100, fresh: false, lane: "import" },
      ),
    ).rejects.toThrow();
    // Its worker is being replaced: the next import job waits for it, and
    // admission keeps the other worker.
    const next = pool.submitBackground(
      { ...job(), request: slow! },
      { killMs: 10_000, fresh: false, lane: "import" },
    );
    const admittedAt = Date.now();
    await pool.submit({ ...job(), request: fast! });
    expect(Date.now() - admittedAt).toBeLessThan(SLOW_MS / 2);
    await next;
  }, 60_000);

  it("reserves every worker but one for the backlog when as many or more are requested", async () => {
    for (const [size, requested] of [
      [2, 2],
      [3, 3],
      [3, 8],
    ] as const) {
      const { evaluator } = await injectedEvaluator(
        { mode: "accept" },
        { size, backlogShare: requested },
      );
      expect(evaluator.backlogShare).toBe(size - 1);
    }
  }, 60_000);
});
