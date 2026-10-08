import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Fiber, Logger, Schedule } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import {
  type HeldLease,
  nextPredecessorLeaseWaitStep,
} from "../src/database/eventHistoryAuthority.predecessor-lease-wait.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// A node restarted after a hard kill finds its predecessor's history lease
// still live: nothing released it. Startup waits that lease out instead of
// crash-looping, and still never takes a lease another process keeps renewing:
// a renewal it observes refuses at once.

const deploymentIdentity = "aa".repeat(32);
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(program));
const acquire = (
  ownerToken: string,
  leaseDurationMs: number,
  deployment = deploymentIdentity,
) =>
  Authority.acquire({
    deploymentIdentity: deployment,
    ownerToken,
    leaseDurationMs,
  });
const withWait = (marginMs: number, pollIntervalMs: number) =>
  Effect.provideService(Authority.PredecessorLeaseWait, {
    marginMs,
    pollIntervalMs,
  });
const captureLogs = (logs: string[]) =>
  Effect.provide(
    Logger.replace(
      Logger.defaultLogger,
      Logger.make(({ logLevel, message }) => {
        logs.push(`${logLevel.label} ${[message].flat().join(" ")}`);
      }),
    ),
  );
const timed = <A, E, R>(program: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const started = performance.now();
    const exit = yield* Effect.exit(program);
    return { exit, elapsedMs: performance.now() - started };
  });
const failureText = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit) ? Cause.pretty(exit.cause) : "succeeded";
const readRow = Effect.map(Authority.retrieve, (row) =>
  row._tag === "Some" ? row.value : undefined,
);

beforeEach(async () => run(resetApplicationTables));

describe("history authority acquisition after a predecessor was killed", () => {
  it("waits out an unrenewed predecessor lease, logs the wait once, then claims the next generation", async () => {
    const predecessor = await run(acquire(randomUUID(), 2_000));
    const successorToken = randomUUID();
    const logs: string[] = [];
    const { exit, elapsedMs } = await run(
      timed(
        acquire(successorToken, 2_000).pipe(
          withWait(1_000, 200),
          captureLogs(logs),
        ),
      ),
    );
    expect(Exit.isSuccess(exit)).toBe(true);
    if (!Exit.isSuccess(exit)) return;
    expect(exit.value.ownerToken).toBe(successorToken);
    expect(exit.value.generation).toBe(
      (BigInt(predecessor.generation) + 1n).toString(),
    );
    // It waited for the lease end; reaching the bound would have refused.
    expect(elapsedMs).toBeGreaterThan(1_000);
    expect(
      logs.filter((line) => line.includes("Startup is waiting")),
    ).toHaveLength(1);
    expect(logs.some((line) => line.includes("claimed as generation"))).toBe(
      true,
    );
    const row = await run(readRow);
    expect(row?.owner_token).toBe(successorToken);
    expect(row?.state).toBe("recovering");
    // The predecessor's generation is revoked.
    await expect(run(Authority.renew(predecessor, 2_000))).rejects.toThrow(
      /generation or owner changed/,
    );
  });

  it("refuses at once, naming the owner, a lease another token renews while startup waits", async () => {
    // A 3 s lease renewed every 200 ms never lapses, even under load. The
    // bound is 40 s: refusing well inside it shows the renewal decided.
    const holder = await run(acquire(randomUUID(), 3_000));
    const logs: string[] = [];
    const { exit, elapsedMs } = await run(
      Effect.gen(function* () {
        const renewer = yield* Effect.fork(
          Effect.repeat(
            Authority.renew(holder, 3_000),
            Schedule.spaced("200 millis"),
          ),
        );
        const attempt = yield* timed(
          acquire(randomUUID(), 30_000).pipe(
            withWait(10_000, 300),
            captureLogs(logs),
          ),
        );
        // The holder renewed throughout: its lease never lapsed.
        expect(renewer.unsafePoll()).toBeNull();
        yield* Fiber.interrupt(renewer);
        return attempt;
      }),
    );
    expect(failureText(exit)).toContain(
      `History authority still has a live owner: ${holder.ownerToken} (generation ${holder.generation}) renewed its lease`,
    );
    expect(elapsedMs).toBeLessThan(5_000);
    expect(
      logs.filter((line) => line.includes("Startup is waiting")),
    ).toHaveLength(1);
    const row = await run(readRow);
    expect(row?.owner_token).toBe(holder.ownerToken);
    expect(row?.generation).toBe(holder.generation);
    await run(Authority.renew(holder, 3_000));
  });

  it("refuses an unrenewed lease still live at the bound, and never claims it", async () => {
    // The holder took a 60 s lease; this node's 1 s lease plus a 500 ms
    // margin bounds its wait well before that lease ends.
    const holder = await run(acquire(randomUUID(), 60_000));
    const { exit, elapsedMs } = await run(
      timed(acquire(randomUUID(), 1_000).pipe(withWait(500, 200))),
    );
    expect(failureText(exit)).toMatch(/still has a live owner after waiting/);
    expect(elapsedMs).toBeGreaterThanOrEqual(1_500);
    expect(elapsedMs).toBeLessThan(6_000);
    const row = await run(readRow);
    expect(row?.owner_token).toBe(holder.ownerToken);
    expect(row?.generation).toBe(holder.generation);
    await run(Authority.renew(holder, 60_000));
  });

  it("refuses a foreign deployment at once, even while waiting is allowed", async () => {
    const foreign = await run(acquire(randomUUID(), 60_000, "dd".repeat(32)));
    const { exit, elapsedMs } = await run(
      timed(acquire(randomUUID(), 60_000).pipe(withWait(10_000, 2_000))),
    );
    expect(failureText(exit)).toMatch(/belongs to another deployment/);
    expect(elapsedMs).toBeLessThan(1_000);
    const row = await run(readRow);
    expect(row?.owner_token).toBe(foreign.ownerToken);
    expect(row?.generation).toBe(foreign.generation);
  });

  it("without the startup wait, a live foreign lease still refuses at once", async () => {
    await run(acquire(randomUUID(), 60_000));
    const { exit, elapsedMs } = await run(timed(acquire(randomUUID(), 60_000)));
    expect(failureText(exit)).toMatch(/still has a live owner$/m);
    expect(elapsedMs).toBeLessThan(1_000);
  });
});

describe("the startup wait's next step", () => {
  const held = (
    leaseUntilMs: number,
    overrides: Partial<HeldLease> = {},
  ): HeldLease => ({
    heldBy: "owner-a",
    generation: "4",
    leaseUntil: new Date(leaseUntilMs),
    remainingMs: 1_000,
    ...overrides,
  });
  const step = (current: HeldLease, previous: HeldLease | undefined) =>
    nextPredecessorLeaseWaitStep({
      held: current,
      previous,
      waitStartedAt: 0,
      now: 100,
      boundMs: 70_000,
      pollIntervalMs: 2_000,
    });

  it("refuses a lease whose end moved forward under the same owner and generation", () => {
    expect(step(held(2_000), held(1_000))).toEqual({
      refuse:
        "History authority still has a live owner: owner-a (generation 4) renewed its lease while startup waited",
    });
  });

  it("keeps waiting on a first sighting, an unchanged lease end, or a new owner or generation", () => {
    const sleep = { sleepMs: 1_025 };
    expect(step(held(1_000), undefined)).toEqual(sleep);
    expect(step(held(1_000), held(1_000))).toEqual(sleep);
    expect(step(held(2_000), held(1_000, { generation: "3" }))).toEqual(sleep);
    expect(step(held(2_000), held(1_000, { heldBy: "owner-b" }))).toEqual(
      sleep,
    );
  });
});
