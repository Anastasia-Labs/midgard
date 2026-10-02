import { EventEmitter } from "node:events";

import { Deferred, Effect, Exit, Fiber, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { runCommitWorkerInThread } from "../src/fibers/block-commitment.run-commit-worker-in-thread.js";
import { makeConfirmationWorkerRunner } from "../src/fibers/block-confirmation.run-confirmation-worker-in-thread.js";
import type { SpawnWorker } from "../src/fibers/worker-lifecycle.js";
import {
  Globals,
  L1ControlPlaneTimeoutError,
  withL1ControlPlane,
} from "../src/services/globals.js";
import {
  L1_CONTROL_PLANE_WEDGED_WAIT_FACTOR,
  l1ControlPlaneWaiterWedgeLimitMs,
} from "../src/services/globals.l1-control-plane.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import { DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS } from "../src/services/globals.next-l1-provider-health-evidence.js";

/** Settles with "hung" when `promise` has not settled within `ms`. */
const within = <A>(promise: Promise<A>, ms: number) =>
  Promise.race([
    promise,
    new Promise<"hung">((resolve) => setTimeout(() => resolve("hung"), ms)),
  ]);

/**
 * A stand-in for a worker thread whose termination the test controls: it
 * resolves only when `finishTermination` is called, like a thread stuck in a
 * long native call.
 */
const stubWorker = (options: { readonly terminateResolves: boolean }) => {
  const emitter = new EventEmitter();
  let finish: (code: number) => void = () => undefined;
  const terminated = new Promise<number>((resolve) => {
    finish = resolve;
  });
  const calls = { terminate: 0 };
  const worker = {
    on: emitter.on.bind(emitter),
    off: emitter.off.bind(emitter),
    terminate: () => {
      calls.terminate += 1;
      if (options.terminateResolves) finish(1);
      return terminated;
    },
  };
  const spawnWorker: SpawnWorker = () =>
    worker as unknown as ReturnType<SpawnWorker>;
  return {
    calls,
    emitter,
    spawnWorker,
    finishTermination: () => finish(1),
  };
};

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

describe("Effect interruption semantics behind the L1 control-plane wedge", () => {
  // The canceler of an `Effect.async` runs uninterruptibly, like the old
  // worker finalizers' `Effect.promise(() => terminate())`.
  const hungCanceler = Effect.async<void>(() =>
    Effect.promise(() => new Promise(() => {})),
  );

  it("timeoutFail without disconnect does not return while the canceler hangs; with disconnect it does", async () => {
    const plain = await within(
      Effect.runPromise(
        hungCanceler.pipe(
          Effect.timeoutFail({ duration: 50, onTimeout: () => "timeout" }),
          Effect.flip,
        ),
      ),
      500,
    );
    const disconnected = await within(
      Effect.runPromise(
        Effect.disconnect(hungCanceler).pipe(
          Effect.timeoutFail({ duration: 50, onTimeout: () => "timeout" }),
          Effect.flip,
        ),
      ),
      500,
    );
    expect(plain).toBe("hung");
    expect(disconnected).toBe("timeout");
  });

  it("a hold timeout waits for an uninterruptible canceler, so a canceler awaiting an unbounded promise wedges the permit", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        // A daemon: the wedged holder never finishes, so nothing may await it.
        const holder = yield* Effect.forkDaemon(
          withL1ControlPlane(
            globals,
            { scope: "wedged", maxHoldMs: 50 },
            Effect.async<void>(() =>
              Effect.promise(() => new Promise(() => {})),
            ),
          ),
        );
        yield* Effect.sleep(250);
        const holderDone = Option.isSome(yield* Fiber.poll(holder));
        const next = yield* withL1ControlPlane(
          globals,
          { scope: "next", maxHoldMs: 1_000 },
          Effect.succeed("next"),
        ).pipe(Effect.timeoutOption(200));
        return { holderDone, next };
      }),
    );
    expect(outcome.holderDone).toBe(false);
    expect(Option.isNone(outcome.next)).toBe(true);
  });

  it("a hold timeout returns once a canceler's wait is bounded", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const exit = yield* withL1ControlPlane(
          globals,
          { scope: "bounded", maxHoldMs: 50 },
          Effect.async<void>(() =>
            Effect.promise(
              () => new Promise<void>((resolve) => setTimeout(resolve, 100)),
            ),
          ),
        ).pipe(Effect.exit);
        const next = yield* withL1ControlPlane(
          globals,
          { scope: "next", maxHoldMs: 1_000 },
          Effect.succeed("next"),
        );
        return { exit, next };
      }),
    );
    expect(Exit.isFailure(outcome.exit)).toBe(true);
    expect(outcome.next).toBe("next");
  });
});

describe("commitment worker with a termination that never completes", () => {
  const commitJob = (
    spawnWorker: SpawnWorker,
    releases: { count: number },
    maxHoldMs: number,
  ) =>
    Effect.gen(function* () {
      const globals = yield* Globals;
      return yield* withL1ControlPlane(
        globals,
        { scope: "block_commitment", maxHoldMs },
        runCommitWorkerInThread<{ readonly done: true }, string>({
          workerEntry: "unused",
          workerOptions: {},
          takeOutput: () => "committed",
          releaseLedgerLease: async () => {
            releases.count += 1;
          },
          terminationWaitMs: 100,
          spawnWorker,
        }),
      );
    });

  it("releases the permit within the hold plus the bounded wait, withholds the ledger lease, and lets the next tick run once", async () => {
    const stub = stubWorker({ terminateResolves: false });
    const releases = { count: 0 };
    const result = await within(
      runWithGlobals(
        Effect.gen(function* () {
          const globals = yield* Globals;
          const startedAtMs = Date.now();
          const exit = yield* commitJob(stub.spawnWorker, releases, 200).pipe(
            Effect.exit,
          );
          const elapsedMs = Date.now() - startedAtMs;
          const ticks = yield* Ref.make(0);
          yield* withL1ControlPlane(
            globals,
            { scope: "block_commitment", maxHoldMs: 1_000 },
            Ref.update(ticks, (n) => n + 1),
          );
          return { exit, elapsedMs, ticks: yield* Ref.get(ticks) };
        }),
      ),
      3_000,
    );
    expect(result).not.toBe("hung");
    if (result === "hung") return;
    expect(Exit.isFailure(result.exit)).toBe(true);
    if (Exit.isFailure(result.exit)) {
      expect(String(result.exit.cause)).toContain("L1ControlPlaneTimeoutError");
    }
    expect(result.elapsedMs).toBeLessThan(200 + 100 + 500);
    expect(result.ticks).toBe(1);
    expect(stub.calls.terminate).toBe(1);
    // The worker never confirmed it stopped: its MPF lease stays withheld.
    expect(releases.count).toBe(0);
    // Once it does stop, the deferred release runs, exactly once.
    stub.finishTermination();
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(releases.count).toBe(1);
  });

  it("fails a job whose output arrived but whose worker never stops, still withholding the lease", async () => {
    const stub = stubWorker({ terminateResolves: false });
    const releases = { count: 0 };
    const exit = await within(
      runWithGlobals(
        Effect.gen(function* () {
          const job = yield* Effect.fork(
            commitJob(stub.spawnWorker, releases, 5_000),
          );
          yield* Effect.sleep(20);
          stub.emitter.emit("message", { done: true });
          return yield* Fiber.await(job);
        }),
      ),
      3_000,
    );
    expect(exit).not.toBe("hung");
    if (exit === "hung") return;
    expect(Exit.isFailure(exit)).toBe(true);
    if (Exit.isFailure(exit)) {
      expect(String(exit.cause)).toContain(
        "Failed to terminate commitment worker",
      );
    }
    expect(releases.count).toBe(0);
  });

  it("a worker that stops normally releases its lease once and is terminated once", async () => {
    const stub = stubWorker({ terminateResolves: true });
    const releases = { count: 0 };
    const output = await runWithGlobals(
      Effect.gen(function* () {
        const job = yield* Effect.fork(
          commitJob(stub.spawnWorker, releases, 5_000),
        );
        yield* Effect.sleep(20);
        stub.emitter.emit("message", { done: true });
        stub.emitter.emit("exit", 1);
        return yield* Fiber.join(job);
      }),
    );
    expect(output).toBe("committed");
    expect(stub.calls.terminate).toBe(1);
    expect(releases.count).toBe(1);
  });
});

describe("confirmation worker bounds", () => {
  it("an interrupted job with a hung termination returns within the bounded wait", async () => {
    const stub = stubWorker({ terminateResolves: false });
    const result = await within(
      runWithGlobals(
        Effect.gen(function* () {
          const globals = yield* Globals;
          const runner = makeConfirmationWorkerRunner({
            workerEntry: "unused",
            terminationWaitMs: 100,
            spawnWorker: stub.spawnWorker,
          });
          const exit = yield* withL1ControlPlane(
            globals,
            { scope: "block_confirmation", maxHoldMs: 100 },
            runner({ data: { firstRun: false, pendingBlock: null } } as never),
          ).pipe(Effect.exit);
          return exit;
        }),
      ),
      3_000,
    );
    expect(result).not.toBe("hung");
  });

  it("fails a job that produces no output within its timeout and terminates the worker", async () => {
    const stub = stubWorker({ terminateResolves: true });
    const exit = await within(
      Effect.runPromise(
        makeConfirmationWorkerRunner({
          workerEntry: "unused",
          jobTimeoutMs: 100,
          spawnWorker: stub.spawnWorker,
        })({ data: { firstRun: false, pendingBlock: null } } as never).pipe(
          Effect.exit,
        ),
      ),
      3_000,
    );
    expect(exit).not.toBe("hung");
    if (exit === "hung") return;
    expect(Exit.isFailure(exit)).toBe(true);
    if (Exit.isFailure(exit)) {
      expect(String(exit.cause)).toContain("produced no output within 100 ms");
    }
    expect(stub.calls.terminate).toBe(1);
  });
});

describe("L1 control-plane liveness reasons", () => {
  it("raises a hold-timeout streak after three consecutive timeouts and clears it on the next completed hold", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const timedOutHold = withL1ControlPlane(
          globals,
          { scope: "slow_scope", maxHoldMs: 20 },
          Effect.sleep(200),
        ).pipe(Effect.exit);
        yield* timedOutHold;
        yield* timedOutHold;
        const afterTwo = yield* currentLivenessReasons(globals);
        yield* timedOutHold;
        const afterThree = yield* currentLivenessReasons(globals);
        yield* withL1ControlPlane(
          globals,
          { scope: "slow_scope", maxHoldMs: 1_000 },
          Effect.void,
        );
        const afterSuccess = yield* currentLivenessReasons(globals);
        return { afterTwo, afterThree, afterSuccess };
      }),
    );
    expect(outcome.afterTwo).toEqual([]);
    expect(outcome.afterThree).toEqual([
      "l1_control_plane_hold_timeouts:slow_scope:3",
    ]);
    expect(outcome.afterSuccess).toEqual([]);
  });

  it("reports a waiter blocked for several multiples of the largest hold, and forgets an interrupted waiter", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const entered = yield* Deferred.make<void>();
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "holder", maxHoldMs: 60_000 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const waiter = yield* Effect.fork(
          withL1ControlPlane(globals, { scope: "waiter" }, Effect.void),
        );
        yield* Effect.sleep(20);
        const activity = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
        const blocked = [...activity.waiters.values()][0];
        const sinceMs = blocked?.sinceMs ?? 0;
        // Queued behind a 60 s hold, the waiter's limit is still sized by the
        // larger budget an unregistered hold could take.
        const limitMs =
          blocked === undefined
            ? 0
            : l1ControlPlaneWaiterWedgeLimitMs(activity, blocked);
        const justBefore = yield* currentLivenessReasons(
          globals,
          sinceMs + limitMs,
        );
        const farFuture = sinceMs + limitMs + 1;
        const wedged = yield* currentLivenessReasons(globals, farFuture);
        yield* Fiber.interrupt(waiter);
        yield* Fiber.interrupt(holder);
        const after = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
        return {
          waitersWhileBlocked: activity.waiters.size,
          holderScope: activity.holder?.scope,
          blockedBehind: blocked?.largestHoldMs,
          limitMs,
          justBefore,
          wedged,
          waitersAfter: after.waiters.size,
          holderAfter: after.holder,
        };
      }),
    );
    expect(outcome.waitersWhileBlocked).toBe(1);
    expect(outcome.holderScope).toBe("holder");
    expect(outcome.blockedBehind).toBe(60_000);
    expect(outcome.limitMs).toBe(
      L1_CONTROL_PLANE_WEDGED_WAIT_FACTOR *
        DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
    );
    expect(
      outcome.justBefore.some((r) =>
        r.startsWith("l1_control_plane_wedged:waiter="),
      ),
    ).toBe(false);
    expect(
      outcome.wedged.some((r) =>
        r.startsWith("l1_control_plane_wedged:holder=holder"),
      ),
    ).toBe(true);
    expect(
      outcome.wedged.some((r) =>
        r.startsWith("l1_control_plane_wedged:waiter=waiter"),
      ),
    ).toBe(true);
    expect(outcome.waitersAfter).toBe(0);
    expect(outcome.holderAfter).toBeNull();
  });

  it("keeps the typed hold-timeout error", async () => {
    const exit = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        return yield* withL1ControlPlane(
          globals,
          { scope: "typed", maxHoldMs: 10 },
          Effect.sleep(100),
        ).pipe(Effect.flip);
      }),
    );
    expect(exit).toBeInstanceOf(L1ControlPlaneTimeoutError);
  });
});
