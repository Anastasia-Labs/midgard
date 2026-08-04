import { readFile } from "node:fs/promises";

import { Deferred, Duration, Effect, Fiber, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { runL1ProviderPreflight } from "@/commands/l1-provider-preflight.js";
import {
  l1ProviderEvidenceIsFresh,
  l1ProviderReadinessEvidenceIsFresh,
  reconcileReadinessProbeWithExactEvidence,
  resolveL1ProviderReadinessEvidence,
  resolveL1ProviderReadinessSnapshot,
  runBoundedDirectL1ProviderPreflight,
  runBusyL1ProviderReadinessProbe,
  runCombinedL1ReadinessProbe,
} from "@/commands/listen-router.js";
import { withScheduledMergeControlPlaneWait } from "@/fibers/merge.js";
import { makeAwaitedWorkerTerminator } from "@/fibers/worker-lifecycle.js";
import {
  Globals,
  L1ControlPlaneTimeoutError,
  nextL1ProviderHealthEvidence,
  withL1ControlPlane,
} from "@/services/globals.js";

describe("L1 control-plane serialization", () => {
  it("makes a commit wait for an in-flight background provider poll", async () => {
    const events = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const pollEntered = yield* Deferred.make<void>();
        const releasePoll = yield* Deferred.make<void>();
        const events: string[] = [];
        const poll = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "poll", maxHoldMs: 5_000 },
            Effect.sync(() => events.push("poll-enter")).pipe(
              Effect.zipRight(Deferred.succeed(pollEntered, undefined)),
              Effect.zipRight(Deferred.await(releasePoll)),
              Effect.tap(() => Effect.sync(() => events.push("poll-exit"))),
            ),
          ),
        );
        yield* Deferred.await(pollEntered);
        const commit = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "commit", maxHoldMs: 5_000 },
            Effect.sync(() => events.push("commit-enter")),
          ),
        );
        yield* Effect.yieldNow();
        expect(events).toEqual(["poll-enter"]);
        yield* Deferred.succeed(releasePoll, undefined);
        yield* Fiber.join(poll);
        yield* Fiber.join(commit);
        return events;
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(events).toEqual(["poll-enter", "poll-exit", "commit-enter"]);
  });

  it("keeps a background poll out for the full simulated child-worker lifetime", async () => {
    const events = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const childEntered = yield* Deferred.make<void>();
        const releaseChild = yield* Deferred.make<void>();
        const events: string[] = [];
        const child = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "child", maxHoldMs: 5_000 },
            Effect.sync(() => events.push("child-enter")).pipe(
              Effect.zipRight(Deferred.succeed(childEntered, undefined)),
              Effect.zipRight(Deferred.await(releaseChild)),
              Effect.tap(() => Effect.sync(() => events.push("child-exit"))),
            ),
          ),
        );
        yield* Deferred.await(childEntered);
        const poll = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "poll", maxHoldMs: 5_000 },
            Effect.sync(() => events.push("poll-enter")),
          ),
        );
        yield* Effect.yieldNow();
        expect(events).toEqual(["child-enter"]);
        yield* Deferred.succeed(releaseChild, undefined);
        yield* Fiber.join(child);
        yield* Fiber.join(poll);
        return events;
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(events).toEqual(["child-enter", "child-exit", "poll-enter"]);
  });

  it("releases the permit on failure and interruption", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const failed = yield* Effect.either(
          withL1ControlPlane(
            globals,
            { scope: "failure", maxHoldMs: 5_000 },
            Effect.fail("provider-failed"),
          ),
        );
        const entered = yield* Deferred.make<void>();
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "interruption", maxHoldMs: 5_000 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const interrupted = yield* Fiber.interrupt(holder);
        const afterInterrupt = yield* withL1ControlPlane(
          globals,
          { scope: "after-interruption", maxHoldMs: 5_000 },
          Effect.succeed("reacquired"),
        );
        return { failed, interrupted, afterInterrupt };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.failed._tag).toBe("Left");
    if (result.failed._tag === "Left") {
      expect(result.failed.left).toBe("provider-failed");
    }
    expect(result.interrupted._tag).toBe("Failure");
    expect(result.afterInterrupt).toBe("reacquired");
  });

  it("serves queued callers in FIFO order", async () => {
    const order = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const holderEntered = yield* Deferred.make<void>();
        const releaseHolder = yield* Deferred.make<void>();
        const order: string[] = [];
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "holder", maxHoldMs: 5_000 },
            Deferred.succeed(holderEntered, undefined).pipe(
              Effect.zipRight(Deferred.await(releaseHolder)),
            ),
          ),
        );
        yield* Deferred.await(holderEntered);
        const waiters = [];
        for (const name of ["first", "second", "third"]) {
          waiters.push(
            yield* Effect.fork(
              withL1ControlPlane(
                globals,
                { scope: name, maxHoldMs: 5_000 },
                Effect.sync(() => order.push(name)),
              ),
            ),
          );
          yield* Effect.yieldNow();
        }
        yield* Deferred.succeed(releaseHolder, undefined);
        yield* Fiber.join(holder);
        for (const waiter of waiters) yield* Fiber.join(waiter);
        return order;
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(order).toEqual(["first", "second", "third"]);
  });

  it("bounds a scheduled merge permit wait without leaking its canceled waiter", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const holderEntered = yield* Deferred.make<void>();
        const releaseHolder = yield* Deferred.make<void>();
        let timedOutEffectEntered = false;
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "holder", maxHoldMs: 5_000 },
            Deferred.succeed(holderEntered, undefined).pipe(
              Effect.zipRight(Deferred.await(releaseHolder)),
            ),
          ),
        );
        yield* Deferred.await(holderEntered);
        const timedOut = yield* withScheduledMergeControlPlaneWait({
          globals,
          waitTimeoutMs: 10,
          effect: Effect.sync(() => {
            timedOutEffectEntered = true;
          }),
        });
        yield* Deferred.succeed(releaseHolder, undefined);
        yield* Fiber.join(holder);
        const reacquired = yield* withScheduledMergeControlPlaneWait({
          globals,
          waitTimeoutMs: 1_000,
          effect: Effect.succeed("reacquired"),
        });
        return { timedOut, timedOutEffectEntered, reacquired };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(Option.isNone(result.timedOut)).toBe(true);
    expect(result.timedOutEffectEntered).toBe(false);
    expect(Option.getOrUndefined(result.reacquired)).toBe("reacquired");
  });

  it("does not apply the scheduled merge wait deadline after acquisition", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        return yield* withScheduledMergeControlPlaneWait({
          globals,
          waitTimeoutMs: 10,
          effect: Effect.sleep(Duration.millis(30)).pipe(
            Effect.as("completed-after-wait-deadline"),
          ),
        });
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(Option.getOrUndefined(result)).toBe("completed-after-wait-deadline");
  });

  it("times out a bounded hold and only then lets the next caller acquire", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const timedOut = yield* Effect.either(
          withL1ControlPlane(
            globals,
            { scope: "bounded", maxHoldMs: 10 },
            Effect.never,
          ),
        );
        const reacquired = yield* withL1ControlPlane(
          globals,
          { scope: "after-timeout", maxHoldMs: 5_000 },
          Effect.succeed(true),
        );
        return { timedOut, reacquired };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.timedOut._tag).toBe("Left");
    if (result.timedOut._tag === "Left") {
      expect(result.timedOut.left).toBeInstanceOf(L1ControlPlaneTimeoutError);
    }
    expect(result.reacquired).toBe(true);
  });

  it("reuses and awaits one asynchronous worker termination", async () => {
    let releaseTermination!: (code: number) => void;
    const termination = new Promise<number>((resolve) => {
      releaseTermination = resolve;
    });
    const worker = { terminate: vi.fn(() => termination) };
    const order: string[] = [];
    const cleanup = vi.fn(async () => {
      order.push("cleanup");
    });
    const terminate = makeAwaitedWorkerTerminator(worker, cleanup);

    const first = terminate();
    const second = terminate();
    expect(first).toBe(second);
    expect(worker.terminate).toHaveBeenCalledTimes(1);

    let completed = false;
    void first.then(() => {
      completed = true;
    });
    await Promise.resolve();
    expect(completed).toBe(false);
    expect(cleanup).not.toHaveBeenCalled();
    releaseTermination(1);
    await first;
    expect(completed).toBe(true);
    expect(cleanup).toHaveBeenCalledTimes(1);
    expect(order).toEqual(["cleanup"]);
  });

  it("does not run post-termination cleanup when termination is unconfirmed", async () => {
    const cleanup = vi.fn(async () => undefined);
    const terminate = makeAwaitedWorkerTerminator(
      {
        terminate: vi.fn(() => Promise.reject(new Error("still running"))),
      },
      cleanup,
    );

    await expect(terminate()).rejects.toThrow("still running");
    expect(cleanup).not.toHaveBeenCalled();
  });

  it("resolves live and cached readiness evidence without stale errors", () => {
    const nowMs = 100_000;
    const ogmiosSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 123,
      observedAtMs: nowMs - 5_000,
      slotLengthMs: 1_000,
    };
    expect(
      resolveL1ProviderReadinessEvidence({
        probe: { mode: "busy", baseRevision: 1 },
        lastSuccessAtMs: nowMs - 5_000,
        lastFailure: null,
        cachedOgmiosSlot: ogmiosSlot,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toEqual({
      healthy: true,
      mode: "cached_control_plane_busy",
      evidenceAgeMs: 5_000,
      error: null,
      ogmiosSlot,
    });
    expect(
      resolveL1ProviderReadinessEvidence({
        probe: { mode: "busy", baseRevision: 1 },
        lastSuccessAtMs: nowMs - 30_001,
        lastFailure: "provider failed",
        cachedOgmiosSlot: ogmiosSlot,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toEqual({
      healthy: false,
      mode: "cached_control_plane_busy",
      evidenceAgeMs: 30_001,
      error: "provider failed",
      ogmiosSlot: null,
    });
    expect(
      resolveL1ProviderReadinessEvidence({
        probe: {
          mode: "live",
          healthy: true,
          ogmiosSlot,
          publishedRevision: 1,
        },
        lastSuccessAtMs: 0,
        lastFailure: "old failure",
        cachedOgmiosSlot: null,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toEqual({
      healthy: true,
      mode: "live",
      evidenceAgeMs: 0,
      error: null,
      ogmiosSlot,
    });
    expect(
      resolveL1ProviderReadinessEvidence({
        probe: {
          mode: "live",
          healthy: false,
          error: "live failure",
          publishedRevision: 1,
        },
        lastSuccessAtMs: nowMs - 1_000,
        lastFailure: null,
        cachedOgmiosSlot: ogmiosSlot,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toEqual({
      healthy: false,
      mode: "live",
      evidenceAgeMs: 1_000,
      error: "live failure",
      ogmiosSlot: null,
    });

    expect(
      resolveL1ProviderReadinessEvidence({
        probe: { mode: "cached_fresh", baseRevision: 1 },
        lastSuccessAtMs: nowMs - 1_000,
        lastFailure: null,
        cachedOgmiosSlot: ogmiosSlot,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toEqual({
      healthy: true,
      mode: "cached_fresh",
      evidenceAgeMs: 1_000,
      error: null,
      ogmiosSlot,
    });
    expect(
      l1ProviderEvidenceIsFresh({
        lastSuccessAtMs: nowMs - 30_000,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toBe(true);
    expect(
      l1ProviderEvidenceIsFresh({
        lastSuccessAtMs: nowMs - 30_001,
        nowMs,
        maxAgeMs: 30_000,
      }),
    ).toBe(false);

    expect(
      nextL1ProviderHealthEvidence({
        current: {
          evidenceRevision: 0,
          lastObservationKind: null,
          lastExactEvidenceRevision: 0,
          lastExactObservationKind: null,
          lastSuccessAtMs: 10,
          lastExactSuccessAtMs: 10,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 20,
          lastFailure: "old failure",
          lastOgmiosSlot: null,
        },
        healthy: true,
        observedAtMs: nowMs,
        ogmiosSlot,
        successKind: "exact",
      }),
    ).toEqual({
      evidenceRevision: 1,
      lastObservationKind: "exact_success",
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success",
      lastSuccessAtMs: nowMs,
      lastExactSuccessAtMs: nowMs,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: "exact",
      lastFailureAtMs: 20,
      lastFailure: null,
      lastOgmiosSlot: ogmiosSlot,
    });
  });

  it("runs HubOracle and Ogmios as one fail-closed live readiness probe", async () => {
    const ogmiosSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 456,
      observedAtMs: 100_000,
      slotLengthMs: 1_000,
    };
    const calls: string[] = [];
    const success = await Effect.runPromise(
      runCombinedL1ReadinessProbe(
        Effect.sync(() => calls.push("hub")),
        Effect.sync(() => {
          calls.push("ogmios");
          return ogmiosSlot;
        }),
      ),
    );
    expect(success).toEqual(ogmiosSlot);
    expect(calls).toEqual(["hub", "ogmios"]);

    calls.length = 0;
    const hubFailure = await Effect.runPromise(
      Effect.either(
        runCombinedL1ReadinessProbe(
          Effect.try({
            try: () => {
              calls.push("hub");
              throw new Error("hub failed");
            },
            catch: (cause) => cause,
          }),
          Effect.sync(() => {
            calls.push("ogmios");
            return ogmiosSlot;
          }),
        ),
      ),
    );
    expect(hubFailure._tag).toBe("Left");
    expect(calls).toEqual(["hub"]);

    calls.length = 0;
    const ogmiosFailure = await Effect.runPromise(
      Effect.either(
        runCombinedL1ReadinessProbe(
          Effect.sync(() => calls.push("hub")),
          Effect.try({
            try: () => {
              calls.push("ogmios");
              throw new Error("ogmios failed");
            },
            catch: (cause) => cause,
          }),
        ),
      ),
    );
    expect(ogmiosFailure._tag).toBe("Left");
    expect(calls).toEqual(["hub", "ogmios"]);
  });

  it("returns a concurrent busy-path request immediately while one direct probe publishes success", async () => {
    const nowMs = 200_000;
    const ogmiosSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 789,
      observedAtMs: nowMs,
      slotLengthMs: 1_000,
    };
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
          evidenceRevision: 1,
          lastObservationKind: "exact_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: 100_000,
          lastExactSuccessAtMs: 100_000,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastOgmiosSlot: null,
        });
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const calls = yield* Ref.make(0);
        const directProbe = Ref.update(calls, (count) => count + 1).pipe(
          Effect.zipRight(Deferred.succeed(entered, undefined)),
          Effect.zipRight(Deferred.await(release)),
          Effect.as(ogmiosSlot),
        );
        const run = runBusyL1ProviderReadinessProbe({
          globals,
          directProbe,
          now: () => nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        const first = yield* Effect.fork(run);
        yield* Deferred.await(entered);
        const second = yield* Effect.fork(run);
        const secondBeforeRelease = yield* Fiber.join(second).pipe(
          Effect.timeoutOption(Duration.millis(250)),
        );
        const callsBeforeRelease = yield* Ref.get(calls);
        yield* Deferred.succeed(release, undefined);
        return {
          first: yield* Fiber.join(first),
          second: yield* Fiber.join(second),
          secondBeforeRelease,
          callsBeforeRelease,
          calls: yield* Ref.get(calls),
          evidence: yield* Ref.get(globals.L1_PROVIDER_HEALTH),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.first).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: true,
      ogmiosSlot,
      publishedRevision: 2,
    });
    expect(Option.getOrUndefined(result.secondBeforeRelease)).toEqual({
      mode: "busy",
      baseRevision: 1,
    });
    expect(result.second).toEqual({ mode: "busy", baseRevision: 1 });
    expect(result.callsBeforeRelease).toBe(1);
    expect(result.calls).toBe(1);
    expect(result.evidence.lastSuccessAtMs).toBe(nowMs);
    expect(result.evidence.lastExactSuccessAtMs).toBe(100_000);
    expect(result.evidence.lastSuccessKind).toBe("direct");
  });

  it("returns a concurrent busy-path request immediately while one direct probe publishes failure", async () => {
    const nowMs = 200_000;
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
          evidenceRevision: 1,
          lastObservationKind: "exact_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: 100_000,
          lastExactSuccessAtMs: 100_000,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastOgmiosSlot: null,
        });
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const calls = yield* Ref.make(0);
        const directProbe = Ref.update(calls, (count) => count + 1).pipe(
          Effect.zipRight(Deferred.succeed(entered, undefined)),
          Effect.zipRight(Deferred.await(release)),
          Effect.zipRight(Effect.fail("direct provider unavailable")),
        );
        const run = runBusyL1ProviderReadinessProbe({
          globals,
          directProbe,
          now: () => nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        const first = yield* Effect.fork(run);
        yield* Deferred.await(entered);
        const second = yield* Effect.fork(run);
        const secondBeforeRelease = yield* Fiber.join(second).pipe(
          Effect.timeoutOption(Duration.millis(250)),
        );
        const callsBeforeRelease = yield* Ref.get(calls);
        yield* Deferred.succeed(release, undefined);
        return {
          first: yield* Fiber.join(first),
          second: yield* Fiber.join(second),
          secondBeforeRelease,
          callsBeforeRelease,
          calls: yield* Ref.get(calls),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.first).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: false,
      error: "direct provider unavailable",
      publishedRevision: 2,
    });
    expect(Option.getOrUndefined(result.secondBeforeRelease)).toEqual({
      mode: "busy",
      baseRevision: 1,
    });
    expect(result.second).toEqual({ mode: "busy", baseRevision: 1 });
    expect(result.callsBeforeRelease).toBe(1);
    expect(result.calls).toBe(1);
  });

  it("allows a later busy request to retry after a direct failure using prior exact success", async () => {
    const nowMs = 200_000;
    const ogmiosSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 789,
      observedAtMs: nowMs,
      slotLengthMs: 1_000,
    };
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
          evidenceRevision: 1,
          lastObservationKind: "exact_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: 100_000,
          lastExactSuccessAtMs: 100_000,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastOgmiosSlot: null,
        });
        const calls = yield* Ref.make(0);
        const failDirect = yield* Ref.make(true);
        const directProbe = Ref.update(calls, (count) => count + 1).pipe(
          Effect.zipRight(Ref.get(failDirect)),
          Effect.flatMap((fail) =>
            fail
              ? Effect.fail("direct provider unavailable")
              : Effect.succeed(ogmiosSlot),
          ),
        );
        const run = runBusyL1ProviderReadinessProbe({
          globals,
          directProbe,
          now: () => nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        const first = yield* run;
        yield* Ref.set(failDirect, false);
        const second = yield* run;
        return {
          first,
          second,
          calls: yield* Ref.get(calls),
          evidence: yield* Ref.get(globals.L1_PROVIDER_HEALTH),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.first).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: false,
      error: "direct provider unavailable",
      publishedRevision: 2,
    });
    expect(result.second).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: true,
      ogmiosSlot,
      publishedRevision: 3,
    });
    expect(result.calls).toBe(2);
    expect(result.evidence).toMatchObject({
      evidenceRevision: 3,
      lastObservationKind: "direct_success",
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success",
      lastSuccessKind: "direct",
    });
  });

  it("fails the busy fallback when exact evidence is 180001ms old", async () => {
    const nowMs = 200_000;
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
          evidenceRevision: 1,
          lastObservationKind: "exact_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: 19_999,
          lastExactSuccessAtMs: 19_999,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastOgmiosSlot: null,
        });
        const calls = yield* Ref.make(0);
        const probe = yield* runBusyL1ProviderReadinessProbe({
          globals,
          directProbe: Ref.update(calls, (count) => count + 1).pipe(
            Effect.as({
              source: "local_ogmios_tip" as const,
              currentSlot: 1,
              observedAtMs: nowMs,
              slotLengthMs: 1_000,
            }),
          ),
          now: () => nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        return { probe, calls: yield* Ref.get(calls) };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.probe).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: false,
      error: "Exact HubOracle evidence is 180001ms old (max 180000ms)",
      publishedRevision: 1,
    });
    expect(result.calls).toBe(0);
  });

  it("does not let a direct success rehabilitate a newer exact failure", async () => {
    const nowMs = 200_000;
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
          evidenceRevision: 1,
          lastObservationKind: "exact_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: 100_000,
          lastExactSuccessAtMs: 100_000,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "exact",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastOgmiosSlot: null,
        });
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const fallback = yield* Effect.fork(
          runBusyL1ProviderReadinessProbe({
            globals,
            directProbe: Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Deferred.await(release)),
              Effect.as({
                source: "local_ogmios_tip" as const,
                currentSlot: 2,
                observedAtMs: nowMs,
                slotLengthMs: 1_000,
              }),
            ),
            now: () => nowMs,
            maxAgeMs: 30_000,
            maxExactAgeMs: 180_000,
          }),
        );
        yield* Deferred.await(entered);
        yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
          nextL1ProviderHealthEvidence({
            current,
            healthy: false,
            error: "exact HubOracle query failed",
            observedAtMs: nowMs - 1,
            successKind: "exact",
          }),
        );
        const concurrentCalls = yield* Ref.make(0);
        const concurrent = yield* runBusyL1ProviderReadinessProbe({
          globals,
          directProbe: Ref.update(concurrentCalls, (count) => count + 1).pipe(
            Effect.as({
              source: "local_ogmios_tip" as const,
              currentSlot: 3,
              observedAtMs: nowMs,
              slotLengthMs: 1_000,
            }),
          ),
          now: () => nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        const evidenceBeforeRelease = yield* Ref.get(
          globals.L1_PROVIDER_HEALTH,
        );
        const concurrentSnapshot = resolveL1ProviderReadinessSnapshot({
          probe: concurrent,
          evidence: evidenceBeforeRelease,
          nowMs,
          maxAgeMs: 30_000,
          maxExactAgeMs: 180_000,
        });
        yield* Deferred.succeed(release, undefined);
        const probe = yield* Fiber.join(fallback);
        return {
          probe,
          concurrent,
          concurrentCalls: yield* Ref.get(concurrentCalls),
          concurrentSnapshot,
          evidence: yield* Ref.get(globals.L1_PROVIDER_HEALTH),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.probe).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: false,
      error: "exact HubOracle query failed",
      publishedRevision: 2,
    });
    expect(result.concurrent).toEqual({ mode: "busy", baseRevision: 2 });
    expect(result.concurrentCalls).toBe(0);
    expect(result.concurrentSnapshot).toEqual({
      healthy: false,
      mode: "cached_control_plane_busy",
      evidenceAgeMs: 100_000,
      error: "exact HubOracle query failed",
      ogmiosSlot: null,
    });
    expect(result.evidence.lastSuccessKind).toBe("exact");
    expect(result.evidence.lastExactFailure).toBe(
      "exact HubOracle query failed",
    );
    expect(
      l1ProviderReadinessEvidenceIsFresh({
        evidence: result.evidence,
        nowMs,
        maxAgeMs: 30_000,
        maxExactAgeMs: 180_000,
      }),
    ).toBe(false);
  });

  it.each([
    { directOutcome: "success" as const },
    { directOutcome: "failure" as const },
  ])(
    "keeps a causally newer exact success with an older timestamp when the direct probe returns $directOutcome",
    async ({ directOutcome }) => {
      const nowMs = 200_000;
      const exactObservedAtMs = nowMs - 10_000;
      const directSlot = {
        source: "local_ogmios_tip" as const,
        currentSlot: 2,
        observedAtMs: nowMs,
        slotLengthMs: 1_000,
      };
      const exactSlot = {
        source: "local_ogmios_tip" as const,
        currentSlot: 999,
        observedAtMs: exactObservedAtMs,
        slotLengthMs: 1_000,
      };
      const result = await Effect.runPromise(
        Effect.gen(function* () {
          const globals = yield* Globals;
          yield* Ref.set(globals.L1_PROVIDER_HEALTH, {
            evidenceRevision: 1,
            lastObservationKind: "exact_success",
            lastExactEvidenceRevision: 1,
            lastExactObservationKind: "exact_success",
            lastSuccessAtMs: 100_000,
            lastExactSuccessAtMs: 100_000,
            lastExactFailureAtMs: 0,
            lastExactFailure: null,
            lastSuccessKind: "exact",
            lastFailureAtMs: 0,
            lastFailure: null,
            lastOgmiosSlot: null,
          });
          const entered = yield* Deferred.make<void>();
          const release = yield* Deferred.make<void>();
          const directResult =
            directOutcome === "success"
              ? Effect.succeed(directSlot)
              : Effect.fail("direct provider unavailable");
          const fallback = yield* Effect.fork(
            runBusyL1ProviderReadinessProbe({
              globals,
              directProbe: Deferred.succeed(entered, undefined).pipe(
                Effect.zipRight(Deferred.await(release)),
                Effect.zipRight(directResult),
              ),
              now: () => nowMs,
              maxAgeMs: 30_000,
              maxExactAgeMs: 180_000,
            }),
          );
          yield* Deferred.await(entered);
          yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
            nextL1ProviderHealthEvidence({
              current,
              healthy: true,
              observedAtMs: exactObservedAtMs,
              ogmiosSlot: exactSlot,
              successKind: "exact",
            }),
          );
          yield* Deferred.succeed(release, undefined);
          return {
            probe: yield* Fiber.join(fallback),
            evidence: yield* Ref.get(globals.L1_PROVIDER_HEALTH),
          };
        }).pipe(Effect.provide(Globals.Default)),
      );

      expect(result.probe).toEqual({
        mode: "cached_fresh",
        baseRevision: 2,
      });
      expect(result.evidence).toMatchObject({
        evidenceRevision: 2,
        lastObservationKind: "exact_success",
        lastSuccessAtMs: exactObservedAtMs,
        lastExactSuccessAtMs: exactObservedAtMs,
        lastSuccessKind: "exact",
        lastFailure: null,
        lastOgmiosSlot: exactSlot,
      });
    },
  );

  it("lets a causally newer exact failure with an older timestamp override a settled direct success", () => {
    const nowMs = 200_000;
    const directSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 2,
      observedAtMs: nowMs,
      slotLengthMs: 1_000,
    };
    const before = {
      evidenceRevision: 1,
      lastObservationKind: "exact_success" as const,
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success" as const,
      lastSuccessAtMs: 100_000,
      lastExactSuccessAtMs: 100_000,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: "exact" as const,
      lastFailureAtMs: 0,
      lastFailure: null,
      lastOgmiosSlot: null,
    };
    const afterDirect = nextL1ProviderHealthEvidence({
      current: before,
      healthy: true,
      observedAtMs: nowMs,
      ogmiosSlot: directSlot,
      successKind: "direct",
    });
    const afterExactFailure = nextL1ProviderHealthEvidence({
      current: afterDirect,
      healthy: false,
      error: "exact HubOracle query failed after direct settlement",
      observedAtMs: nowMs - 1,
      successKind: "exact",
    });
    const reconciled = reconcileReadinessProbeWithExactEvidence({
      probe: {
        mode: "live_preflight_control_plane_busy",
        healthy: true,
        ogmiosSlot: directSlot,
        publishedRevision: afterDirect.evidenceRevision,
      },
      evidence: afterExactFailure,
    });
    const response = resolveL1ProviderReadinessEvidence({
      probe: reconciled,
      lastSuccessAtMs: afterExactFailure.lastSuccessAtMs,
      lastFailure: afterExactFailure.lastFailure,
      cachedOgmiosSlot: afterExactFailure.lastOgmiosSlot,
      nowMs,
      maxAgeMs: 30_000,
    });

    expect(afterExactFailure).toMatchObject({
      lastSuccessAtMs: nowMs,
      lastSuccessKind: "direct",
      lastExactFailureAtMs: nowMs - 1,
      lastExactFailure: "exact HubOracle query failed after direct settlement",
    });
    expect(reconciled).toEqual({
      mode: "live_preflight_control_plane_busy",
      healthy: false,
      error: "exact HubOracle query failed after direct settlement",
      publishedRevision: afterExactFailure.evidenceRevision,
    });
    expect(response).toEqual({
      healthy: false,
      mode: "live_preflight_control_plane_busy",
      evidenceAgeMs: 0,
      error: "exact HubOracle query failed after direct settlement",
      ogmiosSlot: null,
    });
  });

  it("lets a causally newer exact success with an older timestamp override a settled direct result", () => {
    const nowMs = 200_000;
    const exactObservedAtMs = nowMs - 1;
    const directSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 2,
      observedAtMs: nowMs,
      slotLengthMs: 1_000,
    };
    const exactSlot = {
      ...directSlot,
      currentSlot: 999,
      observedAtMs: exactObservedAtMs,
    };
    const before = {
      evidenceRevision: 1,
      lastObservationKind: "exact_success" as const,
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success" as const,
      lastSuccessAtMs: 100_000,
      lastExactSuccessAtMs: 100_000,
      lastExactFailureAtMs: 150_000,
      lastExactFailure: "older exact failure with a newer wall timestamp",
      lastSuccessKind: "exact" as const,
      lastFailureAtMs: 150_000,
      lastFailure: "older exact failure with a newer wall timestamp",
      lastOgmiosSlot: null,
    };
    const afterDirect = nextL1ProviderHealthEvidence({
      current: before,
      healthy: true,
      observedAtMs: nowMs,
      ogmiosSlot: directSlot,
      successKind: "direct",
    });
    const afterExactSuccess = nextL1ProviderHealthEvidence({
      current: afterDirect,
      healthy: true,
      observedAtMs: exactObservedAtMs,
      ogmiosSlot: exactSlot,
      successKind: "exact",
    });
    const reconciled = reconcileReadinessProbeWithExactEvidence({
      probe: {
        mode: "live_preflight_control_plane_busy",
        healthy: true,
        ogmiosSlot: directSlot,
        publishedRevision: afterDirect.evidenceRevision,
      },
      evidence: afterExactSuccess,
    });
    const response = resolveL1ProviderReadinessEvidence({
      probe: reconciled,
      lastSuccessAtMs: afterExactSuccess.lastSuccessAtMs,
      lastFailure: afterExactSuccess.lastFailure,
      cachedOgmiosSlot: afterExactSuccess.lastOgmiosSlot,
      nowMs,
      maxAgeMs: 30_000,
    });

    expect(afterExactSuccess).toMatchObject({
      evidenceRevision: 3,
      lastObservationKind: "exact_success",
      lastSuccessAtMs: exactObservedAtMs,
      lastExactSuccessAtMs: exactObservedAtMs,
      lastExactFailureAtMs: 150_000,
      lastFailure: null,
      lastOgmiosSlot: exactSlot,
    });
    expect(reconciled).toEqual({
      mode: "cached_fresh",
      baseRevision: 3,
    });
    expect(response).toEqual({
      healthy: true,
      mode: "cached_fresh",
      evidenceAgeMs: 1,
      error: null,
      ogmiosSlot: exactSlot,
    });
    expect(
      l1ProviderReadinessEvidenceIsFresh({
        evidence: afterExactSuccess,
        nowMs,
        maxAgeMs: 30_000,
        maxExactAgeMs: 180_000,
      }),
    ).toBe(true);
  });

  it("makes a newer exact failure override a primary-success response", () => {
    const nowMs = 200_000;
    const exactSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 10,
      observedAtMs: nowMs,
      slotLengthMs: 1_000,
    };
    const initial = {
      evidenceRevision: 1,
      lastObservationKind: "exact_success" as const,
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success" as const,
      lastSuccessAtMs: 100_000,
      lastExactSuccessAtMs: 100_000,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: "exact" as const,
      lastFailureAtMs: 0,
      lastFailure: null,
      lastOgmiosSlot: null,
    };
    const afterPrimary = nextL1ProviderHealthEvidence({
      current: initial,
      healthy: true,
      observedAtMs: nowMs,
      ogmiosSlot: exactSlot,
      successKind: "exact",
    });
    const afterFailure = nextL1ProviderHealthEvidence({
      current: afterPrimary,
      healthy: false,
      error: "newer exact failure",
      observedAtMs: nowMs - 10_000,
      successKind: "exact",
    });
    const response = resolveL1ProviderReadinessSnapshot({
      probe: {
        mode: "live",
        healthy: true,
        ogmiosSlot: exactSlot,
        publishedRevision: afterPrimary.evidenceRevision,
      },
      evidence: afterFailure,
      nowMs,
      maxAgeMs: 30_000,
      maxExactAgeMs: 180_000,
    });

    expect(response).toEqual({
      healthy: false,
      mode: "snapshot_exact",
      evidenceAgeMs: 0,
      error: "newer exact failure",
      ogmiosSlot: null,
    });
  });

  it("makes a newer exact failure override a cached-success response", () => {
    const nowMs = 200_000;
    const exactSlot = {
      source: "local_ogmios_tip" as const,
      currentSlot: 10,
      observedAtMs: nowMs - 1_000,
      slotLengthMs: 1_000,
    };
    const cached = {
      evidenceRevision: 1,
      lastObservationKind: "exact_success" as const,
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success" as const,
      lastSuccessAtMs: nowMs - 1_000,
      lastExactSuccessAtMs: nowMs - 1_000,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: "exact" as const,
      lastFailureAtMs: 0,
      lastFailure: null,
      lastOgmiosSlot: exactSlot,
    };
    const afterFailure = nextL1ProviderHealthEvidence({
      current: cached,
      healthy: false,
      error: "exact failure after cache decision",
      observedAtMs: nowMs - 10_000,
      successKind: "exact",
    });
    const response = resolveL1ProviderReadinessSnapshot({
      probe: { mode: "cached_fresh", baseRevision: cached.evidenceRevision },
      evidence: afterFailure,
      nowMs,
      maxAgeMs: 30_000,
      maxExactAgeMs: 180_000,
    });

    expect(response).toEqual({
      healthy: false,
      mode: "snapshot_exact",
      evidenceAgeMs: 1_000,
      error: "exact failure after cache decision",
      ogmiosSlot: null,
    });
  });

  it("uses the final direct snapshot after direct-exact-direct publications", () => {
    const nowMs = 200_000;
    const slot = (currentSlot: number, observedAtMs = nowMs) => ({
      source: "local_ogmios_tip" as const,
      currentSlot,
      observedAtMs,
      slotLengthMs: 1_000,
    });
    const initial = {
      evidenceRevision: 1,
      lastObservationKind: "exact_success" as const,
      lastExactEvidenceRevision: 1,
      lastExactObservationKind: "exact_success" as const,
      lastSuccessAtMs: 100_000,
      lastExactSuccessAtMs: 100_000,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: "exact" as const,
      lastFailureAtMs: 0,
      lastFailure: null,
      lastOgmiosSlot: null,
    };
    const firstDirect = nextL1ProviderHealthEvidence({
      current: initial,
      healthy: true,
      observedAtMs: nowMs,
      ogmiosSlot: slot(2),
      successKind: "direct",
    });
    const exact = nextL1ProviderHealthEvidence({
      current: firstDirect,
      healthy: true,
      observedAtMs: nowMs - 10_000,
      ogmiosSlot: slot(3, nowMs - 10_000),
      successKind: "exact",
    });
    const finalDirect = nextL1ProviderHealthEvidence({
      current: exact,
      healthy: true,
      observedAtMs: nowMs,
      ogmiosSlot: slot(4),
      successKind: "direct",
    });
    const response = resolveL1ProviderReadinessSnapshot({
      probe: {
        mode: "live_preflight_control_plane_busy",
        healthy: true,
        ogmiosSlot: slot(2),
        publishedRevision: firstDirect.evidenceRevision,
      },
      evidence: finalDirect,
      nowMs,
      maxAgeMs: 30_000,
      maxExactAgeMs: 180_000,
    });

    expect(finalDirect).toMatchObject({
      evidenceRevision: 4,
      lastObservationKind: "direct_success",
      lastExactEvidenceRevision: 3,
      lastExactObservationKind: "exact_success",
      lastOgmiosSlot: slot(4),
    });
    expect(response).toEqual({
      healthy: true,
      mode: "snapshot_direct",
      evidenceAgeMs: 0,
      error: null,
      ogmiosSlot: slot(4),
    });
  });

  it("returns an unhealthy direct preflight as a typed failure", async () => {
    const result = await Effect.runPromise(
      Effect.either(
        runBoundedDirectL1ProviderPreflight({
          runPreflight: async () => ({
            ok: false,
            degraded: false,
            route: {
              primary: "kupmios",
              network: "Custom",
            },
            checkedAtMs: 100_000,
            healthySources: [],
            unhealthySources: ["kupmios"],
            sources: [
              {
                source: "kupmios",
                endpoint: "http://kupo.test,http://ogmios.test",
                healthy: false,
                degraded: false,
                failureKind: "network_error",
                latencyMs: 1_999,
                bodySummary:
                  "TypeError: fetch failed; cause=connect EHOSTUNREACH kupo",
              },
            ],
          }),
          timeoutMs: 10,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(String(result.left)).toContain(
        "Direct L1 provider preflight failed (kupmios:network_error:latency_ms=1999:TypeError: fetch failed; cause=connect EHOSTUNREACH kupo)",
      );
    }
  });

  it("aborts an in-flight local slot fetch at the aggregate timeout", async () => {
    let calls = 0;
    let slotFetchAborted = false;
    const fetchImpl = async (
      _input: string,
      init?: RequestInit,
    ): Promise<Response> => {
      calls += 1;
      if (calls <= 2) {
        return new Response("{}", { status: 200 });
      }
      return await new Promise<Response>((_, reject) => {
        const signal = init?.signal;
        signal?.addEventListener(
          "abort",
          () => {
            slotFetchAborted = true;
            reject(signal.reason);
          },
          { once: true },
        );
      });
    };
    const result = await Effect.runPromise(
      Effect.either(
        runBoundedDirectL1ProviderPreflight({
          runPreflight: (signal) =>
            runL1ProviderPreflight({
              config: {
                L1_PROVIDER: "Kupmios",
                L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 5_000,
                L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: 1_000,
                L1_OGMIOS_KEY: "http://ogmios.test",
                L1_KUPO_KEY: "http://kupo.test",
                NETWORK: "Custom",
              },
              fetchImpl,
              signal,
            }),
          timeoutMs: 10,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(calls).toBe(3);
    expect(slotFetchAborted).toBe(true);
  });

  it("keeps acquisition at top-level boundaries and out of commit-time barriers", async () => {
    const [
      commitment,
      confirmation,
      merge,
      ingestion,
      barrier,
      speculative,
      mergeTransaction,
      commitWorker,
    ] = await Promise.all(
      [
        "src/fibers/block-commitment.ts",
        "src/fibers/block-confirmation.ts",
        "src/fibers/merge.ts",
        "src/fibers/user-event-ingestion.ts",
        "src/fibers/user-event-barrier-refresher.ts",
        "src/fibers/speculative-commit-builder.ts",
        "src/transactions/state-queue/merge-to-confirmed-state.ts",
        "src/workers/commit-block-header.ts",
      ].map((path) => readFile(path, "utf8")),
    );

    const action = commitment.slice(
      commitment.indexOf("export const blockCommitmentAction"),
      commitment.indexOf("export const blockCommitmentFiber"),
    );
    expect(
      action.indexOf("shouldSkipIdleCommitPipelineBeforeSchedulerAlignment"),
    ).toBeLessThan(action.indexOf("withL1ControlPlane"));
    expect(action.indexOf("withL1ControlPlane")).toBeLessThan(
      action.indexOf("shouldSkipForRegisteredCommitDueWork"),
    );
    expect(confirmation).toContain('scope: "block_confirmation"');
    const exportedMerge = merge.slice(
      merge.indexOf("export const mergeAction"),
      merge.indexOf("export const mergeFiber"),
    );
    const mergeFiberSource = merge.slice(
      merge.indexOf("export const mergeFiber"),
    );
    expect(exportedMerge).toContain('scope: "state_queue_merge"');
    expect(exportedMerge).toContain("withL1ControlPlane");
    expect(mergeFiberSource).not.toContain("withL1ControlPlane");
    expect(mergeFiberSource).toContain("const action = mergeAction()");
    expect(ingestion).toContain("scope: spanName");
    expect(barrier).toContain('scope: "user_event_barrier_refresh"');

    expect(commitment).toContain("maxHoldMs: 180_000");
    expect(confirmation).toContain("maxHoldMs: 180_000");
    expect(merge).toContain("maxHoldMs: 180_000");
    expect(mergeTransaction).toContain(
      "confirmationRetries: MERGE_CONFIRMATION_PROVIDER_RETRIES",
    );
    expect(mergeTransaction).toContain(
      "export const MERGE_CONFIRMATION_PROVIDER_RETRIES = 12",
    );
    expect(ingestion).toContain("maxHoldMs: 30_000");
    expect(barrier).toContain("maxHoldMs: 60_000");
    expect(speculative).toContain("maxHoldMs: 60_000");
    expect(commitment).toContain("makeAwaitedWorkerTerminator");
    expect(commitment).toContain("releaseTerminatedWorkerLedgerLease");
    expect(commitment).toContain("ledgerStoreLeaseOwner");
    expect(confirmation).toContain("makeAwaitedWorkerTerminator");
    expect(commitment).toContain("return Effect.promise(async () =>");
    expect(confirmation).toContain("return Effect.promise(async () =>");
    expect(speculative).toContain("releaseTerminatedWorkerLedgerLease");
    expect(speculative).toContain(
      "makeAwaitedWorkerTerminator(worker, afterTermination)",
    );
    expect(speculative).toContain(
      "Effect.ensuring(shutdownSpeculativeCommitSession())",
    );
    expect(commitWorker).toContain(
      "const leaseOwner = workerInput.data.ledgerStoreLeaseOwner",
    );

    for (const path of [
      "src/fibers/fetch-and-insert-deposit-utxos.ts",
      "src/fibers/fetch-and-insert-withdrawal-utxos.ts",
      "src/fibers/fetch-and-insert-tx-order-utxos.ts",
    ]) {
      const source = await readFile(path, "utf8");
      const barrierStart = source.indexOf("ForCommitBarrier");
      expect(barrierStart).toBeGreaterThanOrEqual(0);
      expect(source.slice(barrierStart)).not.toContain("withL1ControlPlane");
    }

    const submit = speculative.slice(
      speculative.indexOf(
        "export const submitSpeculativeCandidateOnConfirmation",
      ),
      speculative.indexOf("export const speculativeCommitBuilderFiber"),
    );
    expect(submit.indexOf("waitForCandidate")).toBeLessThan(
      submit.indexOf("withL1ControlPlane"),
    );
    expect(submit.indexOf("withL1ControlPlane")).toBeLessThan(
      submit.indexOf("acquirePipelinePhase"),
    );

    const [config, envExample, listenRouter] = await Promise.all([
      readFile("src/services/config.ts", "utf8"),
      readFile(".env.example", "utf8"),
      readFile("src/commands/listen-router.ts", "utf8"),
    ]);
    expect(config).toMatch(
      /positiveSafeIntegerConfig\(\s*"READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS",\s*30_000,\s*\)/u,
    );
    expect(envExample).toContain(
      "READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS=30000",
    );
    const readinessHandler = listenRouter.slice(
      listenRouter.indexOf("const getReadinessHandler"),
      listenRouter.indexOf("const getContractInfoHandler"),
    );
    expect(readinessHandler).toContain("cachedProviderEvidenceIsFresh");
    expect(readinessHandler).toContain("runCombinedL1ReadinessProbe");
    expect(readinessHandler).toContain("withL1ControlPlaneIfAvailable");
    expect(readinessHandler).toContain("resolveL1ProviderReadinessSnapshot");
    expect(readinessHandler).not.toContain(
      "resolveL1ProviderReadinessEvidence",
    );
    expect(listenRouter).toContain("requestedRevision");
    expect(listenRouter).toContain("expectedRevision");
    expect(listenRouter).toContain("publishedRevision");
    expect(listenRouter).not.toContain("requestedAtMs");
    expect(listenRouter).not.toContain("directObservedAtMs");
    expect(readinessHandler.match(/readLocalOgmiosSubmitSlot/g)).toHaveLength(
      1,
    );
    expect(
      readinessHandler.indexOf("cachedProviderEvidenceIsFresh"),
    ).toBeLessThan(readinessHandler.indexOf("withL1ControlPlaneIfAvailable"));
  });
});
