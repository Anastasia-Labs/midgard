/**
 * The production lifecycle's liveness: `runtime.done` settles only when the
 * proof supervisor or the operations server stops. The follower stopping,
 * on its own or on an intervention, is an L1 readiness condition and never
 * ends the process.
 */
import { describe, expect, it } from "vitest";

import {
  createWatcherRuntimeLifecycle,
  WatcherFollowerExhaustedError,
} from "../../src/runtime/watcher-runtime.create-lifecycle.js";

type LifecycleInput = Parameters<typeof createWatcherRuntimeLifecycle>[0];

const PENDING = Symbol("pending");

/** The promise's outcome, or PENDING once queued work has drained. */
const settled = async (promise: Promise<unknown>) =>
  await Promise.race([
    promise.then(
      () => "resolved",
      () => "rejected",
    ),
    new Promise<typeof PENDING>((resolve) =>
      setTimeout(() => resolve(PENDING), 20),
    ),
  ]);

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((settle) => {
    resolve = settle;
  });
  return { promise, resolve };
};

const lifecycle = (follower: Readonly<{ done: Promise<unknown> }>) => {
  const supervisor = deferred();
  const operationsHttp = deferred();
  const runtime = createWatcherRuntimeLifecycle({
    deploymentAuthority: {},
    faultProofApplication: {},
    faultProofReadiness: [],
    faultProofSupervisor: {
      done: supervisor.promise,
      status: () => ({
        phase: "accepting",
        recovered: true,
        deadlineHealth: "safe",
      }),
    },
    operations: {
      api: {
        status: () => ({ readiness: "ready", l1Readiness: { ready: true } }),
      },
    },
    operationsHttp: { done: operationsHttp.promise, endpoint: {} },
    recoveredFaultProofWorkflowCount: 0,
    availability: { status: () => ({ phase: "running" }) },
    follower,
    decisionDriver: { caughtUp: Promise.resolve() },
    closeAllocatedResources: () => Promise.resolve(),
  } as unknown as LifecycleInput);
  return { runtime, supervisor, operationsHttp };
};

const interventionStatus = {
  atTip: false,
  cursor: null,
  intervention: { kind: "deep_rollback", detail: "rollback deeper than k" },
};

describe("watcher production lifecycle liveness", () => {
  it.each([
    ["stops after an intervention", () => Promise.resolve(interventionStatus)],
    ["stops on its own", () => Promise.resolve(null)],
    ["fails", () => Promise.reject(new Error("the follower loop threw"))],
  ])(
    "keeps runtime.done pending when the follower %s",
    async (_label, stop) => {
      const done = stop();
      void done.catch(() => undefined);
      const { runtime } = lifecycle({ done });
      await runtime.caughtUp;
      expect(await settled(runtime.done)).toBe(PENDING);
      expect(runtime.status()).toMatchObject({ phase: "live", liveness: true });
    },
  );

  it.each(["supervisor", "operationsHttp"] as const)(
    "ends liveness when the %s stops",
    async (which) => {
      const handle = lifecycle({ done: new Promise(() => undefined) });
      handle[which].resolve();
      expect(await settled(handle.runtime.done)).toBe("resolved");
      expect(handle.runtime.status()).toMatchObject({
        phase: "failed",
        liveness: false,
      });
    },
  );

  it("ends liveness, rejecting with the named reason, when the follower's transient budget ran out", async () => {
    const handle = lifecycle({
      done: Promise.resolve({
        state: "exhausted",
        waiting: { cause: "store", detail: "the database did not answer" },
      }),
    });
    await expect(handle.runtime.done).rejects.toBeInstanceOf(
      WatcherFollowerExhaustedError,
    );
    await expect(handle.runtime.done).rejects.toThrow(
      "l1_follower_transient_exhausted: the database did not answer",
    );
    expect(handle.runtime.status()).toMatchObject({
      phase: "failed",
      liveness: false,
    });
  });
});
