import { rmSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import type { TxSignBuilder } from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { committeePromiseActorRuntime } from "../src/availability/promise-actor-runtime.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("committee actor runtime drainage", () => {
  it("keeps a real SDK timed-out unsigned callback busy after its journal lease is released", async () => {
    const s = scene();
    const breach = vi.fn();
    const runtime = committeePromiseActorRuntime(breach);
    const scope = SDK.createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 30,
      nowMs: Date.now,
      monotonicMs: Date.now,
    });
    let finish!: (value: TxSignBuilder) => void;
    const physical = new Promise<TxSignBuilder>((resolve) => {
      finish = resolve;
    });
    const build = () => physical;
    let tracked!: Promise<TxSignBuilder>;
    const run = SDK.runDaAvailabilityOperation(s.context, {
      ...s.operation,
      preparationScope: scope,
      build: () => {
        tracked = runtime.trackUnsigned(scope, build);
        return tracked;
      },
    });
    const refused = expect(run).rejects.toThrow(/read deadline 1100 reached/);
    await vi.advanceTimersByTimeAsync(30);
    await refused;
    expect(
      s.journal.actorSnapshot(s.context.actor, s.context.deploymentIdentity),
    ).toMatchObject({
      retainedRecordCount: 0,
      reservedResourceCount: 0,
      lease: { expiresAtMs: 0 },
    });
    expect(() => runtime.assertIdle()).toThrow("has not drained");
    expect(breach).toHaveBeenCalledWith("unsigned_actor_attempt_expired");
    const nextScope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 100,
      nowMs: Date.now,
      monotonicMs: Date.now,
    });
    const overlapping = vi.fn(async () => s.tx);
    await expect(runtime.trackUnsigned(nextScope, overlapping)).rejects.toThrow(
      "has not drained",
    );
    expect(overlapping).not.toHaveBeenCalled();
    let joined = false;
    const join = runtime.join().then(() => {
      joined = true;
    });
    await Promise.resolve();
    expect(joined).toBe(false);
    finish(s.tx);
    await expect(tracked).rejects.toThrow(/read deadline 1100 reached/);
    await join;
    expect(joined).toBe(true);
    nextScope.close();
    expect(() => runtime.assertIdle()).not.toThrow();
    expect(s.sign).not.toHaveBeenCalled();
    expect(s.context.submit).not.toHaveBeenCalled();
    expect(s.journal.retainedRecordCount()).toBe(0);
    scope.close();
  });

  it("drains successful isolated unsigned work before closing its scope", async () => {
    const breach = vi.fn();
    const runtime = committeePromiseActorRuntime(breach);
    const scope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 100,
      nowMs: Date.now,
      monotonicMs: Date.now,
    });
    await expect(
      runtime.trackUnsigned(scope, async () => "built"),
    ).resolves.toBe("built");
    scope.close();
    expect(() => runtime.assertIdle()).not.toThrow();
    expect(breach).not.toHaveBeenCalled();
  });
});
