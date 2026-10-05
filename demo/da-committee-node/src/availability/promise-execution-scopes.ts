import * as SDK from "@al-ft/midgard-sdk";

import { readAvailabilityCursor } from "../l1/availability-cursor.js";
import type { ChainSyncReplayProvider } from "../l1/provider.js";
import {
  committeeOwnedReadTransports,
  inheritCommitteeReadOwner,
  registerCommitteeReadOwner,
} from "./committee-owned-read-transports.js";
import type { CommitteeSourceReadLimits } from "./scoped-transports.js";

/** Owns one aggregate cursor/source attempt through the final signing fence.
 * The source phase starts once after cursor refresh; boundaries cannot renew it.
 * Owned durable writes are awaited by their callers rather than raced here. */
export const committeePromiseExecutionScopes = (args: {
  provider: ChainSyncReplayProvider;
  limits: CommitteeSourceReadLimits;
  breach: (reason: string) => void;
}) => {
  type Phase = {
    controller: AbortController;
    timer?: ReturnType<typeof setTimeout>;
    sourceStarted: boolean;
    sourceEnd?: number;
  };
  const phases = new WeakMap<SDK.DaAvailabilityReadScope, Phase>();
  const owners = new Set<ReturnType<typeof committeeOwnedReadTransports>>();
  const open = (deadlineEpochMs?: number): SDK.DaAvailabilityReadScope => {
    const owner = committeeOwnedReadTransports();
    owners.add(owner);
    const phase: Phase = {
      controller: new AbortController(),
      sourceStarted: false,
    };
    const raw = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 15000,
      deadlineEpochMs,
      signal: phase.controller.signal,
    });
    const scope: SDK.DaAvailabilityReadScope = Object.freeze({
      ...raw,
      remainingMs: () =>
        Math.max(
          0,
          Math.min(
            raw.remainingMs(),
            phase.sourceEnd === undefined
              ? Infinity
              : phase.sourceEnd - performance.now(),
          ),
        ),
      assertCurrent: () => {
        raw.assertCurrent();
        if (
          phase.sourceEnd !== undefined &&
          performance.now() >= phase.sourceEnd
        ) {
          args.breach("complete_source_budget_exceeded");
          phase.controller.abort(new Error("Complete source budget exceeded"));
          phase.controller.signal.throwIfAborted();
        }
      },
      close: () => {
        if (phase.timer) clearTimeout(phase.timer);
        raw.close();
        void owner
          .drain()
          .finally(() => owners.delete(owner))
          .catch(() => undefined);
      },
    });
    phases.set(scope, phase);
    registerCommitteeReadOwner(scope, owner);
    return scope;
  };
  const refresh = async (scope: SDK.DaAvailabilityReadScope) => {
    const phase = phases.get(scope);
    if (!phase || phase.sourceStarted)
      throw new Error("Cursor/source phase is absent or already started");
    if (!args.provider.refreshAvailabilityCursor)
      throw new Error("Bounded cursor refresh capability is unavailable");
    const cursorScope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: Math.max(
        1,
        Math.ceil(Math.min(5000, scope.remainingMs())),
      ),
      signal: scope.signal,
    });
    inheritCommitteeReadOwner(scope, cursorScope);
    try {
      // Existing authority owns its durable append; do not race that mutation.
      await args.provider.refreshAvailabilityCursor({
        scope: cursorScope,
        limits: args.limits,
        maxEvents: 32,
      });
      cursorScope.assertCurrent();
      await readAvailabilityCursor(args.provider, cursorScope);
    } catch (error) {
      if (cursorScope.signal.aborted) args.breach("cursor_budget_exceeded");
      throw error;
    } finally {
      cursorScope.close();
    }
    scope.assertCurrent();
    phase.sourceStarted = true;
    phase.sourceEnd = performance.now() + Math.min(10000, scope.remainingMs());
    phase.timer = setTimeout(
      () => {
        args.breach("complete_source_budget_exceeded");
        phase.controller.abort(new Error("Complete source budget exceeded"));
      },
      Math.max(1, phase.sourceEnd - performance.now()),
    );
    phase.timer.unref();
  };
  return {
    open,
    refresh,
    assertDrained: () => {
      for (const owner of owners) owner.assertDrained();
    },
  };
};

/** Refusal timers latch NEW-promise readiness. The owner still joins the
 * physical signing/persistence/submission callback; no durable mutation race. */
export const committeePromiseOwnedStage = async <T>(input: {
  stage: string;
  capMs: number;
  breach: (reason: string) => void;
  run: () => Promise<T>;
}): Promise<T> => {
  const start = performance.now();
  const timer = setTimeout(
    () => input.breach(`${input.stage}_budget_exceeded`),
    input.capMs,
  );
  timer.unref();
  try {
    return await input.run();
  } finally {
    clearTimeout(timer);
    if (performance.now() - start > input.capMs)
      input.breach(`${input.stage}_budget_exceeded`);
  }
};
