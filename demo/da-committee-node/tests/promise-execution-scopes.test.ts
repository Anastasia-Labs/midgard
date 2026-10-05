import { afterEach, describe, expect, it, vi } from "vitest";

import {
  committeePromiseExecutionScopes,
  committeePromiseOwnedStage,
} from "../src/availability/promise-execution-scopes.js";
import type { ChainSyncReplayProvider } from "../src/l1/provider.js";

const fixture = () => {
  vi.useFakeTimers();
  let monotonic = 0;
  vi.spyOn(performance, "now").mockImplementation(() => monotonic);
  const cursor = {
    sequence: 1,
    rollbackGeneration: 0,
    point: {
      network: "Custom",
      slot: 1,
      blockHash: "ab".repeat(32),
      providerSource: "fixture",
      observedAt: "2026-10-02T00:00:00Z",
    },
  };
  const provider: ChainSyncReplayProvider = {
    currentChainSyncCursor: async () => cursor,
    loadConsumedChainSyncCursor: async () => cursor,
    replayChainSyncEvents: async () => [],
    acknowledgeChainSyncCursor: async () => ({ rollbackSinceCapture: false }),
    refreshAvailabilityCursor: async () => cursor,
  };
  const breach = vi.fn();
  const scopes = committeePromiseExecutionScopes({
    provider,
    breach,
    limits: {
      requestRefusalMs: 10000,
      rawUtxos: 1024,
      httpResponseBytes: 4194304,
      webSocketMessageBytes: 4194304,
    },
  });
  const advance = async (ms: number) => {
    monotonic += ms;
    await vi.advanceTimersByTimeAsync(ms);
  };
  return { cursor, provider, breach, scopes, advance };
};
afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});
describe("installed cursor/source and owned critical-stage deadlines", () => {
  it("shares one absolute source phase across all later reads and rejects a renewed cursor phase", async () => {
    const f = fixture();
    const scope = f.scopes.open();
    try {
      await f.advance(4500);
      await f.scopes.refresh(scope);
      expect(scope.remainingMs()).toBe(10000);
      await expect(f.scopes.refresh(scope)).rejects.toThrow("already started");
      await f.advance(9000);
      expect(scope.remainingMs()).toBe(1000);
      await f.advance(1000);
      expect(() => scope.assertCurrent()).toThrow(
        "Complete source budget exceeded",
      );
      expect(f.breach).toHaveBeenCalledWith("complete_source_budget_exceeded");
    } finally {
      scope.close();
    }
  });
  it("waits for the cursor owner's physical append callback before returning an expired attempt", async () => {
    const f = fixture();
    let finish!: () => void;
    f.provider.refreshAvailabilityCursor = async () => {
      await new Promise<void>((resolve) => {
        finish = resolve;
      });
      return f.cursor;
    };
    const scope = f.scopes.open();
    let returned = false;
    const refreshing = f.scopes.refresh(scope).catch(() => {
      returned = true;
    });
    await Promise.resolve();
    await f.advance(5000);
    expect(returned).toBe(false);
    finish();
    await refreshing;
    expect(returned).toBe(true);
    scope.close();
  });
  it("latches a breached stage while joining its durable callback rather than racing its write", async () => {
    const f = fixture();
    let finish!: () => void;
    let returned = false;
    const stage = committeePromiseOwnedStage({
      stage: "submit",
      capMs: 3000,
      breach: f.breach,
      run: async () => {
        await new Promise<void>((resolve) => {
          finish = resolve;
        });
        return "durable";
      },
    }).then((value) => {
      returned = true;
      return value;
    });
    await f.advance(3000);
    expect(f.breach).toHaveBeenCalledWith("submit_budget_exceeded");
    expect(returned).toBe(false);
    finish();
    expect(await stage).toBe("durable");
  });
});
