import { afterEach, describe, expect, it, vi } from "vitest";

import {
  committeePromiseExecutionScopes,
  committeePromiseOwnedStage,
} from "../src/availability/promise-execution-scopes.js";

const fixture = () => {
  vi.useFakeTimers();
  let monotonic = 0;
  vi.spyOn(performance, "now").mockImplementation(() => monotonic);
  const follower = { readView: async (): Promise<unknown> => ({}) };
  const breach = vi.fn();
  const scopes = committeePromiseExecutionScopes({
    readCursor: () => follower.readView(),
    breach,
  });
  const advance = async (ms: number) => {
    monotonic += ms;
    await vi.advanceTimersByTimeAsync(ms);
  };
  return { follower, breach, scopes, advance };
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
  it("refuses a follower view read past the 5 s cursor cap with a breach; the read writes nothing, so nothing is joined", async () => {
    const f = fixture();
    let finish!: () => void;
    f.follower.readView = async () => {
      await new Promise<void>((resolve) => {
        finish = resolve;
      });
      return {};
    };
    const scope = f.scopes.open();
    let refused: unknown;
    const refreshing = f.scopes.refresh(scope).catch((error: unknown) => {
      refused = error;
    });
    await Promise.resolve();
    await f.advance(4999);
    expect(refused).toBeUndefined();
    await f.advance(1);
    await refreshing;
    expect(refused).toBeInstanceOf(Error);
    expect(f.breach).toHaveBeenCalledWith("cursor_budget_exceeded");
    finish();
    // The source phase never started: no complete-source breach follows.
    await f.advance(20000);
    expect(f.breach).toHaveBeenCalledTimes(1);
    scope.close();
  });
  it("refuses the source phase while the follower holds the committee, with no breach", async () => {
    const f = fixture();
    f.follower.readView = async () => {
      throw new Error("rollback_beyond_k: deep");
    };
    const scope = f.scopes.open();
    try {
      await expect(f.scopes.refresh(scope)).rejects.toThrow(
        "rollback_beyond_k",
      );
      expect(f.breach).not.toHaveBeenCalled();
      // Nothing started: once the follower is ready the same scope refreshes.
      f.follower.readView = async () => ({});
      await expect(f.scopes.refresh(scope)).resolves.toBeUndefined();
    } finally {
      scope.close();
    }
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
