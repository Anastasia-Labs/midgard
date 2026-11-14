import { describe, expect, it } from "vitest";

import {
  isWatcherPreflightStalledResult,
  runWorkflowWithPreflightStallRetries,
  WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS,
  WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS,
} from "../../src/fault-proofs/preflight-stall-retry.js";

const preflightStall = (reason: string) =>
  Object.freeze({ kind: "stalled", phase: "preflight", reason });
const completed = Object.freeze({ kind: "completed", terminal: {} });

const harness = (results: readonly unknown[]) => {
  let clock = 1_000;
  const sleeps: number[] = [];
  const resumes: number[] = [];
  const stalls: { attempt: number; retrying: boolean }[] = [];
  let live = true;
  const queue = [...results];
  const next = async () => {
    const value = queue.shift();
    if (value === undefined) throw new Error("harness ran out of results");
    return value;
  };
  return {
    sleeps,
    resumes,
    stalls,
    setLive: (value: boolean) => {
      live = value;
    },
    run: (options: { delayMs?: number; budgetMs?: number } = {}) =>
      runWorkflowWithPreflightStallRetries({
        run: next,
        resume: async (attempt) => {
          resumes.push(attempt);
          return await next();
        },
        sleep: async (ms) => {
          sleeps.push(ms);
          clock += ms;
        },
        now: () => clock,
        isLive: () => live,
        onStall: ({ attempt, retrying }) => stalls.push({ attempt, retrying }),
        ...options,
      }),
  };
};

describe("preflight stall retries", () => {
  it("recognises only orchestrator stalls tagged with the preflight phase", () => {
    expect(isWatcherPreflightStalledResult(preflightStall("x"))).toBe(true);
    expect(
      isWatcherPreflightStalledResult({ kind: "stalled", reason: "x" }),
    ).toBe(false);
    expect(
      isWatcherPreflightStalledResult({
        kind: "pending",
        phase: "preflight",
        reason: "x",
      }),
    ).toBe(false);
    expect(isWatcherPreflightStalledResult(null)).toBe(false);
    expect(isWatcherPreflightStalledResult("stalled")).toBe(false);
  });

  it("resumes a preflight stall after the delay and returns the later outcome", async () => {
    const h = harness([
      preflightStall("preflight failed for init:a#0: found 0"),
      preflightStall("preflight failed for init:a#0: found 0"),
      completed,
    ]);
    await expect(h.run({ delayMs: 60_000, budgetMs: 600_000 })).resolves.toBe(
      completed,
    );
    expect(h.sleeps).toEqual([60_000, 60_000]);
    expect(h.resumes).toEqual([1, 2]);
    expect(h.stalls).toEqual([
      { attempt: 1, retrying: true },
      { attempt: 2, retrying: true },
    ]);
  });

  it("returns the stall once the retry budget cannot fit another delay", async () => {
    const stall = preflightStall("preflight failed for init:a#0: found 0");
    const h = harness([stall, stall, stall, stall]);
    await expect(h.run({ delayMs: 60_000, budgetMs: 150_000 })).resolves.toBe(
      stall,
    );
    // 0 → 60 000 → 120 000 elapsed; a third delay would exceed 150 000.
    expect(h.sleeps).toEqual([60_000, 60_000]);
    expect(h.resumes).toEqual([1, 2]);
    expect(h.stalls.at(-1)).toEqual({ attempt: 3, retrying: false });
  });

  it("never resumes stalls from other phases or other outcomes", async () => {
    const plain = Object.freeze({ kind: "stalled", reason: "chain conflict" });
    const h = harness([plain]);
    await expect(h.run()).resolves.toBe(plain);
    expect(h.sleeps).toEqual([]);
    expect(h.resumes).toEqual([]);
    const pending = Object.freeze({ kind: "pending", reason: "x" });
    const p = harness([pending]);
    await expect(p.run()).resolves.toBe(pending);
    expect(p.resumes).toEqual([]);
  });

  it("stops resuming once the runtime is no longer live", async () => {
    const stall = preflightStall("preflight failed for init:a#0: found 0");
    const closed = harness([stall]);
    closed.setLive(false);
    await expect(closed.run()).resolves.toBe(stall);
    expect(closed.sleeps).toEqual([]);
    expect(closed.stalls).toEqual([{ attempt: 1, retrying: false }]);
    // Closing during the delay returns without a resume.
    let live = true;
    const resumes: number[] = [];
    const result = await runWorkflowWithPreflightStallRetries({
      run: async () => stall,
      resume: async (attempt) => {
        resumes.push(attempt);
        return completed;
      },
      sleep: async () => {
        live = false;
      },
      isLive: () => live,
    });
    expect(result).toBe(stall);
    expect(resumes).toEqual([]);
  });

  it("defaults to a one-minute delay inside a thirty-minute budget", () => {
    expect(WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS).toBe(60_000);
    expect(WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS).toBe(1_800_000);
  });
});
