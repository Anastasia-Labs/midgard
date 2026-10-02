import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { makeHistorySourceOutage } from "../src/services/event-history-owner.source-outage.js";

const LIMIT_MS = 1_000;
const outage = () =>
  makeHistorySourceOutage({
    initialMs: 10,
    maxMs: 40,
    outageLimitMs: LIMIT_MS,
  });
// Lost, answered again, then this much later still in the same outage.
const lostThenAnswered = (o: ReturnType<typeof outage>) => {
  o.lost(new Error("socket closed"));
  o.answered();
};

beforeEach(() => {
  vi.useFakeTimers({ toFake: ["performance", "Date"] });
});
afterEach(() => {
  vi.useRealTimers();
});

it("escalates an outage that outlives its limit", () => {
  const o = outage();
  o.lost(new Error("socket closed"));
  vi.advanceTimersByTime(LIMIT_MS);
  expect(o.exceeded()).toBeUndefined();
  vi.advanceTimersByTime(1);
  expect(o.exceeded()).toBeGreaterThan(LIMIT_MS);
});

it("ends the outage when the gate reopens", () => {
  const o = outage();
  lostThenAnswered(o);
  vi.advanceTimersByTime(LIMIT_MS + 1);
  o.reopened();
  expect(o.exceeded()).toBeUndefined();
  expect(o.status()).toMatchObject({
    state: "following",
    since: null,
    attempts: 0,
    lastError: null,
  });
});

it("keeps the last error while the outage lasts", () => {
  const o = outage();
  o.lost(new Error("socket closed"));
  o.reopened();
  expect(o.status()).toMatchObject({
    state: "reconnecting",
    lastError: "socket closed",
  });
});

it("keeps the outage through a reopened gate while still reconnecting", () => {
  const o = outage();
  o.lost(new Error("socket closed"));
  vi.advanceTimersByTime(LIMIT_MS + 1);
  o.reopened();
  expect(o.exceeded()).toBeGreaterThan(LIMIT_MS);
});

it("counts a first-start replay step only past every earlier one", () => {
  const o = outage();
  o.replayed(10);
  lostThenAnswered(o);
  vi.advanceTimersByTime(LIMIT_MS + 1);
  // A new session replays from the activation again.
  for (const height of [3, 7, 10]) o.replayed(height);
  expect(o.exceeded()).toBeGreaterThan(LIMIT_MS);
  o.replayed(11);
  expect(o.exceeded()).toBeUndefined();
});

it("doubles the reconnect delay to its cap and restarts it after progress", () => {
  const o = outage();
  lostThenAnswered(o);
  expect([1, 2, 3, 4].map(() => o.nextDelayMs())).toEqual([10, 20, 40, 40]);
  o.reopened();
  expect(o.nextDelayMs()).toBe(10);
});
