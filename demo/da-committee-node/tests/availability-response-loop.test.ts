import { describe, expect, it } from "vitest";

import type { AvailabilityResponderReport } from "../src/availability/responder.js";
import {
  AVAILABILITY_RESPONDER_RETRY_POLICY,
  availabilityResponderRetryBoundMs,
  createAvailabilityResponseLoop,
} from "../src/availability-response-loop.js";

const headerHash = "33".repeat(28);

describe("availability response loop", () => {
  const manualInterval = () => {
    const handles: (() => void)[] = [];
    const timeouts: { run: () => void; ms: number }[] = [];
    return {
      timers: {
        setInterval: (run: () => void) => {
          handles.push(run);
          return run;
        },
        clearInterval: (handle: unknown) => {
          handles.splice(handles.indexOf(handle as () => void), 1);
        },
        setTimeout: (run: () => void, ms: number) => {
          const entry = { run, ms };
          timeouts.push(entry);
          return entry;
        },
        clearTimeout: (handle: unknown) => {
          const index = timeouts.indexOf(handle as (typeof timeouts)[number]);
          if (index >= 0) timeouts.splice(index, 1);
        },
      },
      handles,
      timeouts,
      /** Fires the one pending retry timer and returns its delay. */
      fireRetry: () => {
        expect(timeouts).toHaveLength(1);
        const entry = timeouts.shift()!;
        entry.run();
        return entry.ms;
      },
    };
  };

  const settle = async () => {
    for (let turn = 0; turn < 20; turn += 1) await Promise.resolve();
  };

  it("drains on its own interval while a scan tick is still running, and the scan's nudge never waits on it", async () => {
    const lines: string[] = [];
    let release: (() => void) | undefined;
    let drains = 0;
    const clock = manualInterval();
    const loop = createAvailabilityResponseLoop({
      drain: async () => {
        drains += 1;
        if (drains === 1)
          await new Promise<void>((resolve) => (release = resolve));
        return { challenges: 1, status: "confirmed", action: "close" };
      },
      write: (_stream, line) => lines.push(line),
      pollIntervalMs: 1_000,
      timers: clock.timers,
    });

    loop.start();
    // An interval firing during that drain joins it rather than piling up.
    clock.handles[0]!();
    await settle();
    expect(drains).toBe(1);
    release!();
    await loop.run();
    clock.handles[0]!();
    await settle();
    expect(drains).toBe(2);
    expect(lines).toHaveLength(2);
    loop.stop();
    expect(clock.handles).toHaveLength(0);
  });

  it("runs exactly one more drain after a drain that the scan's nudges landed on, and the nudges never wait on it", async () => {
    let release: (() => void) | undefined;
    let drains = 0;
    const loop = createAvailabilityResponseLoop({
      drain: async () => {
        drains += 1;
        if (drains === 1)
          await new Promise<void>((resolve) => (release = resolve));
        return { challenges: 0, status: "idle" };
      },
      write: () => undefined,
      pollIntervalMs: 1_000,
      timers: manualInterval().timers,
    });

    loop.start();
    // Two scans finish while the first drain (which may have read the cursor
    // before they moved it) is still running: both nudges return at once.
    await loop.nudge();
    await loop.nudge();
    expect(drains).toBe(1);
    release!();
    await settle();
    expect(drains).toBe(2);
    await settle();
    expect(drains).toBe(2);
    // A nudge with nothing in flight drains at once, as before.
    await loop.nudge();
    expect(drains).toBe(3);
  });

  it("emits a missed deadline once, keeps it a readiness reason across scan waits, and clears it once the challenge is gone", async () => {
    const lines: { stream: string; line: string }[] = [];
    const missed = {
      headerHash,
      responseDeadline: "5000",
    };
    const reports: AvailabilityResponderReport[] = [
      { challenges: 1, status: "unavailable", missedDeadlines: [missed] },
      { challenges: 1, status: "unavailable", missedDeadlines: [missed] },
      { challenges: 0, status: "awaiting_scan" },
      { challenges: 0, status: "idle" },
    ];
    const loop = createAvailabilityResponseLoop({
      drain: async () => reports.shift()!,
      write: (stream, line) => lines.push({ stream, line }),
      pollIntervalMs: 1_000,
      timers: manualInterval().timers,
    });
    const event = "availability_challenge_deadline_missed";
    const emitted = () =>
      lines.filter(({ line }) => line.includes(`"event":"${event}"`));

    await loop.run();
    expect(emitted()).toEqual([
      {
        stream: "stderr",
        line: `${JSON.stringify({ event, ...missed })}\n`,
      },
    ]);
    expect(loop.reasons()).toEqual([`${event}:${headerHash}`]);
    await loop.run();
    await loop.run();
    expect(emitted()).toHaveLength(1);
    expect(loop.reasons()).toEqual([`${event}:${headerHash}`]);
    await loop.run();
    expect(loop.reasons()).toEqual([]);
  });

  it("logs a drain that throws and keeps running", async () => {
    const lines: string[] = [];
    let calls = 0;
    const loop = createAvailabilityResponseLoop({
      drain: async () => {
        calls += 1;
        if (calls === 1) throw new Error("kupmios read failed");
        return { challenges: 0, status: "idle" };
      },
      write: (_stream, line) => lines.push(line),
      pollIntervalMs: 1_000,
      timers: manualInterval().timers,
    });
    await loop.run();
    await loop.run();
    expect(lines).toEqual([
      `${JSON.stringify({ event: "availability_responder_failed", error: "kupmios read failed" })}\n`,
    ]);
    expect(calls).toBe(2);
  });

  it("retries a failing drain on a capped backoff, then holds unready on the poll interval until a drain succeeds", async () => {
    const lines: string[] = [];
    const clock = manualInterval();
    let failing = true;
    let drains = 0;
    const loop = createAvailabilityResponseLoop({
      drain: async () => {
        drains += 1;
        if (!failing) return { challenges: 0, status: "idle" };
        // Alternate the two failure shapes: a throw and a failed step.
        if (drains % 2 === 1) throw new Error("kupmios read failed");
        return { challenges: 1, status: "failed", detail: "build failed" };
      },
      write: (_stream, line) => lines.push(line),
      pollIntervalMs: 60_000,
      timers: clock.timers,
    });
    const exhaustedEvents = () =>
      lines.filter((line) =>
        line.includes('"event":"availability_responder_retries_exhausted"'),
      );

    loop.start();
    await settle();
    expect(drains).toBe(1);
    // The burst: one retry per failure, doubling and capped at the ceiling,
    // with readiness still clear.
    const delays: number[] = [];
    for (
      let retry = 0;
      retry < AVAILABILITY_RESPONDER_RETRY_POLICY.retries;
      retry += 1
    ) {
      expect(loop.reasons()).toEqual([]);
      delays.push(clock.fireRetry());
      await settle();
    }
    expect(delays).toEqual([5_000, 10_000, 10_000]);
    expect(delays.reduce((sum, ms) => sum + ms, 0)).toBeLessThanOrEqual(
      availabilityResponderRetryBoundMs(),
    );
    expect(availabilityResponderRetryBoundMs()).toBe(30_000);
    expect(drains).toBe(1 + AVAILABILITY_RESPONDER_RETRY_POLICY.retries);
    // Exhausted: no fast retry is left, readiness names the last failure,
    // and the event is written once.
    expect(clock.timeouts).toHaveLength(0);
    expect(loop.reasons()).toEqual([
      "availability_responder_retries_exhausted:build failed",
    ]);
    expect(exhaustedEvents()).toHaveLength(1);
    // The loop keeps draining on its poll interval while it holds.
    clock.handles[0]!();
    await settle();
    expect(drains).toBe(2 + AVAILABILITY_RESPONDER_RETRY_POLICY.retries);
    expect(clock.timeouts).toHaveLength(0);
    expect(exhaustedEvents()).toHaveLength(1);
    expect(loop.reasons()).toEqual([
      "availability_responder_retries_exhausted:kupmios read failed",
    ]);
    // The dependency recovers: the next poll clears the hold.
    failing = false;
    clock.handles[0]!();
    await settle();
    expect(loop.reasons()).toEqual([]);
    // A later failure starts a fresh burst.
    failing = true;
    clock.handles[0]!();
    await settle();
    expect(clock.timeouts.map(({ ms }) => ms)).toEqual([5_000]);
    expect(loop.reasons()).toEqual([]);
    loop.stop();
    expect(clock.timeouts).toHaveLength(0);
    expect(clock.handles).toHaveLength(0);
  });

  it("ends a retry burst at the first drain that does not fail", async () => {
    const clock = manualInterval();
    const reports: AvailabilityResponderReport[] = [
      { challenges: 1, status: "failed", detail: "build failed" },
      { challenges: 0, status: "awaiting_scan" },
      { challenges: 1, status: "failed", detail: "build failed" },
    ];
    const loop = createAvailabilityResponseLoop({
      drain: async () => reports.shift()!,
      write: () => undefined,
      pollIntervalMs: 60_000,
      timers: clock.timers,
    });
    loop.start();
    await settle();
    expect(clock.fireRetry()).toBe(5_000);
    await settle();
    // The awaiting-scan drain ended the burst, so the next failure restarts
    // the backoff from its first step.
    expect(clock.timeouts).toHaveLength(0);
    clock.handles[0]!();
    await settle();
    expect(clock.timeouts.map(({ ms }) => ms)).toEqual([5_000]);
    loop.stop();
  });
});
