import { describe, expect, it } from "vitest";

import type { AvailabilityResponderReport } from "../src/availability/responder.js";
import { createAvailabilityResponseLoop } from "../src/availability-response-loop.js";

const headerHash = "33".repeat(28);

describe("availability response loop", () => {
  const manualInterval = () => {
    const handles: (() => void)[] = [];
    return {
      timers: {
        setInterval: (run: () => void) => {
          handles.push(run);
          return run;
        },
        clearInterval: (handle: unknown) => {
          handles.splice(handles.indexOf(handle as () => void), 1);
        },
      },
      handles,
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
});
