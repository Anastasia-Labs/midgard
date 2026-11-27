import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  closeStackWatchers,
  liveStatus,
  removeStackDirectories,
  runningStackWatcher,
} from "./attached-journey-session.test-support.js";
import type { JourneyFixtureStage } from "./fixture.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import { openAttachedJourneySession } from "./journey-session.js";

afterAll(removeStackDirectories);
afterEach(closeStackWatchers);

// These fixtures stage by hand; only a time limit reads a staging's cover.
const stackShiftCoverMs = () => {
  throw new Error("A hand-staged fixture has no staging shift cover");
};

const openedStack = async () => {
  const stack = await runningStackWatcher(liveStatus());
  const session = await openAttachedJourneySession(
    stack.context,
    stack.options(),
  );
  return { stack, session };
};

const until = async (condition: () => boolean) => {
  while (!condition()) await pause(10);
};

describe("one family at a time in a journey session", () => {
  it("stops a family at its next step once its time limit passes, and starts operator-2 again only after the family's work stopped", async () => {
    const { stack, session } = await openedStack();
    const limit = new AbortController();
    let operator2WhenStopped: string[] | undefined;
    // Staging that reports a step every 50 ms for 5 s, far past the limit.
    const stage = async (input: JourneyFixtureStage) => {
      try {
        for (let step = 0; step < 100; step += 1) {
          input.onStage(`awaiting operator-2's shift (${step.toString()})`);
          await pause(50);
        }
        throw new Error("staging ran on past its family's time limit");
      } finally {
        operator2WhenStopped = [...stack.operator2.calls];
      }
    };
    const run = runAutonomousWatcherJourney(
      stack.runDirectory,
      { category: "doubleSpend", stackShiftCoverMs, stage },
      { session, limit: limit.signal },
    );
    setTimeout(() => limit.abort(new Error("Test timed out")), 300);
    await expect(run).rejects.toThrow("The family reached its time limit");
    // Operator-2 was still held when staging stopped, and started after.
    expect(operator2WhenStopped).toEqual(["stop"]);
    expect(stack.operator2.calls).toEqual(["stop", "start"]);
    expect(session.isClosed()).toBe(true);
    const failure = JSON.parse(
      await readFile(
        join(stack.runDirectory, "work/journeys/doubleSpend/failure.json"),
        "utf8",
      ),
    ) as { activeStage: string; error: { message: string } };
    expect(failure.error.message).toBe("The family reached its time limit");
    expect(failure.activeStage).toMatch(/^awaiting operator-2's shift/u);
  });

  it("waits, when the session closes during a family, for the family's work to stop before starting operator-2 again", async () => {
    const { stack, session } = await openedStack();
    let operator2WhenStopped: string[] | undefined;
    // Staging inside one long step that reports nothing until it ends.
    const stage = async (input: JourneyFixtureStage) => {
      input.onStage("awaiting the DA committee's attestation");
      await pause(500);
      operator2WhenStopped = [...stack.operator2.calls];
      throw new Error("the attestation never came");
    };
    const run = runAutonomousWatcherJourney(
      stack.runDirectory,
      { category: "doubleSpend", stackShiftCoverMs, stage },
      { session },
    ).catch((error: unknown) => error);
    await until(() => stack.operator2.calls.length > 0);
    await session.close();
    expect(operator2WhenStopped).toEqual(["stop"]);
    expect(stack.operator2.calls).toEqual(["stop", "start"]);
    expect(await run).toMatchObject({ message: "the attestation never came" });
  });

  it("refuses to start a family before the earlier family's run has unwound, and refuses every family after it closed the session", async () => {
    const { session } = await openedStack();
    const limit = new AbortController();
    const events: string[] = [];
    // A family whose work ignores its abort for 400 ms.
    const first = session
      .runFamily(async () => {
        await pause(400);
        events.push("first family stopped");
      }, limit.signal)
      .catch((error: unknown) => error);
    limit.abort(new Error("Test timed out"));
    await expect(
      session.runFamily(() => Promise.resolve("second family")),
    ).rejects.toThrow(/has not unwound/u);
    await session.familyUnwound();
    events.push("unwound");
    expect(events).toEqual(["first family stopped", "unwound"]);
    expect(session.isClosed()).toBe(true);
    expect(await first).toMatchObject({
      message: "The family reached its time limit",
    });
    await expect(
      session.runFamily(() => Promise.resolve("second family")),
    ).rejects.toThrow(/closed/u);
  });
});
