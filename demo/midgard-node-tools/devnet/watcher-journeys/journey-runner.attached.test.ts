import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  closeStackWatchers,
  liveStatus,
  removeStackDirectories,
  runningStackWatcher,
  seedWorkflowStarted,
  STACK_GENESIS,
} from "./attached-journey-session.test-support.js";
import {
  type AttachedJourneyStaging,
  runAutonomousWatcherJourney,
} from "./journey-runner.js";
import { openAttachedJourneySession } from "./journey-session.js";

afterAll(removeStackDirectories);
afterEach(closeStackWatchers);

const readJson = async (path: string) =>
  JSON.parse(await readFile(path, "utf8")) as Record<string, unknown>;

/** Stack staging that records what the journey had done when it was asked. */
const recordingStaging = (onStage: () => Promise<void>) => ({
  stage: () => onStage().then(() => Promise.reject(new Error("staged"))),
  verifyPublicDa: () => Promise.reject(new Error("not reached")),
  fetchPayloadEnvelope: () => Promise.reject(new Error("not reached")),
});

describe("journey runner on an attached stack", () => {
  it("reads the stack watcher's journals and the stack's genesis, holds operator-2, and launches nothing", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const headerHash = "12".repeat(28);
    await seedWorkflowStarted(stack.storePath, headerHash);
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    const directory = join(stack.runDirectory, "work/journeys/doubleSpend");
    let seen: Record<string, unknown> | undefined;
    const staging = recordingStaging(async () => {
      seen = {
        operator2: [...stack.operator2.calls],
        baseline: await readJson(join(directory, "workflow-baseline.json")),
        timing: await readJson(join(directory, "timing-plan.json")),
      };
    }) satisfies AttachedJourneyStaging;
    await expect(
      runAutonomousWatcherJourney(
        stack.runDirectory,
        {
          category: "doubleSpend",
          stage: () => Promise.reject(new Error("devnet staging")),
        },
        { session, staging },
      ),
    ).rejects.toThrow("staged");
    expect(seen).toMatchObject({
      // Operator-2's node stopped before the drill signed anything as it.
      operator2: ["stop"],
      baseline: {
        category: "doubleSpend",
        prefixes: [{ headerHash, entryCount: 1 }],
      },
      timing: {
        cadence: {
          slotLengthSeconds: STACK_GENESIS.slotLength,
          activeSlotsCoeff: STACK_GENESIS.activeSlotsCoeff,
          confirmationDepth: session.releaseFinality.policy.confirmationDepth,
        },
      },
    });
    expect(await readJson(join(directory, "failure.json"))).toMatchObject({
      activeStage: "invalid commitment and DA attestations",
      error: { message: "staged" },
    });
    // The failed run closed its session, which started operator-2 again.
    expect(stack.operator2.calls).toEqual(["stop", "start"]);
    // Only the stack watcher's own operations endpoint was asked anything.
    expect(new Set(stack.requests)).toEqual(new Set(["/v1/status"]));
  });

  it("refuses an attached session without its stack staging", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    await expect(
      runAutonomousWatcherJourney(
        stack.runDirectory,
        {
          category: "doubleSpend",
          stage: () => Promise.reject(new Error("devnet staging")),
        },
        { session },
      ),
    ).rejects.toThrow(/needs its stack staging/u);
    expect(stack.operator2.calls).toEqual([]);
  });
});
