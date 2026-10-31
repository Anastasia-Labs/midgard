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
import type { JourneyFixtureStage } from "./fixture.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import { openAttachedJourneySession } from "./journey-session.js";

afterAll(removeStackDirectories);
afterEach(closeStackWatchers);

const readJson = async (path: string) =>
  JSON.parse(await readFile(path, "utf8")) as Record<string, unknown>;

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
    // The family's own staging, as every family stages: the runner hands it
    // the stack's context, the session's replayed ledger and the stack's
    // confirmed transactions.
    const stage = async (input: JourneyFixtureStage) => {
      seen = {
        operator2: [...stack.operator2.calls],
        context: input.context,
        directory: input.directory,
        ledgerAt: input.ledgerAt,
        readsConfirmedTransactions:
          typeof input.readConfirmedTransaction === "function",
        baseline: await readJson(join(directory, "workflow-baseline.json")),
        timing: await readJson(join(directory, "timing-plan.json")),
      };
      throw new Error("staged");
    };
    await expect(
      runAutonomousWatcherJourney(
        stack.runDirectory,
        { category: "doubleSpend", stage },
        { session },
      ),
    ).rejects.toThrow("staged");
    expect(seen).toMatchObject({
      // Operator-2's node stopped before the drill signed anything as it.
      operator2: ["stop"],
      context: stack.context,
      directory,
      ledgerAt: session.ledgerAt,
      readsConfirmedTransactions: true,
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
});
