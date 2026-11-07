import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  closeStackWatchers,
  liveStatus,
  removeStackDirectories,
  runningStackWatcher,
  seedWorkflowStarted,
  STACK_GENESIS,
} from "./attached-journey-session.test-support.js";
import type {
  JourneyBlock,
  JourneyFixtureStage,
  StagedJourney,
} from "./fixture.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import { openAttachedJourneySession } from "./journey-session.js";
import type { StackDaRefused } from "./stack-da-publication.js";

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

  describe("a fault the stack's committee refused", () => {
    const headerHash = "34".repeat(28);
    const fault = { headerHash } as JourneyBlock;
    const refused: StackDaRefused = {
      kind: "refused",
      headerHash,
      attestationDeadlineMs: 1_000,
      delivery: {
        threshold: 2,
        peers: { peerA: "accepted", peerB: "rejected: payload_invalid" },
      },
      removal: {
        kind: "agedOut",
        headerHash,
        removalTxHash: "ab".repeat(32),
        removedStateQueueOutRef: `${"ab".repeat(32)}#0`,
      },
    };
    // The family's staging, ending at the committee's observed refusal.
    const stageRefused = async (
      input: JourneyFixtureStage,
    ): Promise<StagedJourney> => {
      await input.onFaultCommitted?.(fault);
      return {
        predecessor: fault,
        current: fault,
        target: refused,
        commitHonestSuccessor: () =>
          Promise.reject(new Error("no successor builds on a refused fault")),
      };
    };
    const awaitingDa = {
      ready: false,
      reasons: ["landed_block_awaiting_da"],
      l1Follower: {
        readiness: [
          {
            reason: "landed_block_awaiting_da",
            detail: `block ${headerHash} awaits its DA payload`,
          },
        ],
      },
    };

    it("ends at its age-out with our node's awaiting-DA hold, without the watcher's proof or a successor", async () => {
      const stack = await runningStackWatcher(liveStatus());
      stack.served.nodeReadyz = awaitingDa;
      const session = await openAttachedJourneySession(
        stack.context,
        stack.options(),
      );
      const directory = join(
        stack.runDirectory,
        "work/journeys/canonicalDecodability",
      );
      await runAutonomousWatcherJourney(
        stack.runDirectory,
        { category: "canonicalDecodability", stage: stageRefused },
        { session },
      );
      await session.close();
      expect(await readJson(join(directory, "result.json"))).toMatchObject({
        status: "passed",
        executionPolicy: "committee-refusal",
        category: "canonicalDecodability",
        headerHash,
        refusal: {
          delivery: refused.delivery,
          removal: refused.removal,
          holdBinding: "header",
          holdObservations: 1,
        },
      });
      expect(existsSync(join(directory, "failure.json"))).toBe(false);
      // Nothing asked the watcher for a decision, a proof or its metrics.
      expect(new Set(stack.requests)).toEqual(new Set(["/v1/status"]));
      expect(stack.nodeRequests).toContain("/readyz");
    });

    it("fails when our node never held it awaiting its DA", async () => {
      const stack = await runningStackWatcher(liveStatus());
      const session = await openAttachedJourneySession(
        stack.context,
        stack.options(),
      );
      const directory = join(
        stack.runDirectory,
        "work/journeys/canonicalDecodability",
      );
      await expect(
        runAutonomousWatcherJourney(
          stack.runDirectory,
          { category: "canonicalDecodability", stage: stageRefused },
          { session },
        ),
      ).rejects.toThrow(
        `Our node never reported landed_block_awaiting_da for the refused fault ${headerHash}`,
      );
      expect(await readJson(join(directory, "failure.json"))).toMatchObject({
        activeStage: "committee refusal and age-out",
      });
      expect(existsSync(join(directory, "result.json"))).toBe(false);
    });

    it("stops watching our node's hold when a step after the fault's commit fails", async () => {
      const failing = [
        {
          // The stack watcher's configuration names no public DA to verify.
          category: "doubleSpend",
          target: { kind: "attested", txHash: "cd".repeat(32) },
          failedStage: "production public DA preflight",
        },
        {
          // No removal was observed, and this stack has no chain to await one on.
          category: "canonicalDecodability",
          target: { ...refused, removal: undefined },
          failedStage: "refused fault aged out",
        },
      ] as const;
      const runs = [];
      for (const { category, target, failedStage } of failing) {
        const stack = await runningStackWatcher(liveStatus());
        stack.served.nodeReadyz = awaitingDa;
        const session = await openAttachedJourneySession(
          stack.context,
          stack.options(),
        );
        const stage = async (
          input: JourneyFixtureStage,
        ): Promise<StagedJourney> => {
          await input.onFaultCommitted?.(fault);
          return {
            predecessor: fault,
            current: fault,
            target,
            commitHonestSuccessor: () =>
              Promise.reject(new Error("the run fails before its successor")),
          };
        };
        try {
          await expect(
            runAutonomousWatcherJourney(
              stack.runDirectory,
              { category, stage },
              { session },
            ),
          ).rejects.toThrow();
        } finally {
          await session.close();
        }
        expect(
          await readJson(
            join(stack.runDirectory, `work/journeys/${category}/failure.json`),
          ),
        ).toMatchObject({ activeStage: failedStage });
        expect(stack.nodeRequests).toContain("/readyz");
        runs.push({ stack, reads: stack.nodeRequests.length });
      }
      // A watch still running reads /readyz again within its 5 s interval.
      await pause(6_000);
      for (const { stack, reads } of runs)
        expect(stack.nodeRequests).toHaveLength(reads);
    }, 30_000);
  });
});
