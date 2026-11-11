import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  createPublicTls,
  removePublicTls,
  tls,
} from "../../tests/full-stack-public-config.fixtures.js";
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
import {
  composedPublicOperator2,
  stopComposedServices,
} from "./public-operator2.test-support.js";
import type { StackDaRefused } from "./stack-da-publication.js";

afterAll(removeStackDirectories);
afterEach(async () => {
  vi.unstubAllEnvs();
  await stopComposedServices();
  await closeStackWatchers();
});

const readJson = async (path: string) =>
  JSON.parse(await readFile(path, "utf8")) as Record<string, unknown>;

// These fixtures stage by hand; only a time limit reads a staging's cover.
const stackShiftCoverMs = () => {
  throw new Error("A hand-staged fixture has no staging shift cover");
};

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
        { category: "doubleSpend", stackShiftCoverMs, stage },
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
        {
          category: "canonicalDecodability",
          stackShiftCoverMs,
          stage: stageRefused,
        },
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

    it("on a public stack, reads our node over HTTPS, and holds and resumes operator-2 behind its ingress", async () => {
      await createPublicTls();
      try {
        const stackTls = {
          caFile: join(tls.directory, "ca.pem"),
          certFile: join(tls.directory, "node.pem"),
          keyFile: join(tls.directory, "node.key"),
          servername: tls.host,
        };
        // Operator-2's public instance, running: its ingress fronts its node.
        const operator2 = await composedPublicOperator2(stackTls);
        await operator2.up();
        vi.stubEnv("PATH", operator2.path);
        const stack = await runningStackWatcher(liveStatus(), {
          tls: stackTls,
        });
        stack.served.nodeReadyz = awaitingDa;
        // The journey's own Compose control of operator-2, not a stand-in.
        const session = await openAttachedJourneySession(stack.context, {
          exec: stack.exec,
          operator2: {
            composeProject: operator2.composeProject,
            endpoint: operator2.endpoint,
          },
        });
        try {
          const before = (await operator2.invocations()).length;
          await runAutonomousWatcherJourney(
            stack.runDirectory,
            {
              category: "canonicalDecodability",
              stackShiftCoverMs,
              stage: stageRefused,
            },
            { session },
          );
          expect(
            await readJson(
              join(
                stack.runDirectory,
                "work/journeys/canonicalDecodability/result.json",
              ),
            ),
          ).toMatchObject({
            status: "passed",
            refusal: { holdBinding: "header", holdObservations: 1 },
          });
          // Our node's hold was read over its HTTPS port.
          expect(stack.nodeRequests).toContain("/readyz");
          // Held: no service of operator-2's runs and nothing answers its
          // endpoint, the ingress included.
          await expect(session.operator2.assertStopped()).resolves.toEqual(
            expect.objectContaining({
              runningServices: ["postgres"],
              readyz: "connection_refused",
            }),
          );
          // Resumed: the ingress answers its own 503 until the node serves.
          await expect(session.operator2.resume()).resolves.toEqual(
            expect.objectContaining({
              terminal: "ready",
              observedReasons: [[]],
            }),
          );
          const answers = await operator2.ingressAnswers();
          expect(answers).toContain("503 unavailable");
          expect(answers.at(-1)).toBe("200");
          expect((await operator2.invocations()).slice(before)).toEqual(
            expect.arrayContaining([
              "stop midgard-node public-ingress",
              "start midgard-node public-ingress",
            ]),
          );
        } finally {
          await session.close();
        }
      } finally {
        await removePublicTls();
      }
    }, 60_000);

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
          {
            category: "canonicalDecodability",
            stackShiftCoverMs,
            stage: stageRefused,
          },
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
      // Which staging was asked to hand our node its transfer before the
      // fault's commit.
      const handsOver: Record<string, boolean> = {};
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
          handsOver[category] = input.beforeFaultCommit !== undefined;
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
              { category, stackShiftCoverMs, stage },
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
      // A refused fault has no successor, so no transfer goes ahead of it.
      expect(handsOver).toEqual({
        doubleSpend: true,
        canonicalDecodability: false,
      });
      // A watch still running reads /readyz again within its 5 s interval.
      await pause(6_000);
      for (const { stack, reads } of runs)
        expect(stack.nodeRequests).toHaveLength(reads);
    }, 30_000);
  });
});
