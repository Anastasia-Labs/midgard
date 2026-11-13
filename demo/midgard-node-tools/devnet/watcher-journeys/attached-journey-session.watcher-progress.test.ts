import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  closeStackWatchers,
  decidedAt,
  liveStatus,
  removeStackDirectories,
  runningStackWatcher,
} from "./attached-journey-session.test-support.js";
import { createJourneyRunPolls } from "./journey-runner.polls.js";
import { openAttachedJourneySession } from "./journey-session.js";

const directories: string[] = [];
afterAll(async () => {
  await removeStackDirectories();
  await Promise.all(
    directories.map((directory) =>
      rm(directory, { recursive: true, force: true }),
    ),
  );
});
afterEach(closeStackWatchers);

const STAGE = "healthy processing after correction";

type Stack = Awaited<ReturnType<typeof runningStackWatcher>>;

/**
 * A family's healthy-processing wait on the stack watcher, once `serve` has
 * set how the watcher answers, with a stall bound of 2.5 s: it polls once a
 * second and its decision arrives on the sixth read, about 5 s in, so it
 * answers only if the watcher's progress restarted the bound.
 */
const healthyWait = async (
  stack: Stack,
  serve: (served: Stack["served"]) => void,
) => {
  const session = await openAttachedJourneySession(
    stack.context,
    stack.options(),
  );
  serve(stack.served);
  const directory = await mkdtemp(join(tmpdir(), "watcher-progress-"));
  directories.push(directory);
  let reads = 0;
  try {
    return await createJourneyRunPolls({
      directory,
      onStage: () => {},
      signal: new AbortController().signal,
    }).pollWhileReplaying(
      STAGE,
      () => Promise.resolve((reads += 1) >= 6 ? "healthy decision" : undefined),
      session.watcher.watcherProgress,
      2_500,
    );
  } finally {
    await session.close();
  }
};

const stalled = new RegExp(`^Timed out waiting for ${STAGE}$`, "u");

describe("a wait budgeted by the stack watcher's progress", () => {
  it("fails at its bound while the watcher's reason detail changes on every read and it decides nothing new", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      served.l1Source.push(decidedAt(1, 100));
      served.statusRead = (reads) => ({
        ...liveStatus(),
        readinessReasons: ["l1_node_behind", "l1_follower_apply_failed"],
        l1Readiness: [
          {
            reason: "l1_node_behind",
            detail: `the node is ${(40 + reads).toString()} s behind wall-clock time`,
          },
          {
            reason: "l1_follower_apply_failed",
            detail: `applying block 101 failed ${reads.toString()} times`,
          },
        ],
      });
    });
    await expect(waited).rejects.toThrow(stalled);
  });

  it("fails at its bound while the watcher's status goes unanswered for a whole read and then answers again", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      served.l1Source.push(decidedAt(1, 100));
      // Three 503s in a row exhaust one operations read's tries.
      served.statusRead = (reads) =>
        reads % 4 === 3 ? liveStatus() : "unavailable";
    });
    await expect(waited).rejects.toThrow(stalled);
  });

  it("stays open past its bound while the watcher decides each new block", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      served.l1Source.push(decidedAt(1, 100));
      served.statusRead = (reads) => {
        served.l1Source.push(decidedAt(reads + 2, reads + 101));
        return liveStatus();
      };
    });
    await expect(waited).resolves.toBe("healthy decision");
  });

  it("counts the blocks a restarted watcher decides beyond the ones it decided before", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      served.l1Source.push(
        decidedAt(1, 100),
        decidedAt(2, 101),
        decidedAt(3, 102),
      );
      // The restarted process numbers its decision passes from 1 again.
      served.statusRead = (reads) => {
        served.l1Source = Array.from({ length: reads + 1 }, (_, pass) =>
          decidedAt(pass + 1, pass + 103),
        );
        return liveStatus();
      };
    });
    await expect(waited).resolves.toBe("healthy decision");
  });

  it("fails at its bound while a restarted watcher decides again only blocks it decided before", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      served.l1Source.push(
        decidedAt(1, 100),
        decidedAt(2, 101),
        decidedAt(3, 102),
      );
      served.statusRead = (reads) => {
        served.l1Source = Array.from({ length: (reads % 3) + 1 }, (_, pass) =>
          decidedAt(pass + 1, pass + 100),
        );
        return liveStatus();
      };
    });
    await expect(waited).rejects.toThrow(stalled);
  });

  it("counts each startup stage the watcher begins, not a stage retrying with new error text", async () => {
    const starting = (stage: string, error?: string) => ({
      liveness: "live",
      readiness: "not_ready",
      readinessReasons: [`startup:${stage}`],
      l1Readiness: [],
      startup: {
        stage,
        outcome: "pending",
        ...(error === undefined ? {} : { error }),
      },
    });
    const advancing = healthyWait(
      await runningStackWatcher(liveStatus()),
      (served) => {
        served.statusRead = (reads) => starting(`stage-${reads.toString()}`);
      },
    );
    await expect(advancing).resolves.toBe("healthy decision");
    const retrying = healthyWait(
      await runningStackWatcher(liveStatus()),
      (served) => {
        served.statusRead = (reads) =>
          starting(
            "open-follower",
            `connect ECONNREFUSED (try ${reads.toString()})`,
          );
      },
    );
    await expect(retrying).rejects.toThrow(stalled);
  });
});
