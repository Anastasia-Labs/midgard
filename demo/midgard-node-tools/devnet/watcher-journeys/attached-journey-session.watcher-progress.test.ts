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

  it("counts the blocks a restarted watcher decides while its new passes stay below the pass it last read", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const waited = healthyWait(stack, (served) => {
      // The first read meets a watcher that made 50 decision passes.
      served.l1Source = Array.from({ length: 50 }, (_, pass) =>
        decidedAt(pass + 1, pass + 100),
      );
      // From the second read on, a restarted process numbers its passes from
      // 1 again, deciding each a new block, and never reaches pass 50.
      served.statusRead = (reads) => {
        if (reads > 0)
          served.l1Source = Array.from({ length: reads }, (_, pass) =>
            decidedAt(pass + 1, pass + 150),
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

  describe("a restarted watcher's startup", () => {
    // A starting watcher's status, at `stage`, with its latest report.
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
    // Its L1 follower held below the node's tip, as startup reports it.
    const catchingUp = (cursorHeight: number, tipHeight: number) =>
      starting(
        "l1_follower_ready",
        `the L1 follower is not ready: l1_follower_catching_up: cursor height ${cursorHeight.toString()}, node tip height ${tipHeight.toString()}`,
      );

    it("stays open past its bound while the follower's cursor rises through one long catch-up stage", async () => {
      const stack = await runningStackWatcher(liveStatus());
      const waited = healthyWait(stack, (served) => {
        served.l1Source.push(decidedAt(1, 100));
        // Serving first, then restarted and catching up from the L1 node.
        served.statusRead = (reads) =>
          reads === 0 ? liveStatus() : catchingUp(100 + reads, 5_000);
      });
      await expect(waited).resolves.toBe("healthy decision");
    });

    it("fails at its bound while the follower's cursor holds and only the node's tip and the text move", async () => {
      const stack = await runningStackWatcher(liveStatus());
      const waited = healthyWait(stack, (served) => {
        served.l1Source.push(decidedAt(1, 100));
        served.statusRead = (reads) =>
          reads === 0 ? liveStatus() : catchingUp(101, 5_000 + reads);
      });
      await expect(waited).rejects.toThrow(stalled);
    });

    it("counts again the startup stages a watcher begins after it restarted from serving", async () => {
      const stack = await runningStackWatcher(liveStatus());
      const waited = healthyWait(stack, (served) => {
        // Stages 0 to 2 of a first startup, serving, then a restart that
        // begins the same stages again.
        served.statusRead = (reads) =>
          reads < 3
            ? starting(`stage-${reads.toString()}`)
            : reads === 3
              ? liveStatus()
              : starting(`stage-${(reads - 4).toString()}`);
      });
      await expect(waited).resolves.toBe("healthy decision");
    });
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
