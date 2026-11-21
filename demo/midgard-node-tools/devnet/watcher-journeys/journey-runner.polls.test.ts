import { mkdtemp, rm } from "node:fs/promises";
import { createServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import { createJourneyRunPolls } from "./journey-runner.polls.js";

const directories: string[] = [];
afterAll(() =>
  Promise.all(
    directories.map((directory) =>
      rm(directory, { recursive: true, force: true }),
    ),
  ),
);

const familyPolls = async (
  signal: AbortSignal = new AbortController().signal,
) => {
  const directory = await mkdtemp(join(tmpdir(), "journey-polls-"));
  directories.push(directory);
  const stages: string[] = [];
  return {
    stages,
    ...createJourneyRunPolls({
      directory,
      onStage: (name) => {
        stages.push(name);
      },
      signal,
    }),
  };
};

/** A loopback URL nothing listens on: every request to it is refused. */
const refusingEndpoint = async () => {
  const probe = createServer();
  await new Promise<void>((resolve) =>
    probe.listen(0, "127.0.0.1", () => resolve()),
  );
  const { port } = probe.address() as { port: number };
  await new Promise<void>((resolve) => probe.close(() => resolve()));
  return `http://127.0.0.1:${port.toString()}/v1/status`;
};

describe("a family's bounded waits", () => {
  it("read through a read that got no answer, within the wait's bound", async () => {
    const polls = await familyPolls();
    const endpoint = await refusingEndpoint();
    let reads = 0;
    await expect(
      polls.poll(
        "watcher decision",
        async () => {
          reads += 1;
          if (reads === 1) await fetch(endpoint);
          return "decided";
        },
        30_000,
      ),
    ).resolves.toBe("decided");
    expect(reads).toBe(2);
    expect(polls.stages).toEqual(["watcher decision"]);
  });

  it("fail by name once the bound passes with only lost answers, carrying the last one", async () => {
    const polls = await familyPolls();
    const endpoint = await refusingEndpoint();
    const failure = await polls
      .poll(
        "watcher decision",
        async () => {
          await fetch(endpoint);
          return "decided";
        },
        0,
      )
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect((failure as Error).message).toBe(
      "Timed out waiting for watcher decision",
    );
    expect((failure as Error).cause).toMatchObject({
      name: "TypeError",
      message: "fetch failed",
    });
  });

  it("fail at once on an answer that names a refusal", async () => {
    const polls = await familyPolls();
    let reads = 0;
    await expect(
      polls.poll(
        "watcher decision",
        () => {
          reads += 1;
          return Promise.reject(new Error("The watcher decided it unprovable"));
        },
        30_000,
      ),
    ).rejects.toThrow("The watcher decided it unprovable");
    expect(reads).toBe(1);
  });

  it("read through a lost answer in a wait budgeted by the watcher's progress", async () => {
    const polls = await familyPolls();
    const endpoint = await refusingEndpoint();
    let reads = 0;
    await expect(
      polls.pollWhileReplaying(
        "healthy processing",
        async () => {
          reads += 1;
          if (reads === 1) await fetch(endpoint);
          return "healthy";
        },
        () => Promise.resolve(0),
        30_000,
      ),
    ).resolves.toBe("healthy");
    expect(reads).toBe(2);
  });

  describe("once the family is aborted", () => {
    const aborted = new Error("the family's time limit passed");

    it.each([
      [
        "a wait",
        (polls: Awaited<ReturnType<typeof familyPolls>>) =>
          polls.poll(
            "watcher decision",
            () => Promise.resolve(undefined),
            3_000,
          ),
      ],
      [
        "a wait budgeted by the watcher's progress",
        (polls: Awaited<ReturnType<typeof familyPolls>>) =>
          polls.pollWhileReplaying(
            "healthy processing",
            () => Promise.resolve(undefined),
            () => Promise.resolve(0),
            3_000,
          ),
      ],
    ])("stop %s at once, with the abort's reason", async (_, wait) => {
      const family = new AbortController();
      const polls = await familyPolls(family.signal);
      const started = performance.now();
      const waited = wait(polls);
      setTimeout(() => family.abort(aborted), 200);
      await expect(waited).rejects.toBe(aborted);
      expect(performance.now() - started).toBeLessThan(1_500);
    });

    it("start no stage", async () => {
      const family = new AbortController();
      const polls = await familyPolls(family.signal);
      family.abort(aborted);
      let ran = false;
      await expect(
        polls.stage("invalid commitment and DA attestations", () => {
          ran = true;
          return Promise.resolve("staged");
        }),
      ).rejects.toBe(aborted);
      expect(ran).toBe(false);
      expect(polls.stages).toEqual([]);
    });
  });
});
