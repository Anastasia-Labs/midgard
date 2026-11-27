import { expect, it } from "vitest";

import { createJourneyReplayLifetime } from "./journey-replay-lifetime.js";

/** A checkpoint write may be awaiting authenticated DA/source reads while shutdown starts.
 * This unit owns that lifetime; the nearest replay test instead owns compiled identity admission. */
it("keeps the follower store open until a pending checked checkpoint write finishes", async () => {
  const lifetime = createJourneyReplayLifetime();
  let finishWrite: (() => void) | undefined;
  let storeClosed = false;
  const writing = lifetime.run(async () => {
    await new Promise<void>((resolve) => {
      finishWrite = resolve;
    });
    if (storeClosed)
      throw new Error("checkpoint wrote after follower store closed");
    return "checked checkpoint persisted";
  });
  const stopping = lifetime.close(async () => {
    storeClosed = true;
  });
  expect(storeClosed).toBe(false);
  finishWrite!();
  await expect(writing).resolves.toBe("checked checkpoint persisted");
  await stopping;
  expect(storeClosed).toBe(true);
});

it("refuses new reads during shutdown and shares the owned close result", async () => {
  const lifetime = createJourneyReplayLifetime();
  let finishRead: (() => void) | undefined;
  let stops = 0;
  const reading = lifetime.run(
    () =>
      new Promise<void>((resolve) => {
        finishRead = resolve;
      }),
  );
  const stop = async () => {
    stops += 1;
  };
  const stopping = lifetime.close(stop);
  expect(lifetime.close(stop)).toBe(stopping);
  await expect(lifetime.run(async () => "late read")).rejects.toThrow(
    "Journey replay host is closed",
  );
  expect(stops).toBe(0);
  finishRead!();
  await reading;
  await stopping;
  expect(stops).toBe(1);
});

it("joins a failed active read before releasing the store without replacing the caller's failure", async () => {
  const lifetime = createJourneyReplayLifetime();
  let failRead: ((error: Error) => void) | undefined;
  let storeClosed = false;
  const reading = lifetime.run(
    () =>
      new Promise<void>((_, reject) => {
        failRead = reject;
      }),
  );
  const caller = expect(reading).rejects.toThrow("authenticated source lost");
  const stopping = lifetime.close(async () => {
    storeClosed = true;
  });
  expect(storeClosed).toBe(false);
  failRead!(new Error("authenticated source lost"));
  await caller;
  await stopping;
  expect(storeClosed).toBe(true);
});

it("serializes concurrent checked writes that share the keeper checkpoint path", async () => {
  const lifetime = createJourneyReplayLifetime();
  let finishFirst: (() => void) | undefined;
  const entries: string[] = [];
  const first = lifetime.run(async () => {
    entries.push("first started");
    await new Promise<void>((resolve) => {
      finishFirst = resolve;
    });
    entries.push("first persisted");
  });
  const second = lifetime.run(async () => {
    entries.push("second persisted");
  });
  expect(entries).toEqual(["first started"]);
  finishFirst!();
  await Promise.all([first, second]);
  expect(entries).toEqual([
    "first started",
    "first persisted",
    "second persisted",
  ]);
  await lifetime.close(async () => {});
});
