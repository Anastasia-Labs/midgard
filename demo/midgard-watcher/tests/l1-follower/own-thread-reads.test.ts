/**
 * The follower's read of our own computation threads: the live outputs
 * under the computation-thread policy whose step datum names our key, each
 * classified against the tip and the block `finalDepth` deep, paged by
 * out-ref under a fixed cap. Nothing here has a stored workflow record.
 */
import { describe, expect, it } from "vitest";

import {
  compareOwnThreadOutRefs,
  readWatcherOwnThread,
  readWatcherOwnThreadPage,
  type WatcherOwnThread,
} from "../../src/l1-follower/own-thread-reads.js";
import {
  CATEGORY_ID,
  FINAL_DEPTH,
  OUR_SCOPE,
  STEP_SCRIPT,
  THEIR_KEY,
  threadScene,
} from "../support/own-thread-sim.js";

type Scene = Awaited<ReturnType<typeof threadScene>>;

const page = async (s: Scene, after: string | null = null, limit = 64) => {
  const read = await readWatcherOwnThreadPage(s.store, {
    ...OUR_SCOPE,
    after,
    limit,
  });
  if (read.kind !== "page") throw new Error(`unready: ${read.reason}`);
  return read;
};

const dispositions = async (s: Scene) =>
  Object.fromEntries(
    (await page(s)).threads.map(({ outRef, disposition }) => [
      outRef,
      disposition,
    ]),
  );

describe("own-thread reads from the follower", () => {
  it("reads only our live threads, with their header, category and lovelace", async () => {
    const s = await threadScene();
    try {
      const [header] = s.headers as [string];
      const ours = await s.open(header, undefined, 4_000_000n);
      await s.open(header, THEIR_KEY);
      // A policy unit that is no thread token (its name is too short).
      await s.open(header, undefined, 3_000_000n, "aa");
      const spent = await s.open(header);
      await s.forward([s.cancelTx(spent)]);
      await s.empty(FINAL_DEPTH);

      const read = await page(s);
      expect(read.scanned).toBe(3);
      expect(read.through).toBeNull();
      expect(read.threads).toEqual([
        expect.objectContaining({
          outRef: ours,
          categoryId: CATEGORY_ID,
          headerHash: header,
          lovelace: 4_000_000n,
          paymentScriptHash: STEP_SCRIPT,
          disposition: "workflow",
        }),
      ]);
      expect(
        await readWatcherOwnThread(s.store, { ...OUR_SCOPE, outRef: spent }),
      ).toBeNull();
    } finally {
      await s.store.close();
    }
  });

  it("reopens a spent thread when a rollback undoes its spend", async () => {
    const s = await threadScene();
    try {
      const thread = await s.open(s.headers[0]!);
      await s.empty(FINAL_DEPTH);
      await s.forward([s.cancelTx(thread)]);
      expect((await page(s)).threads).toEqual([]);
      await s.backward(1);
      expect((await page(s)).threads.map(({ outRef }) => outRef)).toEqual([
        thread,
      ]);
    } finally {
      await s.store.close();
    }
  });

  it("is unready until the follower holds `finalDepth` blocks past its origin", async () => {
    const s = await threadScene();
    try {
      const read = await readWatcherOwnThreadPage(s.store, {
        ...OUR_SCOPE,
        finalDepth: 50,
        after: null,
        limit: 8,
      });
      expect(read.kind).toBe("unready");
    } finally {
      await s.store.close();
    }
  });

  it("pages under the cap rather than truncating", async () => {
    const s = await threadScene();
    try {
      const [header] = s.headers as [string];
      const opened: string[] = [];
      for (let i = 0; i < 5; i += 1) opened.push(await s.open(header));
      await s.open(header, THEIR_KEY);
      await s.empty(FINAL_DEPTH);
      const seen: WatcherOwnThread[] = [];
      let after: string | null = null;
      let pages = 0;
      for (;;) {
        const read = await page(s, after, 2);
        expect(read.scanned).toBeLessThanOrEqual(2);
        seen.push(...read.threads);
        pages += 1;
        if (read.through === null) break;
        after = read.through;
      }
      // Six live outputs under the policy, two a page: the last page is empty.
      expect(pages).toBe(4);
      expect(seen.map(({ outRef }) => outRef)).toEqual(
        [...opened].sort(compareOwnThreadOutRefs),
      );
    } finally {
      await s.store.close();
    }
  });

  it("is due for reclaim once a removal is `finalDepth` deep, not a block before", async () => {
    const s = await threadScene();
    try {
      const thread = await s.open(s.headers[0]!);
      await s.empty(FINAL_DEPTH);
      expect(await dispositions(s)).toEqual({ [thread]: "workflow" });
      await s.removeTail();
      // The removal at depth 1 .. finalDepth - 1: settling.
      for (let depth = 1; depth < FINAL_DEPTH; depth += 1) {
        expect(await dispositions(s), `depth ${depth.toString()}`).toEqual({
          [thread]: "settling",
        });
        await s.empty(1);
      }
      expect(await dispositions(s)).toEqual({ [thread]: "reclaim" });
      expect(
        (await readWatcherOwnThread(s.store, { ...OUR_SCOPE, outRef: thread }))
          ?.disposition,
      ).toBe("reclaim");
    } finally {
      await s.store.close();
    }
  });

  it("returns a reclaim to its workflow when a rollback returns the header", async () => {
    const s = await threadScene();
    try {
      const thread = await s.open(s.headers[0]!);
      await s.empty(FINAL_DEPTH);
      await s.removeTail();
      await s.empty(FINAL_DEPTH - 1);
      expect(await dispositions(s)).toEqual({ [thread]: "reclaim" });
      await s.backward(FINAL_DEPTH);
      expect(await dispositions(s)).toEqual({ [thread]: "workflow" });
    } finally {
      await s.store.close();
    }
  });

  it("reclaims the threads of a merged header, two on one header alike", async () => {
    const s = await threadScene({ headers: 2 });
    try {
      const [first, second] = s.headers as [string, string];
      const a = await s.open(first);
      const b = await s.open(first);
      const c = await s.open(second);
      await s.empty(FINAL_DEPTH);
      await s.mergeOldest();
      await s.empty(FINAL_DEPTH - 1);
      expect(await dispositions(s)).toEqual({
        [a]: "reclaim",
        [b]: "reclaim",
        [c]: "workflow",
      });
    } finally {
      await s.store.close();
    }
  });

  it("lets a thread newer than the deep block settle, though its header never stood", async () => {
    const s = await threadScene();
    try {
      const never = "5e".repeat(28);
      const thread = await s.open(never);
      expect(await dispositions(s)).toEqual({ [thread]: "settling" });
      await s.empty(FINAL_DEPTH - 2);
      expect(await dispositions(s)).toEqual({ [thread]: "settling" });
      await s.empty(1);
      expect(await dispositions(s)).toEqual({ [thread]: "reclaim" });
    } finally {
      await s.store.close();
    }
  });
});
