import { randomUUID } from "node:crypto";
import { readdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import { openWatcherTrustedHeadAuthorityStore } from "../../src/runtime/trusted-head-authority.js";
import {
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";

const compaction = vi.hoisted(() => ({ fail: false }));
vi.mock(
  "../../src/runtime/trusted-head-authority.retention-floor.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../../src/runtime/trusted-head-authority.retention-floor.js")
      >();
    return {
      ...actual,
      compactTrustedHeadRecords: async (
        ...args: Parameters<typeof actual.compactTrustedHeadRecords>
      ) => {
        if (compaction.fail) throw new Error("ENOSPC: no space left on device");
        return actual.compactTrustedHeadRecords(...args);
      },
    };
  },
);

const initialize = async () => {
  const path = await directory();
  const finalityPolicy = policy();
  const options = {
    directory: path,
    policy: finalityPolicy,
    recordAuthenticationKey,
    maxRetainedRecords: 3,
  };
  const store = await openWatcherTrustedHeadAuthorityStore(options);
  let previous = null;
  for (let n = 0; n < 6; n++) {
    const next = head(finalityPolicy, n, "10");
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: previous,
        nextTrustedHead: next,
      }),
    ).toBe(true);
    previous = next;
  }
  return { path, options, store, previous };
};

describe("authenticated trusted-head compaction", () => {
  it("bounds disk history and resumes direct successor publication after restart", async () => {
    const { path, options, previous } = await initialize();
    expect(
      (await readdir(path)).filter((n) => /^\d{20}\.json$/u.test(n)).length,
    ).toBeLessThanOrEqual(3);
    const restarted = await openWatcherTrustedHeadAuthorityStore(options);
    expect(await restarted.readCurrent()).toEqual(previous);
    const next = head(options.policy, 6, "20");
    expect(
      await restarted.compareAndSwap({
        expectedTrustedHead: head(options.policy, 2, "10"),
        nextTrustedHead: head(options.policy, 3, "20"),
      }),
    ).toBe(false);
    expect(
      await restarted.compareAndSwap({
        expectedTrustedHead: previous,
        nextTrustedHead: next,
      }),
    ).toBe(true);
  });

  it("keeps a durable swap when the compaction after it fails, and compacts on the next swap", async () => {
    const { path, options, store, previous } = await initialize();
    const warnings: string[] = [];
    const write = vi
      .spyOn(process.stderr, "write")
      .mockImplementation((chunk: string | Uint8Array) => {
        warnings.push(String(chunk));
        return true;
      });
    const records = async () =>
      (await readdir(path)).filter((n) => /^\d{20}\.json$/u.test(n));
    const published = head(options.policy, 6, "20");
    try {
      compaction.fail = true;
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: previous,
          nextTrustedHead: published,
        }),
      ).toBe(true);
    } finally {
      compaction.fail = false;
      write.mockRestore();
    }
    expect(warnings.join("")).toContain("ENOSPC");
    expect(await store.readCurrent()).toEqual(published);
    expect(await records()).toHaveLength(4);
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: published,
        nextTrustedHead: head(options.policy, 7, "20"),
      }),
    ).toBe(true);
    expect(await records()).toHaveLength(3);
    expect(
      await (await openWatcherTrustedHeadAuthorityStore(options)).readCurrent(),
    ).toEqual(head(options.policy, 7, "20"));
  });

  it("refuses a changed floor, a missing floor and a replayed retired floor", async () => {
    const first = await initialize();
    const floorPath = join(first.path, "retention-floor.json");
    const original = await readFile(floorPath, "utf8");
    await writeFile(
      floorPath,
      original.replace('"revision":"2"', '"revision":"1"'),
    );
    await expect(
      openWatcherTrustedHeadAuthorityStore(first.options),
    ).rejects.toThrow();
    await writeFile(floorPath, original);
    const next = head(first.options.policy, 6, "20");
    expect(
      await first.store.compareAndSwap({
        expectedTrustedHead: first.previous,
        nextTrustedHead: next,
      }),
    ).toBe(true);
    await writeFile(floorPath, original);
    await expect(
      openWatcherTrustedHeadAuthorityStore(first.options),
    ).rejects.toThrow(/gap|floor/u);
    await rm(floorPath);
    await expect(
      openWatcherTrustedHeadAuthorityStore(first.options),
    ).rejects.toThrow(/gap/u);
  });

  it("survives interruption before floor publication and during old-prefix cleanup", async () => {
    const { path, options, previous } = await initialize();
    const unpublishedFloor = `.staged-${randomUUID()}.tmp`;
    await writeFile(
      join(path, unpublishedFloor),
      "interrupted unpublished bytes",
    );
    // A crash during prefix unlink may leave old records below the published floor.
    await writeFile(join(path, "00000000000000000000.json"), "retired record");
    const restarted = await openWatcherTrustedHeadAuthorityStore(options);
    expect(await restarted.readCurrent()).toEqual(previous);
    expect(await readdir(path)).not.toContain(unpublishedFloor);
    // The next compaction also removes the leftover prefix file.
    expect(
      await restarted.compareAndSwap({
        expectedTrustedHead: previous,
        nextTrustedHead: head(options.policy, 6, "20"),
      }),
    ).toBe(true);
    expect((await readdir(path)).sort()).toEqual([
      "00000000000000000004.json",
      "00000000000000000005.json",
      "00000000000000000006.json",
      "retention-floor.json",
    ]);
  });

  it("drops a torn final record left after a compaction and republishes its successor", async () => {
    const { path, options } = await initialize();
    // The chain now starts above the floor at revision 3, not at 0.
    await writeFile(join(path, "00000000000000000005.json"), "", "utf8");
    const restarted = await openWatcherTrustedHeadAuthorityStore(options);
    const fallback = head(options.policy, 4, "10");
    expect(await restarted.readCurrent()).toEqual(fallback);
    expect((await readdir(path)).sort()).toEqual([
      "00000000000000000003.json",
      "00000000000000000004.json",
      "retention-floor.json",
    ]);
    const republished = head(options.policy, 5, "20");
    expect(
      await restarted.compareAndSwap({
        expectedTrustedHead: fallback,
        nextTrustedHead: republished,
      }),
    ).toBe(true);
    expect(
      await (await openWatcherTrustedHeadAuthorityStore(options)).readCurrent(),
    ).toEqual(republished);
  });

  it("fails closed on a torn record that is not final after a compaction", async () => {
    const { path, options } = await initialize();
    await writeFile(join(path, "00000000000000000004.json"), "", "utf8");
    await expect(openWatcherTrustedHeadAuthorityStore(options)).rejects.toThrow(
      "record size",
    );
    expect(
      (await readdir(path)).filter((name) => name.endsWith(".json")),
    ).toHaveLength(4);
  });
});
