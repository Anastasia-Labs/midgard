import { randomUUID } from "node:crypto";
import { readdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import { openWatcherTrustedHeadAuthorityStore } from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  directory,
  head,
  hex32,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";

// Holds each link() until a test releases it, so two writers can both pass
// their chain check before either publishes.
const linkGate = vi.hoisted(() => ({
  current: null as (() => Promise<void>) | null,
}));

vi.mock("node:fs/promises", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs/promises")>();
  return {
    ...actual,
    link: async (...args: Parameters<typeof actual.link>) => {
      await linkGate.current?.();
      return await actual.link(...args);
    },
  };
});

const finalityPolicy = policy();
const first = head(finalityPolicy, 0, "10");
const second = head(finalityPolicy, 1, "20");
const third = head(finalityPolicy, 2, "30");
const recordPath = (path: string, revision: number): string =>
  join(path, `${revision.toString().padStart(20, "0")}.json`);

const open = async (path: string) =>
  await openWatcherTrustedHeadAuthorityStore({
    directory: path,
    policy: finalityPolicy,
    recordAuthenticationKey,
  });

const makeChain = async (): Promise<string> => {
  const path = await directory();
  const store = await open(path);
  let expected = null;
  for (const next of [first, second, third]) {
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: expected,
        nextTrustedHead: next,
      }),
    ).toBe(true);
    expected = next;
  }
  return path;
};

describe("trusted-head authority crash recovery", () => {
  it("drops an empty or torn final record left by an interrupted create", async () => {
    const complete = await readFile(recordPath(await makeChain(), 2), "utf8");
    for (const torn of [
      "",
      complete.slice(0, -1),
      complete.slice(0, 7),
      "\0".repeat(complete.length),
    ]) {
      const path = await makeChain();
      await writeFile(recordPath(path, 2), torn, "utf8");
      const store = await open(path);
      expect(await store.readCurrent()).toEqual(second);
      expect((await readdir(path)).sort()).toEqual([
        "00000000000000000000.json",
        "00000000000000000001.json",
      ]);
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: second,
          nextTrustedHead: third,
        }),
      ).toBe(true);
      expect(await (await open(path)).readCurrent()).toEqual(third);
    }
  });

  it("fails closed on a torn record that is not final, keeping every record", async () => {
    for (const torn of ["", "{"]) {
      const path = await makeChain();
      await writeFile(recordPath(path, 1), torn, "utf8");
      await expect(open(path)).rejects.toThrow(/record size|malformed/u);
      expect(await readdir(path)).toHaveLength(3);
    }

    // A torn final record is not dropped while an earlier record is torn.
    const both = await makeChain();
    await writeFile(recordPath(both, 1), "", "utf8");
    await writeFile(recordPath(both, 2), "", "utf8");
    await expect(open(both)).rejects.toThrow("record size");
    expect(await readdir(both)).toHaveLength(3);

    // Nor when it does not continue the chain.
    const gap = await makeChain();
    await writeFile(recordPath(gap, 4), "", "utf8");
    await expect(open(gap)).rejects.toThrow("gap");
    expect(await readdir(gap)).toHaveLength(4);
  });

  it("keeps a parseable final record under full authentication", async () => {
    const path = await makeChain();
    const forged = JSON.parse(await readFile(recordPath(path, 2), "utf8")) as {
      recordMac: string;
    };
    forged.recordMac = hex32("ee");
    await writeFile(recordPath(path, 2), watcherCanonicalJson(forged), "utf8");
    await expect(open(path)).rejects.toThrow("sidecar record MAC");
    expect(await readdir(path)).toHaveLength(3);
  });

  it("reopens at the previous revision after a crash between staging and link", async () => {
    const path = await makeChain();
    const staged = join(path, `.staged-${randomUUID()}.tmp`);
    const tornStaged = join(path, `.staged-${randomUUID()}.tmp`);
    await writeFile(
      staged,
      await readFile(recordPath(path, 2), "utf8"),
      "utf8",
    );
    await writeFile(tornStaged, "{", "utf8");
    // The link never happened, so the revision name does not exist.
    await rm(recordPath(path, 2));

    const store = await open(path);
    expect(await store.readCurrent()).toEqual(second);
    expect((await readdir(path)).sort()).toEqual([
      "00000000000000000000.json",
      "00000000000000000001.json",
    ]);
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: second,
        nextTrustedHead: third,
      }),
    ).toBe(true);
    expect(await (await open(path)).readCurrent()).toEqual(third);
  });

  it("refuses the losing writer when two swaps race to link one revision", async () => {
    const path = await directory();
    const [left, right] = [await open(path), await open(path)];
    expect(
      await left.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: first,
      }),
    ).toBe(true);

    let arrivals = 0;
    let release!: () => void;
    const released = new Promise<void>((resolve) => {
      release = resolve;
    });
    linkGate.current = async () => {
      arrivals += 1;
      if (arrivals === 2) release();
      await released;
    };
    try {
      const results = await Promise.all([
        left.compareAndSwap({
          expectedTrustedHead: first,
          nextTrustedHead: second,
        }),
        right.compareAndSwap({
          expectedTrustedHead: first,
          nextTrustedHead: head(finalityPolicy, 1, "a0"),
        }),
      ]);
      expect(arrivals).toBe(2);
      expect(results.filter(Boolean)).toHaveLength(1);
      const winner = results[0] ? second : head(finalityPolicy, 1, "a0");
      expect(await left.readCurrent()).toEqual(winner);
      expect(await (await open(path)).readCurrent()).toEqual(winner);
    } finally {
      linkGate.current = null;
    }
    expect((await readdir(path)).sort()).toEqual([
      "00000000000000000000.json",
      "00000000000000000001.json",
    ]);
  });
});
