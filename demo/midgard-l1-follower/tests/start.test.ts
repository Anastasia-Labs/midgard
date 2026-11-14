import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  type FactStore,
  openSqliteFactStore,
  startWhenFree,
} from "../src/index.js";
import { SIM_ORIGIN, simStoreOptions } from "../src/testing/index.js";

const K = 3;

let dir: string;
const opened: FactStore[] = [];
beforeEach(async () => {
  dir = await mkdtemp(join(tmpdir(), "l1-start-"));
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
  await rm(dir, { recursive: true, force: true });
});

const storeAt = (): FactStore => {
  const store = openSqliteFactStore({
    ...simStoreOptions([], K, "sqlite"),
    path: join(dir, "follower.sqlite"),
  });
  opened.push(store);
  return store;
};

describe("startWhenFree", () => {
  it("waits out a lease another store holds, and gives up on abort", async () => {
    const holder = storeAt();
    expect(await holder.start()).toMatchObject({ kind: "ready" });
    expect(await holder.initialize(SIM_ORIGIN)).toMatchObject({
      kind: "initialized",
    });
    const waiter = storeAt();
    const lines: string[] = [];
    const waiting = startWhenFree(waiter, {
      backoffMs: { initial: 2, max: 8 },
      log: (line) => lines.push(line),
    });
    await new Promise((resolve) => setTimeout(resolve, 40));
    await holder.close();
    opened.splice(opened.indexOf(holder), 1);
    expect(await waiting).toMatchObject({ kind: "ready" });
    expect(lines.some((line) => line.includes("store locked"))).toBe(true);
    const third = storeAt();
    const abort = new AbortController();
    const gaveUp = startWhenFree(third, {
      signal: abort.signal,
      backoffMs: { initial: 2, max: 8 },
    });
    await new Promise((resolve) => setTimeout(resolve, 20));
    abort.abort();
    expect(await gaveUp).toBeUndefined();
  });
});
