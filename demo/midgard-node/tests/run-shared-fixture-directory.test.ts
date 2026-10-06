import { spawnSync } from "node:child_process";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  invalidateRunSharedFixtures,
  loadOrCreateRunSharedFixture,
  RUN_SHARED_FIXTURE_DIRECTORY_ENV,
} from "./helpers/run-shared-fixture-directory.js";

/** A `create` that must never run. */
const neverCreate = vi.fn(async () => ({
  shared: "never",
  created: undefined,
}));

/** A pid no process holds: a child that has already exited. */
const endedPid = () => spawnSync(process.execPath, ["-e", ""]).pid;

let directory: string;
const inherited = process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];

beforeEach(async () => {
  directory = await mkdtemp(join(tmpdir(), "midgard-shared-fixture-test-"));
  process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] = directory;
});

afterEach(async () => {
  vi.useRealTimers();
  neverCreate.mockClear();
  if (inherited === undefined)
    delete process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];
  else process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] = inherited;
  await rm(directory, { recursive: true, force: true });
});

/** A `create` that counts its calls and resolves when `release` is called. */
const gatedCreate = (value: unknown) => {
  let calls = 0;
  let release!: () => void;
  const released = new Promise<void>((resolve) => (release = resolve));
  return {
    calls: () => calls,
    release,
    create: async () => {
      calls += 1;
      await released;
      return { shared: value, created: `created ${String(calls)}` };
    },
  };
};

describe("a run-shared fixture", () => {
  it("is created once by the first caller and read by callers that waited on its claim", async () => {
    const gate = gatedCreate({ ledger: [1n, 2n], bytes: Buffer.from("ab") });
    const first = loadOrCreateRunSharedFixture("deployment", gate.create);
    // The first caller holds the claim while it creates.
    await vi.waitFor(async () =>
      expect(await readdir(directory)).toContain("deployment.v8.lock"),
    );
    const second = loadOrCreateRunSharedFixture("deployment", gate.create);
    gate.release();

    expect(await first).toEqual({
      shared: { ledger: [1n, 2n], bytes: Buffer.from("ab") },
      created: "created 1",
    });
    // The waiter reads the published copy; only the creator sees `created`.
    expect(await second).toEqual({
      shared: { ledger: [1n, 2n], bytes: Buffer.from("ab") },
    });
    expect(gate.calls()).toBe(1);
    expect((await readdir(directory)).sort()).toEqual(["deployment.v8"]);
  });

  it("is read from the published copy by a later caller without creating it again", async () => {
    await loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "deployed",
      created: undefined,
    }));
    expect(
      await loadOrCreateRunSharedFixture("deployment", neverCreate),
    ).toEqual({ shared: "deployed" });
    expect(neverCreate).not.toHaveBeenCalled();
  });

  it("is created by a waiter when the holder released its claim without sharing", async () => {
    let fail!: (cause: Error) => void;
    const holder = loadOrCreateRunSharedFixture(
      "deployment",
      () =>
        new Promise<never>((_, reject) => {
          fail = reject;
        }),
    );
    await vi.waitFor(async () =>
      expect(await readdir(directory)).toContain("deployment.v8.lock"),
    );
    const waiter = loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "redeployed",
      created: "by the waiter",
    }));
    fail(new Error("deployment failed"));

    await expect(holder).rejects.toThrow("deployment failed");
    expect(await waiter).toEqual({
      shared: "redeployed",
      created: "by the waiter",
    });
  });

  it("is created by a waiter when the claim names a process that ended", async () => {
    await writeFile(join(directory, "deployment.v8.lock"), String(endedPid()));
    expect(
      await loadOrCreateRunSharedFixture("deployment", async () => ({
        shared: "redeployed",
        created: "after the dead holder",
      })),
    ).toEqual({ shared: "redeployed", created: "after the dead holder" });
    expect(await readdir(directory)).toEqual(["deployment.v8"]);
  });

  it("stops waiting on a live holder after 300 s, naming the fixture, the lock and the holder", async () => {
    // Only the clock is faked: the claim's polls still sleep for real.
    vi.useFakeTimers({ toFake: ["Date"] });
    const started = Date.now();
    const lockPath = join(directory, "deployment.v8.lock");
    // This live process holds the claim and never shares.
    await writeFile(lockPath, String(process.pid));
    let settled = false;
    const waiting = loadOrCreateRunSharedFixture(
      "deployment",
      neverCreate,
    ).finally(() => (settled = true));
    const outcome = expect(waiting).rejects.toThrow(
      `Shared fixture deployment was neither published nor released within 300 s of waiting: ${lockPath} still names pid ${String(process.pid)}`,
    );

    vi.setSystemTime(started + 299_999);
    // Two of the claim's 200 ms polls, on the real clock.
    await new Promise((resolve) => setTimeout(resolve, 450));
    expect(settled).toBe(false);

    vi.setSystemTime(started + 300_000);
    await outcome;
    expect(neverCreate).not.toHaveBeenCalled();
  });

  it("is created again after the run's published fixtures are invalidated", async () => {
    await loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "first sources",
      created: undefined,
    }));
    await invalidateRunSharedFixtures(directory);
    expect(
      await loadOrCreateRunSharedFixture("deployment", async () => ({
        shared: "edited sources",
        created: "again",
      })),
    ).toEqual({ shared: "edited sources", created: "again" });
  });
});
