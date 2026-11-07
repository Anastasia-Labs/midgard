import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, it, mock } from "node:test";
import { setTimeout as sleep } from "node:timers/promises";

import {
  invalidateRunSharedFixtures,
  loadOrCreateRunSharedFixture,
  memoizeForRun,
  RUN_SHARED_FIXTURE_DIRECTORY_ENV,
} from "../run-shared-fixtures.js";

/** A `create` that must never run. */
const neverCreate = async () => {
  throw new Error("create must not run");
};

/** A pid no process holds: a child that has already exited. */
const endedPid = () => spawnSync(process.execPath, ["-e", ""]).pid;

/** Polls `check` until it holds, for at most five seconds. */
const waitFor = async (check) => {
  const deadline = Date.now() + 5_000;
  while (!(await check())) {
    if (Date.now() > deadline) throw new Error("condition never held");
    await sleep(10);
  }
};

let directory;
const inherited = process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];

beforeEach(async () => {
  directory = await mkdtemp(join(tmpdir(), "midgard-run-shared-test-"));
  process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] = directory;
});

afterEach(async () => {
  mock.timers.reset();
  if (inherited === undefined)
    delete process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];
  else process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] = inherited;
  await rm(directory, { recursive: true, force: true });
});

/** A `create` that counts its calls and resolves when `release` is called. */
const gatedCreate = (value) => {
  let calls = 0;
  let release;
  const released = new Promise((resolve) => (release = resolve));
  return {
    calls: () => calls,
    release: () => release(),
    create: async () => {
      calls += 1;
      await released;
      return { shared: value, created: `created ${String(calls)}` };
    },
  };
};

const lockHeld = async () =>
  (await readdir(directory)).includes("deployment.v8.lock");

describe("a run-shared fixture", () => {
  it("is created once by the first caller and read by callers that waited on its claim", async () => {
    const gate = gatedCreate({ ledger: [1n, 2n], bytes: Buffer.from("ab") });
    const first = loadOrCreateRunSharedFixture("deployment", gate.create);
    // The first caller holds the claim while it creates.
    await waitFor(lockHeld);
    const second = loadOrCreateRunSharedFixture("deployment", gate.create);
    gate.release();

    assert.deepEqual(await first, {
      shared: { ledger: [1n, 2n], bytes: Buffer.from("ab") },
      created: "created 1",
    });
    // The waiter reads the published copy; only the creator sees `created`.
    assert.deepEqual(await second, {
      shared: { ledger: [1n, 2n], bytes: Buffer.from("ab") },
    });
    assert.equal(gate.calls(), 1);
    assert.deepEqual((await readdir(directory)).sort(), ["deployment.v8"]);
  });

  it("is read from the published copy by a later caller without creating it again", async () => {
    await loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "deployed",
      created: undefined,
    }));
    assert.deepEqual(
      await loadOrCreateRunSharedFixture("deployment", neverCreate),
      { shared: "deployed" },
    );
  });

  it("is created by a waiter when the holder released its claim without sharing", async () => {
    let fail;
    const holder = loadOrCreateRunSharedFixture(
      "deployment",
      () =>
        new Promise((_, reject) => {
          fail = reject;
        }),
    );
    await waitFor(lockHeld);
    const waiter = loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "redeployed",
      created: "by the waiter",
    }));
    fail(new Error("deployment failed"));

    await assert.rejects(holder, /deployment failed/u);
    assert.deepEqual(await waiter, {
      shared: "redeployed",
      created: "by the waiter",
    });
  });

  it("is created by a waiter when the claim names a process that ended", async () => {
    await writeFile(join(directory, "deployment.v8.lock"), String(endedPid()));
    assert.deepEqual(
      await loadOrCreateRunSharedFixture("deployment", async () => ({
        shared: "redeployed",
        created: "after the dead holder",
      })),
      { shared: "redeployed", created: "after the dead holder" },
    );
    assert.deepEqual(await readdir(directory), ["deployment.v8"]);
  });

  it("stops waiting on a live holder after 300 s, naming the fixture, the lock and the holder", async () => {
    // Only the clock is faked: the claim's polls still sleep for real.
    const started = Date.now();
    mock.timers.enable({ apis: ["Date"], now: started });
    const lockPath = join(directory, "deployment.v8.lock");
    // This live process holds the claim and never shares.
    await writeFile(lockPath, String(process.pid));
    let settled = false;
    const waiting = loadOrCreateRunSharedFixture(
      "deployment",
      neverCreate,
    ).finally(() => (settled = true));
    const outcome = assert.rejects(waiting, {
      message: new RegExp(
        `^Shared fixture deployment was neither published nor released within 300 s of waiting: ${lockPath.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&")} still names pid ${String(process.pid)}`,
        "u",
      ),
    });

    mock.timers.setTime(started + 299_999);
    // Two of the claim's 200 ms polls, on the real clock.
    await sleep(450);
    assert.equal(settled, false);

    mock.timers.setTime(started + 300_000);
    await outcome;
  });

  it("is created again after the run's published fixtures are invalidated", async () => {
    await loadOrCreateRunSharedFixture("deployment", async () => ({
      shared: "first sources",
      created: undefined,
    }));
    await invalidateRunSharedFixtures(directory);
    assert.deepEqual(
      await loadOrCreateRunSharedFixture("deployment", async () => ({
        shared: "edited sources",
        created: "again",
      })),
      { shared: "edited sources", created: "again" },
    );
  });
});

describe("a value memoized for the run", () => {
  it("is produced once and read back by a separate memo of the same name, as another worker would", async () => {
    let produced = 0;
    const produce = async () => {
      produced += 1;
      return { contracts: ["a", "b"] };
    };
    const first = memoizeForRun("derived", produce);
    assert.deepEqual(await first(), { contracts: ["a", "b"] });
    assert.deepEqual(await first(), { contracts: ["a", "b"] });
    // A fresh memo has no in-memory value, like a memo in another worker.
    const other = memoizeForRun("derived", produce);
    assert.deepEqual(await other(), { contracts: ["a", "b"] });
    assert.equal(produced, 1);
  });

  it("gives every call its own copy, so one caller's edit never reaches another", async () => {
    const derived = memoizeForRun("editable", async () => ({
      contracts: { payoutMint: { scriptHash: "aa" } },
    }));
    const edited = await derived();
    delete edited.contracts.payoutMint;
    assert.deepEqual(await derived(), {
      contracts: { payoutMint: { scriptHash: "aa" } },
    });
  });

  it("produces again after a failed production instead of keeping the failure", async () => {
    let attempts = 0;
    const derived = memoizeForRun("flaky", async () => {
      attempts += 1;
      if (attempts === 1) throw new Error("first attempt failed");
      return "second attempt";
    });
    await assert.rejects(derived(), /first attempt failed/u);
    assert.equal(await derived(), "second attempt");
  });
});
