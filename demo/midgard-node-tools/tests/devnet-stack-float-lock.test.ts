import {
  chmodSync,
  existsSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import type { Layout, RunEnv } from "../src/devnet-stack/layout.js";
import { processStartTime } from "../src/devnet-stack/lock.js";
import {
  FLOAT_STEP_TIMEOUT_MS,
  FLOAT_TARGET_LOVELACE,
  type FloatDeps,
  type FloatRecord,
} from "../src/devnet-stack/reserve-float.js";
import {
  provisionReserveFloat,
  withFloatLock,
} from "../src/devnet-stack/reserve-float-chain.js";

/**
 * The holder's release, landed inside an acquire: after its failed create
 * (EEXIST), just before its owner check reads the lock, or just after that
 * check saw the live holder. These are the interleavings a test cannot
 * schedule on the real filesystem. Unset, the filesystem is the real one.
 */
const race = vi.hoisted(() => ({
  releaseOnCreate: undefined as string | undefined,
  releaseBeforeOwnerRead: undefined as string | undefined,
  releaseAfterOwnerRead: undefined as string | undefined,
}));
vi.mock("node:fs", async (importOriginal) => {
  const fs = await importOriginal<typeof import("node:fs")>();
  const openSync = ((...args: Parameters<typeof fs.openSync>) => {
    try {
      return fs.openSync(...args);
    } catch (error) {
      if (
        (error as NodeJS.ErrnoException).code === "EEXIST" &&
        args[0] === race.releaseOnCreate
      ) {
        race.releaseOnCreate = undefined;
        fs.rmSync(args[0]);
      }
      throw error;
    }
  }) as typeof fs.openSync;
  const readFileSync = ((...args: Parameters<typeof fs.readFileSync>) => {
    if (args[0] === race.releaseBeforeOwnerRead) {
      race.releaseBeforeOwnerRead = undefined;
      fs.rmSync(args[0]);
    }
    const content = fs.readFileSync(...args);
    if (args[0] === race.releaseAfterOwnerRead) {
      race.releaseAfterOwnerRead = undefined;
      fs.rmSync(args[0]);
    }
    return content;
  }) as typeof fs.readFileSync;
  return {
    ...fs,
    default: { ...fs, openSync, readFileSync },
    openSync,
    readFileSync,
  };
});

const dirs: string[] = [];
const tempDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-float-lock-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  race.releaseOnCreate = undefined;
  race.releaseBeforeOwnerRead = undefined;
  race.releaseAfterOwnerRead = undefined;
  for (const dir of dirs.splice(0)) {
    chmodSync(dir, 0o700);
    rmSync(dir, { recursive: true, force: true });
  }
});

const layoutAt = (state: string) =>
  ({ state, journal: join(state, "journal.json") }) as Layout;

/** A lock naming a live process other than this one: the test runner's parent. */
const heldByOther = (state: string) => {
  const path = join(state, "reserve-float.lock");
  writeFileSync(path, `${process.ppid} ${processStartTime(process.ppid)}`);
  return path;
};

/** A clock whose sleeps advance it, failing loudly instead of spinning. */
const fakeClock = (maxPolls: number) => {
  const clock = { at: 0, polls: 0 };
  return {
    clock,
    now: () => clock.at,
    sleep: (ms: number) => {
      clock.polls += 1;
      if (clock.polls > maxPolls)
        return Promise.reject(new Error("the lock wait is unbounded"));
      clock.at += ms;
      return Promise.resolve();
    },
  };
};

describe("withFloatLock", () => {
  it("refuses a corrupt kernel mutex without retrying or running the float step", async () => {
    const state = tempDir();
    writeFileSync(
      join(state, "reserve-float.lock.mutex.sqlite"),
      "corrupt database",
    );
    const time = fakeClock(10);
    let steps = 0;
    await expect(
      withFloatLock(layoutAt(state), time, () => Promise.resolve((steps += 1))),
    ).rejects.toThrow(/file is not a database/);
    expect(steps).toBe(0);
    expect(time.clock.polls).toBe(0);
  });

  it("gives up on a live holder once FLOAT_STEP_TIMEOUT_MS has passed", async () => {
    const state = tempDir();
    heldByOther(state);
    const time = fakeClock(1_000);
    let steps = 0;
    await expect(
      withFloatLock(layoutAt(state), time, () => Promise.resolve((steps += 1))),
    ).rejects.toThrow(/another controller \(pid \d+\) holds/);
    expect(steps).toBe(0);
    expect(time.clock.at).toBeGreaterThan(FLOAT_STEP_TIMEOUT_MS);
    // 2 s polls: the first one past the deadline ends the wait.
    expect(time.clock.polls).toBe(FLOAT_STEP_TIMEOUT_MS / 2_000 + 1);
  });

  it("takes a lock its holder released mid-acquire on the next poll, running its step once", async () => {
    const state = tempDir();
    const lock = heldByOther(state);
    race.releaseOnCreate = lock;
    const time = fakeClock(10);
    let steps = 0;
    const result = await withFloatLock(layoutAt(state), time, () =>
      Promise.resolve((steps += 1)),
    );
    expect(race.releaseOnCreate).toBeUndefined();
    expect(result).toBe(1);
    expect(steps).toBe(1);
    expect(time.clock.polls).toBe(1);
    expect(existsSync(lock)).toBe(false);
  });

  it("takes a lock its holder released just after the acquire saw it held, running its step once", async () => {
    const state = tempDir();
    const lock = heldByOther(state);
    race.releaseAfterOwnerRead = lock;
    const time = fakeClock(10);
    let steps = 0;
    const result = await withFloatLock(layoutAt(state), time, () =>
      Promise.resolve((steps += 1)),
    );
    expect(race.releaseAfterOwnerRead).toBeUndefined();
    expect(result).toBe(1);
    expect(steps).toBe(1);
    expect(time.clock.polls).toBe(1);
    expect(existsSync(lock)).toBe(false);
  });

  it("takes a lock its holder released just before the acquire's owner check read it, running its step once", async () => {
    const state = tempDir();
    const lock = heldByOther(state);
    race.releaseBeforeOwnerRead = lock;
    const time = fakeClock(10);
    let steps = 0;
    const result = await withFloatLock(layoutAt(state), time, () =>
      Promise.resolve((steps += 1)),
    );
    expect(race.releaseBeforeOwnerRead).toBeUndefined();
    expect(result).toBe(1);
    expect(steps).toBe(1);
    expect(time.clock.polls).toBe(1);
    expect(existsSync(lock)).toBe(false);
  });

  it("refuses at once when the lock cannot be written for want of permission", async () => {
    const state = tempDir();
    chmodSync(state, 0o500);
    const time = fakeClock(10);
    await expect(
      withFloatLock(layoutAt(state), time, () => Promise.resolve()),
    ).rejects.toThrow(/EACCES/);
    expect(time.clock.polls).toBe(0);
  });

  it("refuses at once, without retrying, when the state directory is missing", async () => {
    const time = fakeClock(10);
    await expect(
      withFloatLock(layoutAt(join(tempDir(), "missing")), time, () =>
        Promise.resolve(),
      ),
    ).rejects.toThrow(/ENOENT/);
    expect(time.clock.polls).toBe(0);
  });
});

describe("provisionReserveFloat", () => {
  it("journals the next sequence after one the caller's journal never saw", async () => {
    const state = tempDir();
    const layout = layoutAt(state);
    // up opened the journal; the maintainer then recorded sequence 1.
    const callers = new Journal(layout.journal);
    const earlier: FloatRecord = {
      sequence: 1,
      reserveAddress: "addr_reserve",
      lovelace: FLOAT_TARGET_LOVELACE.toString(),
      txId: "earlier",
      signedTx: join(state, "reserve-float-1.signed"),
      input: "genesis#0",
      status: "confirmed",
    };
    new Journal(layout.journal).set("reserve-float:1", earlier);
    const built: number[] = [];
    const deps: FloatDeps = {
      reserveAddress: "addr_reserve",
      // A payout drained the float the maintainer paid.
      unspentAt: () => Promise.resolve([]),
      landed: () => Promise.resolve(true),
      outputState: () => Promise.resolve("unspent"),
      buildPayment: (_address, _lovelace, sequence) => {
        built.push(sequence);
        return Promise.resolve({
          txId: `paid-${sequence}`,
          signedTx: join(state, `reserve-float-${sequence}.signed`),
          input: `genesis#${sequence}`,
        });
      },
      submit: () => Promise.resolve(),
      sleep: () => Promise.resolve(),
      now: Date.now,
      log: () => {},
    };
    const outcome = await provisionReserveFloat(
      layout,
      {} as RunEnv,
      callers,
      deps,
    );
    expect(outcome).toEqual({
      action: "topped-up",
      txId: "paid-2",
      lovelace: FLOAT_TARGET_LOVELACE,
    });
    expect(built).toEqual([2]);
    expect(
      new Journal(layout.journal)
        .withPrefix<FloatRecord>("reserve-float:")
        .map((r) => [r.sequence, r.txId, r.status]),
    ).toEqual([
      [1, "earlier", "confirmed"],
      [2, "paid-2", "confirmed"],
    ]);
  });
});
