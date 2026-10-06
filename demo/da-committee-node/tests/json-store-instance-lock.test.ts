import { execFileSync } from "node:child_process";
import {
  closeSync,
  constants,
  openSync,
  readdirSync,
  readFileSync,
  writeSync,
} from "node:fs";
import {
  chmod,
  link,
  readFile,
  rename,
  unlink,
  writeFile,
} from "node:fs/promises";
import { join } from "node:path";

import { afterAll, afterEach, expect, it, vi } from "vitest";

import { JSON_STORE_INSTANCE_LOCK_CHECK_MS } from "../src/store.json-file-instance-lock.js";
import { isNodeError } from "../src/store.parse-stored-record-map.js";
import { tempDir } from "./helpers.js";
import {
  committeeChild,
  health,
  leaveLock,
  lockOf,
  lockPath,
  open,
  openStores,
  patchFs,
  pauseOnce,
  peers,
  removeCommitteeBundle,
  stopChildren,
  successorStamp,
  takenOver,
} from "./helpers/json-store-lock.js";

afterEach(async () => {
  vi.useRealTimers();
  await Promise.all([...openStores].map((store) => store.close()));
  openStores.clear();
  await stopChildren();
});
afterAll(removeCommitteeBundle);

/** Every read of the lock file made from here on, to wait for them. */
const watchLockReads = (dir: string) => {
  const reads: Promise<unknown>[] = [];
  const restore = patchFs({
    readFile:
      (original) =>
      (...args) => {
        const read = original(...args) as Promise<unknown>;
        if (String(args[0]) === lockPath(dir)) reads.push(read);
        return read;
      },
  });
  const settled = async () => {
    await Promise.allSettled(reads);
    await new Promise((resolve) => setImmediate(resolve));
  };
  return { reads, settled, restore };
};

it("notices a successor's stamp at its next idle check, before any write", async () => {
  vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
  const dir = await tempDir();
  const onLost = vi.fn();
  const store = await open(dir, { onLost });
  await leaveLock(dir, successorStamp);
  await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
  await vi.waitFor(() => expect(onLost).toHaveBeenCalledOnce());
  await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS * 3);
  expect(onLost).toHaveBeenCalledOnce();
  await expect(store.savePeerHealth(health("late"))).rejects.toThrow(takenOver);
  expect(await readFile(lockPath(dir), "utf8")).toBe(successorStamp);
});

it("treats a removed stamp as taken over", async () => {
  const dir = await tempDir();
  const onLost = vi.fn();
  const store = await open(dir, { onLost });
  await store.savePeerHealth(health("before"));
  await unlink(lockPath(dir));
  for (const peerId of ["after-1", "after-2"]) {
    await expect(store.savePeerHealth(health(peerId))).rejects.toThrow(
      takenOver,
    );
  }
  expect(onLost).toHaveBeenCalledOnce();
});

it("starts no idle check while one is still reading, however long the read hangs", async () => {
  vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
  const dir = await tempDir();
  const onLost = vi.fn();
  const store = await open(dir, { onLost });
  const stamp = await readFile(lockPath(dir), "utf8");
  // A FIFO in the stamp's place makes every read of it block in the kernel,
  // holding a libuv threadpool thread, until a writer opens it.
  const fifo = `${lockPath(dir)}.fifo`;
  execFileSync("mkfifo", [fifo]);
  await link(fifo, `${lockPath(dir)}.swap`);
  await rename(`${lockPath(dir)}.swap`, lockPath(dir));
  // Threads blocked opening a FIFO with no writer, as the kernel reports them.
  const blockedReads = () =>
    readdirSync("/proc/self/task").filter((task) => {
      try {
        return (
          readFileSync(`/proc/self/task/${task}/wchan`, "utf8") ===
          "wait_for_partner"
        );
      } catch {
        return false;
      }
    }).length;
  const releaseReaders = () => {
    try {
      const writer = openSync(fifo, constants.O_WRONLY | constants.O_NONBLOCK);
      writeSync(writer, stamp);
      closeSync(writer);
    } catch (error) {
      if (!(isNodeError(error) && error.code === "ENXIO")) throw error;
    }
  };
  try {
    await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
    await vi.waitFor(() => expect(blockedReads()).toBe(1));
    await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS * 8);
    await new Promise((resolve) => setTimeout(resolve, 200));
    expect(blockedReads()).toBe(1);
    // The rest of the threadpool still serves this process.
    await writeFile(join(dir, "probe"), "probe");
    expect(await readFile(join(dir, "probe"), "utf8")).toBe("probe");
    // The stamp comes back, then the one hung read gets it too.
    await writeFile(`${lockPath(dir)}.swap`, stamp);
    await rename(`${lockPath(dir)}.swap`, lockPath(dir));
    releaseReaders();
    await vi.waitFor(() => expect(blockedReads()).toBe(0));
    await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
    await store.savePeerHealth(health("after"));
    expect(await peers(store)).toEqual(["after"]);
    expect(onLost).not.toHaveBeenCalled();
  } finally {
    // Unblocks any read still waiting, so a failure here cannot hang the run.
    for (let attempt = 0; attempt < 50 && blockedReads() > 0; attempt += 1) {
      releaseReaders();
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    await unlink(fifo);
  }
});

it("reports a lock file it cannot read once, refusing writes without reporting a loss", async () => {
  vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
  const dir = await tempDir();
  const onLost = vi.fn();
  const log = vi.fn();
  const store = await open(dir, { onLost, log });
  await chmod(lockPath(dir), 0o000);
  const lockReads = watchLockReads(dir);
  try {
    for (let period = 0; period < 4; period += 1) {
      await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
      await lockReads.settled();
    }
    expect(lockReads.reads).toHaveLength(4);
  } finally {
    lockReads.restore();
  }
  expect(log).toHaveBeenCalledOnce();
  expect(JSON.parse(String(log.mock.calls[0]![0]))).toMatchObject({
    event: "committee_store_instance_lock_check_failed",
    lockPath: lockPath(dir),
    error: expect.stringMatching(/EACCES/u) as unknown,
  });
  await expect(store.savePeerHealth(health("refused"))).rejects.toThrow(
    /EACCES/u,
  );
  await chmod(lockPath(dir), 0o600);
  await store.savePeerHealth(health("after"));
  expect(await peers(store)).toEqual(["after"]);
  expect(onLost).not.toHaveBeenCalled();
});

it("takes over a read-only lock file left behind on its first attempt", async () => {
  const dir = await tempDir();
  await leaveLock(dir, successorStamp);
  await chmod(lockPath(dir), 0o400);
  const store = await open(dir);
  await store.savePeerHealth(health("successor"));
  const stamp = await readFile(lockPath(dir), "utf8");
  expect(stamp).not.toBe(successorStamp);
  expect(JSON.parse(stamp)).toMatchObject({ pid: process.pid });
});

it("keeps its mutex, and reports no loss, while a check that began before release is still reading", async () => {
  const dir = await tempDir();
  const onLost = vi.fn();
  const store = await open(dir, { onLost });
  const read = pauseOnce("readFile", lockPath(dir));
  try {
    const checked = lockOf(store)
      .assertHeld()
      .then(
        () => "held",
        (error: unknown) => String(error),
      );
    await read.paused;
    const closing = store.close();
    const contender = await committeeChild(
      join(dir, "committee.json"),
      "contender",
    );
    expect((await contender.wait("opened", "refused")).event).toBe("refused");
    // A successor stamps the lock file as soon as the mutex is free; the
    // check then reads that stamp.
    await leaveLock(dir, successorStamp);
    read.resume();
    await closing;
    expect(await checked).toMatch(/released/u);
  } finally {
    read.resume();
    read.restore();
  }
  expect(onLost).not.toHaveBeenCalled();
}, 30_000);

it("keeps its mutex on close until a write already under way has landed", async () => {
  const dir = await tempDir();
  const storePath = join(dir, "committee.json");
  const store = await open(dir);
  const publish = pauseOnce("rename", storePath, 1);
  try {
    const saved = store.savePeerHealth(health("in-flight"));
    await publish.paused;
    const closing = store.close();
    const contender = await committeeChild(storePath, "contender");
    expect((await contender.wait("opened", "refused")).event).toBe("refused");
    publish.resume();
    await saved;
    await closing;
  } finally {
    publish.resume();
    publish.restore();
  }
  const successor = await committeeChild(storePath, "contender");
  expect((await successor.wait("opened", "refused")).event).toBe("opened");
  successor.child.send("save");
  expect((await successor.wait("saved")).peers).toEqual([
    "in-flight",
    "new-peer",
  ]);
}, 30_000);
