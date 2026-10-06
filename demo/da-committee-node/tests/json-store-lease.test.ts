import {
  closeSync,
  openSync,
  readFileSync,
  readlinkSync,
  writeFileSync,
} from "node:fs";
import { readdir, readFile, rename, writeFile } from "node:fs/promises";
import { hostname } from "node:os";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { retryStartup, startupReason } from "../src/startup.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { JSON_STORE_INSTANCE_LOCK_CHECK_MS } from "../src/store.json-file-instance-lock.js";
import { tempDir } from "./helpers.js";
import {
  closeStore,
  committeeChild,
  type FsPatch,
  health,
  heldLockError,
  leaveLock,
  liveProcess,
  lockOf,
  lockPath,
  mutexHolder,
  open,
  openStores,
  patchFs,
  peers,
  removeCommitteeBundle,
  sidecarPath,
  stop,
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

/** The renewed lease an earlier binary left in the lock file. */
const legacyLease = (overrides: Record<string, unknown>): string =>
  `${JSON.stringify({
    schemaVersion: 2,
    owner: "1:holder",
    pid: 1,
    bootId: readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim(),
    pidNamespace: readlinkSync("/proc/self/ns/pid"),
    hostname: hostname(),
    acquiredAt: new Date().toISOString(),
    renewedAt: new Date().toISOString(),
    ...overrides,
  })}\n`;

describe("the JSON store's instance lock", () => {
  it("refuses every write, and reports the loss once, after another process took it over between two checks", async () => {
    vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
    const dir = await tempDir();
    const onLost = vi.fn();
    const store = await open(dir, { onLost });
    await store.savePeerHealth(health("before"));
    // The successor's stamp lands while the holder maintains its lock: on
    // the first lock-file open the holder makes from here on, if any.
    let tookOver = false;
    const restore = patchFs({
      open:
        (original) =>
        async (...args) => {
          const handle = await original(...args);
          if (!tookOver && String(args[0]).startsWith(lockPath(dir))) {
            tookOver = true;
            writeFileSync(lockPath(dir), successorStamp);
          }
          return handle;
        },
    });
    try {
      await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
      // A write queues behind any maintenance the check period started.
      await store.savePeerHealth(health("during"));
      if (!tookOver) await leaveLock(dir, successorStamp);
      for (const peerId of ["after-1", "after-2", "after-3"]) {
        await expect(store.savePeerHealth(health(peerId))).rejects.toThrow(
          takenOver,
        );
      }
    } finally {
      restore();
    }
    expect(onLost).toHaveBeenCalledOnce();
    expect(onLost.mock.calls[0]![0]).toBeInstanceOf(Error);
    await closeStore(store);
    expect(await readFile(lockPath(dir), "utf8")).toBe(successorStamp);
  });

  it("never writes, renames, touches or removes its lock file after taking it", async () => {
    vi.useFakeTimers({ toFake: ["setInterval", "clearInterval"] });
    const dir = await tempDir();
    const store = await open(dir);
    const touched: string[] = [];
    const observe =
      (name: string, pathArgs: number[]): FsPatch =>
      (original) =>
      (...args) => {
        for (const index of pathArgs) {
          const path = String(args[index]);
          if (path.startsWith(lockPath(dir))) touched.push(`${name} ${path}`);
        }
        return original(...args);
      };
    const restore = patchFs({
      appendFile: observe("appendFile", [0]),
      chmod: observe("chmod", [0]),
      copyFile: observe("copyFile", [1]),
      link: observe("link", [1]),
      lutimes: observe("lutimes", [0]),
      rename: observe("rename", [0, 1]),
      rm: observe("rm", [0]),
      symlink: observe("symlink", [1]),
      truncate: observe("truncate", [0]),
      unlink: observe("unlink", [0]),
      utimes: observe("utimes", [0]),
      writeFile: observe("writeFile", [0]),
      open:
        (original) =>
        (...args) =>
          args[1] === undefined || args[1] === "r"
            ? original(...args)
            : observe("open", [0])(original)(...args),
    });
    try {
      for (let period = 0; period < 10; period += 1) {
        await vi.advanceTimersByTimeAsync(JSON_STORE_INSTANCE_LOCK_CHECK_MS);
        await store.savePeerHealth(health(`peer-${period.toString()}`));
      }
      await closeStore(store);
    } finally {
      restore();
    }
    expect(touched).toEqual([]);
  });

  it.each([
    [
      "a renewed lease naming a live process on this host and pid namespace",
      () => legacyLease({ pid: liveProcess().pid }),
    ],
    [
      "a fresh lease from another host",
      () => legacyLease({ hostname: "another-host" }),
    ],
    [
      "a fresh lease from another pid namespace",
      () => legacyLease({ pidNamespace: "pid:[4026539999]" }),
    ],
    ["an unreadable lock file", () => ""],
  ])(
    "never lets %s block a successor that holds the process mutex",
    async (_kind, leftover) => {
      const dir = await tempDir();
      const left = leftover();
      await leaveLock(dir, left);
      const store = await open(dir);
      await store.savePeerHealth(health("successor"));
      const stamp = await readFile(lockPath(dir), "utf8");
      expect(stamp).not.toBe(left);
      expect(JSON.parse(stamp)).toMatchObject({
        pid: process.pid,
        hostname: hostname(),
      });
    },
  );

  it("is never reported lost by its honest holder, however often it checks", async () => {
    const dir = await tempDir();
    const onLost = vi.fn();
    const store = await open(dir, { checkMs: 1, onLost });
    await Promise.all([
      ...Array.from({ length: 1000 }, () => lockOf(store).assertHeld()),
      ...Array.from({ length: 50 }, (_, index) =>
        store.savePeerHealth(health(`peer-${index.toString()}`)),
      ),
    ]);
    expect(onLost).not.toHaveBeenCalled();
    expect(await peers(store)).toHaveLength(50);
  }, 20_000);

  it.each([
    [
      "its mutex sidecar was replaced",
      async (dir: string) => {
        await writeFile(`${sidecarPath(dir)}.replacement`, "");
        await rename(`${sidecarPath(dir)}.replacement`, sidecarPath(dir));
      },
    ],
    [
      "this process opened and closed its mutex sidecar, dropping the lock",
      (dir: string) => {
        closeSync(openSync(sidecarPath(dir), "r"));
        return Promise.resolve();
      },
    ],
  ])(
    "fails closed once a successor gets in because %s",
    async (_cause, breakLocking) => {
      const dir = await tempDir();
      const onLost = vi.fn();
      const store = await open(dir, { onLost });
      await store.savePeerHealth(health("old-peer"));
      await breakLocking(dir);
      const successor = await committeeChild(
        join(dir, "committee.json"),
        "contender",
      );
      expect((await successor.wait("opened", "refused")).event).toBe("opened");
      for (const peerId of ["late-1", "late-2"]) {
        await expect(store.savePeerHealth(health(peerId))).rejects.toThrow(
          takenOver,
        );
      }
      expect(onLost).toHaveBeenCalledOnce();
      expect(String(onLost.mock.calls[0]![0])).toMatch(
        /locking failed[\s\S]*JSON store ownership/u,
      );
      successor.child.send("save");
      expect((await successor.wait("saved")).peers).toEqual([
        "new-peer",
        "old-peer",
      ]);
    },
    30_000,
  );

  it("is refused to a second open in this process, which leaves the first holder's mutex in force", async () => {
    const dir = await tempDir();
    const store = await open(dir);
    const second = open(dir);
    await expect(second).rejects.toThrow(/its process mutex is held/u);
    expect(startupReason(await second.catch((error: unknown) => error))).toBe(
      "starting:store_instance_lock_held",
    );
    const contender = await committeeChild(
      join(dir, "committee.json"),
      "contender",
    );
    const refusal = await contender.wait("opened", "refused");
    expect(refusal.event).toBe("refused");
    expect(refusal.message).toMatch(heldLockError);
    await store.savePeerHealth(health("holder"));
    expect(await peers(store)).toEqual(["holder"]);
  }, 30_000);

  it("is waited for at startup while another process holds the mutex, and opened exactly once when that process dies", async () => {
    const dir = await tempDir();
    const holder = await mutexHolder(sidecarPath(dir));
    const reasons: string[] = [];
    let opens = 0;
    const store = await retryStartup({
      attempt: async () => {
        const opened = await open(dir);
        opens += 1;
        return opened;
      },
      onFailure: (reason) => reasons.push(reason),
      write: () => undefined,
      sleep: () => stop(holder),
    });
    expect(reasons).toEqual(["starting:store_instance_lock_held"]);
    expect(opens).toBe(1);
    expect(await store.listStateQueueHeaders()).toEqual([]);
  });

  it.each(["another namespace", "another host", "unreadable"])(
    "never takes over a live holder that keeps the process mutex, whatever %s lock file it left",
    async (kind) => {
      const dir = await tempDir();
      const holder = await mutexHolder(sidecarPath(dir));
      const metadata =
        kind === "unreadable"
          ? ""
          : legacyLease(
              kind === "another namespace"
                ? { pidNamespace: "pid:[4026539999]" }
                : { hostname: "another-host" },
            );
      await leaveLock(dir, metadata);
      const refused = open(dir);
      await expect(refused).rejects.toThrow(/its process mutex is held/u);
      expect(
        startupReason(await refused.catch((error: unknown) => error)),
      ).toBe("starting:store_instance_lock_held");
      expect(await readFile(lockPath(dir), "utf8")).toBe(metadata);
      // The mutex alone kept it: once the holder dies the same lock file is
      // overwritten at once.
      await stop(holder);
      await open(dir);
      expect(await readFile(lockPath(dir), "utf8")).not.toBe(metadata);
    },
  );
});

it("old release preserves successor metadata even before observing ownership loss", async () => {
  const dir = await tempDir();
  const store = await open(dir);
  await leaveLock(dir, successorStamp);
  await closeStore(store);
  expect(await readFile(lockPath(dir), "utf8")).toBe(successorStamp);
  await store.close();
});

it("fences a paused live writer, then admits exactly one real successor after process death", async () => {
  const dir = await tempDir();
  const storePath = join(dir, "committee.json");
  const old = await committeeChild(storePath, "paused");
  expect((await old.wait("opened", "refused")).event).toBe("opened");
  old.child.send("save");
  await old.wait("write-paused");
  const contender = await committeeChild(storePath, "contender");
  const refusal = await contender.wait("opened", "refused");
  expect(refusal.event).toBe("refused");
  expect(refusal.message).toMatch(heldLockError);
  expect(old.child.exitCode).toBeNull();
  old.child.send("resume");
  expect((await old.wait("saved")).peers).toEqual(["old-peer"]);
  await stop(old.child);
  const candidates = [
    await committeeChild(storePath, "contender"),
    await committeeChild(storePath, "contender"),
  ];
  const results = await Promise.all(
    candidates.map((candidate) => candidate.wait("opened", "refused")),
  );
  expect(results.filter((result) => result.event === "opened")).toHaveLength(1);
  expect(results.filter((result) => result.event === "refused")).toHaveLength(
    1,
  );
  const successor =
    candidates[results.findIndex((result) => result.event === "opened")]!;
  successor.child.send("save");
  expect((await successor.wait("saved")).peers).toEqual([
    "new-peer",
    "old-peer",
  ]);
  successor.child.send("close");
  await successor.wait("closed");
  const reopened = await open(dir);
  expect(await peers(reopened)).toEqual(["new-peer", "old-peer"]);
}, 30_000);

it("releases the process mutex, and leaves nothing behind, after failing to write its stamp", async () => {
  const dir = await tempDir();
  let injected: string | undefined;
  const restore = patchFs({
    writeFile:
      (original) =>
      async (...args) => {
        const path = String(args[0]);
        if (injected !== undefined || !path.startsWith(`${lockPath(dir)}.`))
          return original(...args);
        injected = path;
        await original(path, '{"owner":');
        throw new Error("synthetic stamp write failure");
      },
  });
  try {
    await expect(JsonFileCommitteeStore.open(dir)).rejects.toThrow(
      "synthetic stamp write failure",
    );
  } finally {
    restore();
  }
  // The stamp is published by one rename, so a torn write never reaches the
  // lock file, and its temporary file is removed.
  expect(injected).toMatch(/\.tmp$/u);
  const left = (await readdir(dir)).filter(
    (name) =>
      name.startsWith("committee.json.lock") && !name.includes(".mutex.sqlite"),
  );
  expect(left).toEqual([]);
  const store = await open(dir);
  await store.savePeerHealth(health("after"));
  expect(await peers(store)).toEqual(["after"]);
  expect(JSON.parse(await readFile(lockPath(dir), "utf8"))).toMatchObject({
    pid: process.pid,
  });
});
