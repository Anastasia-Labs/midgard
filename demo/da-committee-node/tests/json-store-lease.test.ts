import { type ChildProcess, spawn } from "node:child_process";
import { readlinkSync } from "node:fs";
import { readFile, utimes, writeFile } from "node:fs/promises";
import { hostname } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { retryStartup, startupReason } from "../src/startup.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import {
  JSON_STORE_LEASE_STALE_MS,
  type JsonStoreLeaseOptions,
  type JsonStoreLeaseRecord,
} from "../src/store.json-file-lease.js";
import { tempDir } from "./helpers.js";

const openStores = new Set<JsonFileCommitteeStore>();
const children = new Set<ChildProcess>();

afterEach(async () => {
  await Promise.all([...openStores].map((store) => store.close()));
  openStores.clear();
  for (const child of children) child.kill("SIGKILL");
  children.clear();
});

const open = async (
  dir: string,
  options: JsonStoreLeaseOptions = {},
): Promise<JsonFileCommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(dir, options);
  openStores.add(store);
  return store;
};

const lockPath = (dir: string) => join(dir, "committee.json.lock");

const liveProcess = (): ChildProcess => {
  const child = spawn("sleep", ["60"], { stdio: "ignore" });
  children.add(child);
  return child;
};

const exitedPid = async (): Promise<number> => {
  const child = spawn("true", [], { stdio: "ignore" });
  await new Promise((resolve) => child.once("exit", resolve));
  return child.pid!;
};

const bootId = (): string => "11111111-2222-3333-4444-555555555555";
const ownPidNamespace = readlinkSync("/proc/self/ns/pid");

const leaseOf = (
  overrides: Partial<JsonStoreLeaseRecord>,
): JsonStoreLeaseRecord => ({
  schemaVersion: 2,
  owner: `${(overrides.pid ?? 1).toString()}:holder`,
  pid: 1,
  bootId: bootId(),
  pidNamespace: ownPidNamespace,
  hostname: hostname(),
  acquiredAt: new Date().toISOString(),
  renewedAt: new Date().toISOString(),
  ...overrides,
});

const leaveLock = async (dir: string, lease: JsonStoreLeaseRecord | string) =>
  writeFile(
    lockPath(dir),
    typeof lease === "string" ? lease : `${JSON.stringify(lease)}\n`,
  );

const heldLockError = /already exclusively leased/u;

describe("the JSON store's lease on its lock file", () => {
  it("is refused while a live process on this host holds it, and reports the lock as held", async () => {
    const dir = await tempDir();
    await leaveLock(dir, leaseOf({ pid: liveProcess().pid! }));
    const refused = open(dir, { bootId });
    await expect(refused).rejects.toThrow(heldLockError);
    expect(startupReason(await refused.catch((error: unknown) => error))).toBe(
      "starting:store_instance_lock_held",
    );
  });

  it("is taken over, once, from a holder whose process is gone or whose boot ended", async () => {
    for (const lease of [
      leaseOf({ pid: await exitedPid() }),
      leaseOf({ pid: liveProcess().pid!, bootId: "an-earlier-boot" }),
      leaseOf({ pid: process.pid, owner: `${process.pid.toString()}:old` }),
    ]) {
      const dir = await tempDir();
      await leaveLock(dir, lease);
      const onTakeover = vi.fn();
      const store = await open(dir, { bootId, onTakeover });
      expect(onTakeover).toHaveBeenCalledOnce();
      expect(await store.listStateQueueHeaders()).toEqual([]);
      const held = JSON.parse(
        await readFile(lockPath(dir), "utf8"),
      ) as JsonStoreLeaseRecord;
      expect(held).toMatchObject({ pid: process.pid, hostname: hostname() });
      expect(held.owner).not.toBe(lease.owner);
    }
  });

  it("judges a holder in another pid namespace (a container sharing this hostname and boot) by its lease alone", async () => {
    for (const pid of [await exitedPid(), process.pid]) {
      const dir = await tempDir();
      const renewedAt = Date.now();
      await leaveLock(
        dir,
        leaseOf({
          pid,
          pidNamespace: "pid:[4026539999]",
          renewedAt: new Date(renewedAt).toISOString(),
        }),
      );
      const pinned = new Date(renewedAt);
      await utimes(lockPath(dir), pinned, pinned);
      await expect(open(dir, { bootId })).rejects.toThrow(heldLockError);
      const onTakeover = vi.fn();
      await open(dir, {
        bootId,
        now: () => renewedAt + JSON_STORE_LEASE_STALE_MS + 1_000,
        onTakeover,
      });
      expect(onTakeover).toHaveBeenCalledExactlyOnceWith("lease_stale");
    }
  });

  it("is refused while another host's lease is fresh, and taken over once it goes stale", async () => {
    const dir = await tempDir();
    const renewedAt = Date.now();
    await leaveLock(
      dir,
      leaseOf({
        hostname: "another-host",
        renewedAt: new Date(renewedAt).toISOString(),
      }),
    );
    // The lease's age runs from the later of renewedAt and the file's mtime,
    // so the mtime is pinned to renewedAt rather than left to the write.
    const pinned = new Date(renewedAt);
    await utimes(lockPath(dir), pinned, pinned);
    await expect(open(dir, { bootId })).rejects.toThrow(heldLockError);
    const onTakeover = vi.fn();
    await open(dir, {
      bootId,
      now: () => renewedAt + JSON_STORE_LEASE_STALE_MS + 1_000,
      onTakeover,
    });
    expect(onTakeover).toHaveBeenCalledExactlyOnceWith("lease_stale");
  });

  it("judges an unreadable lock (a crash mid-write) by its age alone", async () => {
    const dir = await tempDir();
    await leaveLock(dir, "");
    await expect(open(dir, { bootId })).rejects.toThrow(heldLockError);
    const old = new Date(Date.now() - JSON_STORE_LEASE_STALE_MS - 1_000);
    await utimes(lockPath(dir), old, old);
    const onTakeover = vi.fn();
    await open(dir, { bootId, onTakeover });
    expect(onTakeover).toHaveBeenCalledExactlyOnceWith(
      "unreadable_lease_stale",
    );
  });

  it("is renewed while held, so a live holder is never judged stale", async () => {
    const dir = await tempDir();
    let nowMs = Date.parse("2026-10-01T00:00:00.000Z");
    await open(dir, { bootId, renewMs: 10, now: () => nowMs });
    nowMs += 5 * JSON_STORE_LEASE_STALE_MS;
    await vi.waitFor(async () => {
      const held = JSON.parse(
        await readFile(lockPath(dir), "utf8"),
      ) as JsonStoreLeaseRecord;
      expect(Date.parse(held.renewedAt)).toBe(nowMs);
    });
    await expect(
      open(dir, { bootId, hostname: () => "another-host", now: () => nowMs }),
    ).rejects.toThrow(heldLockError);
  });

  it("is never reported lost by a write that checks it while a renewal rewrites the lock file", async () => {
    const dir = await tempDir();
    const onLost = vi.fn();
    const store = await open(dir, { bootId, renewMs: 1, onLost });
    // The check every store write makes first, many times over while the
    // lease renews each millisecond.
    const lease = Reflect.get(store, "lease") as {
      assertHeld(): Promise<void>;
    };
    for (let round = 0; round < 100; round += 1) {
      await Promise.all(Array.from({ length: 10 }, () => lease.assertHeld()));
    }
    expect(onLost).not.toHaveBeenCalled();
  }, 20_000);

  it("refuses every write, and reports the loss once, after another process took it over", async () => {
    const dir = await tempDir();
    const onLost = vi.fn();
    const store = await open(dir, { bootId, renewMs: 10, onLost });
    const health = {
      peerId: "peer-a",
      consecutiveFailures: 0,
      updatedAt: new Date().toISOString(),
    };
    await store.savePeerHealth(health);
    await leaveLock(dir, leaseOf({ hostname: "another-host" }));
    await expect(store.savePeerHealth(health)).rejects.toThrow(
      /lease was taken over by another process/u,
    );
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(onLost).toHaveBeenCalledOnce();
    // Closing leaves the new holder's lock in place.
    await store.close();
    openStores.delete(store);
    expect(await readFile(lockPath(dir), "utf8")).toContain("another-host");
  });

  it("is waited for at startup, and opened exactly once when its holder goes", async () => {
    const dir = await tempDir();
    const holder = liveProcess();
    await leaveLock(dir, leaseOf({ pid: holder.pid! }));
    const reasons: string[] = [];
    let opens = 0;
    const store = await retryStartup({
      attempt: async () => {
        const opened = await open(dir, { bootId });
        opens += 1;
        return opened;
      },
      onFailure: (reason) => reasons.push(reason),
      write: () => undefined,
      sleep: async () => {
        holder.kill("SIGKILL");
        await new Promise((resolve) => holder.once("exit", resolve));
      },
    });
    expect(reasons).toEqual(["starting:store_instance_lock_held"]);
    expect(opens).toBe(1);
    expect(await store.listStateQueueHeaders()).toEqual([]);
  });
});
