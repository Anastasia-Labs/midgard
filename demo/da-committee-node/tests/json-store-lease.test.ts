import { type ChildProcess, fork, spawn } from "node:child_process";
import { existsSync, readlinkSync } from "node:fs";
import fileSystem from "node:fs/promises";
import { mkdtemp, readFile, rm, utimes, writeFile } from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";
import { hostname } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "tsup";
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

  it("starts a committee restarted in a new pid namespace by taking over its crashed holder's lease", async () => {
    const dir = await tempDir();
    const renewedAt = Date.now();
    // The crashed holder ran in another container: another pid namespace and
    // another hostname. Its death released the process mutex.
    await leaveLock(
      dir,
      leaseOf({
        pid: 1,
        pidNamespace: "pid:[4026539999]",
        hostname: "crashed-container",
        renewedAt: new Date(renewedAt).toISOString(),
      }),
    );
    const pinned = new Date(renewedAt);
    await utimes(lockPath(dir), pinned, pinned);
    let nowMs = renewedAt;
    const reasons: string[] = [];
    const onTakeover = vi.fn();
    const store = await retryStartup({
      attempt: () => open(dir, { bootId, now: () => nowMs, onTakeover }),
      onFailure: (reason) => reasons.push(reason),
      write: () => undefined,
      sleep: async () => {
        nowMs += JSON_STORE_LEASE_STALE_MS + 1_000;
      },
    });
    expect(reasons).toEqual(["starting:store_instance_lock_held"]);
    expect(onTakeover).toHaveBeenCalledExactlyOnceWith("lease_stale");
    expect(await store.listStateQueueHeaders()).toEqual([]);
    expect(
      JSON.parse(await readFile(lockPath(dir), "utf8")) as JsonStoreLeaseRecord,
    ).toMatchObject({ pid: process.pid, hostname: hostname() });
  });

  it.each(["another namespace", "another host", "unreadable"])(
    "never takes over a live holder that keeps the process mutex, even with stale %s metadata",
    async (kind) => {
      const dir = await tempDir();
      const holder = spawn(
        process.execPath,
        [
          "--experimental-transform-types",
          "--no-warnings",
          "--input-type=module",
          "-e",
          `const { JsonStoreProcessMutex } = await import(${JSON.stringify(
            fileURLToPath(
              new URL(
                "../src/store.json-file-process-mutex.ts",
                import.meta.url,
              ),
            ),
          )});
JsonStoreProcessMutex.acquire(${JSON.stringify(`${lockPath(dir)}.mutex.sqlite`)});
process.stdout.write("held\\n");
setInterval(() => {}, 1_000);`,
        ],
        { stdio: ["ignore", "pipe", "inherit"] },
      );
      children.add(holder);
      await new Promise<void>((resolve, reject) => {
        holder.stdout!.on("data", (chunk: Buffer) => {
          if (chunk.toString().includes("held")) resolve();
        });
        holder.once("exit", (code) =>
          reject(new Error(`mutex holder exited with ${String(code)}`)),
        );
      });
      const renewedAt = Date.now() - 10 * JSON_STORE_LEASE_STALE_MS;
      const metadata =
        kind === "unreadable"
          ? ""
          : `${JSON.stringify(
              leaseOf({
                ...(kind === "another namespace"
                  ? { pidNamespace: "pid:[4026539999]" }
                  : { hostname: "another-host" }),
                renewedAt: new Date(renewedAt).toISOString(),
              }),
            )}\n`;
      await leaveLock(dir, metadata);
      const pinned = new Date(renewedAt);
      await utimes(lockPath(dir), pinned, pinned);
      const onTakeover = vi.fn();
      const refused = open(dir, { bootId, onTakeover });
      await expect(refused).rejects.toThrow(/its process mutex is held/u);
      expect(
        startupReason(await refused.catch((error: unknown) => error)),
      ).toBe("starting:store_instance_lock_held");
      expect(onTakeover).not.toHaveBeenCalled();
      expect(await readFile(lockPath(dir), "utf8")).toBe(metadata);
      // The mutex alone kept it: once the holder dies the same stale
      // metadata is taken over.
      const exited = new Promise((resolve) => holder.once("exit", resolve));
      holder.kill("SIGKILL");
      await exited;
      await open(dir, { bootId, onTakeover });
      expect(onTakeover).toHaveBeenCalledExactlyOnceWith(
        kind === "unreadable" ? "unreadable_lease_stale" : "lease_stale",
      );
    },
  );
});

it("old release preserves successor metadata even before observing ownership loss", async () => {
  const dir = await tempDir();
  const store = await open(dir, { bootId, renewMs: 1_000_000_000 });
  const successor = leaseOf({ owner: "successor", hostname: "another-host" });
  await leaveLock(dir, successor);
  await store.close();
  openStores.delete(store);
  expect(existsSync(lockPath(dir))).toBe(true);
  expect(JSON.parse(await readFile(lockPath(dir), "utf8"))).toEqual(successor);
  await store.close();
});

it("fences a paused live writer, then admits exactly one real successor after process death", async () => {
  const fixtureRoot = await mkdtemp(
    join(dirname(fileURLToPath(import.meta.url)), ".json-lease-fixture-"),
  );
  const dir = await tempDir();
  const storePath = join(dir, "committee.json");
  const worker = fileURLToPath(
    new URL("./fixtures/json-store-lease-child.mjs", import.meta.url),
  );
  const owned: ChildProcess[] = [];
  const start = (role: string) => {
    const child = fork(
      worker,
      [
        join(fixtureRoot, "store.json-file-committee-store.js"),
        storePath,
        role,
      ],
      { stdio: ["ignore", "ignore", "inherit", "ipc"], execArgv: [] },
    );
    owned.push(child);
    const events: { event: string; peers?: string[]; message?: string }[] = [];
    child.on("message", (message) =>
      events.push(message as (typeof events)[number]),
    );
    const wait = async (...kinds: string[]) => {
      await vi.waitFor(
        () =>
          expect(events.some((event) => kinds.includes(event.event))).toBe(
            true,
          ),
        { timeout: 5000, interval: 10 },
      );
      return events.find((event) => kinds.includes(event.event))!;
    };
    return { child, wait };
  };
  const stop = async (child: ChildProcess) => {
    if (child.exitCode !== null || child.signalCode !== null) return;
    const exited = new Promise((resolve) => child.once("exit", resolve));
    child.kill("SIGKILL");
    await exited;
  };
  try {
    await build({
      entry: ["src/store.json-file-committee-store.ts"],
      outDir: fixtureRoot,
      format: ["esm"],
      bundle: true,
      splitting: false,
      dts: false,
      silent: true,
      skipNodeModulesBundle: true,
      esbuildOptions(options) {
        options.conditions = ["node", "import"];
      },
    });
    const old = start("paused");
    expect((await old.wait("opened", "refused")).event).toBe("opened");
    old.child.send("save");
    await old.wait("write-paused");
    const contender = start("contender");
    const refusal = await contender.wait("opened", "refused");
    expect(refusal.event).toBe("refused");
    expect(refusal.message).toMatch(heldLockError);
    expect(old.child.exitCode).toBeNull();
    old.child.send("resume");
    expect((await old.wait("saved")).peers).toEqual(["old-peer"]);
    await stop(old.child);
    const candidates = [start("contender"), start("contender")];
    const results = await Promise.all(
      candidates.map((candidate) => candidate.wait("opened", "refused")),
    );
    expect(results.filter((result) => result.event === "opened")).toHaveLength(
      1,
    );
    expect(results.filter((result) => result.event === "refused")).toHaveLength(
      1,
    );
    const successor =
      candidates[results.findIndex((result) => result.event === "opened")];
    successor.child.send("save");
    expect((await successor.wait("saved")).peers).toEqual([
      "new-peer",
      "old-peer",
    ]);
    successor.child.send("close");
    await successor.wait("closed");
    const reopened = await open(dir);
    expect(
      (await reopened.listPeerHealth()).map((peer) => peer.peerId),
    ).toEqual(["new-peer", "old-peer"]);
  } finally {
    await Promise.all(owned.map(stop));
    await rm(fixtureRoot, { recursive: true, force: true });
  }
}, 20_000);

it("releases the process mutex and temporary handle after failed metadata publication", async () => {
  const dir = await tempDir();
  const original = fileSystem.open;
  let injected = false;
  fileSystem.open = async (...args: Parameters<typeof fileSystem.open>) => {
    const handle = await original(...args);
    if (!injected && String(args[0]).endsWith(".metadata.tmp")) {
      injected = true;
      handle.sync = () =>
        Promise.reject(new Error("synthetic metadata sync failure"));
    }
    return handle;
  };
  syncBuiltinESMExports();
  try {
    await expect(JsonFileCommitteeStore.open(dir)).rejects.toThrow(
      "synthetic metadata sync failure",
    );
    expect(injected).toBe(true);
    expect(existsSync(lockPath(dir))).toBe(false);
  } finally {
    fileSystem.open = original;
    syncBuiltinESMExports();
  }
  const store = await open(dir);
  expect(await store.listPeerHealth()).toEqual([]);
});

it("releases an acquired process mutex when constructing its owner timestamp fails", async () => {
  const dir = await tempDir();
  await expect(
    JsonFileCommitteeStore.open(dir, { now: () => Number.NaN }),
  ).rejects.toThrow("Invalid time value");
  expect(existsSync(lockPath(dir))).toBe(false);
  expect(await (await open(dir)).listPeerHealth()).toEqual([]);
});
