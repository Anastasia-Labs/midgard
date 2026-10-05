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

  it.each(["another namespace", "another host", "unreadable"])(
    "refuses an unprovable %s holder even after metadata is stale",
    async (kind) => {
      const dir = await tempDir();
      const renewedAt = Date.now();
      await leaveLock(
        dir,
        kind === "unreadable"
          ? ""
          : leaseOf({
              ...(kind === "another namespace"
                ? { pidNamespace: "pid:[4026539999]" }
                : { hostname: "another-host" }),
              renewedAt: new Date(renewedAt).toISOString(),
            }),
      );
      const pinned = new Date(renewedAt);
      await utimes(lockPath(dir), pinned, pinned);
      await expect(
        open(dir, {
          bootId,
          now: () => renewedAt + JSON_STORE_LEASE_STALE_MS + 1000,
        }),
      ).rejects.toThrow(heldLockError);
    },
  );

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
