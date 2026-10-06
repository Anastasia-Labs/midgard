import { type ChildProcess, fork, spawn } from "node:child_process";
import fileSystem, { mkdtemp, rm, writeFile } from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";
import { hostname } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "tsup";
import { expect, vi } from "vitest";

import { JsonFileCommitteeStore } from "../../src/store.js";
import type { JsonStoreInstanceLockOptions } from "../../src/store.json-file-instance-lock.js";

/** Every store these helpers open, for the test file to close. */
export const openStores = new Set<JsonFileCommitteeStore>();

export const open = async (
  dir: string,
  options: JsonStoreInstanceLockOptions = {},
): Promise<JsonFileCommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(dir, options);
  openStores.add(store);
  return store;
};

export const closeStore = async (
  store: JsonFileCommitteeStore,
): Promise<void> => {
  await store.close();
  openStores.delete(store);
};

export const lockPath = (dir: string) => join(dir, "committee.json.lock");
export const sidecarPath = (dir: string) => `${lockPath(dir)}.mutex.sqlite`;
export const leaveLock = (dir: string, content: string) =>
  writeFile(lockPath(dir), content);
export const heldLockError = /already exclusively leased/u;
export const takenOver = /taken over by another process/u;
export const health = (peerId: string) => ({
  peerId,
  consecutiveFailures: 0,
  updatedAt: new Date().toISOString(),
});
export const peers = async (store: JsonFileCommitteeStore) =>
  (await store.listPeerHealth()).map((peer) => peer.peerId);
export const lockOf = (store: JsonFileCommitteeStore) =>
  Reflect.get(store, "instanceLock") as { assertHeld(): Promise<void> };

/**
 * A successor's stamp. It shares this process's pid, as a restarted
 * container's pid 1 shares its predecessor's, so only the whole stamp tells
 * the two holders apart.
 */
export const successorStamp = `${JSON.stringify({
  owner: `${process.pid.toString()}:successor`,
  pid: process.pid,
  hostname: hostname(),
  acquiredAt: new Date().toISOString(),
})}\n`;

/** Every process these helpers start, for the test file to stop. */
export const children = new Set<ChildProcess>();

export const stop = async (child: ChildProcess): Promise<void> => {
  if (child.exitCode !== null || child.signalCode !== null) return;
  const exited = new Promise((resolve) => child.once("exit", resolve));
  child.kill("SIGKILL");
  await exited;
};

export const stopChildren = async (): Promise<void> => {
  await Promise.all([...children].map(stop));
  children.clear();
};

export const liveProcess = (): ChildProcess => {
  const child = spawn("sleep", ["60"], { stdio: "ignore" });
  children.add(child);
  return child;
};

export type FsPatch = (
  original: (...args: unknown[]) => unknown,
) => (...args: unknown[]) => unknown;

/** Replaces `node:fs/promises` functions, as every importer sees them. */
export const patchFs = (patches: Record<string, FsPatch>): (() => void) => {
  const originals = new Map<string, unknown>();
  for (const [name, patch] of Object.entries(patches)) {
    const original = Reflect.get(fileSystem, name) as (
      ...args: unknown[]
    ) => unknown;
    originals.set(name, original);
    Reflect.set(fileSystem, name, patch(original));
  }
  syncBuiltinESMExports();
  return () => {
    for (const [name, original] of originals)
      Reflect.set(fileSystem, name, original);
    syncBuiltinESMExports();
  };
};

/**
 * Holds the first call of `node:fs/promises` `name` whose argument `argIndex`
 * is `path`, before it runs, until `resume`.
 */
export const pauseOnce = (name: string, path: string, argIndex = 0) => {
  let resume!: () => void;
  const gate = new Promise<void>((resolve) => (resume = resolve));
  let reached!: () => void;
  const paused = new Promise<void>((resolve) => (reached = resolve));
  let armed = true;
  const restore = patchFs({
    [name]:
      (original) =>
      async (...args) => {
        if (armed && String(args[argIndex]) === path) {
          armed = false;
          reached();
          await gate;
        }
        return original(...args);
      },
  });
  return { paused, resume, restore };
};

/** A process that holds only a store's process mutex, as a live holder does. */
export const mutexHolder = async (sidecar: string): Promise<ChildProcess> => {
  const mutexModule = fileURLToPath(
    new URL("../../src/store.json-file-process-mutex.ts", import.meta.url),
  );
  const holder = spawn(
    process.execPath,
    [
      "--experimental-transform-types",
      "--no-warnings",
      "--input-type=module",
      "-e",
      `const { JsonStoreProcessMutex } = await import(${JSON.stringify(mutexModule)});
JsonStoreProcessMutex.acquire(${JSON.stringify(sidecar)});
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
  return holder;
};

let bundleRoot: Promise<string> | undefined;

export const removeCommitteeBundle = async (): Promise<void> => {
  if (bundleRoot !== undefined)
    await rm(await bundleRoot, { recursive: true, force: true });
  bundleRoot = undefined;
};

/**
 * A committee store in another process, built once from this source and
 * driven through `../fixtures/json-store-lease-child.mjs`.
 */
export const committeeChild = async (storePath: string, role: string) => {
  bundleRoot ??= (async () => {
    const root = await mkdtemp(
      join(
        dirname(dirname(fileURLToPath(import.meta.url))),
        ".json-lease-fixture-",
      ),
    );
    await build({
      entry: ["src/store.json-file-committee-store.ts"],
      outDir: root,
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
    return root;
  })();
  const child = fork(
    fileURLToPath(
      new URL("../fixtures/json-store-lease-child.mjs", import.meta.url),
    ),
    [
      join(await bundleRoot, "store.json-file-committee-store.js"),
      storePath,
      role,
    ],
    { stdio: ["ignore", "ignore", "inherit", "ipc"], execArgv: [] },
  );
  children.add(child);
  const events: { event: string; peers?: string[]; message?: string }[] = [];
  child.on("message", (message) =>
    events.push(message as (typeof events)[number]),
  );
  const wait = async (...kinds: string[]) => {
    await vi.waitFor(
      () =>
        expect(events.some((event) => kinds.includes(event.event))).toBe(true),
      { timeout: 10_000, interval: 10 },
    );
    return events.find((event) => kinds.includes(event.event))!;
  };
  return { child, wait };
};
