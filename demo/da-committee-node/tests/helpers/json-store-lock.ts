import { type ChildProcess, fork, spawn } from "node:child_process";
import fileSystem, { mkdtemp, rm } from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "tsup";
import { expect, vi } from "vitest";

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
