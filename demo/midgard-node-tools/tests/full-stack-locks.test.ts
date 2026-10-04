import { type ChildProcess, spawn } from "node:child_process";
import { mkdir, mkdtemp, open, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import type { StackConfig } from "../src/full-stack/config.js";
import {
  assertHoldsControllerLock,
  runUnderControllerLock,
} from "../src/full-stack/controller-lock.js";
import { StackProcesses } from "../src/full-stack/process.js";

const directories: string[] = [];
const holders: ChildProcess[] = [];
afterEach(async () => {
  for (const holder of holders.splice(0)) holder.kill("SIGKILL");
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});
async function directory() {
  const path = await mkdtemp(join(tmpdir(), "midgard-stack-locks-"));
  directories.push(path);
  return path;
}
/** Another process holding `lock` until it is killed. */
async function holdLock(lock: string) {
  const holder = spawn(
    "flock",
    [
      "--nonblock",
      "--no-fork",
      lock,
      process.execPath,
      "-e",
      "console.log('locked');setInterval(()=>{},1000)",
    ],
    { stdio: ["ignore", "pipe", "pipe"], env: { PATH: process.env.PATH } },
  );
  holders.push(holder);
  const exited = new Promise<void>((resolve) =>
    holder.once("exit", () => resolve()),
  );
  await new Promise<void>((resolve, reject) => {
    holder.stdout!.once("data", () => resolve());
    holder.once("error", reject);
    holder.once("exit", (code) =>
      reject(new Error(`Lock holder exited before locking: ${code}`)),
    );
  });
  return {
    release: async () => {
      holder.kill("SIGKILL");
      await exited;
    },
  };
}
const inner = (code: number) => ["-e", `process.exit(${code})`];

describe("stack controller lock", () => {
  it("refuses a second controller and admits one after the holder dies", async () => {
    const lock = join(await directory(), "controller.lock");
    const holder = await holdLock(lock);
    await expect(
      runUnderControllerLock(lock, inner(0), process.env),
    ).rejects.toThrow(`Another stack controller holds ${lock}`);
    await holder.release();
    await expect(
      runUnderControllerLock(lock, inner(0), process.env),
    ).resolves.toBeUndefined();
  });
  it("reports a failing controller as its own failure, not contention", async () => {
    const lock = join(await directory(), "controller.lock");
    await expect(
      runUnderControllerLock(lock, inner(1), process.env),
    ).rejects.toThrow("Stack controller failed (exit 1)");
  });
  it("requires an open descriptor on the lock, not only the inherited marker", async () => {
    const lock = join(await directory(), "controller.lock");
    await expect(assertHoldsControllerLock(lock)).rejects.toThrow(
      "this process does not hold",
    );
    const handle = await open(lock, "w");
    try {
      await expect(assertHoldsControllerLock(lock)).resolves.toBeUndefined();
    } finally {
      await handle.close();
    }
  });
});

describe("stack command execution", () => {
  async function processes(env: Record<string, string> = {}) {
    const nodeRoot = await directory();
    await mkdir(join(nodeRoot, "logs"));
    const config = {
      nodeRoot,
      runDirectory: join(nodeRoot, "logs/run"),
      timeoutMs: 60_000,
    } as StackConfig;
    return new StackProcesses(config, env);
  }
  const printEnv = ["-e", "console.log(JSON.stringify({ env: process.env }))"];
  it("does not start a command while another stack command holds the lock", async () => {
    const stack = await processes();
    const holder = await holdLock(
      join(stack.config.nodeRoot, "logs/full-stack-command.lock"),
    );
    await expect(stack.command("probe", "true", [])).rejects.toThrow(
      "probe did not start: another stack command holds",
    );
    await holder.release();
    await expect(stack.command("probe", "true", [])).resolves.toBeNull();
  });
  it("gives build tools only the host toolchain environment", async () => {
    const stack = await processes({
      STACK_USER_SEED: "secret words",
      NODE_OPTIONS: "--require /nonexistent/hook.js",
    });
    const { env } = (await stack.command(
      "build-probe",
      process.execPath,
      printEnv,
      {},
      stack.config.nodeRoot,
      "host",
    )) as { env: Record<string, string> };
    expect(env.STACK_USER_SEED).toBeUndefined();
    expect(env.NODE_OPTIONS).toBeUndefined();
    expect(env.PATH).toBe(process.env.PATH);
  });
  it("keeps the host PATH and HOME over the stack environment", async () => {
    const stack = await processes({
      STACK_USER_SEED: "secret words",
      PATH: "/nonexistent",
      HOME: "/nonexistent",
    });
    const { env } = (await stack.command(
      "stack-probe",
      process.execPath,
      printEnv,
    )) as { env: Record<string, string> };
    expect(env.STACK_USER_SEED).toBe("secret words");
    expect(env.PATH).toBe(process.env.PATH);
    expect(env.HOME).toBe(process.env.HOME);
  });
});
