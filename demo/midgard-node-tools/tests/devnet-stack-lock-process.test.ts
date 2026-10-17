import { type ChildProcess, spawn } from "node:child_process";
import { once } from "node:events";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

import { afterEach, expect, it } from "vitest";

import type { Layout } from "../src/devnet-stack/layout.js";
import { withFloatLock } from "../src/devnet-stack/reserve-float-chain.js";

const worker = fileURLToPath(
  new URL("./support/devnet-lock-worker.mjs", import.meta.url),
);
const children: ChildProcess[] = [],
  directories: string[] = [];
const scene = () => {
  const path = mkdtempSync(join(tmpdir(), "up2-controller-lock-"));
  directories.push(path);
  return {
    path,
    lock: join(path, "controller.lock"),
    marked: (name: string, state: string) =>
      existsSync(join(path, `${name}-${state}`)),
  };
};
const start = (
  value: ReturnType<typeof scene>,
  name: string,
  pause = false,
) => {
  const child = spawn(
    process.execPath,
    [
      "--conditions=midgard-source",
      "--experimental-transform-types",
      worker,
      value.lock,
      value.path,
      name,
      pause ? "pause" : "run",
    ],
    { stdio: ["ignore", "ignore", "inherit", "ipc"] },
  );
  children.push(child);
  return child;
};
const wait = async (condition: () => boolean) => {
  const deadline = performance.now() + 5_000;
  while (!condition()) {
    if (performance.now() >= deadline)
      throw new Error("owned lock child did not reach expected boundary");
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};
const kill = async (child: ChildProcess) => {
  if (child.exitCode !== null || child.signalCode !== null) return;
  const ended = once(child, "exit");
  child.kill("SIGKILL");
  await ended;
};
afterEach(async () => {
  await Promise.all(children.splice(0).map(kill));
  for (const directory of directories.splice(0))
    rmSync(directory, { recursive: true, force: true });
});

it("permits one owner when two Node controllers encounter the same dead lock record", async () => {
  const value = scene();
  writeFileSync(value.lock, "999999999 0");
  const names = ["a", "b"],
    contenders = names.map((name) => start(value, name, true));
  await wait(() =>
    names.every(
      (name) => value.marked(name, "read") || value.marked(name, "refused"),
    ),
  );
  // On the old algorithm both have already cached the same stale bytes. Resume
  // each in turn so the second stale reader deletes the first live owner.
  for (let index = 0; index < names.length; index++) {
    const name = names[index]!;
    if (!value.marked(name, "read")) continue;
    contenders[index]!.kill("SIGCONT");
    await wait(
      () => value.marked(name, "owned") || value.marked(name, "refused"),
    );
  }
  const owners = names.filter((name) => value.marked(name, "owned"));
  expect(
    owners,
    "a stale contender must not replace an acquired live owner",
  ).toHaveLength(1);
  expect(readFileSync(value.lock, "utf8").split(" ")[0]).toBe(
    String(contenders[names.indexOf(owners[0]!)]!.pid),
  );
});

it("releases kernel ownership on SIGKILL and recovers the stale record without replacing its sidecar", async () => {
  const value = scene(),
    first = start(value, "first");
  await wait(() => value.marked("first", "owned"));
  const inode = statSync(`${value.lock}.mutex.sqlite`).ino;
  await kill(first);
  const next = start(value, "next");
  await wait(() => value.marked("next", "owned"));
  start(value, "blocked");
  await wait(() => value.marked("blocked", "refused"));
  expect(statSync(`${value.lock}.mutex.sqlite`).ino).toBe(inode);
  expect(readFileSync(value.lock, "utf8").split(" ")[0]).toBe(String(next.pid));
});

it("keeps a successor owned after an earlier release closure is called again", async () => {
  const value = scene(),
    first = start(value, "first");
  await wait(() => value.marked("first", "owned"));
  first.send("release");
  await wait(() => value.marked("first", "released"));
  const next = start(value, "next");
  await wait(() => value.marked("next", "owned"));
  first.send("release");
  await wait(() => value.marked("first", "released-2"));
  start(value, "blocked");
  await wait(() => value.marked("blocked", "refused"));
  expect(readFileSync(value.lock, "utf8").split(" ")[0]).toBe(String(next.pid));
});

it("holds ownership until an async float callback settles, including rejection", async () => {
  const value = scene();
  value.lock = join(value.path, "reserve-float.lock");
  let rejectStep: (error: Error) => void = () => {
    throw new Error("callback not entered");
  };
  const pending = withFloatLock(
    { state: value.path } as Layout,
    {
      now: Date.now,
      sleep: () => Promise.reject(new Error("unexpected contention")),
    },
    () =>
      new Promise<void>((_resolve, reject) => {
        rejectStep = reject;
      }),
  );
  start(value, "blocked");
  try {
    await wait(() => value.marked("blocked", "refused"));
  } finally {
    rejectStep(new Error("synthetic callback failure"));
    await expect(pending).rejects.toThrow(/callback failure/);
  }
  const next = start(value, "next");
  await wait(() => value.marked("next", "owned"));
  expect(readFileSync(value.lock, "utf8").split(" ")[0]).toBe(String(next.pid));
});
