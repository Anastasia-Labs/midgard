import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterEach, describe, expect, it } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import { processStartTime } from "../src/devnet-stack/lock.js";

/**
 * Two processes write the same run journal: up or the journey, and the
 * endurance maintainer inside the supervisor. Each `set` reads the file,
 * merges its key and replaces the file, so one process writing between
 * another's read and its replace would have its entry overwritten. The writer
 * below stops right after its `set` has read the journal, until the test
 * lets it go, so the second process's write lands in exactly that window.
 * With an unlink gate it also stops at its first unlink of the lock, which
 * for a waiter over a dead holder is the takeover, until the test lets it go.
 *
 * Every writer runs in its own process and is awaited for a bounded time, so
 * a lock that is never taken over fails the test instead of blocking the
 * test worker in its poll.
 */
const journalModule = fileURLToPath(
  new URL("../src/devnet-stack/journal.ts", import.meta.url),
);
const writer = `
import fs from "node:fs";
import { register, syncBuiltinESMExports } from "node:module";
import { pathToFileURL } from "node:url";
register("data:text/javascript," + encodeURIComponent(
  "export async function resolve(s, c, n) { try { return await n(s, c); } catch (e) {" +
  " if (s.startsWith('.') && s.endsWith('.js')) return n(s.slice(0, -3) + '.ts', c); throw e; } }"));
const [modulePath, path, key, gate, unlinkGate] = process.argv.slice(2);
const { Journal } = await import(pathToFileURL(modulePath).href);
const journal = new Journal(path);
const hold = (dir, signal, release) => {
  fs.writeFileSync(dir + "/" + signal, "");
  while (!fs.existsSync(dir + "/" + release))
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5);
};
if (unlinkGate) {
  const unlink = fs.unlinkSync;
  let armed = true;
  fs.unlinkSync = (...args) => {
    if (armed && args[0] === path + ".lock") {
      armed = false;
      hold(unlinkGate, "unlink", "unlink-go");
    }
    return unlink(...args);
  };
  syncBuiltinESMExports();
}
if (gate) {
  const read = fs.readFileSync;
  let armed = true;
  fs.readFileSync = (...args) => {
    const content = read(...args);
    if (armed && args[0] === path) {
      armed = false;
      hold(gate, "read", "go");
    }
    return content;
  };
  syncBuiltinESMExports();
}
journal.set(key, { writtenBy: process.pid });
`;

const dirs: string[] = [];
const tempDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-journal-lock-"));
  dirs.push(dir);
  return dir;
};
const children: ReturnType<typeof spawn>[] = [];
afterEach(() => {
  for (const child of children.splice(0))
    if (child.exitCode === null && child.signalCode === null) child.kill();
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const WRITER_BOUND_MS = 20_000;

const startWriter = (
  dir: string,
  path: string,
  key: string,
  gate = "",
  unlinkGate = "",
) => {
  const child = spawn(
    process.execPath,
    [
      "--experimental-transform-types",
      "--no-warnings",
      join(dir, "writer.mjs"),
      journalModule,
      path,
      key,
      gate,
      unlinkGate,
    ],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  children.push(child);
  let stderr = "";
  child.stderr!.on("data", (chunk: Buffer) => (stderr += chunk.toString()));
  const exitCode = new Promise<number | null>((resolve) =>
    child.once("exit", resolve),
  );
  // A writer still running after the bound is killed and reads as "hung".
  const exited = Promise.race([
    exitCode,
    new Promise<"hung">((resolve) =>
      setTimeout(() => {
        child.kill();
        resolve("hung");
      }, WRITER_BOUND_MS).unref(),
    ),
  ]);
  return { exited, stderr: () => stderr };
};
const within = <T>(promise: Promise<T>, ms: number) =>
  Promise.race([
    promise.then(() => true),
    new Promise<false>((resolve) => setTimeout(() => resolve(false), ms)),
  ]);
const waitForFile = async (path: string, ms = 20_000) => {
  const deadline = Date.now() + ms;
  while (!existsSync(path)) {
    if (Date.now() > deadline) throw new Error(`${path} never appeared`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};
const appears = (path: string, ms: number) =>
  waitForFile(path, ms).then(
    () => true,
    () => false,
  );

describe("the devnet run journal across processes", () => {
  it("keeps a pending float record when another process writes another key while it is being recorded", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    const gate = join(dir, "gate");
    mkdirSync(gate);

    const float = startWriter(dir, path, "reserve-float:pending", gate);
    await waitForFile(join(gate, "read"));
    const other = startWriter(dir, path, "history-pin");
    const otherFinishedFirst = await within(other.exited, 1_500);
    writeFileSync(join(gate, "go"), "");
    expect([await float.exited, await other.exited]).toEqual([0, 0]);
    expect(float.stderr() + other.stderr()).toBe("");

    const journal = new Journal(path);
    expect(journal.get("seed")).toBe(1);
    expect(journal.get("reserve-float:pending")).toBeDefined();
    expect(journal.get("history-pin")).toBeDefined();
    // The second writer waited for the first to finish its write.
    expect(otherFinishedFirst).toBe(false);
    expect(existsSync(`${path}.lock`)).toBe(false);
  });

  it("takes over the lock of a writer that died holding it", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    // This PID with a start time the kernel never gave it: a dead holder.
    writeFileSync(`${path}.lock`, `${process.pid.toString()} 0`);
    const after = startWriter(dir, path, "after-crash");
    expect(await after.exited).toBe(0);
    expect(after.stderr()).toBe("");
    expect(new Journal(path).get("after-crash")).toBeDefined();
    expect(existsSync(`${path}.lock`)).toBe(false);
  });

  it("clears the takeover claim of a writer that died taking over a dead holder's lock", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    const dead = `${process.pid.toString()} 0`;
    writeFileSync(`${path}.lock`, dead);
    const digest = createHash("sha256").update(dead).digest("hex");
    writeFileSync(
      `${path}.lock.takeover-${digest.slice(0, 16)}`,
      `${process.pid.toString()} 1`,
    );
    const after = startWriter(dir, path, "after-crash");
    expect(await after.exited).toBe(0);
    expect(new Journal(path).get("after-crash")).toBeDefined();
    expect(readdirSync(dir).filter((name) => name.includes(".lock"))).toEqual(
      [],
    );
  });

  it("lets exactly one of two writers waiting on a dead holder take its lock over", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    writeFileSync(`${path}.lock`, `${process.pid.toString()} 0`);
    const gates = ["a", "b"].map((name) => {
      const gate = join(dir, `gate-${name}`);
      mkdirSync(gate);
      return gate;
    });
    const writers = gates.map((gate, index) =>
      startWriter(dir, path, `writer-${index.toString()}`, gate, gate),
    );
    // Hold the first takeover just before its unlink, and give the other
    // writer time to see the same dead holder and reach its own.
    const atUnlink = () =>
      gates.findIndex((gate) => existsSync(join(gate, "unlink")));
    const deadline = Date.now() + 20_000;
    while (atUnlink() < 0) {
      if (Date.now() > deadline) throw new Error("no writer took over");
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    const first = atUnlink();
    const second = 1 - first;
    await appears(join(gates[second]!, "unlink"), 3_000);
    writeFileSync(join(gates[first]!, "unlink-go"), "");
    await waitForFile(join(gates[first]!, "read"));
    writeFileSync(join(gates[second]!, "unlink-go"), "");
    // While the first writes under the lock, the second never does.
    expect(await appears(join(gates[second]!, "read"), 1_500)).toBe(false);
    writeFileSync(join(gates[first]!, "go"), "");
    await waitForFile(join(gates[second]!, "read"));
    writeFileSync(join(gates[second]!, "go"), "");
    expect(await Promise.all(writers.map((w) => w.exited))).toEqual([0, 0]);
    expect(writers.map((w) => w.stderr()).join("")).toBe("");
    const journal = new Journal(path);
    expect(journal.get("writer-0")).toBeDefined();
    expect(journal.get("writer-1")).toBeDefined();
    expect(readdirSync(dir).filter((name) => name.includes(".lock"))).toEqual(
      [],
    );
  });

  it("refuses within its bound while a live writer holds the lock, changing nothing, and writes once it is free", () => {
    const path = join(tempDir(), "journal.json");
    new Journal(path).set("seed", 1);
    const holder = `${process.pid.toString()} ${processStartTime(process.pid) ?? ""}`;
    writeFileSync(`${path}.lock`, holder);
    const before = readFileSync(path, "utf8");
    const journal = new Journal(path, { lockWaitMs: 200 });
    const started = Date.now();
    expect(() => journal.set("blocked", 3)).toThrow(/held by live pid/);
    expect(Date.now() - started).toBeLessThan(5_000);
    expect(readFileSync(path, "utf8")).toBe(before);
    expect(readFileSync(`${path}.lock`, "utf8")).toBe(holder);
    rmSync(`${path}.lock`);
    journal.set("blocked", 3);
    expect(new Journal(path).get("blocked")).toBe(3);
  });
});
