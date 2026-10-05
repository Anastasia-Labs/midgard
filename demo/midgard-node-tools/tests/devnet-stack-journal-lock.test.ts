import { spawn } from "node:child_process";
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
import { performance } from "node:perf_hooks";
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
import { performance } from "node:perf_hooks";
register("data:text/javascript," + encodeURIComponent(
  "export async function resolve(s, c, n) { try { return await n(s, c); } catch (e) {" +
  " if (s.startsWith('.') && s.endsWith('.js')) return n(s.slice(0, -3) + '.ts', c); throw e; } }"));
const [modulePath, path, key, gate, unlinkGate, mode] = process.argv.slice(2);
const { Journal } = await import(pathToFileURL(modulePath).href);
if (mode === "pid-holder") {
  const { processStartTime } = await import(new URL("./lock.ts", pathToFileURL(modulePath)).href);
  fs.writeFileSync(path + ".lock", process.pid + " " + processStartTime(process.pid));
  fs.writeFileSync(gate + "/read", "");
  while (!fs.existsSync(gate + "/go"))
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5);
  fs.unlinkSync(path + ".lock");
  process.exit(0);
}
if (mode === "clock-step") {
  const wallStart = Date.now();
  const elapsedStart = performance.now();
  Date.now = () => wallStart + (performance.now() - elapsedStart) -
    (performance.now() - elapsedStart >= 50 ? 60_000 : 0);
  fs.writeFileSync(gate + "/read", "");
  let result;
  try { new Journal(path, { lockWaitMs: 200 }).set(key, { writtenBy: process.pid }); result = "wrote"; }
  catch (error) { result = String(error); }
  fs.writeFileSync(gate + "/result", JSON.stringify({ result, wallElapsed: Date.now() - wallStart, elapsed: performance.now() - elapsedStart }));
  process.exit(0);
}
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
  let lockReads = 0;
  fs.readFileSync = (...args) => {
    const content = read(...args);
    if (args[0] === path + ".lock") lockReads += 1;
    if (armed && (mode === "stale-check" ? args[0] === path + ".lock" && lockReads === 3 : args[0] === path)) {
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
const children: ReturnType<typeof spawn>[] = [];
const tempDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-journal-lock-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const child of children.splice(0))
    if (child.exitCode === null && child.signalCode === null)
      child.kill("SIGKILL");
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const WRITER_BOUND_MS = 20_000;

const startWriter = (
  dir: string,
  path: string,
  key: string,
  gate = "",
  mode = "",
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
      mode,
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
  return { child, exited, stderr: () => stderr };
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

  it("preserves both rows when two processes encounter the same dead lock and one pauses after its stale comparison", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    const dead = `${process.pid.toString()} 0`;
    writeFileSync(`${path}.lock`, dead);
    const firstGate = join(dir, "first");
    const secondGate = join(dir, "second");
    mkdirSync(firstGate);
    mkdirSync(secondGate);
    const first = startWriter(
      dir,
      path,
      "reserve-float:pending",
      firstGate,
      "stale-check",
    );
    await waitForFile(join(firstGate, "read"));
    expect(readFileSync(`${path}.lock`, "utf8")).toBe(dead);
    const second = startWriter(dir, path, "history-pin", secondGate);
    const secondReadBeforeFirstResumes = await within(
      waitForFile(join(secondGate, "read")),
      1_500,
    );
    if (secondReadBeforeFirstResumes)
      expect(readFileSync(`${path}.lock`, "utf8")).not.toBe(dead);
    writeFileSync(join(firstGate, "go"), "");
    await waitForFile(join(secondGate, "read"));
    const firstFinishedWhileSecondHeld =
      secondReadBeforeFirstResumes && (await within(first.exited, 1_500));
    writeFileSync(join(secondGate, "go"), "");
    const exits = [await first.exited, await second.exited];
    const result = new Journal(path);
    const trace = {
      secondReadBeforeFirstResumes,
      firstFinishedWhileSecondHeld,
      exits,
      seed: result.get("seed"),
      firstRow: result.get("reserve-float:pending"),
      secondRow: result.get("history-pin"),
      stderr: first.stderr() + second.stderr(),
    };
    console.log("stale-takeover-process-trace", JSON.stringify(trace));
    expect(exits).toEqual([0, 0]);
    expect(trace.stderr).toBe("");
    expect(secondReadBeforeFirstResumes).toBe(false);
    expect(firstFinishedWhileSecondHeld).toBe(false);
    expect(trace.seed).toBe(1);
    expect(trace.firstRow).toBeDefined();
    expect(trace.secondRow).toBeDefined();
  });

  it("refuses within its bound while the kernel mutex is held, then merges both rows on retry", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    const gate = join(dir, "held");
    mkdirSync(gate);
    const holder = startWriter(dir, path, "held-row", gate);
    await waitForFile(join(gate, "read"));
    const before = readFileSync(path, "utf8");
    const journal = new Journal(path, { lockWaitMs: 200 });
    const started = Date.now();
    expect(() => journal.set("retry-row", 3)).toThrow(/another journal writer/);
    expect(Date.now() - started).toBeLessThan(5_000);
    expect(readFileSync(path, "utf8")).toBe(before);
    writeFileSync(join(gate, "go"), "");
    expect(await holder.exited).toBe(0);
    expect(holder.stderr()).toBe("");
    journal.set("retry-row", 3);
    const result = new Journal(path);
    expect(result.get("seed")).toBe(1);
    expect(result.get("held-row")).toBeDefined();
    expect(result.get("retry-row")).toBe(3);
  });

  it("releases the kernel mutex after a writer dies inside the read/merge window", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    const gate = join(dir, "death");
    mkdirSync(gate);
    const crashed = startWriter(dir, path, "uncommitted", gate);
    await waitForFile(join(gate, "read"));
    expect(crashed.child.kill("SIGKILL")).toBe(true);
    await crashed.exited;
    new Journal(path, { lockWaitMs: 500 }).set("after-crash", 2);
    const journal = new Journal(path);
    expect(journal.get("seed")).toBe(1);
    expect(journal.get("uncommitted")).toBeUndefined();
    expect(journal.get("after-crash")).toBe(2);
    expect(existsSync(`${path}.lock`)).toBe(false);
    expect(existsSync(`${path}.mutex.sqlite`)).toBe(true);
  });

  it.each(["kernel", "pid"] as const)(
    "keeps the %s wait bounded through a wall-clock rollback and merges on retry",
    async (kind) => {
      const dir = tempDir();
      writeFileSync(join(dir, "writer.mjs"), writer);
      const path = join(dir, "journal.json");
      new Journal(path).set("seed", 1);
      const heldGate = join(dir, "held");
      const clockGate = join(dir, "clock");
      mkdirSync(heldGate);
      mkdirSync(clockGate);
      const holder = startWriter(
        dir,
        path,
        "held-row",
        heldGate,
        kind === "pid" ? "pid-holder" : "",
      );
      await waitForFile(join(heldGate, "read"));
      const before = readFileSync(path, "utf8");
      const waiter = startWriter(
        dir,
        path,
        "retry-row",
        clockGate,
        "clock-step",
      );
      await waitForFile(join(clockGate, "read"));
      const started = performance.now();
      const bounded = await within(waiter.exited, 700);
      if (!bounded) waiter.child.kill("SIGKILL");
      await waiter.exited;
      const unchanged = readFileSync(path, "utf8") === before;
      const result = existsSync(join(clockGate, "result"))
        ? JSON.parse(readFileSync(join(clockGate, "result"), "utf8"))
        : null;
      console.log(
        "journal-clock-process-trace",
        JSON.stringify({
          kind,
          bounded,
          unchanged,
          result,
          elapsedAfterStart: performance.now() - started,
        }),
      );
      writeFileSync(join(heldGate, "go"), "");
      expect(await holder.exited).toBe(0);
      const retry = new Journal(path);
      retry.set("retry-row", 3);
      expect(retry.get("seed")).toBe(1);
      if (kind === "kernel") expect(retry.get("held-row")).toBeDefined();
      expect(retry.get("retry-row")).toBe(3);
      expect(bounded).toBe(true);
      expect(unchanged).toBe(true);
      expect(result.result).toMatch(/held by/);
      expect(result.wallElapsed).toBeLessThan(-59_000);
      expect(result.elapsed).toBeLessThan(700);
      expect(holder.stderr() + waiter.stderr()).toBe("");
    },
  );

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

  it("takes over a dead holder's lock after the writer taking it over dies", async () => {
    const dir = tempDir();
    writeFileSync(join(dir, "writer.mjs"), writer);
    const path = join(dir, "journal.json");
    new Journal(path).set("seed", 1);
    writeFileSync(`${path}.lock`, `${process.pid.toString()} 0`);
    const gate = join(dir, "gate");
    mkdirSync(gate);
    // Killed at its takeover unlink, holding the kernel mutex.
    const crashed = startWriter(dir, path, "uncommitted", "", "", gate);
    await waitForFile(join(gate, "unlink"));
    expect(crashed.child.kill("SIGKILL")).toBe(true);
    await crashed.exited;
    const after = startWriter(dir, path, "after-crash");
    expect(await after.exited).toBe(0);
    expect(after.stderr()).toBe("");
    const journal = new Journal(path);
    expect(journal.get("uncommitted")).toBeUndefined();
    expect(journal.get("after-crash")).toBeDefined();
    expect(existsSync(`${path}.lock`)).toBe(false);
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
      startWriter(dir, path, `writer-${index.toString()}`, gate, "", gate),
    );
    // Hold the first takeover just before its unlink. The kernel mutex keeps
    // the other writer from even reaching its own takeover meanwhile.
    const atUnlink = () =>
      gates.findIndex((gate) => existsSync(join(gate, "unlink")));
    const deadline = Date.now() + 20_000;
    while (atUnlink() < 0) {
      if (Date.now() > deadline) throw new Error("no writer took over");
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    const first = atUnlink();
    const second = 1 - first;
    expect(await appears(join(gates[second]!, "unlink"), 1_500)).toBe(false);
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
