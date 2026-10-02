import { spawn } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
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
const [modulePath, path, key, gate] = process.argv.slice(2);
const { Journal } = await import(pathToFileURL(modulePath).href);
const journal = new Journal(path);
if (gate) {
  const read = fs.readFileSync;
  let armed = true;
  fs.readFileSync = (...args) => {
    const content = read(...args);
    if (armed && args[0] === path) {
      armed = false;
      fs.writeFileSync(gate + "/read", "");
      while (!fs.existsSync(gate + "/go"))
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5);
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
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const startWriter = (dir: string, path: string, key: string, gate = "") => {
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
    ],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  let stderr = "";
  child.stderr.on("data", (chunk: Buffer) => (stderr += chunk.toString()));
  const exited = new Promise<number | null>((resolve) =>
    child.once("exit", resolve),
  );
  return { exited, stderr: () => stderr };
};
const within = <T>(promise: Promise<T>, ms: number) =>
  Promise.race([
    promise.then(() => true),
    new Promise<false>((resolve) => setTimeout(() => resolve(false), ms)),
  ]);
const waitForFile = async (path: string) => {
  const deadline = Date.now() + 20_000;
  while (!existsSync(path)) {
    if (Date.now() > deadline) throw new Error(`${path} never appeared`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};

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

  it("takes over the lock of a writer that died holding it", () => {
    const path = join(tempDir(), "journal.json");
    new Journal(path).set("seed", 1);
    // This PID with a start time the kernel never gave it: a dead holder.
    writeFileSync(`${path}.lock`, `${process.pid.toString()} 0`);
    new Journal(path).set("after-crash", 2);
    expect(new Journal(path).get("after-crash")).toBe(2);
    expect(existsSync(`${path}.lock`)).toBe(false);
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
