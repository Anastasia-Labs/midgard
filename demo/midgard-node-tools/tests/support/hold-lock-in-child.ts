import { type ChildProcess, spawn } from "node:child_process";
import { once } from "node:events";
import { existsSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const worker = fileURLToPath(
  new URL("./devnet-lock-worker.mjs", import.meta.url),
);
const holders: { child: ChildProcess; markers: string }[] = [];

/**
 * Takes `path` through `acquireLock` in a child process, as another
 * controller would, and returns that child's PID once it owns the lock.
 */
export const holdLockInChild = async (path: string): Promise<number> => {
  const markers = mkdtempSync(join(tmpdir(), "devnet-lock-holder-"));
  const child = spawn(
    process.execPath,
    [
      "--conditions=midgard-source",
      "--experimental-transform-types",
      worker,
      path,
      markers,
      "holder",
    ],
    { stdio: ["ignore", "ignore", "inherit", "ipc"] },
  );
  holders.push({ child, markers });
  const deadline = Date.now() + 10_000;
  for (;;) {
    if (existsSync(join(markers, "holder-owned"))) return child.pid!;
    if (existsSync(join(markers, "holder-refused")))
      throw new Error(readFileSync(join(markers, "holder-refused"), "utf8"));
    if (Date.now() >= deadline)
      throw new Error(`the lock holder did not take ${path}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};

/** Kills every holder `holdLockInChild` started; call it after each test. */
export const stopLockHolders = async (): Promise<void> => {
  await Promise.all(
    holders.splice(0).map(async ({ child, markers }) => {
      if (child.exitCode === null && child.signalCode === null) {
        const ended = once(child, "exit");
        child.kill("SIGKILL");
        await ended;
      }
      rmSync(markers, { recursive: true, force: true });
    }),
  );
};
