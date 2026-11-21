/**
 * Scratch directories for installed-application tests, under the OS
 * temporary directory unless the caller names another root.
 *
 * The test that creates one removes it when it is done. A run stopped
 * early never reaches that removal, so every directory still held is also
 * removed after the test file's last test, when the test process exits,
 * when it loses the runner that forked it (a stopped runner's forks are
 * disconnected, then end without an exit event), and when it is stopped by
 * SIGINT, SIGTERM or SIGHUP (the signal is then raised again so the
 * process still stops). Only SIGKILL of the test process itself escapes
 * all of these. Importing this module from a test file registers the
 * file-level removal.
 */
import { rmSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll } from "vitest";

const held = new Set<string>();

const removeHeld = () => {
  for (const directory of held)
    rmSync(directory, { recursive: true, force: true });
  held.clear();
};

afterAll(removeHeld);
process.once("exit", removeHeld);
process.once("disconnect", removeHeld);
for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"] as const)
  process.once(signal, () => {
    removeHeld();
    if (process.listenerCount(signal) === 0) process.kill(process.pid, signal);
  });

/**
 * A fresh directory named `prefix…` under `root`, held until `remove` or
 * process end.
 */
export const scratchDirectory = async (prefix: string, root = tmpdir()) => {
  const path = await mkdtemp(join(root, prefix));
  held.add(path);
  return {
    path,
    remove: async () => {
      held.delete(path);
      await rm(path, { recursive: true, force: true });
    },
  } as const;
};
