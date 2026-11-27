import { spawn, spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { basename, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  createRunJournalDirectory,
  removeRunJournalDirectory,
  RUN_JOURNAL_DIRECTORY_ENV,
  sweepEndedRunJournalDirectories,
} from "./helpers/run-journal-directory.js";

const RAM_BACKED_DIRECTORY = "/dev/shm";

/** A pid no process holds: a child that has already exited. */
const endedPid = () => spawnSync(process.execPath, ["-e", ""]).pid!;

const planted: string[] = [];
const children: ReturnType<typeof spawn>[] = [];
const inherited = process.env[RUN_JOURNAL_DIRECTORY_ENV];

afterEach(async () => {
  children.splice(0).forEach((child) => child.kill());
  await Promise.all(
    planted.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
  if (inherited === undefined) delete process.env[RUN_JOURNAL_DIRECTORY_ENV];
  else process.env[RUN_JOURNAL_DIRECTORY_ENV] = inherited;
});

/** A directory in `/dev/shm` whose name starts with `prefix`, removed after
 * the test whatever the sweep did. */
const plant = async (prefix: string): Promise<string> => {
  const path = await mkdtemp(join(RAM_BACKED_DIRECTORY, prefix));
  planted.push(path);
  return path;
};

describe.skipIf(!existsSync(RAM_BACKED_DIRECTORY))(
  "the run journal directory sweep",
  () => {
    it("removes the journal directories of runs whose process ended, and nothing else", async () => {
      const live = spawn(process.execPath, [
        "-e",
        "setInterval(() => {}, 1000)",
      ]);
      children.push(live);
      const ended = await plant(`midgard-node-journals-${endedPid()}-`);
      const alive = await plant(`midgard-node-journals-${live.pid!}-`);
      const own = await plant(`midgard-node-journals-${process.pid}-`);
      const unnamed = await plant("midgard-node-journals-");
      const foreign = await plant(`other-journals-${endedPid()}-`);

      await sweepEndedRunJournalDirectories();

      expect(existsSync(ended)).toBe(false);
      for (const kept of [alive, own, unnamed, foreign])
        expect(existsSync(kept), `${basename(kept)} is kept`).toBe(true);
    });

    it("names a new run directory by this process, after sweeping ended runs", async () => {
      const ended = await plant(`midgard-node-journals-${endedPid()}-`);

      const directory = await createRunJournalDirectory();
      planted.push(directory);

      expect(existsSync(ended)).toBe(false);
      expect(basename(directory)).toMatch(
        new RegExp(`^midgard-node-journals-${process.pid}-`, "u"),
      );
      expect(process.env[RUN_JOURNAL_DIRECTORY_ENV]).toBe(directory);
      await removeRunJournalDirectory(directory);
      expect(existsSync(directory)).toBe(false);
      expect(process.env[RUN_JOURNAL_DIRECTORY_ENV]).toBeUndefined();
    });
  },
);
