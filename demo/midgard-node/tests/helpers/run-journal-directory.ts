/**
 * The run-scoped directory that holds the publication journals tests write
 * when they publish a deployment on an emulator.
 *
 * A publication journal fsyncs every record before it submits anything
 * (`reference-publication-chain.publication-journal.ts`), and one full
 * deployment writes about 1,850 records, so on a disk the flushes alone took
 * 3.3 s of a 12 s deployment (Node 22.22.2, two pinned cores). The global
 * setup puts this directory on Linux's RAM-backed `/dev/shm` when it has
 * room, where an fsync returns at once: every fsync still runs, and the
 * journal's contents, order and errors stay the same; only the flush to a
 * disk that no test can observe is skipped, as the test Postgres already
 * runs with `fsync=off` (`scripts/start-test-postgres.sh`). Elsewhere it is
 * an ordinary temporary directory. The global teardown removes it, journals
 * included, so a run leaves nothing behind in memory or on disk.
 */
import { constants } from "node:fs";
import { access, mkdtemp, rm, statfs } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

export const RUN_JOURNAL_DIRECTORY_ENV = "MIDGARD_NODE_TEST_JOURNAL_DIR";

const RAM_BACKED_DIRECTORY = "/dev/shm";

/** Room for every deployment journal of a run (about 31 MB each) with a
 * wide margin; a smaller `/dev/shm`, such as a container's 64 MB default,
 * is left alone. */
const RAM_BACKED_MIN_FREE_BYTES = 2 ** 30;

const runJournalBase = async (): Promise<string> => {
  try {
    await access(RAM_BACKED_DIRECTORY, constants.W_OK);
    const { bavail, bsize } = await statfs(RAM_BACKED_DIRECTORY);
    if (bavail * bsize >= RAM_BACKED_MIN_FREE_BYTES)
      return RAM_BACKED_DIRECTORY;
  } catch {
    // No usable RAM-backed directory on this host.
  }
  return tmpdir();
};

/** Always a new directory: a value inherited from an outer process belongs
 * to another run. */
export const createRunJournalDirectory = async (): Promise<string> => {
  const directory = await mkdtemp(
    join(await runJournalBase(), "midgard-node-journals-"),
  );
  process.env[RUN_JOURNAL_DIRECTORY_ENV] = directory;
  return directory;
};

export const removeRunJournalDirectory = async (
  directory: string,
): Promise<void> => {
  await rm(directory, { recursive: true, force: true });
  if (process.env[RUN_JOURNAL_DIRECTORY_ENV] === directory)
    delete process.env[RUN_JOURNAL_DIRECTORY_ENV];
};

/** A fresh directory for one publication journal: inside the run's journal
 * directory, or the OS temporary directory outside the package's global
 * setup. */
export const makeJournalDirectory = (prefix: string): Promise<string> => {
  const directory = process.env[RUN_JOURNAL_DIRECTORY_ENV];
  return mkdtemp(
    join(
      directory === undefined || directory === "" ? tmpdir() : directory,
      prefix,
    ),
  );
};
