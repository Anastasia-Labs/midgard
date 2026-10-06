/**
 * The run-scoped directory where test files share deployed emulator fixtures:
 * the history-source prefix
 * (`history-source-owner-emulator.restore-history-source.ts`) and the
 * deposit-flow fixture (`deposit-flow-emulator-shared.make-fixture.ts`).
 *
 * The global setup creates it once per Vitest run and exports its path to
 * every worker through the environment; the global teardown removes it. A
 * fresh directory per run is what makes sharing safe: a file can only read a
 * fixture that the same sources deployed in the same run. Files that start
 * at once deploy it once: the first claims it with a lock file and the
 * others wait for it (`loadOrCreateRunSharedFixture`). The fixture is
 * published with an atomic rename, so a reader sees one complete fixture or
 * none.
 *
 * Values must be plain data: `v8.serialize` keeps exactly what
 * `structuredClone` keeps, and throws on anything else.
 */
import { randomUUID } from "node:crypto";
import {
  link,
  mkdtemp,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";
import { deserialize, serialize } from "node:v8";

export const RUN_SHARED_FIXTURE_DIRECTORY_ENV =
  "MIDGARD_NODE_TEST_SHARED_FIXTURE_DIR";

/** Always a new directory: a value inherited from an outer process could
 * hold a fixture deployed by other sources. */
export const createRunSharedFixtureDirectory = async (): Promise<string> => {
  const directory = await mkdtemp(
    join(tmpdir(), "midgard-node-shared-fixtures-"),
  );
  process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] = directory;
  return directory;
};

export const removeRunSharedFixtureDirectory = async (
  directory: string,
): Promise<void> => {
  await rm(directory, { recursive: true, force: true });
  if (process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV] === directory)
    delete process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];
};

/** The file holding the fixture named `name`, or `undefined` outside the
 * package's global setup (each file then deploys its own). */
const sharedFixturePath = (name: string): string | undefined => {
  const directory = process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];
  return directory === undefined || directory === ""
    ? undefined
    : join(directory, `${name}.v8`);
};

/** A fresh directory inside the run directory for files a test needs only
 * while the run lasts, so the global teardown removes them with the run;
 * the OS temporary directory outside the package's global setup. */
export const makeRunScratchDirectory = (prefix: string): Promise<string> => {
  const directory = process.env[RUN_SHARED_FIXTURE_DIRECTORY_ENV];
  return mkdtemp(
    join(
      directory === undefined || directory === "" ? tmpdir() : directory,
      prefix,
    ),
  );
};

/** The fixture another file of this run published as `name`, if any. */
const readRunSharedFixture = async (name: string): Promise<unknown> => {
  const path = sharedFixturePath(name);
  if (path === undefined) return undefined;
  let bytes: Buffer;
  try {
    bytes = await readFile(path);
  } catch (cause) {
    if ((cause as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw cause;
  }
  return deserialize(bytes);
};

/** Best effort: a fixture that cannot be shared only costs a later file its
 * own deployment. */
const shareRunSharedFixture = async (
  name: string,
  fixture: unknown,
): Promise<void> => {
  const path = sharedFixturePath(name);
  if (path === undefined) return;
  const staging = `${path}.${String(process.pid)}-${randomUUID()}`;
  try {
    await writeFile(staging, serialize(fixture), { mode: 0o600 });
    await rename(staging, path);
  } catch (cause) {
    await rm(staging, { force: true }).catch(() => {});
    process.stderr.write(
      `[shared-fixture] ${name} not shared (${String(cause)}); later files deploy their own\n`,
    );
  }
};

const CLAIM_POLL_MS = 200;

/** Whether the process that wrote `lockPath` may still share its fixture:
 * an unreadable or vanished lock counts as alive, so the caller looks again. */
const claimHolderAlive = async (lockPath: string): Promise<boolean> => {
  let pid: number;
  try {
    pid = Number(await readFile(lockPath, "utf8"));
  } catch {
    return true;
  }
  try {
    process.kill(pid, 0);
    return true;
  } catch (cause) {
    return (cause as NodeJS.ErrnoException).code !== "ESRCH";
  }
};

/** Claims `name` for this process: the lock file appears atomically with
 * the holder's pid in it, or not at all. */
const claim = async (lockPath: string): Promise<boolean> => {
  const staging = `${lockPath}.${String(process.pid)}-${randomUUID()}`;
  await writeFile(staging, String(process.pid), { mode: 0o600 });
  try {
    await link(staging, lockPath);
    return true;
  } catch (cause) {
    if ((cause as NodeJS.ErrnoException).code === "EEXIST") return false;
    throw cause;
  } finally {
    await rm(staging, { force: true });
  }
};

/**
 * The fixture this run shares as `name`, created once: a file that finds it
 * already shared reads it; otherwise the first file to claim it runs
 * `create` and shares the `shared` part of the result, while every other
 * file waits for that fixture instead of creating its own. A waiter takes
 * over the claim when its holder released it without sharing (its `create`
 * failed, or sharing did) or its process ended. `created` is present only in
 * the file that ran `create`. Without the package's global setup every call
 * runs `create`.
 */
export const loadOrCreateRunSharedFixture = async <Shared, Created>(
  name: string,
  create: () => Promise<{ readonly shared: Shared; readonly created: Created }>,
): Promise<{ readonly shared: Shared; readonly created?: Created }> => {
  const path = sharedFixturePath(name);
  if (path === undefined) return create();
  const lockPath = `${path}.lock`;
  for (;;) {
    const shared = (await readRunSharedFixture(name)) as Shared | undefined;
    if (shared !== undefined) return { shared };
    if (await claim(lockPath)) {
      try {
        // The holder before us may have shared it just before releasing.
        const late = (await readRunSharedFixture(name)) as Shared | undefined;
        if (late !== undefined) return { shared: late };
        const result = await create();
        await shareRunSharedFixture(name, result.shared);
        return result;
      } finally {
        await rm(lockPath, { force: true });
      }
    }
    if (!(await claimHolderAlive(lockPath))) {
      // Two waiters may both clear a dead holder's claim; at worst both
      // create the fixture, which the atomic publication keeps harmless.
      await rm(lockPath, { force: true });
      continue;
    }
    await sleep(CLAIM_POLL_MS);
  }
};
