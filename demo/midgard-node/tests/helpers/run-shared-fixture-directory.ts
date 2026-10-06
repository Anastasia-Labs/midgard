/**
 * The run-scoped directory where test files share deployed emulator fixtures:
 * the history-source prefix
 * (`history-source-owner-emulator.restore-history-source.ts`) and the
 * deposit-flow fixture (`deposit-flow-emulator-shared.make-fixture.ts`).
 *
 * The global setup creates it once per Vitest run and exports its path to
 * every worker through the environment; the global teardown removes it. A
 * fresh directory per run is what makes sharing safe: a file can only read a
 * fixture that the same sources deployed in the same run. Two workers that
 * both miss deploy independently; each publishes with an atomic rename, so a
 * reader sees one complete fixture or none.
 *
 * Values must be plain data: `v8.serialize` keeps exactly what
 * `structuredClone` keeps, and throws on anything else.
 */
import { randomUUID } from "node:crypto";
import { mkdtemp, readFile, rename, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
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

/** The fixture another file of this run published as `name`, if any. */
export const readRunSharedFixture = async (name: string): Promise<unknown> => {
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
export const shareRunSharedFixture = async (
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
