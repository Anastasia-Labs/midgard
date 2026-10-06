/**
 * The run-scoped directory where test files share the deployed history-source
 * prefix (`history-source-owner-emulator.restore-history-source.ts`).
 *
 * The global setup creates it once per Vitest run and exports its path to
 * every worker through the environment; the global teardown removes it. A
 * fresh directory per run is what makes sharing safe: a file can only read a
 * prefix that the same sources deployed in the same run. Two workers that
 * both miss deploy independently; each publishes with an atomic rename, so a
 * reader sees one complete prefix or none.
 *
 * Values must be plain data: `v8.serialize` keeps exactly what
 * `structuredClone` keeps, and throws on anything else.
 */
import { randomUUID } from "node:crypto";
import { mkdtemp, readFile, rename, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { deserialize, serialize } from "node:v8";

export const HISTORY_SOURCE_PREFIX_DIRECTORY_ENV =
  "MIDGARD_NODE_TEST_HISTORY_SOURCE_PREFIX_DIR";

/** Always a new directory: a value inherited from an outer process could
 * hold a prefix deployed by other sources. */
export const createHistorySourcePrefixDirectory = async (): Promise<string> => {
  const directory = await mkdtemp(
    join(tmpdir(), "midgard-node-history-source-prefix-"),
  );
  process.env[HISTORY_SOURCE_PREFIX_DIRECTORY_ENV] = directory;
  return directory;
};

export const removeHistorySourcePrefixDirectory = async (
  directory: string,
): Promise<void> => {
  await rm(directory, { recursive: true, force: true });
  if (process.env[HISTORY_SOURCE_PREFIX_DIRECTORY_ENV] === directory)
    delete process.env[HISTORY_SOURCE_PREFIX_DIRECTORY_ENV];
};

/** The file holding `key`'s shared prefix, or `undefined` outside the
 * package's global setup (each file then deploys its own). */
const sharedPrefixPath = (key: string): string | undefined => {
  const directory = process.env[HISTORY_SOURCE_PREFIX_DIRECTORY_ENV];
  return directory === undefined || directory === ""
    ? undefined
    : join(directory, `history-source-${key}.v8`);
};

/** The prefix another file of this run published under `key`, if any. */
export const readSharedHistorySourcePrefix = async (
  key: string,
): Promise<unknown> => {
  const path = sharedPrefixPath(key);
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

/** Best effort: a prefix that cannot be shared only costs a later file its
 * own deployment. */
export const shareHistorySourcePrefix = async (
  key: string,
  prefix: unknown,
): Promise<void> => {
  const path = sharedPrefixPath(key);
  if (path === undefined) return;
  const staging = `${path}.${String(process.pid)}-${randomUUID()}`;
  try {
    await writeFile(staging, serialize(prefix), { mode: 0o600 });
    await rename(staging, path);
  } catch (cause) {
    await rm(staging, { force: true }).catch(() => {});
    process.stderr.write(
      `[history-source] deployed prefix not shared (${String(cause)}); later files deploy their own\n`,
    );
  }
};
