/** Crash-safe publication for the append-only record directories whose
 * revisions are exclusive-create files named by revision. A record name only
 * ever refers to complete, fsynced bytes. */
import { randomUUID } from "node:crypto";
import { type FileHandle, link, open, readdir, unlink } from "node:fs/promises";
import { join } from "node:path";

/**
 * Staging files inside a record directory. The name never matches a record
 * name, so a scan can tell a staging file from a revision.
 */
export const STAGED_RECORD_FILE =
  /^\.staged-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\.tmp$/u;

export const stagedRecordPath = (directory: string): string =>
  join(directory, `.staged-${randomUUID()}.tmp`);

export const syncDirectory = async (directory: string): Promise<void> => {
  let handle: FileHandle | undefined;
  try {
    handle = await open(directory, "r");
    await handle.sync();
  } finally {
    await handle?.close();
  }
};

/**
 * Writes and fsyncs `bytes` at `stagingPath`, then links it to `path`. `link`
 * fails with EEXIST when `path` exists, exactly like an exclusive create, so
 * a losing writer never replaces a revision. The staging path must be on the
 * same filesystem as `path`. The caller fsyncs the directory holding `path`.
 * A crash leaves at most a staging file and never a partial `path`.
 */
export const publishExclusiveFile = async (input: {
  readonly stagingPath: string;
  readonly path: string;
  readonly bytes: Uint8Array | string;
}): Promise<void> => {
  const handle = await open(input.stagingPath, "wx", 0o600);
  try {
    try {
      await handle.writeFile(input.bytes);
      await handle.sync();
    } finally {
      await handle.close();
    }
    await link(input.stagingPath, input.path);
  } finally {
    await unlink(input.stagingPath);
  }
};

/**
 * Removes staging files a crashed writer left in `directory`. Call it only
 * while opening the store, before this process publishes anything.
 */
export const removeStagedRecordFiles = async (
  directory: string,
): Promise<void> => {
  const staged = (await readdir(directory, { withFileTypes: true })).filter(
    (entry) => entry.isFile() && STAGED_RECORD_FILE.test(entry.name),
  );
  for (const entry of staged) await unlink(join(directory, entry.name));
  if (staged.length !== 0) await syncDirectory(directory);
};

/**
 * True for the bytes an interrupted exclusive create of a JSON record can
 * leave under its final name: nothing, or a prefix (or zero fill) that is not
 * UTF-8 JSON. A complete record always parses, so a parseable record is never
 * torn; its authentication and chain checks still apply.
 */
export const isTornJsonRecord = (bytes: Uint8Array): boolean => {
  if (bytes.byteLength === 0) return true;
  try {
    JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
    return false;
  } catch {
    return true;
  }
};
