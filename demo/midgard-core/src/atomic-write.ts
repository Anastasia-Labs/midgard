import {
  chmodSync,
  closeSync,
  fsyncSync,
  mkdirSync,
  openSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import {
  chmod,
  type FileHandle,
  link,
  mkdir,
  open,
  rename,
  rm,
  unlink,
} from "node:fs/promises";
import { dirname } from "node:path";

export type AtomicWriteOptions = {
  readonly mode?: number;
  /** Mode of the directories the write creates (before the umask). */
  readonly directoryMode?: number;
};

const directoryOptions = (options: AtomicWriteOptions) =>
  options.directoryMode === undefined
    ? { recursive: true as const }
    : { recursive: true as const, mode: options.directoryMode };

const syncDirectory = async (path: string): Promise<void> => {
  const handle = await open(path, "r");
  try {
    await handle.sync();
  } finally {
    await handle.close();
  }
};

/**
 * A directory `mkdir` just created is itself a new entry in its parent. Sync
 * each of those parents too, or a crash can drop the new directory (and the
 * file published inside it) even though the file and its parent were synced.
 */
const createdDirectoryParents = (
  firstCreated: string | undefined,
  parentPath: string,
): string[] => {
  const parents: string[] = [];
  if (firstCreated === undefined) return parents;
  for (let directory = parentPath; ; directory = dirname(directory)) {
    parents.push(dirname(directory));
    if (directory === firstCreated || dirname(directory) === directory)
      return parents;
  }
};

const syncCreatedAncestors = async (
  firstCreated: string | undefined,
  parentPath: string,
): Promise<void> => {
  for (const parent of createdDirectoryParents(firstCreated, parentPath))
    await syncDirectory(parent);
};

const tempPathFor = (path: string): string =>
  `${path}.tmp-${process.pid.toString()}-${Date.now().toString()}-${Math.random()
    .toString(16)
    .slice(2)}`;

export const writeTextFileAtomic = async (
  path: string,
  contents: string | Uint8Array,
  options: AtomicWriteOptions = {},
): Promise<void> => {
  const firstCreated = await mkdir(dirname(path), directoryOptions(options));
  const parentPath = dirname(path);
  const tempPath = tempPathFor(path);
  let tempHandle: FileHandle | undefined;
  try {
    tempHandle = await open(tempPath, "wx", options.mode ?? 0o666);
    await tempHandle.writeFile(contents);
    if (options.mode !== undefined) {
      await chmod(tempPath, options.mode);
    }
    await tempHandle.sync();
    await tempHandle.close();
    tempHandle = undefined;

    await rename(tempPath, path);
    await syncDirectory(parentPath);
    await syncCreatedAncestors(firstCreated, parentPath);
  } catch (error) {
    if (tempHandle !== undefined) {
      await tempHandle.close().catch(() => {});
    }
    await rm(tempPath, { force: true }).catch(() => {});
    throw error;
  }
};

export const writeTextFileAtomicNoReplace = async (
  path: string,
  contents: string | Uint8Array,
  options: AtomicWriteOptions = {},
): Promise<void> => {
  const firstCreated = await mkdir(dirname(path), directoryOptions(options));
  const parentPath = dirname(path);
  const tempPath = tempPathFor(path);
  let tempHandle: FileHandle | undefined;
  try {
    tempHandle = await open(tempPath, "wx", options.mode ?? 0o666);
    await tempHandle.writeFile(contents);
    if (options.mode !== undefined) {
      await chmod(tempPath, options.mode);
    }
    await tempHandle.sync();
    await tempHandle.close();
    tempHandle = undefined;

    // Hard-link publication is atomic and fails with EEXIST rather than
    // replacing immutable evidence created by another writer.
    await link(tempPath, path);
    await unlink(tempPath);
    await syncDirectory(parentPath);
    await syncCreatedAncestors(firstCreated, parentPath);
  } catch (error) {
    if (tempHandle !== undefined) {
      await tempHandle.close().catch(() => {});
    }
    await rm(tempPath, { force: true }).catch(() => {});
    throw error;
  }
};

/** Cleanup after a failed write; the write's own failure is the one thrown. */
const ignoreCleanupFailure = (cleanup: () => void): void => {
  try {
    cleanup();
  } catch {
    // Reported through the write's failure instead.
  }
};

const syncDirectorySync = (path: string): void => {
  const fd = openSync(path, "r");
  try {
    fsyncSync(fd);
  } finally {
    closeSync(fd);
  }
};

/**
 * `writeTextFileAtomic` for callers that must not yield, such as one holding a
 * synchronous lock: the same temp file, file sync, rename and directory syncs.
 */
export const writeTextFileAtomicSync = (
  path: string,
  contents: string | Uint8Array,
  options: AtomicWriteOptions = {},
): void => {
  const parentPath = dirname(path);
  const firstCreated = mkdirSync(parentPath, directoryOptions(options));
  const tempPath = tempPathFor(path);
  let tempFd: number | undefined;
  try {
    tempFd = openSync(tempPath, "wx", options.mode ?? 0o666);
    writeFileSync(tempFd, contents);
    if (options.mode !== undefined) {
      chmodSync(tempPath, options.mode);
    }
    fsyncSync(tempFd);
    closeSync(tempFd);
    tempFd = undefined;

    renameSync(tempPath, path);
    syncDirectorySync(parentPath);
    for (const parent of createdDirectoryParents(firstCreated, parentPath))
      syncDirectorySync(parent);
  } catch (error) {
    const openFd = tempFd;
    if (openFd !== undefined) ignoreCleanupFailure(() => closeSync(openFd));
    ignoreCleanupFailure(() => rmSync(tempPath, { force: true }));
    throw error;
  }
};

export const writeJsonFileAtomic = async (
  path: string,
  value: unknown,
  options: AtomicWriteOptions = {},
): Promise<void> =>
  writeTextFileAtomic(path, `${JSON.stringify(value, null, 2)}\n`, options);
