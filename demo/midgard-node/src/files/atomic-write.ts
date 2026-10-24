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
};

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
const syncCreatedAncestors = async (
  firstCreated: string | undefined,
  parentPath: string,
): Promise<void> => {
  if (firstCreated === undefined) return;
  for (let directory = parentPath; ; directory = dirname(directory)) {
    await syncDirectory(dirname(directory));
    if (directory === firstCreated || dirname(directory) === directory) return;
  }
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
  const firstCreated = await mkdir(dirname(path), { recursive: true });
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
  const firstCreated = await mkdir(dirname(path), { recursive: true });
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

export const writeJsonFileAtomic = async (
  path: string,
  value: unknown,
  options: AtomicWriteOptions = {},
): Promise<void> =>
  writeTextFileAtomic(path, `${JSON.stringify(value, null, 2)}\n`, options);
