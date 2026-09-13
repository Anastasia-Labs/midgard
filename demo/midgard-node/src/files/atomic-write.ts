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

const tempPathFor = (path: string): string =>
  `${path}.tmp-${process.pid.toString()}-${Date.now().toString()}-${Math.random()
    .toString(16)
    .slice(2)}`;

export const writeTextFileAtomic = async (
  path: string,
  contents: string,
  options: AtomicWriteOptions = {},
): Promise<void> => {
  await mkdir(dirname(path), { recursive: true });
  const parentPath = dirname(path);
  const tempPath = tempPathFor(path);
  let tempHandle: FileHandle | undefined;
  try {
    tempHandle = await open(tempPath, "wx", options.mode ?? 0o666);
    await tempHandle.writeFile(contents, { encoding: "utf8" });
    if (options.mode !== undefined) {
      await chmod(tempPath, options.mode);
    }
    await tempHandle.sync();
    await tempHandle.close();
    tempHandle = undefined;

    await rename(tempPath, path);
    const parentHandle = await open(parentPath, "r");
    try {
      await parentHandle.sync();
    } finally {
      await parentHandle.close();
    }
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
  contents: string,
  options: AtomicWriteOptions = {},
): Promise<void> => {
  await mkdir(dirname(path), { recursive: true });
  const parentPath = dirname(path);
  const tempPath = tempPathFor(path);
  let tempHandle: FileHandle | undefined;
  try {
    tempHandle = await open(tempPath, "wx", options.mode ?? 0o666);
    await tempHandle.writeFile(contents, { encoding: "utf8" });
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
    const parentHandle = await open(parentPath, "r");
    try {
      await parentHandle.sync();
    } finally {
      await parentHandle.close();
    }
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
