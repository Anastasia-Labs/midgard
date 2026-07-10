import { chmod, mkdir, rename, rm, writeFile } from "node:fs/promises";
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
  const tempPath = tempPathFor(path);
  try {
    await writeFile(tempPath, contents, {
      encoding: "utf8",
      ...(options.mode === undefined ? {} : { mode: options.mode }),
    });
    if (options.mode !== undefined) {
      await chmod(tempPath, options.mode);
    }
    await rename(tempPath, path);
    if (options.mode !== undefined) {
      await chmod(path, options.mode);
    }
  } catch (error) {
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
