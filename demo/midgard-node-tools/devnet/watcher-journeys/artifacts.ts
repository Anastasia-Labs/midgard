import { randomUUID } from "node:crypto";
import { open, readFile, rename, rm } from "node:fs/promises";
import { dirname } from "node:path";

export const writeJourneyFile = async (
  path: string,
  contents: string | Uint8Array,
) => {
  const temporary = `${path}.${randomUUID()}.tmp`;
  try {
    const file = await open(temporary, "wx", 0o600);
    try {
      await file.writeFile(contents);
      await file.sync();
    } finally {
      await file.close();
    }
    await rename(temporary, path);
    const directory = await open(dirname(path), "r");
    try {
      await directory.sync();
    } finally {
      await directory.close();
    }
  } catch (cause) {
    await rm(temporary, { force: true });
    throw cause;
  }
};

export const writeJourneyArtifact = async (path: string, value: unknown) =>
  writeJourneyFile(
    path,
    JSON.stringify(
      value,
      (_, item) => {
        if (typeof item === "bigint") return { bigint: item.toString() };
        if (item instanceof Uint8Array)
          return { bytes: Buffer.from(item).toString("hex") };
        if (item?.type === "Buffer" && Array.isArray(item.data))
          return { bytes: Buffer.from(item.data).toString("hex") };
        return item;
      },
      2,
    ),
  );

export const readJourneyArtifact = async <T>(path: string): Promise<T> =>
  JSON.parse(await readFile(path, "utf8"), (_, item) => {
    if (
      item !== null &&
      typeof item === "object" &&
      Object.keys(item).length === 1
    ) {
      if (typeof item.bigint === "string") return BigInt(item.bigint);
      if (typeof item.bytes === "string") return Buffer.from(item.bytes, "hex");
    }
    return item;
  }) as T;
