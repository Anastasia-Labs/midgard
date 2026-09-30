import { randomUUID } from "node:crypto";
import { mkdir, open, readFile, rename } from "node:fs/promises";
import { dirname } from "node:path";

export type StepRecord = {
  status: "running" | "complete";
  attempts: number;
  data: unknown;
};
export type StackJournal = {
  schemaVersion: "midgard-full-stack-v1";
  runId: string;
  intentDigest: string;
  steps: Record<string, StepRecord>;
};

/** Rename plus fsync prevents a crash from publishing a partial checkpoint. */
export async function writeDurableJson(path: string, value: unknown) {
  return writeDurableBytes(
    path,
    Buffer.from(
      `${JSON.stringify(value, (_, item) => (typeof item === "bigint" ? item.toString() : item), 2)}\n`,
    ),
  );
}

export async function writeDurableBytes(path: string, value: Uint8Array) {
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.${randomUUID()}.tmp`;
  const file = await open(temporary, "wx", 0o600);
  try {
    await file.writeFile(value);
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
}

export async function readJsonIfPresent(
  path: string,
): Promise<unknown | undefined> {
  try {
    return JSON.parse(await readFile(path, "utf8"));
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw error;
  }
}

export function parseJournal(
  value: unknown,
  intentDigest: string,
): StackJournal {
  const journal = value as StackJournal;
  if (
    !journal ||
    journal.schemaVersion !== "midgard-full-stack-v1" ||
    journal.intentDigest !== intentDigest ||
    typeof journal.runId !== "string" ||
    !journal.runId ||
    !journal.steps ||
    typeof journal.steps !== "object" ||
    Array.isArray(journal.steps)
  ) {
    throw new Error(
      "Saved stack identity differs from this configuration; preserve the run directory",
    );
  }
  for (const record of Object.values(journal.steps)) {
    if (
      !record ||
      typeof record !== "object" ||
      Array.isArray(record) ||
      !["running", "complete"].includes(record.status) ||
      !Number.isSafeInteger(record.attempts) ||
      record.attempts < 1 ||
      !("data" in record)
    )
      throw new Error("Malformed stack checkpoint");
  }
  return journal;
}
