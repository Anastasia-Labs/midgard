import { readFile } from "node:fs/promises";

import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";

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

const serialize = (value: unknown) =>
  `${JSON.stringify(value, (_, item) => (typeof item === "bigint" ? item.toString() : item), 2)}\n`;

/** Private, atomic and fsynced (file, parent and any directory it created). */
export async function writeDurableJson(path: string, value: unknown) {
  await writeTextFileAtomic(path, serialize(value), { mode: 0o600 });
}

/** Like writeDurableJson, but fails with EEXIST instead of replacing a file. */
export async function createDurableJson(path: string, value: unknown) {
  await writeTextFileAtomicNoReplace(path, serialize(value), { mode: 0o600 });
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
