import { mkdir, open, readFile, stat } from "node:fs/promises";
import { join } from "node:path";

import type { ShadowResult } from "./compare.js";
import type { DiffEntry } from "./diff.js";

/** One line of the soak journal (`<dir>/journal.jsonl`), append-only. */
export type JournalRecord =
  | Readonly<{
      type: "start";
      at: string;
      /** `role/name` of every comparator in this run. */
      comparators: readonly string[];
      /** Roles with no comparator plugged in yet. */
      rolesWithout: readonly string[];
      cursor: Readonly<{ slot: number; hash: string; height: number }>;
    }>
  | Readonly<{
      type: "block";
      at: string;
      /** `resume`: compared at the cursor after a restart, no event. */
      event: "roll_forward" | "roll_backward" | "resume";
      seq: string | null;
      slot: number;
      hash: string;
      height: number;
      generation: number;
      results: readonly ShadowResult[];
    }>
  | Readonly<{
      type: "stop";
      at: string;
      /** `intervention` stops the soak until an operator acts. */
      reason: "intervention" | "limit" | "signal" | "refused";
      detail: string;
    }>;

export type BlockRecord = Extract<JournalRecord, { type: "block" }>;

export const JOURNAL_FILE = "journal.jsonl";

const isRecord = (value: unknown): value is JournalRecord =>
  typeof value === "object" &&
  value !== null &&
  "type" in value &&
  (value.type === "start" || value.type === "block" || value.type === "stop");

/** Every record of a journal; a torn or foreign line is counted, not fatal. */
export const readJournal = async (
  dir: string,
): Promise<{ records: JournalRecord[]; corrupt: number }> => {
  let text: string;
  try {
    text = await readFile(join(dir, JOURNAL_FILE), "utf8");
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT")
      return { records: [], corrupt: 0 };
    throw error;
  }
  const records: JournalRecord[] = [];
  let corrupt = 0;
  for (const line of text.split("\n")) {
    if (line.trim() === "") continue;
    try {
      const parsed = JSON.parse(line) as unknown;
      if (isRecord(parsed)) records.push(parsed);
      else corrupt += 1;
    } catch {
      corrupt += 1;
    }
  }
  return { records, corrupt };
};

/** The append side: each record is one line, written and fsynced. */
export class Journal {
  private constructor(
    private readonly handle: Awaited<ReturnType<typeof open>>,
  ) {}

  static async open(dir: string): Promise<Journal> {
    await mkdir(dir, { recursive: true });
    const path = join(dir, JOURNAL_FILE);
    const handle = await open(path, "a");
    // A crash mid-append leaves a torn last line: end it so the next record
    // starts on its own line (the reader counts the torn one as corrupt).
    const size = (await stat(path)).size;
    if (size > 0) {
      const last = Buffer.alloc(1);
      const reader = await open(path, "r");
      try {
        await reader.read(last, 0, 1, size - 1);
      } finally {
        await reader.close();
      }
      if (last[0] !== 0x0a) await handle.write("\n");
    }
    return new Journal(handle);
  }

  async append(record: JournalRecord): Promise<void> {
    await this.handle.write(`${JSON.stringify(record)}\n`);
    await this.handle.sync();
  }

  async close(): Promise<void> {
    await this.handle.close();
  }
}

type Counts = {
  equal: number;
  differs: number;
  skipped: number;
  error: number;
};

export type SoakSummary = Readonly<{
  /** Comparisons after a roll-forward (a block re-applied after a rollback counts again). */
  blocks: number;
  rollbacks: number;
  resumes: number;
  starts: number;
  lastBlock: Readonly<{ slot: number; hash: string; height: number }> | null;
  comparators: Readonly<Record<string, Counts>>;
  rolesWithout: readonly string[];
  firstDiff: Readonly<{
    slot: number;
    height: number;
    comparator: string;
    diff: readonly DiffEntry[];
  }> | null;
  firstError: Readonly<{
    slot: number;
    height: number;
    comparator: string;
    error: string;
  }> | null;
  lastStop: Readonly<{ reason: string; detail: string; at: string }> | null;
  corruptLines: number;
}>;

/** The soak's report: block count, per-comparator outcomes, first diff. */
export const summarise = (
  records: readonly JournalRecord[],
  corruptLines = 0,
): SoakSummary => {
  const comparators: Record<string, Counts> = {};
  let blocks = 0;
  let rollbacks = 0;
  let resumes = 0;
  let starts = 0;
  let rolesWithout: readonly string[] = [];
  let lastBlock: SoakSummary["lastBlock"] = null;
  let firstDiff: SoakSummary["firstDiff"] = null;
  let firstError: SoakSummary["firstError"] = null;
  let lastStop: SoakSummary["lastStop"] = null;
  for (const record of records) {
    if (record.type === "start") {
      starts += 1;
      rolesWithout = record.rolesWithout;
      for (const name of record.comparators)
        comparators[name] ??= { equal: 0, differs: 0, skipped: 0, error: 0 };
      continue;
    }
    if (record.type === "stop") {
      lastStop = {
        reason: record.reason,
        detail: record.detail,
        at: record.at,
      };
      continue;
    }
    if (record.event === "roll_forward") blocks += 1;
    else if (record.event === "roll_backward") rollbacks += 1;
    else resumes += 1;
    lastBlock = { slot: record.slot, hash: record.hash, height: record.height };
    for (const result of record.results) {
      const key = `${result.role}/${result.name}`;
      const counts = (comparators[key] ??= {
        equal: 0,
        differs: 0,
        skipped: 0,
        error: 0,
      });
      counts[result.outcome] += 1;
      if (result.outcome === "differs" && firstDiff === null)
        firstDiff = {
          slot: record.slot,
          height: record.height,
          comparator: key,
          diff: result.diff,
        };
      if (result.outcome === "error" && firstError === null)
        firstError = {
          slot: record.slot,
          height: record.height,
          comparator: key,
          error: result.error,
        };
    }
  }
  return {
    blocks,
    rollbacks,
    resumes,
    starts,
    lastBlock,
    comparators,
    rolesWithout,
    firstDiff,
    firstError,
    lastStop,
    corruptLines,
  };
};

/** The report as text: block count, outcomes, first diff, last stop. */
export const formatSummary = (summary: SoakSummary): string => {
  const lines = [
    `blocks compared: ${summary.blocks} (rollbacks ${summary.rollbacks}, resumes ${summary.resumes}, starts ${summary.starts})`,
    `last block: ${summary.lastBlock === null ? "none" : `height ${summary.lastBlock.height} slot ${summary.lastBlock.slot}`}`,
    ...Object.entries(summary.comparators).map(
      ([name, c]) =>
        `${name}: equal ${c.equal}, differs ${c.differs}, skipped ${c.skipped}, error ${c.error}`,
    ),
    `roles without a comparator yet: ${summary.rolesWithout.length === 0 ? "none" : summary.rolesWithout.join(", ")}`,
    `first non-empty diff: ${summary.firstDiff === null ? "none" : `${summary.firstDiff.comparator} at height ${summary.firstDiff.height}: ${JSON.stringify(summary.firstDiff.diff.slice(0, 3))}`}`,
    `first comparator error: ${summary.firstError === null ? "none" : `${summary.firstError.comparator} at height ${summary.firstError.height}: ${summary.firstError.error}`}`,
    `last stop: ${summary.lastStop === null ? "none" : `${summary.lastStop.reason} (${summary.lastStop.detail}) at ${summary.lastStop.at}`}`,
  ];
  if (summary.corruptLines > 0)
    lines.push(`torn journal lines: ${summary.corruptLines}`);
  return lines.join("\n");
};
