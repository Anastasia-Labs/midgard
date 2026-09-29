import { createReadStream } from "node:fs";
import { appendFile } from "node:fs/promises";
import { default as readline } from "node:readline";

import {
  type OpenLoopCorpusRow,
  type OpenLoopSubmitRecord,
  parseOpenLoopCorpusLine,
} from "../stress-open-loop.js";

export const appendEvent = async (
  eventsNdjsonPath: string,
  event: Readonly<Record<string, unknown>>,
): Promise<void> => {
  await appendFile(eventsNdjsonPath, `${JSON.stringify(event)}\n`, "utf8");
};

export async function* readNdjsonLines(path: string): AsyncGenerator<string> {
  const reader = readline.createInterface({
    input: createReadStream(path, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  for await (const line of reader) {
    const trimmed = line.trim();
    if (trimmed.length > 0) {
      yield trimmed;
    }
  }
}

export const readJsonFile = async (path: string): Promise<unknown> => {
  const chunks: string[] = [];
  for await (const chunk of createReadStream(path, { encoding: "utf8" })) {
    chunks.push(chunk);
  }
  return JSON.parse(chunks.join("")) as unknown;
};

export const readSubmitRecords = async (
  path: string,
): Promise<readonly OpenLoopSubmitRecord[]> => {
  const records: OpenLoopSubmitRecord[] = [];
  let index = 0;
  for await (const line of readNdjsonLines(path)) {
    index += 1;
    const parsed = JSON.parse(line) as Partial<OpenLoopSubmitRecord>;
    if (
      typeof parsed.txHash !== "string" ||
      typeof parsed.scheduledAtMs !== "number" ||
      typeof parsed.submittedAtMs !== "number" ||
      typeof parsed.scheduleSlipMs !== "number" ||
      typeof parsed.latencyMs !== "number" ||
      !("statusCode" in parsed) ||
      !("error" in parsed)
    ) {
      throw new Error(`submit-records row ${index.toString()} is invalid`);
    }
    records.push({
      txHash: parsed.txHash.toLowerCase(),
      scheduledAtMs: parsed.scheduledAtMs,
      submittedAtMs: parsed.submittedAtMs,
      scheduleSlipMs: parsed.scheduleSlipMs,
      latencyMs: parsed.latencyMs,
      statusCode:
        typeof parsed.statusCode === "number" ? parsed.statusCode : null,
      responseTxId:
        typeof parsed.responseTxId === "string" ? parsed.responseTxId : null,
      error: typeof parsed.error === "string" ? parsed.error : null,
    });
  }
  return records;
};

export const readCorpusRowsForRecords = async ({
  corpusPath,
  records,
}: {
  readonly corpusPath: string;
  readonly records: readonly OpenLoopSubmitRecord[];
}): Promise<ReadonlyMap<string, OpenLoopCorpusRow>> => {
  const needed = new Set(records.map((record) => record.txHash.toLowerCase()));
  const found = new Map<string, OpenLoopCorpusRow>();
  if (needed.size === 0) {
    return found;
  }
  let index = 0;
  for await (const line of readNdjsonLines(corpusPath)) {
    index += 1;
    const row = parseOpenLoopCorpusLine(line, index);
    if (needed.has(row.txHash)) {
      found.set(row.txHash, row);
      if (found.size === needed.size) {
        break;
      }
    }
  }
  const missing = [...needed].filter((txHash) => !found.has(txHash));
  if (missing.length > 0) {
    throw new Error(
      `corpus lookup missed ${missing.length.toString()} submitted tx hashes`,
    );
  }
  return found;
};
