import fs from "node:fs";
import path from "node:path";

import { describe, expect, it } from "vitest";

/**
 * No production code writes submitted_unconfirmed. The signed-intent release
 * replaces an active journal in any unlanded status, submitted_unconfirmed
 * included, without the fatal guard that once refused one (see
 * `ACTIVE_STATUSES` in history-expired-intent-release.table.ts): only a row
 * an earlier version persisted reads it. A new writer must re-establish that
 * such a journal is safe to replace, so this pins the status's one writer,
 * `markLocalFinalizationComplete`, and that nothing in src calls it.
 */

const SRC = path.resolve(__dirname, "../src");

const sources = (dir: string): string[] =>
  fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) return sources(full);
    return entry.name.endsWith(".ts") ? [full] : [];
  });

/** A status column set to submitted_unconfirmed: in SQL by the Status
 * constant or the literal, or in an object written as a row. */
const WRITES = [
  /\bSTATUS\)?\}?\s*=\s*\$\{\s*(?:\w+\.)*Status\.SubmittedUnconfirmed\s*\}/u,
  /\bstatus\s*=\s*'submitted_unconfirmed'/iu,
  /\[(?:\w+\.)*STATUS\]\s*:\s*(?:\w+\.)*Status\.SubmittedUnconfirmed\b/u,
  /\bstatus\s*:\s*(?:\w+\.)*Status\.SubmittedUnconfirmed\b/u,
  /\bstatus\s*:\s*["']submitted_unconfirmed["']/u,
];

/** Lines matching `pattern`, other than comment lines. */
const occurrences = (pattern: (line: string) => boolean) =>
  sources(SRC).flatMap((file) =>
    fs
      .readFileSync(file, "utf8")
      .split("\n")
      .flatMap((line, index) =>
        pattern(line) && !/^\s*(?:\/\/|\/\*|\*)/u.test(line)
          ? [`${path.relative(SRC, file)}:${(index + 1).toString()}`]
          : [],
      ),
  );

const WRITER_FILE =
  "database/pendingBlockFinalizations.assert-canonical-event-members.ts";

describe("submitted_unconfirmed", () => {
  it("has one writer in src, markLocalFinalizationComplete", () => {
    const writes = occurrences((line) =>
      WRITES.some((write) => write.test(line)),
    );
    // The scan finds the known writer, so the patterns are not vacuous.
    expect(writes).toHaveLength(1);
    expect(writes[0]).toMatch(
      new RegExp(`^${WRITER_FILE.replace(/\./gu, "\\.")}:`, "u"),
    );
    const writer = fs.readFileSync(path.join(SRC, WRITER_FILE), "utf8");
    const line = Number(writes[0]!.split(":")[1]);
    const before = writer.split("\n").slice(0, line).join("\n");
    expect(before.lastIndexOf("export const ")).toBe(
      before.lastIndexOf("export const markLocalFinalizationComplete"),
    );
  });

  it("is never written: markLocalFinalizationComplete has no caller in src", () => {
    const uses = occurrences((line) =>
      /\bmarkLocalFinalizationComplete\b/u.test(line),
    );
    // Its definition, its log span, and the table module's re-export.
    expect(uses.sort()).toEqual(
      [
        `${WRITER_FILE}:${definitionLine("export const markLocalFinalizationComplete")}`,
        `${WRITER_FILE}:${definitionLine("withLogSpan(`markLocalFinalizationComplete")}`,
        `database/pendingBlockFinalizations.ts:${exportLine()}`,
      ].sort(),
    );
  });
});

const lineOf = (file: string, text: string) =>
  (
    fs
      .readFileSync(path.join(SRC, file), "utf8")
      .split("\n")
      .findIndex((line) => line.includes(text)) + 1
  ).toString();

const definitionLine = (text: string) => lineOf(WRITER_FILE, text);

const exportLine = () =>
  lineOf(
    "database/pendingBlockFinalizations.ts",
    "markLocalFinalizationComplete",
  );
