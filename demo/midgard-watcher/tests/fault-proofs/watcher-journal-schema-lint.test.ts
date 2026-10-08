import { readFileSync } from "node:fs";

import {
  declaredTables,
  lintDeterminism,
  lintSchema,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

import {
  WATCHER_JOURNAL_MIGRATIONS,
  WATCHER_JOURNAL_TABLES,
} from "../../src/fault-proofs/watcher-journal-schema.js";

/** The journal modules: every commit's rows, MACs and digests come from these
 * alone, so a restart recomputes exactly what was written. */
const JOURNAL_MODULES = [
  "src/fault-proofs/watcher-journal-schema.ts",
  "src/fault-proofs/watcher-journal-database.ts",
  "src/fault-proofs/watcher-journal-database.codec.ts",
  "src/fault-proofs/watcher-journal-database.verify.ts",
  "src/fault-proofs/watcher-journal-database.types.ts",
  "src/fault-proofs/fault-proof-objective-table.ts",
  "src/fault-proofs/fault-proof-queue-journal.ts",
];

const read = (path: string) => ({
  path,
  source: readFileSync(new URL(`../../${path}`, import.meta.url), "utf8"),
});

describe("watcher journal schema lints", () => {
  it("declares a class and retention for every journal table", () => {
    expect(lintSchema([WATCHER_JOURNAL_MIGRATIONS])).toEqual([]);
    expect(
      declaredTables([WATCHER_JOURNAL_MIGRATIONS]).map((table) => [
        table.table,
        table.tableClass,
      ]),
    ).toEqual([
      ["watcher_journal_migrations", "A"],
      ["watcher_journal_heads", "B"],
      ["watcher_journal_revisions", "B"],
      ...Object.values(WATCHER_JOURNAL_TABLES).map((table) => [table, "B"]),
    ]);
  });

  it("refuses a journal table without its header", () => {
    const [ledger, tables] = WATCHER_JOURNAL_MIGRATIONS.migrations;
    expect(
      lintSchema([
        {
          ...WATCHER_JOURNAL_MIGRATIONS,
          migrations: [
            ledger!,
            {
              ...tables!,
              sql: tables!.sql.replace(
                /-- class: B; retention: one row per proof objective[^\n]*\n/u,
                "",
              ),
            },
          ],
        },
      ]).map(({ table }) => table),
    ).toEqual(["watcher_fault_proof_objectives"]);
  });

  it("keeps the journal modules free of clocks, randomness and the network", () => {
    const files = JOURNAL_MODULES.map(read);
    expect(lintDeterminism(files)).toEqual([]);
    const [queue] = files.filter((file) =>
      file.path.endsWith("fault-proof-queue-journal.ts"),
    );
    expect(
      lintDeterminism([
        {
          ...queue!,
          source: `${queue!.source}\nexport const t = Date.now();\n`,
        },
      ]).map((problem) => problem.rule),
    ).toEqual(["clock"]);
  });
});
