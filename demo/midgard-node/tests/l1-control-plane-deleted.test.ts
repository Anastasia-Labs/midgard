/**
 * The node's event-history control plane is deleted (plan §13.1, N1-close):
 * the history owner, its runtime, producer and recovery, the authority, the
 * journal, the ledger receipts and recovery plans, the Ogmios ChainSync
 * source (`l1-event-history-*`), the ledger snapshot, the
 * `history-genesis-pin` command and its pin, the node's Kupo and Ogmios
 * readers (`l1-kupmios.*`) and the two endpoint keys. The follower and the
 * follower-change driver took every duty they had.
 *
 * This gate reads every tracked file of the node and node-tools packages and
 * every tracked Markdown document, and finds none that names a deleted
 * module, table or key. Two kinds of text may still name them:
 * - the applied migrations, which created the tables, and the forward
 *   migration that drops them (`src/database/migrations/sql/`);
 * - a Markdown block (paragraph or list item) carrying the
 *   `<!-- doc-links:historical -->` marker, and the dated plan, decision and
 *   implementation-log records listed in HISTORICAL_RECORDS.
 */
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

const REPO_ROOT = fileURLToPath(new URL("../../..", import.meta.url));
const THIS_FILE = relative(REPO_ROOT, fileURLToPath(import.meta.url));

const CODE_ROOTS = ["demo/midgard-node/", "demo/midgard-node-tools/"];
const DOC = /\.mdx?$/u;
const BINARY = /\.(?:png|jpe?g|gif|ico|cbor|bin|wasm|gz|zip|sqlite|db)$/iu;

/** Paths whose text may name the deleted tables: the applied migrations that
 * created them and the forward migration that drops them. */
const MIGRATIONS = "demo/midgard-node/src/database/migrations/sql/";

/** Dated records that describe the deleted code as it was. */
const HISTORICAL_RECORDS = [
  // Plans and decision records, each dated; the L1 plan's §13 lists the
  // deleted rows by name.
  "docs/exec-plans/",
  // Implementation logs of the event-history owner while it was built.
  "docs/fault-proofs/event-history-progress.md",
  "docs/fault-proofs/event-history-delivery-checkpoint.md",
];

const HISTORICAL_MARKER = "doc-links:historical";

const DELETED: readonly Readonly<{ name: string; pattern: RegExp }>[] = [
  {
    name: "a deleted Ogmios ChainSync history module",
    pattern:
      /\bl1-event-history-|\bl1-ledger-snapshot\b|l1-follower\.recovery/u,
  },
  {
    name: "a deleted event-history owner module",
    pattern: /\bevent-history-(?:owner|runtime|producer|recovery)\b/u,
  },
  {
    name: "a deleted event-history store module",
    pattern:
      /\beventHistory(?:Journal|LedgerReceipts|LedgerRepair|ReplayReceipts|RecoveryPlans|Authority|Materialization)/u,
  },
  {
    name: "a deleted event-history table",
    pattern:
      /\bevent_history_(?:authority|replay_receipts|cursor|block_applications|live_outputs|incarnations|l2_ledger_receipt|recovery_plans)/u,
  },
  {
    name: "the deleted history-genesis-pin command or its pin",
    pattern: /\bhistory-genesis-pin\b|\bL1_HISTORY_GENESIS_LOSSLESS_SHA256\b/u,
  },
  {
    name: "the deleted EVENT_HISTORY_OWNER Ref",
    pattern: /\bEVENT_HISTORY_OWNER\b/u,
  },
  {
    name: "the node's deleted Kupo/Ogmios reader or key",
    pattern: /\bl1-kupmios\b|\bL1_(?:OGMIOS|KUPO)_KEY\b/u,
  },
];

const trackedFiles = (): string[] =>
  execFileSync(
    "git",
    ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
    { cwd: REPO_ROOT, encoding: "utf8" },
  )
    .split("\0")
    .filter((path) => path.length > 0);

const scanned = (path: string): boolean =>
  path !== THIS_FILE &&
  !BINARY.test(path) &&
  (CODE_ROOTS.some((root) => path.startsWith(root)) || DOC.test(path));

const exempt = (path: string): boolean =>
  path.startsWith(MIGRATIONS) ||
  HISTORICAL_RECORDS.some((record) =>
    record.endsWith("/") ? path.startsWith(record) : path === record,
  );

/** Each line with its number, leaving out every blank-line-delimited Markdown
 * block that carries the historical marker. */
const linesToCheck = (path: string, text: string) => {
  const lines = text.split(/\r?\n/u).map((line, index) => ({
    line,
    number: index + 1,
  }));
  if (!DOC.test(path)) return lines;
  const kept: typeof lines = [];
  let block: typeof lines = [];
  const flush = () => {
    if (!block.some(({ line }) => line.includes(HISTORICAL_MARKER)))
      kept.push(...block);
    block = [];
  };
  for (const entry of lines) {
    if (entry.line.trim() === "") flush();
    else block.push(entry);
  }
  flush();
  return kept;
};

/** Every line that names a deleted module, table or key, as `path:line: what`. */
const deletedNameReaders = (files: readonly string[]): string[] =>
  files
    .filter((path) => scanned(path) && !exempt(path))
    .flatMap((path) => {
      let text: string;
      try {
        text = readFileSync(join(REPO_ROOT, path), "utf8");
      } catch {
        return [];
      }
      return linesToCheck(path, text).flatMap(({ line, number }) =>
        DELETED.filter(({ pattern }) => pattern.test(line)).map(
          ({ name }) => `${path}:${number}: ${name}`,
        ),
      );
    });

describe("the deleted event-history control plane", () => {
  const files = trackedFiles();

  it("has no reader left in the node, node-tools or the docs", () => {
    expect(deletedNameReaders(files)).toEqual([]);
  });

  it("scans the files it guards", () => {
    const checked = files.filter((path) => scanned(path) && !exempt(path));
    for (const path of [
      "demo/midgard-node/src/index.ts",
      "demo/midgard-node/.env.example",
      "demo/midgard-node/docker-compose.kupmios.yaml",
      "demo/midgard-node/README.md",
      "demo/midgard-node-tools/src/index.ts",
      "docs-site/content/docs/getting-started/l1-backend.mdx",
      ".agents/skills/midgard-e2e-acceptance/references/release-readiness.md",
    ])
      expect(checked).toContain(path);
  });

  it("refuses a deleted name and skips a historical-marked block", () => {
    const source = "names `L1_KUPO_KEY`";
    const finds = (path: string, text: string) =>
      linesToCheck(path, text).some(({ line }) =>
        DELETED.some(({ pattern }) => pattern.test(line)),
      );
    expect(finds("docs/a.md", source)).toBe(true);
    expect(finds("docs/a.md", `${source}\n<!-- ${HISTORICAL_MARKER} -->`)).toBe(
      false,
    );
    expect(finds("src/a.ts", `${source} // ${HISTORICAL_MARKER}`)).toBe(true);
  });
});
