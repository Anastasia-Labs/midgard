// The node's event-history control plane is deleted (plan §13.1, N1-close):
// the history owner, its runtime, producer and recovery, the authority, the
// journal, the ledger receipts and recovery plans, the Ogmios ChainSync
// source (`l1-event-history-*`), the ledger snapshot, the
// `history-genesis-pin` command and its pin, the node's Kupo and Ogmios
// readers (`l1-kupmios.*`) and the two endpoint keys. The follower and the
// follower-change driver took every duty they had.
//
// Two gates read tracked files for a name of the deleted code: the node's
// `tests/l1-control-plane-deleted.test.ts` over the node and node-tools
// sources, and `scripts/ci/l1-control-plane-deleted-docs.test.mjs` over every
// Markdown document. Two kinds of text may still name them:
// - the applied migrations, which created the tables, and the forward
//   migration that drops them (`src/database/migrations/sql/`);
// - a Markdown block (paragraph or list item) carrying the
//   `<!-- doc-links:historical -->` marker, and the dated plan, decision and
//   implementation-log records listed in HISTORICAL_RECORDS.

import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { join } from "node:path";

export const CODE_ROOTS = ["demo/midgard-node/", "demo/midgard-node-tools/"];
export const DOC = /\.mdx?$/u;
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

export const HISTORICAL_MARKER = "doc-links:historical";

export const DELETED = [
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
    name: "the deleted (event-)history owner",
    pattern: /\bhistory[- ]owners?\b/iu,
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

/** Every tracked or untracked-but-not-ignored path under `root`. */
export const trackedFiles = (root) =>
  execFileSync(
    "git",
    ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
    { cwd: root, encoding: "utf8" },
  )
    .split("\0")
    .filter((path) => path.length > 0 && !BINARY.test(path));

/** A path whose text may still name the deleted code. */
export const exempt = (path) =>
  path.startsWith(MIGRATIONS) ||
  HISTORICAL_RECORDS.some((record) =>
    record.endsWith("/") ? path.startsWith(record) : path === record,
  );

/** Each line with its number, leaving out every blank-line-delimited Markdown
 * block that carries the historical marker. */
export const linesToCheck = (path, text) => {
  const lines = text.split(/\r?\n/u).map((line, index) => ({
    line,
    number: index + 1,
  }));
  if (!DOC.test(path)) return lines;
  const kept = [];
  let block = [];
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

/** Whether `text` at `path` names deleted code outside a historical block. */
export const namesDeleted = (path, text) =>
  linesToCheck(path, text).some(({ line }) =>
    DELETED.some(({ pattern }) => pattern.test(line)),
  );

/** Every line of `files` (relative to `root`) that names a deleted module,
 * table or key, as `path:line: what`. */
export const deletedNameReaders = (root, files) =>
  files
    .filter((path) => !exempt(path))
    .flatMap((path) => {
      let text;
      try {
        text = readFileSync(join(root, path), "utf8");
      } catch {
        return [];
      }
      return linesToCheck(path, text).flatMap(({ line, number }) =>
        DELETED.filter(({ pattern }) => pattern.test(line)).map(
          ({ name }) => `${path}:${number}: ${name}`,
        ),
      );
    });
