#!/usr/bin/env node
/**
 * Asserts that structural facts stated in the docs still match the source they
 * describe: no documented symbol has disappeared, no source symbol is
 * undocumented, and the counts written in prose still add up.
 *
 * This checks symbols and counts, not meaning. A page can still be misleading
 * while passing. It exists because every stale claim found in the copy audit
 * was a symbol or a count that drifted with no way to fail.
 */
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const read = (path) => readFileSync(resolve(repoRoot, path), "utf8");

const NUMBER_WORDS = [
  "zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
  "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
  "sixteen", "seventeen", "eighteen", "nineteen", "twenty",
];

const failures = [];
const fail = (message) => failures.push(message);

/** Every symbol the source exports must be named somewhere in the page. */
const requireDocumented = (docPath, symbols, sourcePath, label) => {
  const doc = read(docPath);
  for (const symbol of symbols) {
    if (!doc.includes(symbol)) {
      fail(
        `${docPath}: ${label} \`${symbol}\` exists in ${sourcePath} but is not documented.`,
      );
    }
  }
};

/** A count written in prose must match the count derived from source. */
const requireCount = (docPath, count, phrase, sourcePath) => {
  const doc = read(docPath);
  const expected = phrase.replace("{n}", NUMBER_WORDS[count] ?? String(count));
  if (!doc.includes(expected)) {
    fail(
      `${docPath}: expected the phrase "${expected}" (${sourcePath} yields ${count}).`,
    );
  }
};

/** Pulls every double-quoted string out of a source fragment. */
const quoted = (fragment) =>
  [...fragment.matchAll(/"([^"]+)"/g)].map((match) => match[1]);

// --- Node background fibers ------------------------------------------------
const FIBERS_SOURCE = "demo/midgard-node/src/fibers/index.ts";
const FIBERS_DOC = "docs-site/content/docs/operators/node/background-fibers.mdx";

const fibers = [...read(FIBERS_SOURCE).matchAll(/export \* from "\.\/([\w-]+)\.js";/g)]
  .map((match) => match[1]);

if (fibers.length === 0) fail(`${FIBERS_SOURCE}: no fiber exports found.`);
requireDocumented(FIBERS_DOC, fibers, FIBERS_SOURCE, "fiber");
requireCount(FIBERS_DOC, fibers.length, "The {n} long-running fibers", FIBERS_SOURCE);

// --- Fault-proof CLI commands ----------------------------------------------
// bin.ts rejects an unknown command by listing every valid one. That error
// string is the command allow-list.
const CLI_SOURCE = "demo/midgard-fault-proofs/src/bin.ts";
const CLI_DOC = "docs-site/content/docs/fault-proofs/overview.mdx";

const usageError = read(CLI_SOURCE).match(/Expected command ([^\n]+?)\.\\n\$\{usage\}/);
if (!usageError) {
  fail(`${CLI_SOURCE}: could not find the "Expected command ..." allow-list.`);
} else {
  const commands = quoted(usageError[1]);
  requireDocumented(CLI_DOC, commands, CLI_SOURCE, "command");
  requireCount(CLI_DOC, commands.length, "The {n} commands", CLI_SOURCE);
}

// --- lucid-midgard transaction statuses ------------------------------------
const STATUS_SOURCE = "demo/lucid-midgard/src/builder/status.ts";
const STATUS_DOC = "docs-site/content/docs/sdk/lucid-midgard/submission-observability.mdx";

const statusSet = read(STATUS_SOURCE).match(
  /const TX_STATUS_KINDS: ReadonlySet<string> = new Set\(\[([\s\S]*?)\]\)/,
);
if (!statusSet) {
  fail(`${STATUS_SOURCE}: could not find the TX_STATUS_KINDS set.`);
} else {
  const statuses = quoted(statusSet[1]);
  requireDocumented(STATUS_DOC, statuses, STATUS_SOURCE, "status");
  requireCount(STATUS_DOC, statuses.length, "`TxStatus` has {n} kinds", STATUS_SOURCE);
}

// --- Report ----------------------------------------------------------------
if (failures.length > 0) {
  console.error("Documentation has drifted from source:\n");
  for (const failure of failures) console.error(`  - ${failure}`);
  console.error(
    "\nUpdate the page, or the source, so the two agree. See docs-site/README.md.",
  );
  process.exit(1);
}

console.log(
  `Docs facts check passed: ${fibers.length} fibers, fault-proof commands, and transaction statuses all documented.`,
);
