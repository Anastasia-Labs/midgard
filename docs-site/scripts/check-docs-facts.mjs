#!/usr/bin/env node
/**
 * Asserts that structural facts stated in the docs still match the source they
 * describe: no source symbol is undocumented, and the counts written in prose
 * still add up.
 *
 * This checks symbols and counts, not meaning. A page can still be misleading
 * while passing. It exists because every stale claim found in the copy audit
 * was a symbol or a count that drifted with no way to fail.
 *
 * To cover a new fact, add an entry to FACTS below.
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

/** Pulls every double-quoted string out of a source fragment. */
const quoted = (fragment) =>
  [...fragment.matchAll(/"([^"]+)"/g)].map((match) => match[1]);

/**
 * Each fact names a source file, the doc page that restates it, an `extract`
 * that returns the list of symbols the source defines, the singular noun for
 * error messages, and the prose count phrase (`{n}` is the spelled-out length).
 */
const FACTS = [
  {
    label: "fiber",
    source: "demo/midgard-node/src/fibers/index.ts",
    doc: "docs-site/content/docs/operators/node/background-fibers.mdx",
    extract: (src) =>
      [...src.matchAll(/export \* from "\.\/([\w-]+)\.js";/g)].map((m) => m[1]),
    countPhrase: "The {n} long-running fibers",
  },
  {
    // bin.ts rejects an unknown command by listing every valid one. That error
    // string is the command allow-list.
    label: "command",
    source: "demo/midgard-fault-proofs/src/bin.ts",
    doc: "docs-site/content/docs/fault-proofs/overview.mdx",
    extract: (src) => {
      const usage = src.match(/Expected command ([^\n]+?)\.\\n\$\{usage\}/);
      return usage ? quoted(usage[1]) : [];
    },
    countPhrase: "The {n} commands",
  },
  {
    label: "node command",
    source: "demo/midgard-node/src/index.ts",
    doc: "docs-site/content/docs/operators/node/cli-reference.mdx",
    extract: (src) => [
      ...new Set(
        [...src.matchAll(/\.command\("([a-z0-9:._-]+)"/g)].map((m) => m[1]),
      ),
    ],
    countPhrase: "The {n} commands",
  },
  {
    label: "workspace member",
    source: "demo/pnpm-workspace.yaml",
    doc: "docs-site/content/docs/getting-started/repository-map.mdx",
    extract: (src) =>
      [...src.matchAll(/^\s*-\s+([\w-]+)\s*$/gm)].map((m) => m[1]),
    countPhrase: "The {n} workspace members",
  },
  {
    label: "L1 env variable",
    source: "l1-services/.env.example",
    doc: "docs-site/content/docs/getting-started/l1-backend.mdx",
    extract: (src) => [...src.matchAll(/^([A-Z_]+)=/gm)].map((m) => m[1]),
    countPhrase: "the {n} variables",
  },
  {
    label: "status",
    source: "demo/lucid-midgard/src/builder/status.ts",
    doc: "docs-site/content/docs/sdk/lucid-midgard/submission-observability.mdx",
    extract: (src) => {
      const set = src.match(
        /const TX_STATUS_KINDS: ReadonlySet<string> = new Set\(\[([\s\S]*?)\]\)/,
      );
      return set ? quoted(set[1]) : [];
    },
    countPhrase: "`TxStatus` has {n} kinds",
  },
];

const failures = [];
const fail = (message) => failures.push(message);

for (const { label, source, doc, extract, countPhrase } of FACTS) {
  const symbols = extract(read(source));
  if (symbols.length === 0) {
    fail(`${source}: found no ${label}s to check. Has the source shape changed?`);
    continue;
  }

  const page = read(doc);
  for (const symbol of symbols) {
    if (!page.includes(symbol)) {
      fail(`${doc}: ${label} \`${symbol}\` exists in ${source} but is not documented.`);
    }
  }

  const expected = countPhrase.replace("{n}", NUMBER_WORDS[symbols.length] ?? String(symbols.length));
  if (!page.includes(expected)) {
    fail(`${doc}: expected the phrase "${expected}" (${source} yields ${symbols.length}).`);
  }
}

const demoPackage = JSON.parse(read("demo/package.json"));
const localDev = read("docs-site/content/docs/getting-started/local-dev.mdx");
if (!localDev.includes(`\`${demoPackage.engines.node}\``)) {
  fail(
    `docs-site/content/docs/getting-started/local-dev.mdx: expected Node engine \`${demoPackage.engines.node}\` from demo/package.json.`,
  );
}

const daDocs = read("docs-site/content/docs/onchain/da-validators.mdx");
for (const [entrypoint, source] of [
  ["da_attestation", "onchain/aiken/validators/da-attestation.ak"],
  ["da_params_governor", "onchain/aiken/validators/da-params-governor.ak"],
]) {
  const validator = read(source);
  if (!validator.includes(`validator ${entrypoint}(`)) {
    fail(`${source}: missing documented ${entrypoint} entrypoint.`);
  }
  if (!daDocs.includes(`\`${entrypoint}\``)) {
    fail(`docs-site/content/docs/onchain/da-validators.mdx: missing \`${entrypoint}\`.`);
  }
}

const phaseOrder = ["Withdrawal", "ForcedTransaction", "L2Transaction", "Deposit"];
const mpf = read("demo/midgard-node/src/workers/utils/mpf.ts");
let previous = -1;
for (const phase of phaseOrder) {
  const position = mpf.indexOf(`case \"${phase}\":`, previous + 1);
  if (position <= previous) {
    fail(`demo/midgard-node/src/workers/utils/mpf.ts: canonical phase ${phase} is missing or out of order.`);
  }
  previous = position;
}
const blockSpec = read("technical-spec/1-ledger-state/1-block.tex");
if (!blockSpec.includes("withdrawals, forced transactions, L2 transaction requests, and deposits")) {
  fail("technical-spec/1-ledger-state/1-block.tex: canonical transition phase order changed.");
}

if (failures.length > 0) {
  console.error("Documentation has drifted from source:\n");
  for (const failure of failures) console.error(`  - ${failure}`);
  console.error("\nUpdate the page, or the source, so the two agree. See docs-site/README.md.");
  process.exit(1);
}

console.log(`Docs facts check passed: ${FACTS.length + 3} fact groups.`);
