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
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "ten",
  "eleven",
  "twelve",
  "thirteen",
  "fourteen",
  "fifteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen",
  "twenty",
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
    // bin.ts defines every valid command once in the canonical help template.
    // Extract the first token after the binary name from each usage line.
    label: "command",
    source: "demo/midgard-fault-proofs/src/bin.ts",
    doc: "docs-site/content/docs/fault-proofs/overview.mdx",
    extract: (src) => {
      const usage = src.match(/const usage = `Usage:\n([\s\S]*?)\n`;/);
      return usage
        ? [
            ...usage[1].matchAll(
              /^\s*midgard-fault-proofs ([a-z0-9-]+)(?:\s|$)/gm,
            ),
          ].map((match) => match[1])
        : [];
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
  {
    label: "source catalogue category",
    source: "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
    doc: "docs-site/content/docs/onchain/fraud-proof-machines.mdx",
    extract: (src) => {
      const order = src.match(
        /FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = \[([\s\S]*?)\] as const/,
      );
      return order ? quoted(order[1]) : [];
    },
    countPhrase: "The {n} source catalogue categories",
  },
];

const failures = [];
const fail = (message) => failures.push(message);

for (const { label, source, doc, extract, countPhrase } of FACTS) {
  const symbols = extract(read(source));
  if (symbols.length === 0) {
    fail(
      `${source}: found no ${label}s to check. Has the source shape changed?`,
    );
    continue;
  }

  const page = read(doc);
  for (const symbol of symbols) {
    if (!page.includes(symbol)) {
      fail(
        `${doc}: ${label} \`${symbol}\` exists in ${source} but is not documented.`,
      );
    }
  }

  const expected = countPhrase.replace(
    "{n}",
    NUMBER_WORDS[symbols.length] ?? String(symbols.length),
  );
  if (!page.includes(expected)) {
    fail(
      `${doc}: expected the phrase "${expected}" (${source} yields ${symbols.length}).`,
    );
  }
}

// The source inventory is useful only if IDs and actual runner installations
// stay checked together. Catalogue array position is deliberately not identity.
const catalogueSource = read("demo/midgard-sdk/src/fraud-proof/catalogue.ts");
const catalogueOrder = quoted(
  catalogueSource.match(
    /FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = \[([\s\S]*?)\] as const/,
  )?.[1] ?? "",
);
const catalogueIds = new Map(
  [
    ...(
      catalogueSource.match(
        /FRAUD_PROOF_CATALOGUE_CATEGORY_IDS = \{([\s\S]*?)\}/,
      )?.[1] ?? ""
    ).matchAll(/(\w+): "([0-9a-f]{8})"/g),
  ].map((match) => [match[1], match[2]]),
);
const installedCategories = quoted(
  read(
    "demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts",
  ).match(
    /WATCHER_INSTALLED_WORKFLOW_CATEGORIES = Object\.freeze\(\[([\s\S]*?)\] as const/,
  )?.[1] ?? "",
);
const catalogueDocPath = "docs/fault-proofs/catalogue-status.md";
const catalogueDoc = read(catalogueDocPath);
const inventory = [
  ...catalogueDoc.matchAll(
    /^\|\s*`([0-9a-f]{8})`\s*\|\s*`(\w+)`\s*\|\s*(Yes|No)\s*\|$/gm,
  ),
].map((match) => [match[1], match[2], match[3]]);
const expectedInventory = catalogueOrder.map((category) => [
  catalogueIds.get(category),
  category,
  installedCategories.includes(category) ? "Yes" : "No",
]);
if (
  catalogueOrder.length === 0 ||
  catalogueIds.size !== catalogueOrder.length ||
  installedCategories.length === 0 ||
  installedCategories.some((category) => !catalogueIds.has(category)) ||
  JSON.stringify(inventory) !== JSON.stringify(expectedInventory)
) {
  fail(
    `${catalogueDocPath}: category IDs, order, or watcher installations differ from source.`,
  );
}
const inventorySummary = catalogueDoc.replace(/\s+/g, " ");
if (
  !inventorySummary.includes(
    `The ${catalogueOrder.length} source catalogue categories and all ${installedCategories.length} watcher installations`,
  )
) {
  fail(`${catalogueDocPath}: inventory summary counts differ from source.`);
}

const languageSource = read(
  "demo/midgard-core/src/codec/script-language-views.ts",
);
const languageBlock =
  languageSource.match(
    /MIDGARD_SUPPORTED_SCRIPT_LANGUAGES = Object\.freeze\(\[([\s\S]*?)\] as const/,
  )?.[1] ?? "";
const languages = [...languageBlock.matchAll(/name: "(\w+)"/g)].map(
  (match) => match[1],
);
const languagePhrase = `exactly ${languages.map((name) => `\`${name}\``).join(" and ")} in`;
if (
  languages.length === 0 ||
  !read("demo/lucid-midgard/README.md")
    .replace(/\s+/g, " ")
    .includes(languagePhrase)
) {
  fail(
    "demo/lucid-midgard/README.md: supportedScriptLanguages must match the exact core advertisement.",
  );
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
    fail(
      `docs-site/content/docs/onchain/da-validators.mdx: missing \`${entrypoint}\`.`,
    );
  }
}

const phaseOrder = [
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
  "Deposit",
];
const mpf = read("demo/midgard-node/src/mpf/transition-cbor.ts");
let previous = -1;
for (const phase of phaseOrder) {
  const position = mpf.indexOf(`case \"${phase}\":`, previous + 1);
  if (position <= previous) {
    fail(
      `demo/midgard-node/src/mpf/transition-cbor.ts: canonical phase ${phase} is missing or out of order.`,
    );
  }
  previous = position;
}
const blockSpec = read("technical-spec/1-ledger-state/1-block.tex");
if (
  !blockSpec.includes(
    "withdrawals, forced transactions, L2 transaction requests, and deposits",
  )
) {
  fail(
    "technical-spec/1-ledger-state/1-block.tex: canonical transition phase order changed.",
  );
}

if (failures.length > 0) {
  console.error("Documentation has drifted from source:\n");
  for (const failure of failures) console.error(`  - ${failure}`);
  console.error(
    "\nUpdate the page, or the source, so the two agree. See docs-site/README.md.",
  );
  process.exit(1);
}

console.log(`Docs facts check passed: ${FACTS.length + 5} fact groups.`);
