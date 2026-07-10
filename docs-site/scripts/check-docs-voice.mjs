#!/usr/bin/env node
/**
 * Asserts the docs speak to their reader, not about themselves.
 *
 * A docs sentence fails when its implied audience or subject is not the
 * reader. Three tests define the class:
 *   1. Subject: the sentence is about the system, not the page or its author
 *      ("This page summarizes <file>" fails).
 *   2. Actor: the page's reader can perform the sentence's verbs
 *      ("Do not add fallback paths" fails on an operator page).
 *   3. World: every referenced artifact is reachable and meaningful from the
 *      rendered site, not from a repo checkout or agent tooling.
 *
 * The patterns below are high-precision markers of those failures. If a line
 * is a legitimate use, add it to ALLOW with a reason instead of weakening the
 * pattern.
 */
import { readFileSync, readdirSync, statSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const CONTENT = resolve(dirname(fileURLToPath(import.meta.url)), "../content/docs");

/** file suffix -> array of allowed line substrings, each with a reason. */
const ALLOW = {
  // none yet
};

const CLASSES = [
  ["page-about-page", [
    /\bthis (page|section|document(ation)?) (summarizes|mirrors|records|carries|is kept|is updated|is based|exists)/i,
    /\bthese (pages|docs) (are|summarize|do not)/i,
    /not reproduced here/i,
    /\bread (it|this page) as\b/i,
    /orientation layer/i,
  ]],
  ["provenance-narration", [
    /\bper the `/i,
    /\bper its\b/i,
    /\btaken from `/i,
    /\bcomes? from the (repository|source|readme)/i,
    /\bgrounded in\b/i,
    /\bthe (doc|note|plan|document) (says|states|stages|records)/i,
    /\bsource note\b/i,
    /\bits (jsdoc|module header)\b/i,
  ]],
  ["maintainer-imperative", [
    /\bdo not add\b/i,
    /\bmust never become\b/i,
    /\bprefer the current canonical\b/i,
    /\bkeep strict behavior\b/i,
    /\bcodifies a\b/i,
  ]],
  ["internal-world", [
    /AGENTS\.md/,
    /\.agents\//,
    /agent guidance/i,
    /\bworktree\b/i,
    /CLAUDE\.md/,
    /tx-validation/,
    /readiness docs\b/i,
    /\bthe skill\b/i,
  ]],
  ["team-voice", [
    /\bwe (use|keep|do|have|recommend|decided|chose)\b/i,
    /\blet's\b/i,
  ]],
  ["filler", [
    /\bit is (important|worth) (to note|noting)\b/i,
    /\bnote that\b/i,
    /\balways-current\b/i,
  ]],
  ["em-dash", [/—/]],
  ["named-chain", [/\b(Ethereum|Solana|Arbitrum|Polygon)\b/]],
];

const files = [];
const walk = (d) => {
  for (const entry of readdirSync(d)) {
    const p = join(d, entry);
    if (statSync(p).isDirectory()) walk(p);
    else if (p.endsWith(".mdx")) files.push(p);
  }
};
walk(CONTENT);

const failures = [];
for (const file of files.sort()) {
  const rel = relative(CONTENT, file);
  const allowed = Object.entries(ALLOW)
    .filter(([suffix]) => rel.endsWith(suffix))
    .flatMap(([, subs]) => subs);
  readFileSync(file, "utf8").split("\n").forEach((line, i) => {
    if (allowed.some((sub) => line.includes(sub))) return;
    for (const [cls, patterns] of CLASSES) {
      for (const pattern of patterns) {
        if (pattern.test(line)) {
          failures.push(`${rel}:${i + 1} [${cls}] ${line.trim().slice(0, 110)}`);
        }
      }
    }
  });
}

if (failures.length > 0) {
  console.error("Docs voice check failed. These lines address the wrong audience or subject:\n");
  for (const failure of failures) console.error(`  - ${failure}`);
  console.error("\nDelete the line, restate it as a system fact, or demote it to a source link.");
  process.exit(1);
}

console.log(`Docs voice check passed: ${files.length} pages, 0 wrong-audience lines.`);
