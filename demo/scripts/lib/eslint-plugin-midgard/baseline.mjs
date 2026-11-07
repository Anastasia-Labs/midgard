// The ratchet behind the Midgard ESLint rules.
//
// `baseline.json` lists the violations each rule already had when it was
// introduced, so a rule can be switched on as an error without rewriting
// every old call site in the same change. Its shape is
//
//   { "<rule id>": { "<file relative to demo/>": { "reason": "...",
//                                                  "sites": ["<line>", ...] } } }
//
// A site is the trimmed, whitespace-collapsed text of the source line the
// violation is reported on. Matching by line text rather than line number
// keeps an entry stable when code above it moves; a line that appears twice
// is listed twice.
//
// The ratchet only turns one way:
//   - a violation whose line is not listed fails the lint;
//   - a listed site that no longer occurs also fails the lint (in the file it
//     belonged to), so a fixed violation must leave the baseline in the same
//     change: `node scripts/lib/eslint-plugin-midgard/prune-baseline.mjs`
//     removes every site that no longer occurs and never adds one;
//   - adding an entry is a hand edit, and the plugin's test refuses an entry
//     without a reason, for a rule that does not exist, or for a file that
//     does not exist.

import { readFileSync } from "node:fs";
import { dirname, join, relative, sep } from "node:path";
import { fileURLToPath } from "node:url";

export const BASELINE_PATH = join(
  dirname(fileURLToPath(import.meta.url)),
  "baseline.json",
);

// The prune command sets this to lint without the baseline, so that it sees
// every violation that still occurs.
export const BASELINE_ENV = "MIDGARD_ESLINT_BASELINE";

export const PRUNE_COMMAND =
  "node scripts/lib/eslint-plugin-midgard/prune-baseline.mjs";

export const fingerprint = (line) => line.trim().replace(/\s+/gu, " ");

export const loadBaseline = (path = BASELINE_PATH) =>
  process.env[BASELINE_ENV] === "off"
    ? {}
    : JSON.parse(readFileSync(path, "utf8"));

// Path of the linted file relative to the directory the configuration names
// as its root (demo/), with forward slashes on every platform.
export const relativePath = (context) => {
  const root = context.settings?.midgard?.root ?? context.cwd;
  return relative(root, context.filename).split(sep).join("/");
};

const STALE_MESSAGE_ID = "staleBaseline";

const staleMessage = `{{count}} baselined violation(s) of this rule no longer occur in this file: {{sites}}. Fix: run \`${PRUNE_COMMAND}\` from demo/ to drop them from the baseline; if the line was only reformatted, edit its site in scripts/lib/eslint-plugin-midgard/baseline.json by hand.`;

/**
 * Wraps a rule so that its reports go through the baseline.
 *
 * `create(context, report)` returns the rule's listeners and calls
 * `report(descriptor)` instead of `context.report`. The wrapper adds the
 * `staleBaseline` message and a `Program:exit` listener that reports every
 * baselined site of this rule and file that was not matched.
 */
export const defineRule = ({ meta, create }) => ({
  meta: {
    ...meta,
    messages: { ...meta.messages, [STALE_MESSAGE_ID]: staleMessage },
  },
  create(context) {
    const ruleId = context.id;
    const file = relativePath(context);
    const entry = context.settings?.midgard?.baseline?.[ruleId]?.[file];
    const remaining = new Map();
    for (const site of entry?.sites ?? []) {
      remaining.set(site, (remaining.get(site) ?? 0) + 1);
    }
    const lines = context.sourceCode.lines;
    const report = (descriptor) => {
      const start = descriptor.loc?.start ?? descriptor.node.loc.start;
      const site = fingerprint(lines[start.line - 1] ?? "");
      const count = remaining.get(site) ?? 0;
      if (count > 0) {
        remaining.set(site, count - 1);
        return;
      }
      context.report(descriptor);
    };
    const listeners = create(context, report, file);
    const exit = listeners["Program:exit"];
    return {
      ...listeners,
      "Program:exit"(node) {
        exit?.(node);
        const stale = [...remaining].flatMap(([site, count]) =>
          Array.from({ length: count }, () => site),
        );
        if (stale.length > 0) {
          context.report({
            loc: { line: 1, column: 0 },
            messageId: STALE_MESSAGE_ID,
            data: {
              count: String(stale.length),
              sites: stale.map((site) => JSON.stringify(site)).join(", "),
            },
          });
        }
      },
    };
  },
});

/**
 * The baseline with every site that no longer occurs removed. `actual` maps
 * rule id to file to the list of sites the rule reports without a baseline.
 * The result never contains a rule, file or site that `baseline` lacks.
 */
export const pruneBaseline = (baseline, actual) => {
  const pruned = {};
  let removed = 0;
  for (const [ruleId, files] of Object.entries(baseline)) {
    for (const [file, entry] of Object.entries(files)) {
      const available = new Map();
      for (const site of actual[ruleId]?.[file] ?? []) {
        available.set(site, (available.get(site) ?? 0) + 1);
      }
      const kept = entry.sites.filter((site) => {
        const count = available.get(site) ?? 0;
        if (count === 0) return false;
        available.set(site, count - 1);
        return true;
      });
      removed += entry.sites.length - kept.length;
      if (kept.length > 0) {
        pruned[ruleId] ??= {};
        pruned[ruleId][file] = { ...entry, sites: kept };
      }
    }
  }
  return { pruned, removed };
};

/**
 * Problems with the baseline's own shape. `ruleIds` are the rules the plugin
 * defines; `fileExists` answers for a path relative to demo/.
 */
export const baselineProblems = (baseline, { ruleIds, fileExists }) => {
  const problems = [];
  for (const [ruleId, files] of Object.entries(baseline)) {
    if (!ruleIds.includes(ruleId)) {
      problems.push(`${ruleId}: not a rule of the midgard plugin`);
    }
    for (const [file, entry] of Object.entries(files)) {
      const where = `${ruleId} ${file}`;
      if (!fileExists(file)) problems.push(`${where}: file does not exist`);
      if (typeof entry.reason !== "string" || entry.reason.trim().length < 20) {
        problems.push(`${where}: every entry needs a reason (20+ characters)`);
      }
      if (
        !Array.isArray(entry.sites) ||
        entry.sites.length === 0 ||
        entry.sites.some(
          (site) => typeof site !== "string" || site !== fingerprint(site),
        )
      ) {
        problems.push(
          `${where}: sites must be a non-empty list of trimmed, whitespace-collapsed source lines`,
        );
      }
      const extra = Object.keys(entry).filter(
        (key) => key !== "reason" && key !== "sites",
      );
      if (extra.length > 0) {
        problems.push(`${where}: unknown key(s) ${extra.join(", ")}`);
      }
    }
  }
  return problems;
};
