#!/usr/bin/env node

// Checks that every rule in the agent-facing instruction files names what
// enforces it, and that the name is true.
//
// A rule is a Markdown list item or paragraph (see markdown-blocks.mjs) that
//   - contains a directive word outside inline code (`DIRECTIVE` below), or
//   - is a list item in a section whose heading (level 2 or deeper; a
//     document title does not count) contains "rule".
// Each rule carries exactly one tag from the vocabulary in
// docs/agents/README.md. A rule written as a plain imperative without any
// directive word, outside a "rules" section, is not detected; that is this
// check's own blind spot.
//
// Findings:
//   - a rule with no tag, or with more than one;
//   - an unknown tag kind;
//   - a tag that does not resolve: the ESLint rule is not configured in any
//     tracked eslint.config.*, the workflow has no step with that exact name,
//     the hook or script is not a tracked file, the Aiken module has no `test`
//     definition, or no tracked source file defines the runtime symbol;
//   - an under-claim: a `[review]` rule that mentions something a known check
//     enforces (`KNOWN_ENFORCERS`);
//   - a file over its line budget (`LINE_BUDGETS`).
//
// Exit codes: 0 no findings, 1 findings, 2 could not look (git or a file could
// not be read). "Could not look" is never reported as a pass.
//
// Usage: node scripts/agents/check-enforcement-tags.mjs [--root <dir>]

import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { markdownBlocks, withoutInlineCode } from "./markdown-blocks.mjs";

export const DIRECTIVE =
  /\b(?:must|never|always|do not|don't|may not|required)\b/iu;

export const TAG_KINDS = [
  "eslint",
  "hook",
  "ci",
  "script",
  "aiken-test",
  "runtime",
  "review",
];

// Checks that exist today and the words that identify a rule they enforce.
// A rule tagged `[review]` that matches one is under-claimed: tag it with the
// check instead, and write the check's blind spot next to it. The real-
// repository test resolves every tag here, so this table cannot go stale.
export const KNOWN_ENFORCERS = [
  {
    pattern: /localUPLCEval/u,
    tag: "[eslint: midgard/local-uplc-eval]",
  },
  {
    pattern: /\blocaleCompare\b/u,
    tag: "[eslint: midgard/locale-compare-explicit-locale]",
  },
  {
    pattern: /\bapplyParamsToScript\b|apply_params_to_script/u,
    tag: "[eslint: midgard/apply-params-through-blueprint]",
  },
  {
    pattern: /\bsetMinFee\b/u,
    tag: "[eslint: midgard/exact-fee-no-change-output]",
  },
  {
    pattern: /\boverrideUTxOs\b/u,
    tag: "[eslint: midgard/scoped-utxo-override]",
  },
  {
    pattern: /workspace packages by name|\.\.\/<package>\/(?:src|dist)/iu,
    tag: "[eslint: no-restricted-imports]",
  },
  {
    pattern: /commit[^.]*plutus\.json|plutus\.json[^.]*\bcommit/iu,
    tag: "[hook: pre-commit]",
  },
  {
    pattern: /run-focused-check|guard-focused-selector/u,
    tag: "[script: onchain/aiken/scripts/run-focused-check.mjs]",
  },
  {
    pattern: /pinned (?:fork|compiler)|stock compiler/iu,
    tag: "[script: onchain/aiken/scripts/pinned-compiler.mjs]",
  },
  {
    pattern: /validate-runbook/u,
    tag: "[script: .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs]",
  },
  {
    pattern: /parameter's width|re-checks? a `?validator main/iu,
    tag: "[runtime: applyBlueprintParams]",
  },
];

// Line budgets, set on 2026-09-25 at ceil(lines x 1.1 / 10) x 10 of each
// file's size then. A file that outgrows its budget is asking to be split or
// trimmed, not for a bigger number. A file with no entry here uses
// DEFAULT_LINE_BUDGET.
export const DEFAULT_LINE_BUDGET = 120;
export const LINE_BUDGETS = {
  "AGENTS.md": 110,
  "demo/AGENTS.md": 30,
  "demo/midgard-node/AGENTS.md": 30,
  "demo/midgard-node-tools/devnet/AGENTS.md": 20,
  "onchain/aiken/AGENTS.md": 30,
  "docs/agents/README.md": 80,
  "docs/agents/component-configuration.md": 70,
  "docs/agents/contracts.md": 40,
  "docs/agents/domain.md": 20,
  "docs/agents/issue-tracker.md": 60,
  "docs/agents/naming-and-versioning.md": 20,
  "docs/agents/production-l2.md": 60,
  "docs/agents/state-reset.md": 40,
  "docs/agents/transaction-finalization.md": 50,
  "docs/agents/triage-labels.md": 20,
  "docs/agents/verification.md": 80,
  "docs/agents/withdraw-zero-yielding.md": 170,
  ".agents/skills/midgard-e2e-acceptance/SKILL.md": 230,
  ".agents/skills/midgard-typescript-cleanup/SKILL.md": 80,
  // Set on 2026-09-26, by the same rule, for the skills wave that was written
  // before this check reached it (aiken-contract-build was extended by it).
  ".agents/skills/adding-fault-proof-families/SKILL.md": 260,
  ".agents/skills/aiken-contract-build/SKILL.md": 260,
  ".agents/skills/committing-safely/SKILL.md": 140,
  ".agents/skills/debugging-ci-failures/SKILL.md": 230,
  ".agents/skills/editing-agent-instructions/SKILL.md": 160,
  ".agents/skills/fixing-flaky-tests/SKILL.md": 150,
  ".agents/skills/regenerating-goldens-and-ledgers/SKILL.md": 210,
  ".agents/skills/reviewing-consensus-changes/SKILL.md": 260,
  ".agents/skills/running-the-devnet/SKILL.md": 170,
  ".agents/skills/splitting-oversized-modules/SKILL.md": 190,
  ".agents/skills/writing-reports-and-prs/SKILL.md": 150,
  ".agents/skills/writing-tests/SKILL.md": 230,
};

export class CouldNotLook extends Error {}

const git = (root, args) => {
  try {
    return execFileSync("git", args, {
      cwd: root,
      encoding: "utf8",
      maxBuffer: 256 * 1024 * 1024,
      stdio: ["ignore", "pipe", "pipe"],
    });
  } catch (error) {
    throw new CouldNotLook(
      `git ${args.join(" ")} failed in ${root}: ${error.stderr?.toString().trim() || error.message}`,
    );
  }
};

export const trackedFiles = (root) =>
  new Set(git(root, ["ls-files", "-z"]).split("\0").filter(Boolean));

// Generated from a registry that is itself the enforcement: every entry in
// required-checks.md is a check preflight runs, and `--check-docs` keeps the
// file equal to the registry. Tagging each line would restate that.
export const GENERATED_FILES = new Set(["docs/agents/required-checks.md"]);

export const agentFacingFiles = (tracked) =>
  [...tracked]
    .filter(
      (path) =>
        !GENERATED_FILES.has(path) &&
        (path === "AGENTS.md" ||
          path.endsWith("/AGENTS.md") ||
          /^docs\/agents\/[^/]+\.md$/u.test(path) ||
          /^\.agents\/skills\/[^/]+\/SKILL\.md$/u.test(path)),
    )
    .sort();

const readTracked = (root, path) => {
  try {
    return readFileSync(join(root, path), "utf8");
  } catch (error) {
    throw new CouldNotLook(`cannot read ${path}: ${error.message}`);
  }
};

const tagPattern = /\[([a-z][a-z-]*)(?::\s*([^\]]*?))?\s*\](?!\()/gu;

export const tagsIn = (text) =>
  [...withoutInlineCode(text).matchAll(tagPattern)]
    .map(([whole, kind, value]) => ({ whole, kind, value: value ?? "" }))
    .filter(({ kind, value }) => TAG_KINDS.includes(kind) || value !== "");

const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");

const workflowStepNames = (source) =>
  new Set(
    [...source.matchAll(/^\s*(?:-\s+)?name:\s*(.+?)\s*$/gmu)].map(([, name]) =>
      name.replace(/^(["'])(.*)\1$/u, "$2"),
    ),
  );

// Resolves one tag against the repository; returns a reason string when it
// does not resolve, undefined when it does.
export const resolveTag = ({ kind, value }, { root, tracked }) => {
  switch (kind) {
    case "review":
      return value === "" ? undefined : "[review] takes no value";
    case "eslint": {
      if (value === "") return "[eslint] needs a rule name";
      const configs = [...tracked].filter((path) =>
        /(?:^|\/)eslint\.config\.[cm]?[jt]s$/u.test(path),
      );
      const configured = new RegExp(
        `(["'])${escapeRegExp(value)}\\1\\s*:`,
        "u",
      );
      return configs.some((path) => configured.test(readTracked(root, path)))
        ? undefined
        : `ESLint rule ${value} is not configured in ${configs.join(", ") || "any tracked eslint.config"}`;
    }
    case "hook":
      return tracked.has(`.githooks/${value}`)
        ? undefined
        : `.githooks/${value} is not a tracked hook`;
    case "ci": {
      const slash = value.indexOf("/");
      if (slash <= 0) return "[ci] needs <workflow>/<step name>";
      const workflow = value.slice(0, slash);
      const step = value.slice(slash + 1).trim();
      // The workflow is named by its file (`aiken-ci`) or by its top-level
      // `name:` (`Aiken CI`), which is what the Actions UI and a failing
      // check show.
      const workflows = [...tracked].filter((path) =>
        /^\.github\/workflows\/[^/]+\.ya?ml$/u.test(path),
      );
      const file =
        workflows.find(
          (path) => path.replace(/^.*\/|\.ya?ml$/gu, "") === workflow,
        ) ??
        workflows.find(
          (path) =>
            /^name:\s*(.+?)\s*$/mu
              .exec(readTracked(root, path))?.[1]
              .replace(/^(["'])(.*)\1$/u, "$2") === workflow,
        );
      if (file === undefined) return `no tracked workflow ${workflow}.yml`;
      return workflowStepNames(readTracked(root, file)).has(step)
        ? undefined
        : `${file} has no step named "${step}"`;
    }
    case "script":
      return tracked.has(value) ? undefined : `${value} is not a tracked file`;
    case "aiken-test": {
      const module = value.replace(/\/+$/u, "");
      if (module === "") return "[aiken-test] needs a module path";
      const candidates = ["lib", "validators"].map(
        (directory) => `onchain/aiken/${directory}/${module}.ak`,
      );
      const inDirectory = [...tracked].filter(
        (path) =>
          path.endsWith(".ak") &&
          ["lib", "validators"].some((directory) =>
            path.startsWith(`onchain/aiken/${directory}/${module}/`),
          ),
      );
      const files = [
        ...candidates.filter((path) => tracked.has(path)),
        ...inDirectory,
      ];
      if (files.length === 0) return `no tracked Aiken module ${module}`;
      return files.some((path) =>
        /^test\s+[a-z_][a-z0-9_]*/mu.test(readTracked(root, path)),
      )
        ? undefined
        : `Aiken module ${module} defines no test`;
    }
    case "runtime": {
      if (!/^[A-Za-z_$][\w$]*$/u.test(value))
        return "[runtime] needs one identifier";
      let found;
      try {
        found = execFileSync(
          "git",
          [
            "grep",
            "-l",
            "-E",
            `(const|let|var|function|class|type|interface|fn|def)[[:space:]]+${value}([^[:alnum:]_$]|$)`,
            "--",
            ":!*.md",
          ],
          { cwd: root, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
        );
      } catch (error) {
        if (error.status === 1) found = "";
        else throw new CouldNotLook(`git grep failed: ${error.message}`);
      }
      return found.trim() === ""
        ? `no tracked source file defines ${value}`
        : undefined;
    }
    default:
      return `unknown tag kind [${kind}]`;
  }
};

export const isRule = (block) =>
  DIRECTIVE.test(withoutInlineCode(block.text)) ||
  (block.sectionLevel >= 2 &&
    /rule/iu.test(block.section) &&
    /^\s*(?:[-*+]|\d+[.)])\s/u.test(block.text));

export const checkFile = (path, source, context) => {
  const findings = [];
  const lineCount = source.split("\n").length - (source.endsWith("\n") ? 1 : 0);
  const budget = LINE_BUDGETS[path] ?? DEFAULT_LINE_BUDGET;
  if (lineCount > budget)
    findings.push(
      `${path}: ${lineCount} lines is over its budget of ${budget}`,
    );

  for (const block of markdownBlocks(source)) {
    const tags = tagsIn(block.text);
    const where = `${path}:${block.line}`;
    for (const tag of tags) {
      const problem = resolveTag(tag, context);
      if (problem !== undefined)
        findings.push(`${where}: ${tag.whole}: ${problem}`);
    }
    if (!isRule(block)) continue;
    if (tags.length === 0) {
      findings.push(`${where}: rule has no enforcement tag`);
      continue;
    }
    if (tags.length > 1) {
      findings.push(
        `${where}: rule has ${tags.length} tags; use exactly one (${tags.map((tag) => tag.whole).join(" ")})`,
      );
      continue;
    }
    if (tags[0].kind === "review") {
      for (const enforcer of KNOWN_ENFORCERS) {
        if (enforcer.pattern.test(block.text))
          findings.push(
            `${where}: under-claimed [review]: ${enforcer.tag} enforces this rule`,
          );
      }
    }
  }
  return findings;
};

export const checkEnforcementTags = (root) => {
  const tracked = trackedFiles(root);
  const context = { root, tracked };
  const files = agentFacingFiles(tracked);
  if (files.length === 0)
    throw new CouldNotLook(`no agent-facing files are tracked under ${root}`);
  const findings = files.flatMap((path) =>
    checkFile(path, readTracked(root, path), context),
  );
  return { files, findings };
};

const main = (argv) => {
  const rootFlag = argv.indexOf("--root");
  const root = resolve(
    rootFlag >= 0
      ? argv[rootFlag + 1]
      : join(dirname(fileURLToPath(import.meta.url)), "../.."),
  );
  try {
    const { files, findings } = checkEnforcementTags(root);
    for (const finding of findings) console.error(finding);
    if (findings.length > 0) {
      console.error(
        `check-enforcement-tags: ${findings.length} finding(s) in ${files.length} file(s); tag vocabulary: docs/agents/README.md`,
      );
      return 1;
    }
    console.log(`check-enforcement-tags: ${files.length} file(s) clean`);
    return 0;
  } catch (error) {
    if (!(error instanceof CouldNotLook)) throw error;
    console.error(`check-enforcement-tags: could not look: ${error.message}`);
    return 2;
  }
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
