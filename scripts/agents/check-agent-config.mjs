#!/usr/bin/env node

// Checks the shape of the shared agent configuration.
//
// CLAUDE.md (every tracked one) is a thin pointer to AGENTS.md, so the
// repository's rules have one source:
//   - it is a symlink to AGENTS.md, or
//   - it links to a tracked AGENTS.md, stays within CLAUDE_MD_LINE_BUDGET
//     lines, states no rule (no directive word outside inline code, see
//     check-enforcement-tags.mjs, and no enforcement tag), and every paragraph
//     or list item points at a tracked repository path (a link or a backticked
//     path), so each one routes rather than instructs.
//
// A tracked .claude/settings.json uses only allowlisted keys and cannot widen
// what an agent may do:
//   - top level: `$schema`, `hooks`, `permissions`, `includeCoAuthoredBy`;
//   - `hooks`: only `SessionStart`, whose entries hold `matcher` and `hooks`,
//     each hook `{ type: "command", command, timeout? }`, and each command runs
//     a tracked script under scripts/ with no shell composition (`;`, `|`,
//     `&`, a backtick, `$(`, `<` or `>`);
//   - `permissions`: only `deny` and `ask`, lists of strings; `allow`,
//     `defaultMode` and `additionalDirectories` widen permissions and fail;
//   - `includeCoAuthoredBy`: only `false`.
// A tracked .claude/settings.local.json is always a finding: it is personal.
//
// Exit codes: 0 clean, 1 findings, 2 could not look (git or a file could not
// be read).
//
// Usage: node scripts/agents/check-agent-config.mjs [--root <dir>]

import { lstatSync, readFileSync, readlinkSync } from "node:fs";
import { dirname, join, normalize, posix, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  CouldNotLook,
  repositoryIndex,
  trackedFiles,
} from "./check-doc-links.mjs";
import { DIRECTIVE, TAG_KINDS } from "./check-enforcement-tags.mjs";
import { markdownBlocks, withoutInlineCode } from "./markdown-blocks.mjs";

export const CLAUDE_MD_LINE_BUDGET = 40;

const SETTINGS_KEYS = [
  "$schema",
  "hooks",
  "permissions",
  "includeCoAuthoredBy",
];
const HOOK_EVENTS = ["SessionStart"];
const PERMISSION_KEYS = ["deny", "ask"];
const SHELL_COMPOSITION = /[;|&`<>]|\$\(/u;

const read = (root, path) => {
  try {
    return readFileSync(join(root, path), "utf8");
  } catch (error) {
    throw new CouldNotLook(`cannot read ${path}: ${error.message}`);
  }
};

const directoryOf = (path) =>
  posix.dirname(path) === "." ? "" : posix.dirname(path);

const linkTargets = (text) =>
  [...withoutInlineCode(text).matchAll(/\]\(\s*<?([^)\s>]+)>?\s*\)/gu)].map(
    ([, target]) => target.replace(/[#?].*$/u, ""),
  );

const backticked = (text) =>
  [...text.matchAll(/(`+)([^`]+?)\1/gu)].map(([, , token]) =>
    token.trim().replace(/[.,;:]+$/u, ""),
  );

export const checkClaudeMd = (path, source, index) => {
  const findings = [];
  const directory = directoryOf(path);
  const lines = source.split("\n").length - (source.endsWith("\n") ? 1 : 0);
  if (lines > CLAUDE_MD_LINE_BUDGET)
    findings.push(
      `${path}: ${lines} lines is over its budget of ${CLAUDE_MD_LINE_BUDGET}; move rules to AGENTS.md`,
    );

  const blocks = markdownBlocks(source);
  const linksAgents = blocks.some((block) =>
    linkTargets(block.text).some((target) => {
      const resolved = posix.normalize(posix.join(directory, target));
      return posix.basename(resolved) === "AGENTS.md" && index.exists(resolved);
    }),
  );
  if (!linksAgents)
    findings.push(`${path}: does not link to a tracked AGENTS.md`);

  const tag = new RegExp(`\\[(?:${TAG_KINDS.join("|")})[\\]:]`, "u");
  for (const block of blocks) {
    const where = `${path}:${block.line}`;
    const prose = withoutInlineCode(block.text);
    const word = DIRECTIVE.exec(prose)?.[0];
    if (word !== undefined)
      findings.push(
        `${where}: states a rule ("${word}"); rules belong in AGENTS.md`,
      );
    else if (tag.test(prose))
      findings.push(
        `${where}: carries an enforcement tag; rules belong in AGENTS.md`,
      );
    const cites = [
      ...linkTargets(block.text).map((target) =>
        posix.normalize(posix.join(directory, target)),
      ),
      ...backticked(block.text).flatMap((token) => [
        posix.normalize(posix.join(directory, token)),
        posix.normalize(token),
      ]),
    ].some((candidate) => candidate !== "." && index.exists(candidate));
    if (!cites)
      findings.push(
        `${where}: points at no tracked repository path; CLAUDE.md only routes`,
      );
  }
  return findings;
};

const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const unexpectedKeys = (value, allowed) =>
  Object.keys(value).filter((key) => !allowed.includes(key));

const checkCommand = (where, command, tracked) => {
  if (typeof command !== "string" || command.trim() === "")
    return [`${where}: command must be a non-empty string`];
  if (SHELL_COMPOSITION.test(command))
    return [
      `${where}: command composes shell (${command}); run one tracked script`,
    ];
  const scripts = command
    .split(/\s+/u)
    .map((word) =>
      word
        .replace(/^["']|["']$/gu, "")
        .replace(/^"?\$\{?CLAUDE_PROJECT_DIR\}?"?\//u, "")
        .replace(/^\.\//u, ""),
    )
    .filter((word) => word.startsWith("scripts/"));
  if (!scripts.some((script) => tracked.has(script)))
    return [`${where}: command runs no tracked script under scripts/`];
  return [];
};

export const checkSettings = (path, source, tracked) => {
  let settings;
  try {
    settings = JSON.parse(source);
  } catch (error) {
    return [`${path}: not valid JSON: ${error.message}`];
  }
  if (!isObject(settings)) return [`${path}: is not a JSON object`];
  const findings = unexpectedKeys(settings, SETTINGS_KEYS).map(
    (key) => `${path}: key "${key}" is not allowlisted`,
  );

  if (
    "includeCoAuthoredBy" in settings &&
    settings.includeCoAuthoredBy !== false
  )
    findings.push(`${path}: includeCoAuthoredBy may only be false`);

  if ("permissions" in settings) {
    const permissions = settings.permissions;
    if (!isObject(permissions)) {
      findings.push(`${path}: permissions is not an object`);
    } else {
      for (const key of unexpectedKeys(permissions, PERMISSION_KEYS))
        findings.push(
          `${path}: permissions.${key} is not allowlisted; shared settings only deny or ask`,
        );
      for (const key of PERMISSION_KEYS)
        if (
          key in permissions &&
          !(
            Array.isArray(permissions[key]) &&
            permissions[key].every((entry) => typeof entry === "string")
          )
        )
          findings.push(`${path}: permissions.${key} is not a list of strings`);
    }
  }

  if ("hooks" in settings) {
    const hooks = settings.hooks;
    if (!isObject(hooks)) {
      findings.push(`${path}: hooks is not an object`);
    } else {
      for (const event of unexpectedKeys(hooks, HOOK_EVENTS))
        findings.push(`${path}: hooks.${event} is not allowlisted`);
      for (const event of HOOK_EVENTS.filter((name) => name in hooks)) {
        if (!Array.isArray(hooks[event])) {
          findings.push(`${path}: hooks.${event} is not a list`);
          continue;
        }
        hooks[event].forEach((entry, entryIndex) => {
          const at = `${path}: hooks.${event}[${entryIndex}]`;
          if (!isObject(entry) || !Array.isArray(entry.hooks)) {
            findings.push(`${at} needs a hooks list`);
            return;
          }
          for (const key of unexpectedKeys(entry, ["matcher", "hooks"]))
            findings.push(`${at}.${key} is not allowlisted`);
          entry.hooks.forEach((hook, hookIndex) => {
            const hookAt = `${at}.hooks[${hookIndex}]`;
            if (!isObject(hook) || hook.type !== "command") {
              findings.push(`${hookAt}: only command hooks are allowed`);
              return;
            }
            for (const key of unexpectedKeys(hook, [
              "type",
              "command",
              "timeout",
            ]))
              findings.push(`${hookAt}.${key} is not allowlisted`);
            findings.push(...checkCommand(hookAt, hook.command, tracked));
          });
        });
      }
    }
  }
  return findings;
};

const isSymlinkTo = (root, path, target) => {
  try {
    return (
      lstatSync(join(root, path)).isSymbolicLink() &&
      posix.basename(readlinkSync(join(root, path))) === target
    );
  } catch (error) {
    throw new CouldNotLook(`cannot inspect ${path}: ${error.message}`);
  }
};

export const checkAgentConfig = (root) => {
  const tracked = trackedFiles(root);
  const index = repositoryIndex(tracked);
  const findings = [];
  const claudeFiles = [...tracked].filter(
    (path) => path === "CLAUDE.md" || path.endsWith("/CLAUDE.md"),
  );
  for (const path of claudeFiles.sort()) {
    if (isSymlinkTo(root, path, "AGENTS.md")) continue;
    findings.push(...checkClaudeMd(path, read(root, path), index));
  }
  const settingsFiles = [...tracked].filter((path) =>
    /(?:^|\/)\.claude\/settings(?:\.local)?\.json$/u.test(path),
  );
  for (const path of settingsFiles.sort()) {
    if (path.endsWith("settings.local.json"))
      findings.push(`${path}: personal settings are never tracked`);
    else findings.push(...checkSettings(path, read(root, path), tracked));
  }
  return { files: [...claudeFiles, ...settingsFiles], findings };
};

const main = (argv) => {
  const rootFlag = argv.indexOf("--root");
  const root = resolve(
    rootFlag >= 0
      ? argv[rootFlag + 1]
      : join(dirname(fileURLToPath(import.meta.url)), "../.."),
  );
  try {
    const { files, findings } = checkAgentConfig(normalize(root));
    for (const finding of findings) console.error(finding);
    if (findings.length > 0) {
      console.error(
        `check-agent-config: ${findings.length} finding(s) in ${files.length} file(s)`,
      );
      return 1;
    }
    console.log(`check-agent-config: ${files.length} file(s) clean`);
    return 0;
  } catch (error) {
    if (!(error instanceof CouldNotLook)) throw error;
    console.error(`check-agent-config: could not look: ${error.message}`);
    return 2;
  }
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
