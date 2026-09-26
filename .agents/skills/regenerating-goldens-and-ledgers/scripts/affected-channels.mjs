#!/usr/bin/env node
// Map changed files to the generated-artifact channels they make stale, and
// print the check and sync commands for each. The channel table lives next to
// this file in channels.json; `--verify-table` proves the table still matches
// the tree (every generator, ledger and generated file is claimed, every
// declared path, script, command and CI step exists).
//
// Exit codes:
//   0  looked; the report lists the affected channels, or says none are
//   1  looked and found problems (an unclaimed generator or generated file,
//      or a table that no longer matches the tree)
//   2  could not look (bad arguments, unreadable table, git diff or
//      git ls-files failed, unknown base ref)

import { spawnSync } from "node:child_process";
import {
  existsSync,
  readFileSync,
  statSync,
  openSync,
  readSync,
  closeSync,
} from "node:fs";
import { dirname, join, posix, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const defaultRoot = resolve(here, "../../../..");
const defaultTable = join(here, "channels.json");

const EXIT_OK = 0;
const EXIT_PROBLEMS = 1;
const EXIT_COULD_NOT_LOOK = 2;

class CouldNotLook extends Error {}

// ---------------------------------------------------------------- arguments

const usage = `usage: affected-channels.mjs [--base <ref>] [--files <paths...>] [--json]
                            [--root <dir>] [--table <file>]
       affected-channels.mjs --verify-table [--root <dir>] [--table <file>] [--json]

Without --files, the changed set is the working tree against
merge-base(<ref>, HEAD) (default ref HEAD), plus untracked files.`;

export const parseArguments = (argv) => {
  const options = {
    base: "HEAD",
    files: undefined,
    json: false,
    verifyTable: false,
    root: defaultRoot,
    table: defaultTable,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const argument = argv[index];
    const value = () => {
      const next = argv[index + 1];
      if (next === undefined || next.startsWith("--")) {
        throw new CouldNotLook(`${argument} needs a value\n${usage}`);
      }
      index += 1;
      return next;
    };
    if (argument === "--base") options.base = value();
    else if (argument === "--root") options.root = resolve(value());
    else if (argument === "--table") options.table = resolve(value());
    else if (argument === "--json") options.json = true;
    else if (argument === "--verify-table") options.verifyTable = true;
    else if (argument === "--files") {
      options.files = [];
      while (index + 1 < argv.length && !argv[index + 1].startsWith("--")) {
        options.files.push(argv[index + 1]);
        index += 1;
      }
    } else if (argument === "--help" || argument === "-h") {
      options.help = true;
    } else {
      throw new CouldNotLook(`unknown argument ${argument}\n${usage}`);
    }
  }
  return options;
};

// -------------------------------------------------------------------- globs

const globCache = new Map();

// `**/` spans zero or more directories, `**` anything, `*` and `?` stay
// inside one path segment, `{a,b}` is alternation.
export const globToRegExp = (glob) => {
  const cached = globCache.get(glob);
  if (cached) return cached;
  let source = "";
  let braces = 0;
  for (let index = 0; index < glob.length; index += 1) {
    const character = glob[index];
    if (character === "*") {
      if (glob[index + 1] === "*") {
        if (glob[index + 2] === "/") {
          source += "(?:.*/)?";
          index += 2;
        } else {
          source += ".*";
          index += 1;
        }
      } else {
        source += "[^/]*";
      }
    } else if (character === "?") source += "[^/]";
    else if (character === "{") {
      braces += 1;
      source += "(?:";
    } else if (character === "}" && braces > 0) {
      braces -= 1;
      source += ")";
    } else if (character === "," && braces > 0) source += "|";
    else source += character.replace(/[.+^$()|[\]\\]/u, "\\$&");
  }
  const expression = new RegExp(`^${source}$`, "u");
  globCache.set(glob, expression);
  return expression;
};

export const matchesGlob = (file, glob) => globToRegExp(glob).test(file);

const isGlob = (pattern) => /[*?{]/u.test(pattern);

// -------------------------------------------------------------------- table

export const loadTable = (tablePath) => {
  let table;
  try {
    table = JSON.parse(readFileSync(tablePath, "utf8"));
  } catch (error) {
    throw new CouldNotLook(
      `could not read the channel table ${tablePath}: ${error.message}`,
    );
  }
  if (!Array.isArray(table.channels)) {
    throw new CouldNotLook(
      `the channel table ${tablePath} has no "channels" array`,
    );
  }
  table.inputSets ??= {};
  table.ignored ??= [];
  return table;
};

const expandSets = (patterns, table, seen = new Set()) => {
  const out = [];
  for (const pattern of patterns ?? []) {
    if (pattern.startsWith("@")) {
      if (seen.has(pattern)) continue;
      const set = table.inputSets[pattern];
      if (!set) throw new CouldNotLook(`unknown input set ${pattern}`);
      out.push(...expandSets(set, table, new Set([...seen, pattern])));
    } else out.push(pattern);
  }
  return out;
};

// ----------------------------------------------------------------- the tree

const run = (command, args, cwd) =>
  spawnSync(command, args, {
    cwd,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
  });

export const createTree = (root) => {
  const text = new Map();
  const read = (file) => {
    if (!text.has(file)) {
      try {
        text.set(file, readFileSync(join(root, file), "utf8"));
      } catch {
        text.set(file, undefined);
      }
    }
    return text.get(file);
  };
  const exists = (file) => existsSync(join(root, file));
  let tracked;
  const trackedFiles = () => {
    if (tracked) return tracked;
    const result = run("git", ["ls-files", "-z"], root);
    if (result.status !== 0) {
      throw new CouldNotLook(
        `could not list tracked files (git ls-files in ${root}): ${(result.stderr || result.error?.message || "").trim()}`,
      );
    }
    tracked = result.stdout.split("\0").filter(Boolean);
    return tracked;
  };
  return { root, read, exists, trackedFiles };
};

// Aiken module index: module name (hyphens normalized to underscores) to
// file. `lib/<m>.ak` and `validators/<m>.ak` both answer to `<m>`.
const aikenIndex = (tree) => {
  if (tree.aikenModules) return tree.aikenModules;
  const modules = new Map();
  for (const file of tree.trackedFiles()) {
    const match = /^onchain\/aiken\/(lib|validators)\/(.+)\.ak$/u.exec(file);
    if (!match) continue;
    const name = match[2].replaceAll("-", "_");
    const key = `${match[1]}:${name}`;
    modules.set(key, file);
  }
  tree.aikenModules = modules;
  return modules;
};

export const resolveAikenModule = (tree, moduleName) => {
  const modules = aikenIndex(tree);
  const name = moduleName.replaceAll("-", "_");
  return modules.get(`lib:${name}`) ?? modules.get(`validators:${name}`);
};

const directAikenImports = (tree, file) => {
  const source = tree.read(file);
  if (source === undefined) return [];
  const out = [];
  for (const match of source.matchAll(/^\s*use\s+([a-z0-9_/]+)/gmu)) {
    const target = resolveAikenModule(tree, match[1]);
    if (target) out.push(target);
  }
  return out;
};

const aikenClosure = (tree, roots) => {
  const seen = new Set();
  const queue = [...roots];
  while (queue.length) {
    const file = queue.pop();
    if (seen.has(file)) continue;
    seen.add(file);
    queue.push(...directAikenImports(tree, file));
  }
  return seen;
};

// Workspace packages under demo/: package name to directory, and each
// package's runtime workspace dependencies (dependencies and
// peerDependencies; devDependencies do not reach a generator's output).
const workspace = (tree) => {
  if (tree.workspace) return tree.workspace;
  const byName = new Map();
  const manifests = new Map();
  for (const file of tree.trackedFiles()) {
    const match = /^(demo\/[^/]+)\/package\.json$/u.exec(file);
    if (!match) continue;
    try {
      const manifest = JSON.parse(tree.read(file));
      byName.set(manifest.name, match[1]);
      manifests.set(match[1], manifest);
    } catch {
      // An unreadable manifest is reported by --verify-table via producers.
    }
  }
  tree.workspace = { byName, manifests };
  return tree.workspace;
};

export const producerClosure = (tree, directories) => {
  const { byName, manifests } = workspace(tree);
  const seen = new Set();
  const queue = [...directories];
  while (queue.length) {
    const directory = queue.pop();
    if (seen.has(directory)) continue;
    seen.add(directory);
    const manifest = manifests.get(directory);
    if (!manifest) continue;
    for (const name of Object.keys({
      ...manifest.dependencies,
      ...manifest.peerDependencies,
    })) {
      const dependency = byName.get(name);
      if (dependency) queue.push(dependency);
    }
  }
  return [...seen].sort();
};

// -------------------------------------------------------------- resolution

// For one channel, the rules that decide whether a changed file makes it
// stale. Each rule carries the reason printed next to the file.
export const channelTriggers = (tree, table, channel) => {
  const globs = [];
  const outputGlobs = [];
  const files = new Map();
  const addGlob = (glob, reason) => globs.push({ glob, reason });
  const addFile = (file, reason) => {
    if (!files.has(file)) files.set(file, reason);
  };
  for (const generator of channel.generators ?? [])
    addFile(generator, "generator");
  for (const pattern of expandSets(channel.inputs, table))
    addGlob(pattern, "declared input");
  for (const pattern of channel.outputs ?? []) outputGlobs.push(pattern);
  for (const directory of producerClosure(tree, channel.producers ?? [])) {
    addGlob(`${directory}/src/**`, `producer package ${directory}`);
    addGlob(`${directory}/package.json`, `producer package ${directory}`);
  }
  if (channel.aikenImportsOfOutputs) {
    for (const output of channel.outputs ?? []) {
      if (!output.endsWith(".ak") || isGlob(output)) continue;
      for (const imported of directAikenImports(tree, output)) {
        addFile(imported, `imported by ${posix.basename(output)}`);
      }
    }
  }
  if (channel.aikenClosure) {
    for (const file of aikenClosure(tree, channel.aikenClosure)) {
      addFile(
        file,
        `in the import closure of ${channel.aikenClosure.map((f) => posix.basename(f)).join(", ")}`,
      );
    }
  }
  if (channel.ledger) {
    const modules = ledgerModules(tree, channel.ledger);
    const roots = modules.map((m) => m.file).filter(Boolean);
    for (const file of aikenClosure(tree, roots)) {
      addFile(file, "in the import closure of a measured module");
    }
  }
  const exclude = expandSets(channel.exclude, table);
  return { globs, outputGlobs, files, exclude };
};

export const ledgerModules = (tree, ledgerPath) => {
  const source = tree.read(ledgerPath);
  if (source === undefined) return [];
  let ledger;
  try {
    ledger = JSON.parse(source);
  } catch {
    return [];
  }
  const names = new Set();
  for (const entry of ledger.modules ?? []) {
    if (typeof entry?.module === "string") names.add(entry.module);
  }
  return [...names].map((name) => ({
    name,
    file: resolveAikenModule(tree, name),
  }));
};

// `viaRegeneration` is set when the file is another channel's output rather
// than a file the contributor changed: two channels that write the same file
// do not make each other stale by writing it, so outputs only count for
// changed files.
const triggerReason = (triggers, file, viaRegeneration = false) => {
  if (
    !viaRegeneration &&
    triggers.outputGlobs.some((glob) => matchesGlob(file, glob))
  ) {
    return "output: hand edit or regeneration";
  }
  if (triggers.exclude.some((glob) => matchesGlob(file, glob))) {
    // An excluded file still counts when it is named explicitly.
    return triggers.files.get(file);
  }
  if (triggers.files.has(file)) return triggers.files.get(file);
  const hit = triggers.globs.find(({ glob }) => matchesGlob(file, glob));
  return hit?.reason;
};

// ---------------------------------------------------------------- discovery

const generatorPatterns = [
  "**/scripts/generate-*",
  "**/scripts/sync-*",
  "**/scripts/write-*",
  "onchain/aiken/scripts/verify-*-exec-ledger-*.mjs",
];
const ledgerPatterns = [
  "onchain/aiken/scripts/*-exec-ledger-*.json",
  "docs/fault-proofs/size-plans/*-fit-ledger.json",
];
const generatedNamePatterns = [
  "**/*.generated.*",
  "**/*-golden.test.ak",
  "**/*-generated.ak",
  "**/generated-*",
];
const markerExtensions = /\.(ak|ts|mts|mjs|js|json|md|ya?ml|toml|sh)$/u;
const markerText = /generated by|do not edit/iu;
const writerEnvPattern = /\b(MIDGARD_(?:WRITE|SYNC|REGENERATE)_[A-Z0-9_]+)\b/gu;
const packageScriptPattern = /^(?:fixtures|docs|deployment):|:sync$|generate/u;
const outsideDiscovery = (file) =>
  file.startsWith(".agents/") || file.includes("node_modules/");

const isTestFile = (file) => /\.test\.[cm]?[jt]s$/u.test(file);

export const looksLikeGenerator = (file) =>
  !isTestFile(file) &&
  !outsideDiscovery(file) &&
  generatorPatterns.some((glob) => matchesGlob(file, glob));

const looksLikeLedger = (file) =>
  ledgerPatterns.some((glob) => matchesGlob(file, glob));

const looksGeneratedByName = (file) =>
  !outsideDiscovery(file) &&
  generatedNamePatterns.some((glob) => matchesGlob(file, glob));

const readHead = (tree, file) => {
  try {
    const path = join(tree.root, file);
    if (!statSync(path).isFile()) return "";
    const descriptor = openSync(path, "r");
    try {
      const buffer = Buffer.alloc(400);
      const length = readSync(descriptor, buffer, 0, 400, 0);
      return buffer.subarray(0, length).toString("utf8");
    } finally {
      closeSync(descriptor);
    }
  } catch {
    return "";
  }
};

const carriesMarker = (tree, file) =>
  !outsideDiscovery(file) &&
  markerExtensions.test(file) &&
  markerText.test(readHead(tree, file));

const claims = (table) => {
  const generators = new Set();
  const outputs = [];
  const ledgers = new Set();
  const packageScripts = new Set();
  const writerEnv = new Set();
  for (const channel of table.channels) {
    for (const generator of channel.generators ?? []) generators.add(generator);
    for (const output of channel.outputs ?? []) outputs.push(output);
    if (channel.ledger) ledgers.add(channel.ledger);
    for (const script of channel.packageScripts ?? [])
      packageScripts.add(script);
    for (const name of channel.writerEnv ?? []) writerEnv.add(name);
  }
  for (const entry of table.ignored) {
    if (entry.path) generators.add(entry.path);
    if (entry.packageScript) packageScripts.add(entry.packageScript);
  }
  const isOutput = (file) => outputs.some((glob) => matchesGlob(file, glob));
  return { generators, outputs, isOutput, ledgers, packageScripts, writerEnv };
};

// A changed or tracked file that looks generated or generating but that the
// table does not claim. Shared by the mapping mode and --verify-table.
export const unclaimed = (tree, table, files) => {
  const claimed = claims(table);
  const problems = [];
  for (const file of files) {
    // A deleted file cannot be regenerated; a table entry still naming it
    // is caught by --verify-table.
    if (!tree.exists(file)) continue;
    if (looksLikeGenerator(file) && !claimed.generators.has(file)) {
      problems.push(
        `${file} looks like a generator but no channel claims it (add it to channels.json)`,
      );
    } else if (
      looksLikeLedger(file) &&
      !claimed.ledgers.has(file) &&
      !claimed.isOutput(file)
    ) {
      problems.push(
        `${file} looks like a ledger but no channel claims it (add it to channels.json)`,
      );
    } else if (
      (looksGeneratedByName(file) ||
        (tree.exists(file) && carriesMarker(tree, file))) &&
      !claimed.isOutput(file) &&
      !claimed.generators.has(file)
    ) {
      problems.push(
        `${file} looks generated but no channel claims it as an output (add it to channels.json)`,
      );
    }
  }
  return problems;
};

// ---------------------------------------------------------------- mapping

export const changedFiles = (root, base) => {
  const fail = (what, result) =>
    new CouldNotLook(
      `could not read git diff: ${what} failed in ${root}: ${(result.stderr || result.error?.message || `exit ${result.status}`).trim()}`,
    );
  const verify = run(
    "git",
    ["rev-parse", "--verify", "--quiet", `${base}^{commit}`],
    root,
  );
  if (verify.status !== 0) throw fail(`resolving base ${base}`, verify);
  let mergeBase = verify.stdout.trim();
  if (base !== "HEAD") {
    const result = run("git", ["merge-base", base, "HEAD"], root);
    if (result.status !== 0) throw fail(`git merge-base ${base} HEAD`, result);
    mergeBase = result.stdout.trim();
  }
  const diff = run(
    "git",
    ["diff", "--name-only", "--no-renames", "-z", mergeBase, "--"],
    root,
  );
  if (diff.status !== 0) throw fail("git diff", diff);
  const untracked = run(
    "git",
    ["ls-files", "--others", "--exclude-standard", "-z"],
    root,
  );
  if (untracked.status !== 0) throw fail("git ls-files --others", untracked);
  const files = new Set(
    [...diff.stdout.split("\0"), ...untracked.stdout.split("\0")].filter(
      Boolean,
    ),
  );
  return { mergeBase, files: [...files].sort() };
};

const normalizeFile = (root, file) => {
  const absolute = resolve(process.cwd(), file);
  const relative = posix.normalize(
    (absolute.startsWith(`${root}/`)
      ? absolute.slice(root.length + 1)
      : file
    ).replaceAll("\\", "/"),
  );
  return relative.replace(/^\.\//u, "");
};

export const affectedChannels = (tree, table, changed) => {
  const affected = new Map();
  const triggers = new Map(
    table.channels.map((channel) => [
      channel.id,
      channelTriggers(tree, table, channel),
    ]),
  );
  // Changed files first, then outputs of affected channels, until nothing
  // new is affected: a regenerated env/*.ak or generated-deployment-profiles.ts
  // is itself an input to other channels.
  let frontier = changed.map((file) => ({ file, via: undefined }));
  const seenFrontier = new Set(changed);
  while (frontier.length) {
    const next = [];
    for (const channel of table.channels) {
      const channelTrigger = triggers.get(channel.id);
      for (const { file, via } of frontier) {
        if (via === channel.id) continue;
        const reason = triggerReason(channelTrigger, file, via !== undefined);
        if (!reason) continue;
        const entry = affected.get(channel.id) ?? { channel, reasons: [] };
        entry.reasons.push(
          via
            ? `${file} (${reason}; regenerated by ${via})`
            : `${file} (${reason})`,
        );
        if (!affected.has(channel.id)) {
          affected.set(channel.id, entry);
          const downstream = [
            ...(channel.outputs ?? []).filter((output) => !isGlob(output)),
            ...(channel.outputs ?? [])
              .filter(isGlob)
              .flatMap((glob) =>
                tree.trackedFiles().filter((f) => matchesGlob(f, glob)),
              ),
          ];
          for (const output of downstream) {
            const key = `${channel.id}\0${output}`;
            if (seenFrontier.has(key)) continue;
            seenFrontier.add(key);
            next.push({ file: output, via: channel.id });
          }
          for (const id of channel.then ?? []) {
            const key = `${channel.id}\0then\0${id}`;
            if (seenFrontier.has(key)) continue;
            seenFrontier.add(key);
            const target = table.channels.find((c) => c.id === id);
            if (target && !affected.has(id)) {
              affected.set(id, {
                channel: target,
                reasons: [`runs after ${channel.id}`],
              });
            }
          }
        }
      }
    }
    frontier = next;
  }
  // Report in table order, which is also the order to run them in.
  return table.channels
    .filter((c) => affected.has(c.id))
    .map((c) => affected.get(c.id));
};

// ------------------------------------------------------------ verification

const parsePackageScript = (reference) => {
  const index = reference.indexOf(":");
  return {
    directory: reference.slice(0, index),
    script: reference.slice(index + 1),
  };
};

const packageScripts = (tree, directory) => {
  const source = tree.read(`${directory}/package.json`);
  if (source === undefined) return undefined;
  try {
    return JSON.parse(source).scripts ?? {};
  } catch {
    return undefined;
  }
};

const makeTargets = (tree) => {
  const source = tree.read("Makefile") ?? "";
  return new Set(
    [...source.matchAll(/^([A-Za-z0-9_.-]+):/gmu)].map((m) => m[1]),
  );
};

// Check every command segment names something that exists: a package
// script, a file handed to node, a Makefile target, or an executable path.
export const verifyCommand = (tree, command) => {
  const problems = [];
  for (const rawSegment of command.split(/&&|;/u)) {
    const words = rawSegment.trim().split(/\s+/u).filter(Boolean);
    while (words.length && /^[A-Z_][A-Z0-9_]*=/u.test(words[0])) words.shift();
    if (!words.length) continue;
    const [head, ...rest] = words;
    if (head === "cd") {
      if (!tree.exists(rest[0] ?? ""))
        problems.push(`cd target ${rest[0]} does not exist`);
      tree.cwd = rest[0];
      continue;
    }
    const cwd = tree.cwd ?? ".";
    const at = (file) => posix.normalize(posix.join(cwd, file));
    if (head === "pnpm") {
      const dirIndex = rest.indexOf("--dir");
      const directory =
        dirIndex >= 0
          ? posix.normalize(posix.join(cwd, rest[dirIndex + 1]))
          : cwd;
      const after = rest.filter(
        (_, i) => dirIndex < 0 || (i !== dirIndex && i !== dirIndex + 1),
      );
      const scripts = packageScripts(tree, directory);
      if (!scripts) {
        problems.push(`pnpm --dir ${directory}: no readable package.json`);
        continue;
      }
      if (after[0] === "exec") {
        for (const word of after.slice(1)) {
          if (
            /\.(test\.)?[cm]?[jt]s$/u.test(word) &&
            !tree.exists(posix.join(directory, word))
          ) {
            problems.push(
              `pnpm --dir ${directory} exec: ${word} does not exist`,
            );
          }
        }
        continue;
      }
      const script = after[0] === "run" ? after[1] : after[0];
      if (!script || !(script in scripts)) {
        problems.push(`pnpm --dir ${directory}: no script "${script}"`);
      }
    } else if (head === "node") {
      const file = rest.find((word) => !word.startsWith("-"));
      if (!file || !tree.exists(at(file)))
        problems.push(`node: ${file} does not exist (from ${cwd})`);
    } else if (head === "make") {
      if (!makeTargets(tree).has(rest[0]))
        problems.push(`make: no target ${rest[0]} in Makefile`);
    } else if (head.startsWith("./")) {
      if (!tree.exists(at(head)))
        problems.push(`${head} does not exist (from ${cwd})`);
    } else if (head !== "git") {
      problems.push(
        `unrecognized command "${head}" (the verifier only knows pnpm, node, make, git, cd and ./paths)`,
      );
    }
  }
  delete tree.cwd;
  return problems;
};

// A workflow step's `run` (or the whole step) as text: from its `- name:`
// line to the next list item at the same indentation.
export const workflowStep = (source, stepName) => {
  const lines = source.split("\n");
  const start = lines.findIndex((line) => {
    const match = /^(\s*)- name:\s*(.+?)\s*$/u.exec(line);
    return match && match[2].replace(/^["']|["']$/gu, "") === stepName;
  });
  if (start < 0) return undefined;
  const indent = /^(\s*)/u.exec(lines[start])[1].length;
  let end = start + 1;
  while (end < lines.length) {
    const match = /^(\s*)- /u.exec(lines[end]);
    if (match && match[1].length === indent) break;
    if (/^\S/u.test(lines[end])) break;
    end += 1;
  }
  return lines.slice(start, end).join("\n");
};

const commandTokens = (channel) => {
  const tokens = new Set();
  const check = channel.check?.run ?? "";
  for (const match of check.matchAll(/\brun\s+([A-Za-z0-9:_.-]+)/gu))
    tokens.add(match[1]);
  for (const match of check.matchAll(/pnpm --dir \S+ ([A-Za-z0-9:_.-]+)/gu))
    tokens.add(match[1]);
  for (const generator of channel.generators ?? [])
    tokens.add(posix.basename(generator));
  for (const output of channel.outputs ?? [])
    if (!isGlob(output)) tokens.add(posix.basename(output));
  tokens.delete("run");
  tokens.delete("exec");
  return [...tokens];
};

export const verifyTable = (tree, table) => {
  const problems = [];
  const tracked = tree.trackedFiles();
  const trackedSet = new Set(tracked);
  const ids = new Set();
  const problem = (channel, message) =>
    problems.push(`${channel ? `[${channel.id}] ` : ""}${message}`);
  const matchesSomething = (glob) =>
    isGlob(glob)
      ? tracked.some((file) => matchesGlob(file, glob))
      : trackedSet.has(glob) || tree.exists(glob);

  for (const [name, patterns] of Object.entries(table.inputSets)) {
    for (const pattern of patterns) {
      if (!pattern.startsWith("@") && !matchesSomething(pattern))
        problem(undefined, `input set ${name}: ${pattern} matches nothing`);
    }
  }

  for (const channel of table.channels) {
    if (!channel.id) {
      problem(undefined, "a channel has no id");
      continue;
    }
    if (ids.has(channel.id)) problem(channel, "duplicate channel id");
    ids.add(channel.id);
    if (!channel.summary) problem(channel, "no summary");
    for (const generator of channel.generators ?? []) {
      if (!trackedSet.has(generator))
        problem(channel, `generator ${generator} is not a tracked file`);
    }
    for (const pattern of channel.outputs ?? []) {
      if (!matchesSomething(pattern))
        problem(channel, `output ${pattern} matches no tracked file`);
    }
    if (!channel.outputs?.length) problem(channel, "no outputs");
    let inputs = [];
    try {
      inputs = [
        ...expandSets(channel.inputs, table),
        ...expandSets(channel.exclude, table),
      ];
    } catch (error) {
      problem(channel, error.message);
    }
    for (const pattern of inputs) {
      if (!matchesSomething(pattern))
        problem(channel, `input ${pattern} matches nothing`);
    }
    for (const directory of channel.producers ?? []) {
      if (!workspace(tree).manifests.has(directory))
        problem(channel, `producer ${directory} is not a workspace package`);
    }
    for (const root of channel.aikenClosure ?? []) {
      if (!trackedSet.has(root))
        problem(channel, `Aiken closure root ${root} is not tracked`);
    }
    if (channel.ledger) {
      if (!trackedSet.has(channel.ledger))
        problem(channel, `ledger ${channel.ledger} is not tracked`);
      const modules = ledgerModules(tree, channel.ledger);
      if (!modules.length)
        problem(channel, `ledger ${channel.ledger} lists no measured modules`);
      for (const { name, file } of modules) {
        if (!file)
          problem(channel, `measured module ${name} resolves to no .ak file`);
      }
    }
    for (const reference of channel.packageScripts ?? []) {
      const { directory, script } = parsePackageScript(reference);
      const scripts = packageScripts(tree, directory);
      if (!scripts || !(script in scripts))
        problem(channel, `package script ${reference} does not exist`);
    }
    for (const name of channel.writerEnv ?? []) {
      const sources = [
        ...(channel.generators ?? []),
        ...inputs.filter((p) => !isGlob(p)),
      ];
      const named = sources.some((file) => tree.read(file)?.includes(name));
      if (!named)
        problem(
          channel,
          `writer env ${name} appears in none of the channel's generators or inputs`,
        );
    }
    for (const mode of ["sync", "check"]) {
      const entry = channel[mode];
      if (!entry || (!entry.run && !entry.manual && !entry.none)) {
        problem(channel, `${mode} needs one of run, manual or none`);
        continue;
      }
      if (entry.run)
        for (const message of verifyCommand(tree, entry.run))
          problem(channel, `${mode}: ${message}`);
    }
    if (
      !channel.ci ||
      (!channel.ci.none && !(channel.ci.workflow && channel.ci.step))
    ) {
      problem(channel, "ci needs workflow and step, or none");
    } else if (channel.ci.workflow) {
      const source = tree.read(channel.ci.workflow);
      const step =
        source === undefined
          ? undefined
          : workflowStep(source, channel.ci.step);
      if (source === undefined)
        problem(channel, `workflow ${channel.ci.workflow} does not exist`);
      else if (step === undefined)
        problem(
          channel,
          `no step "${channel.ci.step}" in ${channel.ci.workflow}`,
        );
      else if (channel.ci.via) {
        const packageDirectory = /^(demo\/[^/]+)\//u.exec(channel.ci.via)?.[1];
        if (!trackedSet.has(channel.ci.via))
          problem(channel, `ci.via ${channel.ci.via} is not tracked`);
        if (!packageDirectory || !step.includes(`--dir ${packageDirectory}`)) {
          problem(
            channel,
            `step "${channel.ci.step}" does not run the package that holds ${channel.ci.via}`,
          );
        }
      } else if (
        !commandTokens(channel).some((token) => step.includes(token))
      ) {
        problem(
          channel,
          `step "${channel.ci.step}" mentions none of the channel's check command, generators or outputs`,
        );
      }
    }
    for (const id of channel.then ?? []) {
      if (!table.channels.some((c) => c.id === id))
        problem(channel, `then names unknown channel ${id}`);
    }
    // Every Aiken path a generator names must be one of its outputs, so a
    // generator that starts writing a new module cannot drift out of the table.
    for (const generator of channel.generators ?? []) {
      const source = tree.read(generator);
      if (source === undefined) continue;
      const code = source
        .replace(/\/\*[\s\S]*?\*\//gu, "")
        .replace(/^\s*\/\/.*$/gmu, "");
      for (const match of code.matchAll(
        /["'`](onchain\/aiken\/[^"'`]+\.ak)["'`]/gu,
      )) {
        if (
          !(channel.outputs ?? []).some((glob) => matchesGlob(match[1], glob))
        ) {
          problem(
            channel,
            `generator ${generator} names ${match[1]}, which is not among the channel's outputs`,
          );
        }
      }
    }
  }

  for (const entry of table.ignored) {
    if (!entry.why)
      problem(
        undefined,
        `ignored entry ${entry.path ?? entry.packageScript} has no reason`,
      );
    if (entry.path && !trackedSet.has(entry.path))
      problem(undefined, `ignored path ${entry.path} is gone; drop the entry`);
    if (entry.packageScript) {
      const { directory, script } = parsePackageScript(entry.packageScript);
      const scripts = packageScripts(tree, directory);
      if (!scripts || !(script in scripts))
        problem(
          undefined,
          `ignored package script ${entry.packageScript} is gone; drop the entry`,
        );
    }
  }

  // Coverage: everything in the tree that looks generated or generating.
  const claimed = claims(table);
  problems.push(...unclaimed(tree, table, tracked));
  for (const directory of ["demo", ...workspace(tree).manifests.keys()]) {
    const scripts = packageScripts(tree, directory);
    if (!scripts) continue;
    for (const script of Object.keys(scripts)) {
      if (!packageScriptPattern.test(script)) continue;
      if (!claimed.packageScripts.has(`${directory}:${script}`)) {
        problems.push(
          `package script ${directory}:${script} looks like a generator or check but no channel claims it`,
        );
      }
    }
  }
  const discoveredEnv = new Set();
  for (const file of tracked) {
    if (
      !file.startsWith("demo/") ||
      outsideDiscovery(file) ||
      !/\.(ts|mts|mjs|js)$/u.test(file)
    )
      continue;
    for (const match of (tree.read(file) ?? "").matchAll(writerEnvPattern))
      discoveredEnv.add(match[1]);
  }
  for (const name of [...discoveredEnv].sort()) {
    if (!claimed.writerEnv.has(name))
      problems.push(
        `writer env ${name} is used in demo/ but no channel claims it`,
      );
  }
  return problems;
};

// ------------------------------------------------------------------ output

const describeCi = (ci) =>
  ci.none
    ? `none: ${ci.none}`
    : `${ci.workflow} / ${ci.step}${ci.via ? ` (via ${ci.via})` : ""}`;

const describeMode = (entry) =>
  entry.run ??
  (entry.manual ? `manual: ${entry.manual}` : `none: ${entry.none}`);

const renderMapping = ({ source, files, affected, problems }) => {
  const lines = [
    `affected-channels: ${files.length} changed file(s) ${source}`,
  ];
  if (!affected.length)
    lines.push("looked: no generated-artifact channel is affected.");
  else {
    lines.push(`${affected.length} channel(s) affected, in run order:`);
    for (const { channel, reasons } of affected) {
      lines.push("", `${channel.id}  [${channel.kind ?? "channel"}]`);
      const shown = reasons.slice(0, 3);
      for (const reason of shown) lines.push(`  because  ${reason}`);
      if (reasons.length > shown.length)
        lines.push(`  because  ... and ${reasons.length - shown.length} more`);
      lines.push(`  check    ${describeMode(channel.check)}`);
      lines.push(`  sync     ${describeMode(channel.sync)}`);
      lines.push(`  ci       ${describeCi(channel.ci)}`);
    }
  }
  for (const message of problems) lines.push(`PROBLEM: ${message}`);
  return lines.join("\n");
};

export const main = (argv, io = { out: console.log, err: console.error }) => {
  let options;
  try {
    options = parseArguments(argv);
    if (options.help) {
      io.out(usage);
      return EXIT_OK;
    }
    const table = loadTable(options.table);
    const tree = createTree(options.root);
    if (options.verifyTable) {
      const problems = verifyTable(tree, table);
      if (options.json)
        io.out(
          JSON.stringify(
            { status: problems.length ? "problems" : "ok", problems },
            null,
            2,
          ),
        );
      else if (problems.length)
        for (const message of problems) io.out(`PROBLEM: ${message}`);
      else
        io.out(
          `verify-table: ok, ${table.channels.length} channels cover every generator, ledger and generated file.`,
        );
      return problems.length ? EXIT_PROBLEMS : EXIT_OK;
    }
    let files;
    let source;
    if (options.files) {
      files = [
        ...new Set(
          options.files.map((file) => normalizeFile(options.root, file)),
        ),
      ].sort();
      source = "(from --files)";
    } else {
      const changed = changedFiles(options.root, options.base);
      files = changed.files;
      source = `(working tree against ${changed.mergeBase.slice(0, 12)}, merge-base of ${options.base} and HEAD, plus untracked)`;
    }
    const affected = affectedChannels(tree, table, files);
    const problems = unclaimed(tree, table, files);
    if (options.json) {
      io.out(
        JSON.stringify(
          {
            status: problems.length
              ? "problems"
              : affected.length
                ? "affected"
                : "none-affected",
            changedFiles: files,
            problems,
            channels: affected.map(({ channel, reasons }) => ({
              id: channel.id,
              kind: channel.kind,
              reasons,
              check: channel.check,
              sync: channel.sync,
              ci: channel.ci,
            })),
          },
          null,
          2,
        ),
      );
    } else io.out(renderMapping({ source, files, affected, problems }));
    return problems.length ? EXIT_PROBLEMS : EXIT_OK;
  } catch (error) {
    if (error instanceof CouldNotLook) {
      if (options?.json)
        io.out(
          JSON.stringify(
            { status: "could-not-look", error: error.message },
            null,
            2,
          ),
        );
      io.err(`could not look: ${error.message}`);
      return EXIT_COULD_NOT_LOOK;
    }
    throw error;
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
