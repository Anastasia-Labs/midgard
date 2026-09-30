import { spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));

const defaultRoot = resolve(here, "../../../..");

const defaultTable = join(here, "channels.json");

export const EXIT_OK = 0;

export const EXIT_PROBLEMS = 1;

export const EXIT_COULD_NOT_LOOK = 2;

export class CouldNotLook extends Error {}

// ---------------------------------------------------------------- arguments

export const usage = `usage: affected-channels.mjs [--base <ref>] [--files <paths...>] [--json]
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

export const globCache = new Map();

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

export const isGlob = (pattern) => /[*?{]/u.test(pattern);

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

export const expandSets = (patterns, table, seen = new Set()) => {
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

export const run = (command, args, cwd) =>
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

export const directAikenImports = (tree, file) => {
  const source = tree.read(file);
  if (source === undefined) return [];
  const out = [];
  for (const match of source.matchAll(/^\s*use\s+([a-z0-9_/]+)/gmu)) {
    const target = resolveAikenModule(tree, match[1]);
    if (target) out.push(target);
  }
  return out;
};

export const aikenClosure = (tree, roots) => {
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
export const workspace = (tree) => {
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
