import { spawnSync } from "node:child_process";

export const EXIT = Object.freeze({
  passed: 0,
  failed: 1,
  noRun: 2,
  queryFailed: 3,
  pending: 4,
  usage: 64,
});

export const DEFAULT_REPO = "Anastasia-Labs/midgard";

// GitHub evaluates a pull request's `paths:` filter against at most the first
// 300 changed files it lists. Past that, whether a path-filtered workflow
// should have run cannot be decided from here.
export const PATH_FILTER_FILE_LIMIT = 300;

export const FAILED_CONCLUSIONS = new Set([
  "failure",
  "cancelled",
  "timed_out",
  "startup_failure",
  "action_required",
  "stale",
]);

export const PASSED_CONCLUSIONS = new Set(["success", "neutral"]);

export class QueryError extends Error {}

// Each gh call is bounded, so a hung connection is a failed look (exit 3),
// never a wait without end.
export const GH_TIMEOUT_MS = 120_000;

export const ghRunner = (args) =>
  spawnSync("gh", args, {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
    timeout: GH_TIMEOUT_MS,
  });

export const call = (run, args) => {
  let result;
  try {
    result = run(args);
  } catch (error) {
    throw new QueryError(`gh ${args.join(" ")}: ${String(error)}`);
  }
  if (result === undefined || result === null) {
    throw new QueryError(`gh ${args.join(" ")}: no result`);
  }
  if (result.error !== undefined && result.error !== null) {
    throw new QueryError(`gh ${args.join(" ")}: ${String(result.error)}`);
  }
  if (result.status !== 0) {
    const detail = String(result.stderr ?? "").trim() || "nonzero exit";
    throw new QueryError(`gh ${args.join(" ")}: ${detail}`);
  }
  return String(result.stdout ?? "");
};

export const callJson = (run, args) => {
  const text = call(run, args);
  try {
    return JSON.parse(text);
  } catch {
    throw new QueryError(`gh ${args.join(" ")}: response is not JSON`);
  }
};

export const callLines = (run, args) =>
  call(run, args)
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line.length > 0);

// ---------------------------------------------------------------------------
// Workflow trigger parsing. A deliberately small reader for the `on:` block:
// scalar, flow list, or a mapping of events whose filters are flow lists or
// dash lists. Anything else is reported as not understood, never guessed.

const stripComment = (line) => {
  let quote = null;
  for (let i = 0; i < line.length; i += 1) {
    const ch = line[i];
    if (quote !== null) {
      if (ch === quote) quote = null;
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === "#" && (i === 0 || /\s/u.test(line[i - 1]))) {
      return line.slice(0, i);
    }
  }
  return line;
};

const unquote = (value) => value.trim().replace(/^(["'])([\s\S]*)\1$/u, "$2");

const indentOf = (line) => line.length - line.trimStart().length;

const parseFlowList = (text) => {
  const inner = text.trim();
  if (!inner.startsWith("[") || !inner.endsWith("]")) return null;
  const body = inner.slice(1, -1).trim();
  if (body === "") return [];
  return body.split(",").map(unquote);
};

const FILTER_KEYS = new Map([
  ["branches", "branches"],
  ["branches-ignore", "branchesIgnore"],
  ["paths", "paths"],
  ["paths-ignore", "pathsIgnore"],
  ["types", "types"],
  ["tags", "tags"],
  ["tags-ignore", "tagsIgnore"],
]);

export const parseWorkflow = (text) => {
  const lines = text.split(/\r?\n/u).map(stripComment);
  const nameLine = lines.find((line) => /^name:\s*\S/u.test(line));
  const name = nameLine === undefined ? null : unquote(nameLine.slice(5));
  const onIndex = lines.findIndex((line) => /^(on|"on"|'on'):/u.test(line));
  if (onIndex === -1) return { name, error: "no top-level `on:` key" };
  const inline = lines[onIndex].replace(/^(on|"on"|'on'):/u, "").trim();
  if (inline !== "") {
    const list = parseFlowList(inline);
    if (list !== null) {
      return { name, events: Object.fromEntries(list.map((e) => [e, {}])) };
    }
    if (/^[a-z_]+$/u.test(inline)) return { name, events: { [inline]: {} } };
    return { name, error: `inline \`on: ${inline}\` is not understood` };
  }

  const block = [];
  for (const line of lines.slice(onIndex + 1)) {
    if (line.trim() === "") continue;
    if (indentOf(line) === 0) break;
    block.push(line);
  }
  if (block.length === 0) return { name, error: "empty `on:` block" };

  const eventIndent = indentOf(block[0]);
  const events = {};
  let event = null;
  let filter = null;
  for (const line of block) {
    const indent = indentOf(line);
    const trimmed = line.trim();
    if (indent === eventIndent) {
      const match = /^([A-Za-z_]+):\s*(.*)$/u.exec(trimmed);
      if (match === null) {
        return { name, error: `\`on:\` entry not understood: ${trimmed}` };
      }
      const rest = match[2].trim();
      if (rest !== "" && rest !== "null" && rest !== "{}" && rest !== "~") {
        return { name, error: `\`${match[1]}: ${rest}\` is not understood` };
      }
      event = match[1];
      events[event] = {};
      filter = null;
      continue;
    }
    if (event === null || indent < eventIndent) {
      return { name, error: `\`on:\` block not understood at: ${trimmed}` };
    }
    // Only push and pull_request bodies gate the runs this script judges;
    // other events (workflow_dispatch inputs, schedule crons) nest freely.
    if (event !== "push" && event !== "pull_request") continue;
    if (trimmed.startsWith("- ")) {
      if (filter === null) {
        return { name, error: `list item outside a filter: ${trimmed}` };
      }
      const item = trimmed.slice(2).trim();
      if (/^[&*{[|>!]/u.test(item)) {
        return { name, error: `list item not understood: ${trimmed}` };
      }
      events[event][filter].push(unquote(item));
      continue;
    }
    const match = /^([A-Za-z_-]+):\s*(.*)$/u.exec(trimmed);
    if (match === null) {
      return { name, error: `\`on:\` block not understood at: ${trimmed}` };
    }
    const key = FILTER_KEYS.get(match[1]);
    if (key === undefined) {
      return { name, error: `\`${event}.${match[1]}\` is not understood` };
    }
    const rest = match[2].trim();
    if (/^[&*{|>!]/u.test(rest)) {
      return {
        name,
        error: `\`${event}.${match[1]}: ${rest}\` is not understood`,
      };
    }
    if (rest === "") {
      events[event][key] = [];
      filter = key;
    } else {
      const list = parseFlowList(rest);
      events[event][key] = list ?? [unquote(rest)];
      filter = null;
    }
  }
  return { name, events };
};

// ---------------------------------------------------------------------------
// GitHub filter pattern matching (`*`, `**`, `?`, leading `!` negation; the
// last matching pattern decides).

const globToRegExp = (pattern) => {
  let out = "";
  for (let i = 0; i < pattern.length; i += 1) {
    const ch = pattern[i];
    if (ch === "*" && pattern[i + 1] === "*") {
      if (pattern[i + 2] === "/") {
        out += "(?:.*/)?";
        i += 2;
      } else {
        out += ".*";
        i += 1;
      }
    } else if (ch === "*") {
      out += "[^/]*";
    } else if (ch === "?") {
      out += "[^/]";
    } else {
      out += ch.replace(/[.+^${}()|[\]\\]/u, "\\$&");
    }
  }
  return new RegExp(`^${out}$`, "u");
};

export const matchesPatterns = (value, patterns) => {
  let included = false;
  for (const raw of patterns) {
    const negated = raw.startsWith("!");
    const pattern = negated ? raw.slice(1) : raw;
    if (globToRegExp(pattern).test(value)) included = !negated;
  }
  return included;
};

// true / false, or null when the changed files are unknown.
export const pathsAllow = (filters, files) => {
  if (filters.paths === undefined && filters.pathsIgnore === undefined) {
    return true;
  }
  if (files === null) return null;
  if (filters.paths !== undefined) {
    return files.some((file) => matchesPatterns(file, filters.paths));
  }
  return files.some((file) => !matchesPatterns(file, filters.pathsIgnore));
};

export const branchAllows = (filters, branch) => {
  if (filters.branches !== undefined) {
    return matchesPatterns(branch, filters.branches);
  }
  if (filters.branchesIgnore !== undefined) {
    return !matchesPatterns(branch, filters.branchesIgnore);
  }
  // A push filter that names only tags does not fire on branch pushes.
  return filters.tags === undefined && filters.tagsIgnore === undefined;
};

export const PR_DEFAULT_TYPES = ["opened", "synchronize", "reopened"];
