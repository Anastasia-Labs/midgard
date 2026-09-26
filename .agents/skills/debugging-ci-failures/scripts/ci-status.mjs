#!/usr/bin/env node
// Which GitHub Actions workflows ran on the latest head of a branch or pull
// request, which did not and why, and where each red run stopped.
//
// A missing run is not a pass. GitHub creates no pull_request run while a pull
// request has merge conflicts, pushes trigger only on the branches a workflow
// names, and a `paths:` filter can skip a workflow outright. This script reads
// each workflow's triggers at the head commit, decides which workflows should
// have run, and reports every expected workflow that has no run.
//
// Usage:
//   node ci-status.mjs <pr-number|branch> [--repo owner/name] [--json]
//
// Exit codes (distinct, so a caller never mistakes "could not look" for
// "looked and found nothing"):
//   0  every expected workflow ran on the head commit and passed
//   1  at least one run on the head commit failed (or was cancelled)
//   2  at least one expected workflow has no run on the head commit, or no
//      workflow covers the head at all
//   3  could not query GitHub (gh missing, not authenticated, network, a
//      response that did not parse)
//   4  nothing failed and nothing is missing, but runs are still in progress
//   64 usage error
//
// Read-only: every gh call is a GET. The gh calls go through an injectable
// runner so the tests use fixture JSON instead of the network.

import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

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

const FAILED_CONCLUSIONS = new Set([
  "failure",
  "cancelled",
  "timed_out",
  "startup_failure",
  "action_required",
  "stale",
]);
const PASSED_CONCLUSIONS = new Set(["success", "neutral"]);

export class QueryError extends Error {}

export const ghRunner = (args) =>
  spawnSync("gh", args, { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });

const call = (run, args) => {
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

const callJson = (run, args) => {
  const text = call(run, args);
  try {
    return JSON.parse(text);
  } catch {
    throw new QueryError(`gh ${args.join(" ")}: response is not JSON`);
  }
};

const callLines = (run, args) =>
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
const pathsAllow = (filters, files) => {
  if (filters.paths === undefined && filters.pathsIgnore === undefined) {
    return true;
  }
  if (files === null) return null;
  if (filters.paths !== undefined) {
    return files.some((file) => matchesPatterns(file, filters.paths));
  }
  return files.some((file) => !matchesPatterns(file, filters.pathsIgnore));
};

const branchAllows = (filters, branch) => {
  if (filters.branches !== undefined) {
    return matchesPatterns(branch, filters.branches);
  }
  if (filters.branchesIgnore !== undefined) {
    return !matchesPatterns(branch, filters.branchesIgnore);
  }
  // A push filter that names only tags does not fire on branch pushes.
  return filters.tags === undefined && filters.tagsIgnore === undefined;
};

const PR_DEFAULT_TYPES = ["opened", "synchronize", "reopened"];

// Returns { expected: true|false|null, reason }. `null` means the trigger
// applies but its path filter could not be evaluated.
export const expectation = (workflow, context) => {
  if (workflow.error !== undefined) {
    return {
      expected: null,
      reason: `trigger not understood (${workflow.error})`,
    };
  }
  const reasons = [];
  let expected = false;
  const pr = context.pr;
  const prFilters = workflow.events.pull_request;
  if (prFilters !== undefined) {
    const types = prFilters.types ?? PR_DEFAULT_TYPES;
    if (pr === null) {
      reasons.push("pull_request: no open pull request for this head");
    } else if (pr.state !== "OPEN") {
      reasons.push(`pull_request: pull request is ${pr.state}`);
    } else if (!types.includes("synchronize") && !types.includes("opened")) {
      reasons.push("pull_request: types filter excludes pushes");
    } else if (!branchAllows(prFilters, pr.baseRefName)) {
      reasons.push(`pull_request: base ${pr.baseRefName} not in filter`);
    } else {
      const allowed = pathsAllow(prFilters, pr.files);
      const conflict =
        pr.mergeable === "CONFLICTING"
          ? " — and the pull request is CONFLICTING, so GitHub creates no pull_request run for any workflow until the conflict is resolved"
          : "";
      if (allowed === false) {
        reasons.push("pull_request: no changed file matches its paths filter");
      } else if (allowed === null) {
        expected = null;
        reasons.push(
          `pull_request: paths filter over ${String(pr.changedFiles)} changed files cannot be evaluated (GitHub reads at most ${String(PATH_FILTER_FILE_LIMIT)})${conflict}`,
        );
      } else {
        expected = true;
        reasons.push(`pull_request: trigger applies${conflict}`);
      }
    }
  }
  const pushFilters = workflow.events.push;
  if (pushFilters !== undefined) {
    if (!branchAllows(pushFilters, context.branch)) {
      reasons.push(`push: branch ${context.branch} not in filter`);
    } else {
      const filtered =
        pushFilters.paths !== undefined ||
        pushFilters.pathsIgnore !== undefined;
      const allowed = filtered
        ? pathsAllow(pushFilters, context.pushFiles())
        : true;
      if (allowed === false) {
        reasons.push(
          "push: no file in the head commit matches its paths filter (a multi-commit push is judged on its whole diff)",
        );
      } else if (allowed === null) {
        if (expected !== true) expected = null;
        reasons.push("push: paths filter could not be evaluated");
      } else {
        expected = true;
        reasons.push("push: trigger applies");
      }
    }
  }
  const others = Object.keys(workflow.events).filter(
    (event) => event !== "push" && event !== "pull_request",
  );
  if (others.length > 0) {
    reasons.push(`also on ${others.join(", ")} (not evaluated)`);
  }
  if (prFilters === undefined && pushFilters === undefined) {
    reasons.unshift("no push or pull_request trigger");
  }
  return { expected, reason: reasons.join("; ") };
};

// ---------------------------------------------------------------------------
// Querying.

const resolveTarget = (target) => {
  if (/^\d+$/u.test(target)) {
    return { prNumber: Number(target), branch: null };
  }
  return { prNumber: null, branch: target };
};

const loadPr = (run, repo, number) => {
  const pr = callJson(run, [
    "pr",
    "view",
    String(number),
    "-R",
    repo,
    "--json",
    "number,state,headRefName,headRefOid,baseRefName,mergeable,mergeStateStatus,changedFiles,url",
  ]);
  let files = null;
  if (
    typeof pr.changedFiles === "number" &&
    pr.changedFiles <= PATH_FILTER_FILE_LIMIT
  ) {
    files = callLines(run, [
      "api",
      "--paginate",
      `repos/${repo}/pulls/${String(number)}/files?per_page=100`,
      "--jq",
      ".[].filename",
    ]);
  }
  return { ...pr, files };
};

export const collectStatus = (
  target,
  { run = ghRunner, repo = DEFAULT_REPO } = {},
) => {
  const resolved = resolveTarget(target);
  let pr = null;
  let branch;
  let headSha;
  const notes = [];
  if (resolved.prNumber !== null) {
    pr = loadPr(run, repo, resolved.prNumber);
    branch = pr.headRefName;
    headSha = pr.headRefOid;
  } else {
    branch = resolved.branch;
    headSha = call(run, [
      "api",
      `repos/${repo}/branches/${branch}`,
      "--jq",
      ".commit.sha",
    ]).trim();
    const prs = callJson(run, [
      "pr",
      "list",
      "-R",
      repo,
      "--head",
      branch,
      "--state",
      "open",
      "--json",
      "number",
    ]);
    if (prs.length > 1) {
      notes.push(
        `${String(prs.length)} open pull requests use this branch; reporting #${String(prs[0].number)}`,
      );
    }
    if (prs.length > 0) pr = loadPr(run, repo, prs[0].number);
  }
  if (!/^[0-9a-f]{40}$/u.test(headSha)) {
    throw new QueryError(`head commit not resolved (got ${headSha})`);
  }
  if (pr !== null && pr.mergeable === "CONFLICTING") {
    notes.push(
      "pull request is CONFLICTING: GitHub creates no pull_request runs for it until the conflict is resolved",
    );
  } else if (pr !== null && pr.mergeable === "UNKNOWN") {
    notes.push(
      "GitHub has not finished computing mergeability; pull_request runs may not exist yet",
    );
  }

  let pushFiles;
  const pushFilesOnce = () => {
    if (pushFiles === undefined) {
      pushFiles = callLines(run, [
        "api",
        `repos/${repo}/commits/${headSha}`,
        "--jq",
        ".files[].filename",
      ]);
    }
    return pushFiles;
  };

  const listing = callJson(run, [
    "api",
    `repos/${repo}/contents/.github/workflows?ref=${headSha}`,
  ]);
  if (!Array.isArray(listing)) {
    throw new QueryError("workflow directory listing is not an array");
  }
  const workflowFiles = listing
    .filter((entry) => entry.type === "file" && /\.ya?ml$/u.test(entry.name))
    .map((entry) => entry.path);
  const workflows = workflowFiles.map((path) => ({
    path,
    ...parseWorkflow(
      call(run, [
        "api",
        "-H",
        "Accept: application/vnd.github.raw+json",
        `repos/${repo}/contents/${path}?ref=${headSha}`,
      ]),
    ),
  }));

  const known = callJson(run, [
    "workflow",
    "list",
    "-R",
    repo,
    "--all",
    "--json",
    "id,name,path",
  ]);
  const pathById = new Map(known.map((w) => [w.id, w.path]));
  const runs = callJson(run, [
    "run",
    "list",
    "-R",
    repo,
    "--commit",
    headSha,
    "--limit",
    "100",
    "--json",
    "databaseId,workflowDatabaseId,workflowName,status,conclusion,event,createdAt,url",
  ]);
  const branchRuns = callJson(run, [
    "run",
    "list",
    "-R",
    repo,
    "--branch",
    branch,
    "--limit",
    "1",
    "--json",
    "workflowName,headSha,createdAt,status,conclusion,event",
  ]);

  const latestRunFor = (workflow) =>
    runs
      .filter(
        (r) =>
          pathById.get(r.workflowDatabaseId) === workflow.path ||
          (!pathById.has(r.workflowDatabaseId) &&
            r.workflowName === workflow.name),
      )
      .sort((a, b) =>
        String(b.createdAt).localeCompare(String(a.createdAt)),
      )[0];

  const context = { pr, branch, pushFiles: pushFilesOnce };
  const rows = workflows.map((workflow) => {
    const { expected, reason } = expectation(workflow, context);
    const latest = latestRunFor(workflow);
    const row = {
      workflow: workflow.name ?? workflow.path,
      path: workflow.path,
      expected,
      reason,
    };
    if (latest === undefined) {
      row.state =
        expected === true
          ? "missing"
          : expected === null
            ? "undetermined"
            : "not-triggered";
      return row;
    }
    row.run = {
      id: latest.databaseId,
      event: latest.event,
      status: latest.status,
      conclusion: latest.conclusion,
      url: latest.url,
    };
    if (latest.status !== "completed") row.state = "pending";
    else if (PASSED_CONCLUSIONS.has(latest.conclusion)) row.state = "passed";
    else if (FAILED_CONCLUSIONS.has(latest.conclusion)) row.state = "failed";
    else row.state = "undetermined";
    if (row.state === "failed") {
      row.jobs = failedJobs(
        callJson(run, [
          "run",
          "view",
          String(latest.databaseId),
          "-R",
          repo,
          "--json",
          "jobs",
        ]),
      );
    }
    return row;
  });

  // Runs whose workflow file is not in the head tree still count.
  const matched = new Set(
    rows.filter((row) => row.run !== undefined).map((row) => row.run.id),
  );
  for (const r of runs) {
    if (matched.has(r.databaseId)) continue;
    const sameWorkflowNewer = runs.some(
      (other) =>
        other.workflowDatabaseId === r.workflowDatabaseId &&
        String(other.createdAt) > String(r.createdAt),
    );
    if (sameWorkflowNewer) continue;
    const state =
      r.status !== "completed"
        ? "pending"
        : PASSED_CONCLUSIONS.has(r.conclusion)
          ? "passed"
          : FAILED_CONCLUSIONS.has(r.conclusion)
            ? "failed"
            : "undetermined";
    rows.push({
      workflow: r.workflowName,
      path: pathById.get(r.workflowDatabaseId) ?? null,
      expected: false,
      reason: "workflow file is not in the head tree",
      state,
      run: {
        id: r.databaseId,
        event: r.event,
        status: r.status,
        conclusion: r.conclusion,
        url: r.url,
      },
      ...(state === "failed"
        ? {
            jobs: failedJobs(
              callJson(run, [
                "run",
                "view",
                String(r.databaseId),
                "-R",
                repo,
                "--json",
                "jobs",
              ]),
            ),
          }
        : {}),
    });
  }

  return {
    repo,
    target,
    branch,
    headSha,
    pr:
      pr === null
        ? null
        : {
            number: pr.number,
            state: pr.state,
            baseRefName: pr.baseRefName,
            mergeable: pr.mergeable,
            mergeStateStatus: pr.mergeStateStatus,
            changedFiles: pr.changedFiles,
            url: pr.url,
          },
    latestBranchRun: branchRuns[0] ?? null,
    notes,
    workflows: rows,
  };
};

export const failedJobs = (view) =>
  (view.jobs ?? [])
    .filter((job) => FAILED_CONCLUSIONS.has(job.conclusion))
    .map((job) => {
      const steps = job.steps ?? [];
      const failing = steps.filter((step) =>
        FAILED_CONCLUSIONS.has(step.conclusion),
      );
      const firstFailing = failing[0]?.number ?? Infinity;
      const hidden = steps.filter(
        (step) =>
          step.number > firstFailing &&
          step.conclusion === "skipped" &&
          !/^Post /u.test(step.name),
      );
      return {
        name: job.name,
        conclusion: job.conclusion,
        failingSteps: failing.map((step) => ({
          number: step.number,
          name: step.name,
        })),
        skippedAfterFailure: hidden.length,
      };
    });

export const verdict = (report) => {
  const states = report.workflows.map((row) => row.state);
  if (states.includes("failed")) return EXIT.failed;
  if (states.includes("missing") || states.includes("undetermined")) {
    return EXIT.noRun;
  }
  if (states.includes("pending")) return EXIT.pending;
  if (!states.includes("passed")) return EXIT.noRun;
  return EXIT.passed;
};

const VERDICT_TEXT = new Map([
  [EXIT.passed, "all expected workflows ran on the head and passed"],
  [EXIT.failed, "a run on the head failed"],
  [
    EXIT.noRun,
    "a workflow that is or may be expected has no run on the head (absence is not a pass)",
  ],
  [EXIT.pending, "runs on the head are still in progress"],
]);

export const renderText = (report, code) => {
  const lines = [];
  const pr = report.pr;
  lines.push(
    pr === null
      ? `target   branch ${report.branch} (no open pull request)`
      : `target   PR #${String(pr.number)} ${report.branch} -> ${pr.baseRefName} (${pr.state}, mergeable ${pr.mergeable}, ${String(pr.changedFiles)} changed files)`,
  );
  lines.push(`head     ${report.headSha}`);
  const last = report.latestBranchRun;
  if (last !== null) {
    const onHead =
      last.headSha === report.headSha ? "the head" : last.headSha.slice(0, 10);
    lines.push(
      `latest   ${String(last.createdAt)} ${String(last.workflowName)} on ${onHead} (${String(last.conclusion || last.status)})`,
    );
  } else {
    lines.push("latest   no run on this branch at all");
  }
  for (const note of report.notes) lines.push(`note     ${note}`);
  lines.push("");
  const order = [
    "failed",
    "missing",
    "undetermined",
    "pending",
    "passed",
    "not-triggered",
  ];
  const rows = [...report.workflows].sort(
    (a, b) => order.indexOf(a.state) - order.indexOf(b.state),
  );
  for (const row of rows) {
    lines.push(
      `${row.state.toUpperCase().padEnd(13)} ${row.workflow} (${String(row.path)})`,
    );
    if (row.run !== undefined) {
      lines.push(
        `              run ${String(row.run.id)} ${row.run.event} ${row.run.url}`,
      );
    }
    for (const job of row.jobs ?? []) {
      for (const step of job.failingSteps) {
        lines.push(
          `              job ${job.name}: step ${String(step.number)} "${step.name}" failed`,
        );
      }
      if (job.skippedAfterFailure > 0) {
        lines.push(
          `              ${String(job.skippedAfterFailure)} later steps were skipped and measured nothing`,
        );
      }
    }
    if (row.state !== "passed" && row.state !== "failed") {
      lines.push(`              ${row.reason}`);
    }
  }
  lines.push("");
  lines.push(`verdict  exit ${String(code)}: ${VERDICT_TEXT.get(code)}`);
  return lines.join("\n");
};

const USAGE =
  "usage: ci-status.mjs <pr-number|branch> [--repo owner/name] [--json]";

export const main = (
  argv,
  { run = ghRunner, out = console.log, err = console.error } = {},
) => {
  const args = [...argv];
  let repo = DEFAULT_REPO;
  let json = false;
  const positional = [];
  while (args.length > 0) {
    const arg = args.shift();
    if (arg === "--json") json = true;
    else if (arg === "--repo") {
      const value = args.shift();
      if (value === undefined || !/^[\w.-]+\/[\w.-]+$/u.test(value)) {
        err(USAGE);
        return EXIT.usage;
      }
      repo = value;
    } else if (arg === "-h" || arg === "--help") {
      out(USAGE);
      return EXIT.usage;
    } else if (arg.startsWith("-")) {
      err(`unknown option ${arg}\n${USAGE}`);
      return EXIT.usage;
    } else positional.push(arg);
  }
  if (positional.length !== 1 || !/^[\w./-]+$/u.test(positional[0])) {
    err(USAGE);
    return EXIT.usage;
  }
  let report;
  try {
    report = collectStatus(positional[0], { run, repo });
  } catch (error) {
    if (error instanceof QueryError) {
      err(`could not query GitHub: ${error.message}`);
      err(
        `verdict  exit ${String(EXIT.queryFailed)}: could not look; this says nothing about CI`,
      );
      return EXIT.queryFailed;
    }
    throw error;
  }
  const code = verdict(report);
  out(
    json
      ? JSON.stringify({ ...report, exitCode: code }, null, 2)
      : renderText(report, code),
  );
  return code;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
