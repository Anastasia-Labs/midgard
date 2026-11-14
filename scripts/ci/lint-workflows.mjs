#!/usr/bin/env node

// Lints .github/workflows/*.yml and .github/actions/**/action.yml for the ways
// a CI signal reports green without having checked anything:
//
//   unpinned-action          a third-party `uses:` not pinned to a 40-hex SHA
//                            with a `# vX.Y.Z` comment naming the release
//   permissions              no least-privilege `permissions:` at the top level
//                            or on every job
//   timeout                  a job without `timeout-minutes`
//   gate-missing             a workflow of two or more jobs with no summary gate
//                            (a job whose step runs check-needs-results.mjs)
//   gate-if                  a gate that does not run when a dependency failed:
//                            its `if:` lacks `!cancelled()` / `always()`
//   gate-allowlist           a gate step that does not hand `toJSON(needs)` to
//                            check-needs-results.mjs with an explicit `--allow`
//                            subset of success,skipped, or that can be skipped
//                            or ignored itself
//   gate-needs-closure       a gate that needs a job but not that job's needs,
//                            so an upstream failure reads as a downstream skip
//   gate-coverage            a job the gate does not need at all
//   skipped-counts-as-success  a gate allowing `skipped` over a job that skips
//                            itself (a job `if:`, e.g. a path filter)
//   continue-on-error        a job or step whose failure is reported as success
//   needs-unknown            `needs:` naming a job that does not exist
//   aiken-check-unguarded    a step running `aiken check` directly, which exits
//                            0 having collected no tests; run it through
//                            onchain/aiken/scripts/guard-focused-selector.mjs
//                            (`--all` for the whole suite) or run-focused-check.mjs
//   marker-reason            a malformed or reasonless exemption marker
//
// Exemptions are explicit and carry a reason:
//   # workflow-lint: allow <check> — <reason>
// A marker on the same or the previous line exempts a `uses:` line; a marker
// inside a job's block exempts that job; any other check is exempted by a
// marker anywhere in the file.
//
// Exit: 0 clean, 1 findings, 2 usage error, 3 could not check (the `yaml`
// package is not installed; run `pnpm --dir demo install`).

import { readdirSync, readFileSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const checks = [
  "unpinned-action",
  "permissions",
  "timeout",
  "gate-missing",
  "gate-if",
  "gate-allowlist",
  "gate-needs-closure",
  "gate-coverage",
  "skipped-counts-as-success",
  "continue-on-error",
  "needs-unknown",
  "aiken-check-unguarded",
  "marker-reason",
];

const defaultRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const gateScript =
  /(^|\s)node\s+(\.\/)?scripts\/ci\/check-needs-results\.mjs(\s|$)/u;
const markerPattern = /#\s*workflow-lint:\s*(.*)$/u;
const wellFormedMarker = /^allow\s+([a-z-]+)\s+(?:—|--)\s*(\S.*)$/u;
const pinnedUses = /^[^@\s]+@[0-9a-f]{40}\s+#\s*v?\d+\.\d+\.\d+\s*$/u;
// `aiken check` as a command word: at a line start or after a shell separator.
const bareAikenCheck = /(^|[\s;&|(`])aiken\s+check(\s|$)/u;

export const loadYaml = (root = defaultRoot) => {
  try {
    return createRequire(join(root, "demo/package.json"))("yaml");
  } catch {
    return undefined;
  }
};

const listFiles = (root) => {
  const found = [];
  const walk = (dir, match) => {
    let names;
    try {
      names = readdirSync(dir);
    } catch {
      return;
    }
    for (const name of names) {
      const path = join(dir, name);
      if (statSync(path).isDirectory()) walk(path, match);
      else if (match(name)) found.push(path);
    }
  };
  walk(join(root, ".github/workflows"), (name) => /\.ya?ml$/u.test(name));
  walk(join(root, ".github/actions"), (name) => /^action\.ya?ml$/u.test(name));
  return found.sort();
};

const toArray = (value) =>
  value === undefined || value === null
    ? []
    : Array.isArray(value)
      ? value
      : [value];

const lintFile = (yaml, root, path) => {
  const file = relative(root, path);
  const text = readFileSync(path, "utf8");
  const lines = text.split("\n");
  const findings = [];
  const markers = [];
  lines.forEach((line, index) => {
    const match = markerPattern.exec(line);
    if (match === null) return;
    const parsed = wellFormedMarker.exec(match[1].trim());
    if (parsed === null || !checks.includes(parsed[1])) {
      findings.push({
        file,
        line: index + 1,
        check: "marker-reason",
        message: `malformed exemption '${match[0].trim()}'; write '# workflow-lint: allow <check> — <reason>' with a known check`,
      });
    } else markers.push({ line: index + 1, check: parsed[1] });
  });
  const exempt = (check, from = 1, to = lines.length) =>
    markers.some(
      (marker) =>
        marker.check === check && marker.line >= from && marker.line <= to,
    );
  const report = (check, line, message, scope = [1, lines.length]) => {
    if (!exempt(check, ...scope)) findings.push({ file, line, check, message });
  };

  lines.forEach((line, index) => {
    const match = /^\s*(?:-\s*)?uses:\s*(.+?)\s*$/u.exec(line);
    if (match === null) return;
    const target = match[1].replace(/^["']|["']$/gu, "");
    if (target.startsWith("./") || target.startsWith("docker://")) return;
    if (!pinnedUses.test(match[1])) {
      report(
        "unpinned-action",
        index + 1,
        `'${target}' is not pinned to a commit SHA with a '# vX.Y.Z' comment`,
        [index, index + 1],
      );
    }
  });

  const lineCounter = new yaml.LineCounter();
  const document = yaml.parseDocument(text, { lineCounter });
  if (document.errors.length > 0) {
    findings.push({
      file,
      line: 1,
      check: "parse",
      message: `could not parse: ${document.errors[0].message}`,
    });
    return findings;
  }
  const workflow = document.toJS() ?? {};

  const runSteps = [
    ...Object.values(workflow.jobs ?? {}).flatMap((job) => toArray(job?.steps)),
    ...toArray(workflow.runs?.steps),
  ].filter((step) => typeof step?.run === "string");
  for (const step of runSteps) {
    const commands = step.run
      .split("\n")
      .map((line) => line.replace(/(^|\s)#.*$/u, ""));
    const command = commands.find((line) => bareAikenCheck.test(line));
    if (command !== undefined) {
      const at = lines.findIndex((line) => {
        const code = line.replace(/(^|\s)#.*$/u, "");
        return code.includes(command.trim()) && bareAikenCheck.test(code);
      });
      report(
        "aiken-check-unguarded",
        at + 1 || 1,
        `step '${step.name ?? step.run}' runs 'aiken check' directly, which exits 0 having collected no tests; run it through node onchain/aiken/scripts/guard-focused-selector.mjs (--all for the whole suite) or run-focused-check.mjs`,
      );
    }
  }

  if (!file.startsWith(".github/workflows/")) return findings;

  const jobsNode = document.get("jobs", true);
  const jobs = workflow.jobs ?? {};
  const span = {};
  for (const pair of jobsNode?.items ?? []) {
    const start = lineCounter.linePos(pair.key.range[0]).line;
    const end = lineCounter.linePos(
      pair.value?.range?.[1] ?? pair.key.range[1],
    ).line;
    span[String(pair.key.value)] = [start, end];
  }
  const names = Object.keys(jobs);
  const needsOf = (name) => toArray(jobs[name]?.needs).map(String);

  if (workflow.permissions === undefined) {
    for (const name of names.filter(
      (job) => jobs[job]?.permissions === undefined,
    )) {
      report(
        "permissions",
        span[name][0],
        `job '${name}' has no permissions and the workflow sets none`,
        span[name],
      );
    }
  }
  for (const name of names) {
    const job = jobs[name] ?? {};
    if (job.uses === undefined && job["timeout-minutes"] === undefined) {
      report(
        "timeout",
        span[name][0],
        `job '${name}' has no timeout-minutes`,
        span[name],
      );
    }
    for (const need of needsOf(name).filter((need) => !(need in jobs))) {
      report(
        "needs-unknown",
        span[name][0],
        `job '${name}' needs unknown job '${need}'`,
        span[name],
      );
    }
    const steps = toArray(job.steps);
    if (
      job["continue-on-error"] !== undefined &&
      job["continue-on-error"] !== false
    ) {
      report(
        "continue-on-error",
        span[name][0],
        `job '${name}' sets continue-on-error`,
        span[name],
      );
    }
    for (const step of steps.filter(
      (step) =>
        step?.["continue-on-error"] !== undefined &&
        step["continue-on-error"] !== false,
    )) {
      report(
        "continue-on-error",
        span[name][0],
        `a step of job '${name}' ('${step.name ?? step.uses ?? step.run}') sets continue-on-error`,
        span[name],
      );
    }
  }

  const gates = names.filter((name) =>
    toArray(jobs[name]?.steps).some(
      (step) => typeof step?.run === "string" && gateScript.test(step.run),
    ),
  );
  if (gates.length === 0 && names.length >= 2) {
    report(
      "gate-missing",
      1,
      `${String(names.length)} jobs and no summary gate job running scripts/ci/check-needs-results.mjs`,
    );
  }
  for (const gate of gates) {
    const job = jobs[gate];
    const at = span[gate];
    if (!/!\s*cancelled\(\)|always\(\)/u.test(String(job.if ?? ""))) {
      report(
        "gate-if",
        at[0],
        `gate '${gate}' must run when a dependency failed: add 'if: \${{ !cancelled() }}'`,
        at,
      );
    }
    const gateSteps = toArray(job.steps).filter(
      (step) => typeof step?.run === "string" && gateScript.test(step.run),
    );
    for (const step of gateSteps) {
      const allow = /--allow\s+(\S+)/u.exec(step.run)?.[1]?.split(",") ?? [];
      const problems = [];
      if (
        !/^\$\{\{\s*toJSON\(\s*needs\s*\)\s*\}\}$/u.test(
          String(step.env?.NEEDS ?? ""),
        )
      )
        problems.push("env NEEDS is not '${{ toJSON(needs) }}'");
      if (allow.length === 0) problems.push("no --allow list");
      for (const value of allow.filter(
        (value) => !["success", "skipped"].includes(value),
      ))
        problems.push(`--allow ${value} is not success or skipped`);
      if (step.if !== undefined) problems.push("the step has its own if:");
      if (problems.length > 0)
        report(
          "gate-allowlist",
          at[0],
          `gate '${gate}': ${problems.join("; ")}`,
          at,
        );
      if (allow.includes("skipped")) {
        for (const need of needsOf(gate).filter(
          (need) => jobs[need]?.if !== undefined,
        )) {
          if (!exempt("skipped-counts-as-success", ...(span[need] ?? [0, 0]))) {
            report(
              "skipped-counts-as-success",
              at[0],
              `gate '${gate}' allows skipped, and '${need}' skips itself (if: ${String(jobs[need].if)}), so a skipped check reads as a pass`,
              at,
            );
          }
        }
      }
    }
    const needed = new Set(needsOf(gate));
    for (const need of needed) {
      for (const upstream of needsOf(need).filter(
        (upstream) => !needed.has(upstream),
      )) {
        report(
          "gate-needs-closure",
          at[0],
          `gate '${gate}' needs '${need}' but not its dependency '${upstream}'`,
          at,
        );
      }
    }
    for (const name of names.filter(
      (name) => name !== gate && !needed.has(name) && !gates.includes(name),
    )) {
      if (!exempt("gate-coverage", ...span[name])) {
        report(
          "gate-coverage",
          at[0],
          `gate '${gate}' does not need job '${name}'`,
          at,
        );
      }
    }
  }
  return findings;
};

export const lintWorkflows = (root = defaultRoot, yaml = loadYaml(root)) => {
  if (yaml === undefined || yaml === null)
    return {
      couldNotCheck:
        "the yaml package is not installed (pnpm --dir demo install)",
      findings: [],
      files: [],
    };
  const files = listFiles(root);
  if (files.length === 0)
    return {
      couldNotCheck: `no workflow files under ${join(root, ".github")}`,
      findings: [],
      files,
    };
  return {
    findings: files.flatMap((path) => lintFile(yaml, root, path)),
    files,
  };
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const args = process.argv.slice(2);
  let root = defaultRoot;
  if (args[0] === "--root" && args[1] !== undefined && args.length === 2)
    root = resolve(args[1]);
  else if (args.length > 0) {
    console.error("usage: lint-workflows.mjs [--root DIR]");
    process.exit(2);
  }
  // yaml comes from the linted tree's demo workspace, else from this one's.
  const result = lintWorkflows(root, loadYaml(root) ?? loadYaml(defaultRoot));
  if (result.couldNotCheck !== undefined) {
    console.error(`workflow lint: could not check: ${result.couldNotCheck}`);
    process.exit(3);
  }
  for (const finding of result.findings) {
    console.error(
      `${finding.file}:${String(finding.line)}: [${finding.check}] ${finding.message}`,
    );
  }
  if (result.findings.length > 0) {
    console.error(
      `workflow lint: ${String(result.findings.length)} finding(s) in ${String(result.files.length)} file(s)`,
    );
    process.exit(1);
  }
  console.log(`workflow lint: ${String(result.files.length)} file(s) clean`);
}
