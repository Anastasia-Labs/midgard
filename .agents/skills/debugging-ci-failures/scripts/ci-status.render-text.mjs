import {
  collectStatus,
  verdict,
  VERDICT_TEXT,
} from "./ci-status.collect-status.mjs";
import {
  DEFAULT_REPO,
  EXIT,
  ghRunner,
  QueryError,
} from "./ci-status.parse-workflow.mjs";
import {
  DEFAULT_TIMEOUT_MINUTES,
  MAX_TIMEOUT_MINUTES,
  waitForStatus,
} from "./ci-status.wait.mjs";

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

export const USAGE =
  "usage: ci-status.mjs <pr-number|branch> [--repo owner/name] [--json] [--wait [--timeout <minutes>|<N>m|<N>h]]";

export const main = (
  argv,
  {
    run = ghRunner,
    out = console.log,
    err = console.error,
    wait: waitOptions,
  } = {},
) => {
  const args = [...argv];
  let repo = DEFAULT_REPO;
  let json = false;
  let wait = false;
  let timeoutMinutes;
  const positional = [];
  while (args.length > 0) {
    const arg = args.shift();
    if (arg === "--json") json = true;
    else if (arg === "--wait") wait = true;
    else if (arg === "--timeout") {
      const value = args.shift();
      // Bare digits are minutes; `m` and `h` may say so explicitly.
      const match = /^(\d+)([mh]?)$/u.exec(value ?? "");
      timeoutMinutes = match
        ? Number(match[1]) * (match[2] === "h" ? 60 : 1)
        : Number.NaN;
      if (!(timeoutMinutes >= 1 && timeoutMinutes <= MAX_TIMEOUT_MINUTES)) {
        const seconds =
          match &&
          match[2] === "" &&
          timeoutMinutes > MAX_TIMEOUT_MINUTES &&
          timeoutMinutes % 60 === 0
            ? ` (${String(value)} reads as minutes; for ${String(value)} seconds write ${String(Number(match[1]) / 60)})`
            : "";
        err(
          `--timeout takes whole minutes (90 or 90m) or hours (2h), from 1 minute to ${String(MAX_TIMEOUT_MINUTES)}${seconds}\n${USAGE}`,
        );
        return EXIT.usage;
      }
    } else if (arg === "--repo") {
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
  if (timeoutMinutes !== undefined && !wait) {
    err(`--timeout bounds --wait; it means nothing without it\n${USAGE}`);
    return EXIT.usage;
  }
  let report;
  let code;
  try {
    if (wait) {
      ({ report, code } = waitForStatus(positional[0], {
        run,
        repo,
        timeoutMinutes: timeoutMinutes ?? DEFAULT_TIMEOUT_MINUTES,
        progress: (line) => err(`waiting  ${line}`),
        ...waitOptions,
      }));
    } else {
      report = collectStatus(positional[0], { run, repo });
      code = verdict(report);
    }
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
  out(
    json
      ? JSON.stringify({ ...report, exitCode: code }, null, 2)
      : renderText(report, code),
  );
  return code;
};
