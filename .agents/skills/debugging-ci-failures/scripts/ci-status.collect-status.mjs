import {
  expectation,
  failedJobs,
  loadPr,
  resolveTarget,
} from "./ci-status.expectation.mjs";
import {
  call,
  callJson,
  callLines,
  DEFAULT_REPO,
  EXIT,
  FAILED_CONCLUSIONS,
  ghRunner,
  parseWorkflow,
  PASSED_CONCLUSIONS,
  QueryError,
} from "./ci-status.parse-workflow.mjs";

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

export const VERDICT_TEXT = new Map([
  [EXIT.passed, "all expected workflows ran on the head and passed"],
  [EXIT.failed, "a run on the head failed"],
  [
    EXIT.noRun,
    "a workflow that is or may be expected has no run on the head (absence is not a pass)",
  ],
  [EXIT.pending, "runs on the head are still in progress"],
]);
