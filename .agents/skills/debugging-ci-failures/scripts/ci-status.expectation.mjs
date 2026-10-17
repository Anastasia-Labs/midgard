import {
  branchAllows,
  callJson,
  callLines,
  FAILED_CONCLUSIONS,
  PATH_FILTER_FILE_LIMIT,
  pathsAllow,
  PR_DEFAULT_TYPES,
} from "./ci-status.parse-workflow.mjs";

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

export const resolveTarget = (target) => {
  if (/^\d+$/u.test(target)) {
    return { prNumber: Number(target), branch: null };
  }
  return { prNumber: null, branch: target };
};

export const loadPr = (run, repo, number) => {
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
