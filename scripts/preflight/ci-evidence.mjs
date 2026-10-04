// Reuse only the two audited, file-only validators. A green job name alone
// cannot establish its checkout, runtime, command or completed coverage.
import { execFileSync } from "node:child_process";
import {
  readFileSync,
  mkdtempSync,
  rmSync,
  existsSync,
  statSync,
  readdirSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadYaml } from "../ci/lint-workflows.mjs";
import {
  hashFiles,
  inputIdentity,
  sha256,
  workspacePackages,
} from "../contrib/files.mjs";
import { buildRegistry } from "./registry.mjs";

const REPO = "Anastasia-Labs/midgard";
const WORKFLOW = ".github/workflows/repo-tools-ci.yml";
export const CI_VALIDATORS = {
  "required-checks-doc": [
    "Check the generated required-checks list",
    "node scripts/preflight.mjs --check-docs",
  ],
  "contributor-build-guards": [
    "Check deterministic contributor build guards",
    "node scripts/contrib/enroll-builds.mjs",
  ],
};
const git = (root, ...args) =>
  execFileSync("git", args, { cwd: root, encoding: "utf8" }).trim();
export const nodeProfile = () => ({
  node: process.version,
  platform: process.platform,
  arch: process.arch,
  nodeOptions: process.env.NODE_OPTIONS ?? "",
  execArgv: process.execArgv,
});
export const validatorInputs = (root) => {
  const registry = buildRegistry(root);
  // Include discovered manifests/Aiken modules and concrete reference inputs,
  // even when ignored. Git-tree equality alone cannot attest those files.
  const paths = [
    ...registry.packages.map((pkg) => `${pkg.directory}/package.json`),
    // Enrollment owns a broader discovery inventory than pnpm-workspace.yaml.
    ...workspacePackages(root).map((pkg) => `${pkg.directory}/package.json`),
    // Manifest metadata may override pkg.directory. Bind the physical files
    // discovery read as well as the mapped paths enrollment later consumes.
    ...readdirSync(resolve(root, "demo"), { withFileTypes: true })
      .filter((entry) => entry.isDirectory())
      .map((entry) => `demo/${entry.name}/package.json`)
      .filter((path) => existsSync(resolve(root, path))),
    ...registry.index.modules.map((module) => module.file),
    ...registry.checks
      .flatMap((check) => check.triggers)
      .filter(
        (path) =>
          !/[*?{}]/u.test(path) &&
          existsSync(resolve(root, path)) &&
          statSync(resolve(root, path)).isFile(),
      ),
  ];
  return sha256(
    JSON.stringify([
      inputIdentity(root, "@repository").sha256,
      hashFiles(root, paths).sha256,
    ]),
  );
};
export const ciIdentity = (root) => ({
  schema: "midgard-repo-validator-ci/v1",
  commit: git(root, "rev-parse", "HEAD"),
  tree: git(root, "rev-parse", "HEAD^{tree}"),
  parents: git(root, "show", "-s", "--format=%P", "HEAD").split(" "),
  profile: nodeProfile(),
  inputs: validatorInputs(root),
});

export const validateCiEvidence = ({
  run,
  job,
  identity,
  workflow,
  head,
  base,
  remoteBase,
  tree,
  profile,
  inputs,
}) => {
  if (base !== remoteBase)
    throw new Error(
      "target reference is stale; fetch origin before reusing CI",
    );
  if (
    run.head_sha !== head ||
    run.path !== WORKFLOW ||
    run.event !== "pull_request" ||
    run.status !== "completed" ||
    run.conclusion !== "success"
  )
    throw new Error(
      "CI run is not a completed exact-head Repo Tools pull-request run",
    );
  if (
    identity.schema !== "midgard-repo-validator-ci/v1" ||
    identity.tree !== tree ||
    identity.parents.length !== 2 ||
    identity.parents[0] !== base ||
    identity.parents[1] !== head
  )
    throw new Error(
      "CI checkout/target is stale or differs from the intended integration tree",
    );
  if (JSON.stringify(identity.profile) !== JSON.stringify(profile))
    throw new Error("CI Node environment differs; run the validators locally");
  if (identity.inputs !== inputs)
    throw new Error("validator input inventory differs from completed CI");
  if (
    job.name !== "Repository tool tests and workflow lint" ||
    job.status !== "completed" ||
    job.conclusion !== "success"
  )
    throw new Error("repository validator job did not complete successfully");
  const sourceJob = workflow.jobs?.["repo-tools"];
  // These two commands are audited only in the checkout root with the common
  // Node profile. A later identity step cannot attest per-step overrides.
  if (
    workflow.defaults?.run ||
    workflow.env ||
    sourceJob?.defaults?.run ||
    sourceJob?.env
  )
    throw new Error(
      "CI validator defaults or environment are outside the audited context",
    );
  const sourceSteps = sourceJob?.steps ?? [];
  const evidence = new Map();
  for (const [id, [name, command]] of Object.entries(CI_VALIDATORS)) {
    const definitions = sourceSteps.filter((step) => step.name === name);
    const executions = job.steps.filter((step) => step.name === name);
    if (
      definitions.length !== 1 ||
      definitions[0].run !== command ||
      definitions[0]["continue-on-error"] ||
      definitions[0]["working-directory"] !== undefined ||
      definitions[0].shell !== undefined ||
      definitions[0].env !== undefined ||
      executions.length !== 1 ||
      executions[0].status !== "completed" ||
      executions[0].conclusion !== "success"
    )
      throw new Error(`CI command or completed coverage differs for ${id}`);
    evidence.set(id, {
      command,
      run: run.html_url,
      job: job.html_url,
      head,
      base,
      tree,
      profile,
      inputs,
      coverage: name,
    });
  }
  return evidence;
};

export const readCiEvidence = (root, runId, base, tree) => {
  if (!/^\d+$/u.test(runId))
    throw new Error("--ci-run needs a numeric GitHub run ID");
  if (git(root, "status", "--porcelain", "--untracked-files=all"))
    throw new Error(
      "CI reuse requires a clean checkout; commit owned changes or run locally",
    );
  if (!tree || git(root, "rev-parse", "HEAD^{tree}") !== tree)
    throw new Error(
      "local checkout differs from the intended integration tree; run locally",
    );
  const gh = (...args) =>
    execFileSync("gh", args, {
      cwd: root,
      encoding: "utf8",
      maxBuffer: 32 * 1024 * 1024,
      timeout: 30_000,
    });
  const api = (path) => JSON.parse(gh("api", `repos/${REPO}/${path}`));
  const run = api(`actions/runs/${runId}`);
  const jobs = api(`actions/runs/${runId}/jobs?per_page=100`).jobs;
  const job = jobs.find(
    (item) => item.name === "Repository tool tests and workflow lint",
  );
  if (!job) throw new Error("repository validator job is absent");
  const directory = mkdtempSync(join(tmpdir(), "midgard-ci-validator-"));
  try {
    gh(
      "run",
      "download",
      runId,
      "-R",
      REPO,
      "-n",
      "repo-validator-identity",
      "-D",
      directory,
    );
    const yaml = loadYaml(root);
    if (!yaml)
      throw new Error(
        "install demo workspace root dependencies to verify workflow commands",
      );
    const remoteBase = api(
      "branches/colll78%2Fcanonical-v1-watcher-l1-source-checkpoint",
    ).commit.sha;
    return validateCiEvidence({
      run,
      job,
      identity: JSON.parse(
        readFileSync(join(directory, "identity.json"), "utf8"),
      ),
      workflow: yaml.parse(readFileSync(resolve(root, WORKFLOW), "utf8")),
      head: git(root, "rev-parse", "HEAD"),
      base: git(root, "rev-parse", `${base}^{commit}`),
      remoteBase,
      tree,
      profile: nodeProfile(),
      inputs: validatorInputs(root),
    });
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  if (process.argv.length !== 3 || process.argv[2] !== "--identity")
    throw new Error("usage: node scripts/preflight/ci-evidence.mjs --identity");
  process.stdout.write(
    `${JSON.stringify(ciIdentity(resolve(fileURLToPath(new URL("../..", import.meta.url)))))}\n`,
  );
}
