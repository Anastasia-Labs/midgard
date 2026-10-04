import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";
import {
  CI_VALIDATORS,
  validateCiEvidence,
  validatorInputs,
} from "./ci-evidence.mjs";
import { enrollBuilds } from "../contrib/enroll-builds.mjs";

const fixture = () => {
  const profile = {
    node: "v22.22.2",
    platform: "linux",
    arch: "x64",
    nodeOptions: "",
    execArgv: [],
  };
  return {
    head: "candidate",
    base: "target",
    remoteBase: "target",
    tree: "merged-tree",
    profile,
    inputs: "source-and-discovered-inputs",
    run: {
      head_sha: "candidate",
      path: ".github/workflows/repo-tools-ci.yml",
      event: "pull_request",
      status: "completed",
      conclusion: "success",
      html_url: "run",
    },
    job: {
      name: "Repository tool tests and workflow lint",
      status: "completed",
      conclusion: "success",
      html_url: "job",
      steps: Object.values(CI_VALIDATORS).map(([name]) => ({
        name,
        status: "completed",
        conclusion: "success",
      })),
    },
    identity: {
      schema: "midgard-repo-validator-ci/v1",
      tree: "merged-tree",
      parents: ["target", "candidate"],
      profile: { ...profile },
      inputs: "source-and-discovered-inputs",
    },
    workflow: {
      jobs: {
        "repo-tools": {
          steps: Object.values(CI_VALIDATORS).map(([name, run]) => ({
            name,
            run,
          })),
        },
      },
    },
  };
};
test("exact tested tree, runtime, commands and successful steps cover only audited validators", () => {
  const evidence = validateCiEvidence(fixture());
  assert.deepEqual(
    [...evidence.keys()],
    ["required-checks-doc", "contributor-build-guards"],
  );
  assert.equal(evidence.has("demo-test-db"), false);
  assert.equal(evidence.get("required-checks-doc").tree, "merged-tree");
});
test("stale or incomplete CI never satisfies a local validator", () => {
  const mutations = [
    (f) => {
      f.run.head_sha = "old";
    },
    (f) => {
      f.identity.parents[0] = "old-target";
    },
    (f) => {
      f.remoteBase = "advanced-target";
    },
    (f) => {
      f.identity.tree = "other-source";
    },
    (f) => {
      f.identity.inputs = "different-config-or-ignored-input";
    },
    (f) => {
      f.identity.profile.node = "v24.0.0";
    },
    (f) => {
      f.identity.profile.nodeOptions = "--conditions=other";
    },
    (f) => {
      f.run.status = "in_progress";
    },
    (f) => {
      f.run.conclusion = "failure";
    },
    (f) => {
      f.job.steps[0].conclusion = "skipped";
    },
    (f) => {
      f.workflow.jobs["repo-tools"].steps[0].run = "echo passed";
    },
    (f) => {
      f.workflow.jobs["repo-tools"].steps[0]["continue-on-error"] = true;
    },
    (f) => {
      f.job.steps.pop();
    },
  ];
  for (const mutate of mutations) {
    const input = fixture();
    mutate(input);
    assert.throws(() => validateCiEvidence(input));
  }
});

test("ignored unlisted manifests that enrollment discovers invalidate evidence", () => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-ci-inputs-"));
  const root = join(directory, "checkout");
  try {
    const source = fileURLToPath(new URL("../..", import.meta.url));
    mkdirSync(root);
    for (const path of execFileSync("git", ["ls-files", "-z"], {
      cwd: source,
      encoding: "utf8",
    })
      .split("\0")
      .filter(Boolean)) {
      const absolute = resolve(source, path);
      if (!existsSync(absolute) || !statSync(absolute).isFile()) continue;
      mkdirSync(dirname(resolve(root, path)), { recursive: true });
      copyFileSync(absolute, resolve(root, path));
    }
    execFileSync("git", ["init", "--quiet"], { cwd: root });
    execFileSync("git", ["add", "."], { cwd: root });
    writeFileSync(
      resolve(root, ".git/info/exclude"),
      "demo/private-validator-input/\n",
    );
    enrollBuilds(root);
    const before = validatorInputs(root);
    mkdirSync(resolve(root, "demo/private-validator-input"));
    writeFileSync(
      resolve(root, "demo/private-validator-input/package.json"),
      JSON.stringify({
        name: "private-validator-input",
        scripts: { build: "echo unguarded" },
      }),
    );
    assert.equal(
      execFileSync("git", ["ls-files", "--others", "--exclude-standard"], {
        cwd: root,
        encoding: "utf8",
      }),
      "",
    );
    assert.throws(() => enrollBuilds(root), /unguarded package builds/u);
    assert.notEqual(validatorInputs(root), before);
    writeFileSync(
      resolve(root, "demo/private-validator-input/package.json"),
      JSON.stringify({
        name: "private-validator-input",
        directory: "demo/midgard-watcher",
        scripts: { build: "echo unguarded" },
      }),
    );
    assert.throws(() => enrollBuilds(root), /unexpected guarded build recipe/u);
    assert.notEqual(validatorInputs(root), before);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("unaudited validator directories, shells and environment overrides refuse reuse", () => {
  const mutations = [
    (f) => {
      f.workflow.defaults = {
        run: { "working-directory": "/tmp/old-checkout" },
      };
    },
    (f) => {
      f.workflow.env = { NODE_OPTIONS: "--conditions=other" };
    },
    (f) => {
      f.workflow.jobs["repo-tools"].defaults = { run: { shell: "custom {0}" } };
    },
    (f) => {
      f.workflow.jobs["repo-tools"].env = {
        NODE_OPTIONS: "--conditions=other",
      };
    },
    (f) => {
      f.workflow.jobs["repo-tools"].steps[0]["working-directory"] =
        "/tmp/old-checkout";
    },
    (f) => {
      f.workflow.jobs["repo-tools"].steps[0].shell = "custom {0}";
    },
    (f) => {
      f.workflow.jobs["repo-tools"].steps[0].env = {
        NODE_OPTIONS: "--conditions=other",
      };
    },
  ];
  for (const mutate of mutations) {
    const input = fixture();
    mutate(input);
    assert.throws(() => validateCiEvidence(input));
  }
});
