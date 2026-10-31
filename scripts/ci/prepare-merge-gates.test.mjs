import assert from "node:assert/strict";
import {
  mkdtempSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { spawnSync } from "node:child_process";
import { test } from "node:test";
import { fileURLToPath } from "node:url";
import {
  gates,
  main,
  prepareMergeGates,
  workflowPatch,
} from "./prepare-merge-gates.mjs";
const root = fileURLToPath(new URL("../../", import.meta.url));
const yaml = createRequire(join(root, "demo/package.json"))("yaml");
const read = (file) =>
  readFileSync(join(root, ".github/workflows", file), "utf8");
const modify = (file, edit) => (name) => {
  if (name !== file) return read(name);
  const doc = yaml.parseDocument(read(name));
  edit(doc);
  return doc.toString();
};

test("proposal requires explicit literal branches and always leaves enforcement disabled", () => {
  for (const branches of [
    [],
    ["main", "main"],
    ["*"],
    ["refs/heads/main"],
    ["bad..branch"],
  ]) {
    assert.throws(() => prepareMergeGates(branches));
  }
  const { ruleset } = prepareMergeGates(["main", "tx-validation"]);
  assert.equal(ruleset.enforcement, "disabled");
  assert.deepEqual(ruleset.conditions.ref_name.include, [
    "refs/heads/main",
    "refs/heads/tx-validation",
  ]);
  assert.deepEqual(ruleset.bypass_actors, []);
  const checks = ruleset.rules.find((r) => r.type === "required_status_checks");
  assert.equal(checks.parameters.strict_required_status_checks_policy, true);
  assert.deepEqual(
    checks.parameters.required_status_checks.map((r) => r.context),
    [
      "Repository tool tests and workflow lint",
      "skills",
      "Aiken CI gate",
      "Node CI gate",
      "watcher",
    ],
  );
});

test("candidate patch applies and removes PR filters without changing jobs or push triggers", () => {
  const { changes } = prepareMergeGates(["main"]);
  assert.deepEqual(
    changes.map((c) => c.path),
    [
      ".github/workflows/aiken-ci.yml",
      ".github/workflows/midgard-node-ci.yml",
      ".github/workflows/midgard-watcher-ci.yml",
    ],
  );
  const scratch = mkdtempSync(join(tmpdir(), "midgard-gate-test-"));
  try {
    for (const c of changes) {
      mkdirSync(dirname(join(scratch, c.path)), { recursive: true });
      writeFileSync(join(scratch, c.path), c.before);
    }
    const result = spawnSync("git", ["apply", "-"], {
      cwd: scratch,
      input: workflowPatch(changes),
      encoding: "utf8",
      env: {
        ...process.env,
        GIT_DIR: join(scratch, "absent.git"),
        GIT_WORK_TREE: scratch,
      },
    });
    assert.equal(result.status, 0, result.stderr);
    for (const c of changes) {
      assert.equal(readFileSync(join(scratch, c.path), "utf8"), c.after);
      const before = yaml.parse(c.before),
        after = yaml.parse(c.after);
      before.on.pull_request = null;
      assert.deepEqual(after, before);
    }
    for (const [file] of gates) {
      const candidate = changes.find((c) => c.path.endsWith(file));
      assert.equal(
        yaml.parse(candidate?.after ?? read(file)).on.pull_request,
        null,
      );
    }
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});

test("refuses missing triggers/jobs, conditional jobs, matrix jobs and ambiguous contexts", () => {
  const file = "repo-tools-ci.yml";
  for (const edit of [
    (d) => d.deleteIn(["on", "pull_request"]),
    (d) => d.deleteIn(["jobs", "repo-tools"]),
    (d) => d.setIn(["jobs", "repo-tools", "if"], "false"),
    (d) => d.setIn(["jobs", "repo-tools", "if"], false),
    (d) =>
      d.setIn(["jobs", "repo-tools", "strategy"], {
        matrix: { os: ["linux"] },
      }),
    (d) => d.setIn(["jobs", "repo-tools", "name"], "skills"),
    (d) => d.setIn(["jobs", "repo-tools", "name"], "${{ matrix.os }}"),
  ])
    assert.throws(() => prepareMergeGates(["main"], modify(file, edit)));
});

test("malformed YAML and unknown CLI options fail instead of producing a partial policy", () => {
  assert.throws(() => prepareMergeGates(["main"], () => "jobs: ["));
  for (const args of [
    ["--activate"],
    ["--branch"],
    ["--branch", "main", "--format", "active"],
  ])
    assert.throws(() => main(args));
  assert.equal(
    JSON.parse(main(["--branch", "main", "--format", "ruleset"])).enforcement,
    "disabled",
  );
});
