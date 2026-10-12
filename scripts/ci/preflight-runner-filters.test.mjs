// A workflow that runs a check by id (`node scripts/preflight.mjs --run
// <id>`) runs preflight's runner, so a change to the runner must re-run it.
// A path filter that misses a runner module makes that step a gate that
// cannot fail: the change lands without the step ever running on it. This
// test holds each such workflow's path filters to the runner's closure
// (preflight-runner-closure.mjs), with the probe modules of the capabilities
// its named checks need.
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { loadYaml } from "./lint-workflows.mjs";
import { runnerClosure } from "./preflight-runner-closure.mjs";
import { matchesFilter } from "./workflow-path-filters.mjs";
import { buildRegistry } from "../preflight/registry.mjs";

const RUN_BY_ID = /\bnode scripts\/preflight\.mjs\b[^\n]*?--run\s/u;

/** The check ids a workflow's steps run by id. */
const idsRunBy = (workflow) =>
  Object.values(workflow.jobs ?? {}).flatMap((job) =>
    (job.steps ?? [])
      .map((step) => step.run)
      .filter((run) => typeof run === "string" && RUN_BY_ID.test(run))
      .flatMap((run) =>
        [...run.matchAll(/--run\s+(\S+)/gu)].map(([, id]) => id),
      ),
  );

const triggers = (filters, file) =>
  filters.paths
    ? matchesFilter(filters.paths, file)
    : filters["paths-ignore"]
      ? !matchesFilter(filters["paths-ignore"], file)
      : true;

/**
 * Every `<workflow> <event>: <file>` whose change would not trigger a
 * workflow that runs the runner on it.
 */
const filterGaps = (workflows, { capabilitiesOf, closure }) =>
  workflows.flatMap(({ file, workflow }) => {
    const ids = idsRunBy(workflow);
    if (ids.length === 0 || typeof workflow.on !== "object") return [];
    const files = closure([...new Set(ids.flatMap(capabilitiesOf))].sort());
    return Object.entries(workflow.on)
      .filter(([, filters]) => filters && typeof filters === "object")
      .flatMap(([event, filters]) =>
        files
          .filter((path) => !triggers(filters, path))
          .map((path) => `${file} ${event}: ${path}`),
      );
  });

const fixture = (on, run = "node scripts/preflight.mjs --run a") => [
  { file: "ci.yml", workflow: { on, jobs: { j: { steps: [{ run }] } } } },
];
const fixtureClosure = {
  capabilitiesOf: (id) => (id === "a" ? ["probe"] : []),
  closure: (capabilities) => [
    "scripts/preflight.mjs",
    "scripts/preflight/run.mjs",
    ...(capabilities.includes("probe") ? ["demo/probe.mjs"] : []),
  ],
};

test("a filter that names every runner module and probe module passes", () => {
  assert.deepEqual(
    filterGaps(
      fixture({
        push: { paths: ["scripts/preflight.mjs", "scripts/preflight/run.mjs"] },
        pull_request: { paths: ["scripts/**", "demo/**"] },
        workflow_dispatch: null,
      }),
      {
        ...fixtureClosure,
        capabilitiesOf: () => [],
      },
    ),
    [],
  );
});

test("a filter that misses a runner or probe module is refused, per event", () => {
  assert.deepEqual(
    filterGaps(
      fixture({
        push: { paths: ["scripts/preflight.mjs", "demo/**"] },
        pull_request: { "paths-ignore": ["demo/**"] },
      }),
      fixtureClosure,
    ),
    [
      "ci.yml push: scripts/preflight/run.mjs",
      "ci.yml pull_request: demo/probe.mjs",
    ],
  );
});

test("a workflow that does not run checks by id is not held to the runner", () => {
  assert.deepEqual(
    filterGaps(
      fixture({ push: { paths: ["docs/**"] } }, "node scripts/preflight.mjs"),
      fixtureClosure,
    ),
    [],
  );
});

const root = fileURLToPath(new URL("../..", import.meta.url));
const yaml = loadYaml(root);

test("the runner closure follows imports and pins every computed import", () => {
  const { files, undeclared } = runnerClosure(root, ["core-dist"]);
  assert.deepEqual(undeclared, []);
  for (const path of [
    "scripts/preflight.mjs",
    "scripts/preflight/run.mjs",
    "scripts/preflight/probes.mjs",
    "scripts/preflight/runtime-owner.mjs",
    "scripts/contrib/process.mjs",
    "demo/scripts/assert-midgard-core-dist-current.mjs",
    "demo/midgard-core/scripts/write-dist-source-digest.mjs",
  ])
    assert.ok(files.includes(path), path);
  // Spawned check commands and tests are not the runner.
  for (const path of files) {
    assert.ok(!path.endsWith(".test.mjs"), path);
    assert.notEqual(path, "scripts/preflight/aiken-fmt-check.mjs");
  }
  assert.ok(
    !runnerClosure(root).files.includes(
      "demo/scripts/assert-midgard-core-dist-current.mjs",
    ),
    "a probe module joins only when a named check needs its capability",
  );
});

test(
  "every workflow that runs a check by id re-runs when the runner changes",
  {
    skip:
      yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
        ? "could not check: yaml absent (run `pnpm --dir demo install`)"
        : false,
  },
  () => {
    const directory = resolve(root, ".github/workflows");
    const workflows = readdirSync(directory)
      .filter((file) => /\.ya?ml$/u.test(file))
      .map((file) => ({
        file,
        workflow: yaml.parse(readFileSync(resolve(directory, file), "utf8")),
      }));
    const callers = workflows.filter(
      ({ workflow }) => idsRunBy(workflow).length > 0,
    );
    assert.deepEqual(callers.map(({ file }) => file).sort(), [
      "aiken-ci.yml",
      "midgard-node-ci.yml",
      "repo-tools-ci.yml",
    ]);
    const checks = new Map(
      buildRegistry(root).checks.map((check) => [check.id, check]),
    );
    assert.deepEqual(
      filterGaps(workflows, {
        capabilitiesOf: (id) => checks.get(id)?.capabilities ?? [],
        closure: (capabilities) => {
          const { files, undeclared } = runnerClosure(root, capabilities);
          assert.deepEqual(undeclared, []);
          return files;
        },
      }),
      [],
    );
  },
);
