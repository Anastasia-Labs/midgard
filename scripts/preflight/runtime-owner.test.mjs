import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";
import { loadYaml } from "../ci/lint-workflows.mjs";
import { buildRegistry } from "./registry.mjs";
import { planPreflight, runPreflight } from "./run.mjs";
import {
  readRuntimeWorkflow,
  RUNTIME_CHECKS,
  RUNTIME_WORKFLOW,
} from "./runtime-owner.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const registry = buildRegistry(root);
const yaml = loadYaml(root);
const document = () =>
  yaml.parse(readFileSync(resolve(root, RUNTIME_WORKFLOW), "utf8"));
const ids = (plan) => plan.planned.map(({ check }) => check.id);

for (const [label, changed] of [
  [
    "test-only",
    [
      "demo/midgard-fault-proofs/tests/resolved-output-non-canonical-lifecycle.test.ts",
    ],
  ],
  ["production source", ["demo/midgard-sdk/src/index.ts"]],
  ["shared build tool", ["scripts/contrib/process.mjs"]],
]) {
  test(`ordinary PR schedules runtime once in CI: ${label}`, async () => {
    assert.ok(yaml, "yaml is required to prove hosted coverage");
    const local = planPreflight(registry, changed, { runtimeOwner: "local" });
    const ordinary = planPreflight(registry, changed, { runtimeOwner: "ci" });
    assert.deepEqual(
      ids(ordinary),
      ids(local).filter((id) => !RUNTIME_CHECKS.includes(id)),
    );
    assert.deepEqual(
      ordinary.hosted.map(({ id }) => id),
      ids(local).filter((id) => RUNTIME_CHECKS.includes(id)),
    );
    assert.ok(ordinary.hosted.length > 0);
    assert.deepEqual(
      ordinary.hosted.map(({ gate }) => gate),
      ordinary.hosted.map(() => "Node CI gate"),
    );
    assert.deepEqual(local.hosted, []);
    // Dispatch a frozen plan with an injected runner: no actual runtime matrix.
    const dispatched = [];
    const result = await runPreflight({
      root,
      base: "HEAD",
      plan: ordinary,
      probes: { get: async () => ({ status: "available" }), invalidate() {} },
      log() {},
      runStep: async (_root, step) => {
        dispatched.push(step.argv);
        return { status: 0, output: "" };
      },
    });
    assert.equal(result.exitCode, 0);
    assert.ok(result.results.every(({ id }) => !RUNTIME_CHECKS.includes(id)));
    assert.equal(
      dispatched.length,
      ordinary.planned.reduce((count, { steps }) => count + steps.length, 0),
    );
    const fast = planPreflight(registry, changed, {
      prePush: true,
      runtimeOwner: "ci",
    });
    assert.deepEqual(
      fast.planned,
      planPreflight(registry, changed, { prePush: true, runtimeOwner: "local" })
        .planned,
    );
    assert.deepEqual(fast.hosted, []);
  });
}

test("unmatched paths and unverifiable workflows keep local runtime obligations", () => {
  for (const [candidate, changed] of [
    // A full-run preflight path Node CI does not run for (not a module of
    // the by-id runner).
    [registry, ["scripts/preflight/aiken-fmt-check.mjs"]],
    [
      { ...registry, runtimeWorkflow: undefined },
      ["demo/midgard-sdk/src/index.ts"],
    ],
  ]) {
    const plan = planPreflight(candidate, changed, { runtimeOwner: "ci" });
    assert.deepEqual(plan.hosted, []);
    assert.ok(ids(plan).includes("demo-test"));
  }
});

test("hosted ownership preserves distinct transaction preparation and acceptance modes", () => {
  const plan = planPreflight(registry, ["demo/midgard-sdk/src/index.ts"], {
    runtimeOwner: "ci",
  });
  for (const id of ["tx-preparation:node", "tx-preparation:emulator"])
    assert.ok(ids(plan).includes(id), id);
  assert.ok(plan.hosted.some(({ id }) => id === "tx-preparation:sdk"));
  const different = planPreflight(
    {
      ...registry,
      runtimeWorkflow: { ...registry.runtimeWorkflow, sdkLane: false },
    },
    ["demo/midgard-sdk/src/index.ts"],
    { runtimeOwner: "ci" },
  );
  assert.ok(ids(different).includes("tx-preparation:sdk"));
  assert.ok(!different.hosted.some(({ id }) => id === "tx-preparation:sdk"));
  const full = planPreflight(registry, [], {
    full: true,
    runtimeOwner: "local",
  });
  assert.deepEqual(full.hosted, []);
  assert.equal(full.planned.length, registry.checks.length);
});

// Every registered package script must have a reached hosted invocation. This
// catches new packages/scripts and removals from CI, rather than trusting a
// claim that "tsup covers typecheck" or source tests exercise compiled workers.
test("Node CI covers every package build, typecheck and full suite", () => {
  assert.ok(yaml, "yaml is required to prove hosted coverage");
  const { jobs } = document();
  const workspace = JSON.parse(
    readFileSync(resolve(root, "demo/package.json"), "utf8"),
  );
  assert.equal(
    workspace.scripts["test:tx-prep:sdk"],
    "pnpm --filter @al-ft/lucid-midgard test && pnpm --filter @al-ft/midgard-sdk test",
  );
  const commands = Object.entries(jobs).flatMap(([jobId, job]) =>
    job.steps
      .filter(
        (step) =>
          step.run &&
          !step["continue-on-error"] &&
          (step.if === undefined || step.if === "${{ !cancelled() }}"),
      )
      .map((step) => ({ jobId, run: step.run })),
  );
  for (const pkg of registry.packages) {
    for (const script of ["build", "typecheck", "test"]) {
      if (!pkg.scripts[script]) continue;
      const prefix = `pnpm --dir ${pkg.directory}`;
      const invocations =
        script === "test"
          ? [`${prefix} test`, `${prefix} run test`]
          : [`${prefix} run ${script}`];
      // These package hooks are actual build prerequisites in CI.
      if (script === "build" && pkg.name === "midgard-node") {
        assert.match(pkg.scripts["db:migrate"], /^pnpm run build &&/u);
        invocations.push(`${prefix} run db:migrate`);
      }
      if (script === "build" && pkg.name === "da-committee-node") {
        assert.equal(pkg.scripts.pretest, "pnpm run build");
        invocations.push(`${prefix} test`);
      }
      const reached = commands.filter(({ run }) =>
        invocations.some((invocation) =>
          new RegExp(`${invocation}(?=\\s|$)`, "u").test(run),
        ),
      );
      assert.ok(
        reached.length > 0,
        `${pkg.name}:${script} lacks hosted coverage`,
      );
      for (const { jobId } of reached) {
        assert.ok(
          jobs.gate.needs.includes(jobId),
          `${jobId} must reach Node CI gate`,
        );
        assert.equal(jobs[jobId].if, undefined);
      }
    }
  }
  for (const [jobId, shards] of [
    ["midgard-node", 3],
    ["midgard-watcher", 3],
    ["midgard-fault-proofs", 6],
  ]) {
    const job = jobs[jobId];
    assert.equal(job.strategy["fail-fast"], false);
    assert.equal(job.strategy.matrix.shard.length, shards);
    assert.ok(
      job.steps.some((step) =>
        step.run?.includes(
          "--shard=${{ matrix.shard }}/${{ strategy.job-total }}",
        ),
      ),
    );
  }
  assert.ok(
    jobs["l1-node-transport"].steps.some(
      (step) =>
        step.run === "pnpm --dir demo/l1-node-transport run native:build",
    ),
  );
  assert.ok(
    jobs["da-e2e"].steps.some(
      (step) => step.env?.MIDGARD_RUN_DA_PHASE5_JOINED_E2E === "1",
    ),
  );
});

test("workflow routing and summary failures cannot silently delegate runtime", () => {
  assert.ok(yaml, "yaml is required to prove hosted coverage");
  const scratch = mkdtempSync(join(tmpdir(), "runtime-owner-"));
  try {
    mkdirSync(join(scratch, ".github/workflows"), { recursive: true });
    writeFileSync(join(scratch, RUNTIME_WORKFLOW), yaml.stringify(document()));
    assert.ok(
      readRuntimeWorkflow(scratch, { yaml }),
      "the unmutated control must delegate",
    );
    mkdirSync(join(scratch, "demo"));
    const workspace = JSON.parse(
      readFileSync(resolve(root, "demo/package.json"), "utf8"),
    );
    writeFileSync(
      join(scratch, "demo/package.json"),
      JSON.stringify(workspace),
    );
    assert.equal(readRuntimeWorkflow(scratch, { yaml }).sdkLane, true);
    workspace.scripts["test:tx-prep:sdk"] += " && echo extra-acceptance";
    writeFileSync(
      join(scratch, "demo/package.json"),
      JSON.stringify(workspace),
    );
    assert.equal(readRuntimeWorkflow(scratch, { yaml }).sdkLane, false);
    for (const mutate of [
      (w) => {
        w.on.pull_request.branches = ["main"];
      },
      (w) => {
        w.on.pull_request.paths.push("!demo/**");
      },
      (w) => {
        w.jobs.gate.needs = [];
      },
      (w) => {
        w.jobs.gate.if = undefined;
      },
      (w) => {
        w.jobs["midgard-node"].if = "false";
      },
      (w) => {
        w.jobs.gate.steps.at(-1).run += ",skipped";
      },
      (w) => {
        w.jobs.gate.steps.at(-1)["continue-on-error"] = true;
      },
    ]) {
      const workflow = document();
      mutate(workflow);
      writeFileSync(join(scratch, RUNTIME_WORKFLOW), yaml.stringify(workflow));
      assert.equal(readRuntimeWorkflow(scratch, { yaml }), undefined);
    }
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});
