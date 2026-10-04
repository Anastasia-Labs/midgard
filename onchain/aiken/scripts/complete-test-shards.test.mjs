import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  collectShardReports,
  createShardPlan,
  runShardCheck,
  runShardInvocation,
  shardArguments,
  validateShardArtifact,
  validateShardPlan,
} from "./complete-test-shards.mjs";

const digest = (text) => createHash("sha256").update(text).digest("hex");
const fixture = () => {
  const root = mkdtempSync(join(tmpdir(), "midgard-complete-shards-"));
  const project = join(root, "project");
  mkdirSync(join(project, "lib/midgard"), { recursive: true });
  mkdirSync(join(project, "env"));
  writeFileSync(join(project, "aiken.toml"), 'name = "owner/project"\n');
  writeFileSync(join(project, "aiken.lock"), "packages = []\n");
  for (const module of ["alpha.test", "beta.test", "gamma.test", "omega.test"])
    writeFileSync(
      join(project, "lib/midgard", `${module}.ak`),
      "test exact_case() { True }\n",
    );
  const plan = createShardPlan(project, 2, 42);
  return {
    root,
    project,
    plan,
    cleanup: () => rmSync(root, { recursive: true, force: true }),
  };
};
const unit = (title = "exact_case") => ({
  title,
  status: "pass",
  on_failure: "fail_immediately",
  execution_units: { mem: 12, cpu: 34 },
  traces: ["retained"],
});
const property = (on_failure = "fail_immediately") => ({
  title: "property_case",
  status: "pass",
  on_failure,
  iterations: on_failure === "succeed_immediately" ? 1 : 100,
  counterexample: on_failure === "succeed_immediately" ? "0" : null,
});
const artifactFor = (plan, index, test = unit()) => {
  const module = plan.shards[index - 1].modules.find((name) =>
    name.startsWith("midgard/"),
  );
  const rawStdout = JSON.stringify({
    seed: plan.seed,
    summary: {
      total: 1,
      passed: 1,
      failed: 0,
      kind: {
        unit: Object.hasOwn(test, "iterations") ? 0 : 1,
        property: Object.hasOwn(test, "iterations") ? 1 : 0,
      },
    },
    modules: [
      {
        name: module,
        summary: {
          total: 1,
          passed: 1,
          failed: 0,
          kind: {
            unit: Object.hasOwn(test, "iterations") ? 0 : 1,
            property: Object.hasOwn(test, "iterations") ? 1 : 0,
          },
        },
        tests: [test],
      },
    ],
  });
  return {
    schema: "midgard-aiken-shard-report-v1",
    metadata: {
      planHash: plan.planHash,
      sourceHash: plan.sourceHash,
      compiler: {
        pin: structuredClone(plan.compiler),
        version: plan.compiler.version,
        sha256: "a".repeat(64),
      },
      environment: plan.environment,
      seed: plan.seed,
      maxSuccess: 100,
      index,
      count: plan.count,
      args: shardArguments(plan, index),
      exitStatus: 0,
      signal: null,
      error: null,
      rawStdoutSha256: digest(rawStdout),
    },
    rawStdout,
    rawStderr: "complete stderr",
  };
};
const withReport = (artifact, change) => {
  const modified = structuredClone(artifact);
  const report = JSON.parse(modified.rawStdout);
  change(report);
  modified.rawStdout = JSON.stringify(report);
  modified.metadata.rawStdoutSha256 = digest(modified.rawStdout);
  return modified;
};

test("whole-module planning joins dotted and substring collisions without losing any source module", () => {
  const f = fixture();
  try {
    writeFileSync(
      join(f.project, "lib/midgard/foo-bar.test.ak"),
      "test first() { True }",
    );
    writeFileSync(
      join(f.project, "lib/midgard/foo-bar.extra.test.ak"),
      "test second() { True }",
    );
    writeFileSync(
      join(f.project, "lib/midgard/foo.ak"),
      "test third() { True }",
    );
    const plan = createShardPlan(f.project, 2, 42);
    const owners = new Map(
      plan.shards.flatMap((shard) =>
        shard.modules.map((module) => [module, shard.index]),
      ),
    );
    for (const module of [
      "midgard/foo_bar.test",
      "midgard/foo_bar.extra.test",
      "midgard/foo",
    ])
      assert.equal(owners.get(module), owners.get("midgard/foo"));
    assert.equal(
      plan.shards.reduce((sum, shard) => sum + shard.modules.length, 0),
      owners.size,
    );
    for (const module of owners.keys()) {
      assert.equal(
        plan.shards.filter((shard) =>
          shard.selectors.some((selector) =>
            module.includes(selector.slice(0, -1)),
          ),
        ).length,
        1,
        module,
      );
    }
    assert.equal(validateShardPlan(plan, f.project), plan);
  } finally {
    f.cleanup();
  }
});

test("new modules enter automatically and stale or truncated plans refuse", () => {
  const f = fixture();
  try {
    writeFileSync(
      join(f.project, "lib/midgard/new_test.ak"),
      "test added() { True }",
    );
    assert.throws(
      () => validateShardPlan(f.plan, f.project),
      /current complete source/u,
    );
    const fresh = createShardPlan(f.project, 2, 42);
    assert(
      fresh.shards.some((shard) => shard.modules.includes("midgard/new_test")),
    );
    const truncated = structuredClone(fresh);
    truncated.shards[0].modules.pop();
    assert.throws(
      () => validateShardPlan(truncated, f.project),
      /current complete source/u,
    );
  } finally {
    f.cleanup();
  }
});

test("source edits, invalid paths, escaping symlinks and cycles cannot be silently omitted", () => {
  const f = fixture();
  try {
    writeFileSync(
      join(f.project, "lib/midgard/alpha.test.ak"),
      "test changed() { False }",
    );
    assert.throws(
      () => validateShardPlan(f.plan, f.project),
      /current complete source/u,
    );
    writeFileSync(join(f.project, "lib/Invalid.ak"), "test hidden() { True }");
    assert.throws(
      () => createShardPlan(f.project, 2, 42),
      /invalid module path/u,
    );
    rmSync(join(f.project, "lib/Invalid.ak"));
    writeFileSync(join(f.root, "external.ak"), "test external() { True }");
    symlinkSync(
      join(f.root, "external.ak"),
      join(f.project, "lib/external.ak"),
    );
    assert.throws(() => createShardPlan(f.project, 2, 42), /symlink escapes/u);
    rmSync(join(f.project, "lib/external.ak"));
    symlinkSync(join(f.project, "lib"), join(f.project, "lib/loop"));
    assert.throws(() => createShardPlan(f.project, 2, 42), /symlink cycle/u);
  } finally {
    f.cleanup();
  }
});

test("env alias is assigned and every internal invocation retains seed, depth and whole-module filters", () => {
  const f = fixture();
  try {
    const envOwner = f.plan.shards.find((shard) =>
      shard.modules.includes("env"),
    );
    assert(envOwner);
    for (const shard of f.plan.shards) {
      const args = shardArguments(f.plan, shard.index);
      assert.deepEqual(args.slice(0, 6), [
        "check",
        "--plain-numbers",
        "--seed",
        "42",
        "--max-success",
        "100",
      ]);
      assert(shard.selectors.every((selector) => selector.endsWith(".")));
      assert(!args.includes("-e"));
    }
    assert.throws(() => createShardPlan(f.project, 4, -1), /uint32 seed/u);
    assert.throws(() => shardArguments(f.plan, 0), /unknown/u);
  } finally {
    f.cleanup();
  }
});

test("accepts all three polarities and preserves normal and expected-failure property depth", () => {
  const f = fixture();
  try {
    for (const polarity of [
      "fail_immediately",
      "succeed_eventually",
      "succeed_immediately",
    ]) {
      const t = { ...unit(), on_failure: polarity };
      assert.equal(
        validateShardArtifact(artifactFor(f.plan, 1, t), f.plan).ids.size,
        1,
      );
      assert.equal(
        validateShardArtifact(
          artifactFor(f.plan, 1, property(polarity)),
          f.plan,
        ).ids.size,
        1,
      );
    }
    assert.throws(
      () =>
        validateShardArtifact(
          artifactFor(f.plan, 1, { ...property(), iterations: 99 }),
          f.plan,
        ),
      /preserve all100/u,
    );
  } finally {
    f.cleanup();
  }
});

test("rejects green-looking summaries with missing, repeated, wrong-shard or failing test records", () => {
  const f = fixture();
  try {
    const a = artifactFor(f.plan, 1);
    const corruptions = [
      (r) => {
        r.modules[0].summary.total += 1;
      },
      (r) => {
        r.modules[0].summary.kind.unit = 0;
      },
      (r) => {
        r.modules = [];
      },
      (r) => {
        r.summary.total = 2;
      },
      (r) => {
        r.summary.kind.property = 1;
      },
      (r) => {
        r.modules[0].tests.push(r.modules[0].tests[0]);
      },
      (r) => {
        r.modules[0].name = f.plan.shards[1].modules.find((m) =>
          m.startsWith("midgard/"),
        );
      },
      (r) => {
        r.modules[0].tests[0].status = "fail";
      },
      (r) => {
        r.modules[0].tests[0].on_failure = "invented";
      },
      (r) => {
        r.modules[0].tests[0].execution_units.mem = -1;
      },
      (r) => {
        r.seed += 1;
      },
    ];
    for (const change of corruptions)
      assert.throws(
        () => validateShardArtifact(withReport(a, change), f.plan),
        /Aiken shards/u,
      );
  } finally {
    f.cleanup();
  }
});

test("rejects stale compiler/source/seed/environment/shard identities and null or nonzero exits", () => {
  const f = fixture();
  try {
    const changes = [
      (m) => {
        m.sourceHash = "wrong";
      },
      (m) => {
        m.seed += 1;
      },
      (m) => {
        m.environment = "testnet";
      },
      (m) => {
        m.count = 1;
      },
      (m) => {
        m.compiler.version = "stock";
      },
      (m) => {
        m.compiler.pin.rev = "a".repeat(40);
      },
      (m) => {
        m.args.pop();
      },
      (m) => {
        m.exitStatus = null;
        m.signal = "SIGTERM";
      },
      (m) => {
        m.exitStatus = 1;
      },
      (m) => {
        m.error = "ENOBUFS";
      },
    ];
    for (const change of changes) {
      const a = artifactFor(f.plan, 1);
      change(a.metadata);
      assert.throws(() => validateShardArtifact(a, f.plan), /Aiken shards/u);
    }
  } finally {
    f.cleanup();
  }
});

test("collector requires every exact shard file and baseline full-record equality", () => {
  const f = fixture();
  try {
    const reports = join(f.root, "reports");
    mkdirSync(reports);
    const artifacts = f.plan.shards.map((shard) =>
      artifactFor(f.plan, shard.index),
    );
    writeFileSync(join(reports, "shard-1.json"), JSON.stringify(artifacts[0]));
    assert.throws(
      () =>
        collectShardReports({
          plan: f.plan,
          projectDirectory: f.project,
          reportsDirectory: reports,
        }),
      /missing/u,
    );
    writeFileSync(join(reports, "shard-2.json"), JSON.stringify(artifacts[1]));
    const collected = collectShardReports({
      plan: f.plan,
      projectDirectory: f.project,
      reportsDirectory: reports,
    });
    assert.equal(collected.total, 2);
    const baseline = join(f.root, "baseline.json");
    const modules = artifacts.flatMap((a) => JSON.parse(a.rawStdout).modules);
    writeFileSync(
      baseline,
      JSON.stringify({
        seed: 42,
        summary: {
          total: 2,
          passed: 2,
          failed: 0,
          kind: { unit: 2, property: 0 },
        },
        modules,
      }),
    );
    assert.equal(
      collectShardReports({
        plan: f.plan,
        projectDirectory: f.project,
        reportsDirectory: reports,
        baseline,
      }).total,
      2,
    );
    modules[0].tests[0].execution_units.mem += 1;
    writeFileSync(
      baseline,
      JSON.stringify({
        seed: 42,
        summary: {
          total: 2,
          passed: 2,
          failed: 0,
          kind: { unit: 2, property: 0 },
        },
        modules,
      }),
    );
    assert.throws(
      () =>
        collectShardReports({
          plan: f.plan,
          projectDirectory: f.project,
          reportsDirectory: reports,
          baseline,
        }),
      /complete baseline records/u,
    );
    writeFileSync(join(reports, "unexpected.json"), "{}");
    assert.throws(
      () =>
        collectShardReports({
          plan: f.plan,
          projectDirectory: f.project,
          reportsDirectory: reports,
        }),
      /unexpected/u,
    );
  } finally {
    f.cleanup();
  }
});

test("collector refuses one shard report placed under another shard filename", () => {
  const f = fixture();
  try {
    const reports = join(f.root, "reports");
    mkdirSync(reports);
    for (const index of [1, 2])
      writeFileSync(
        join(reports, `shard-${index}.json`),
        JSON.stringify(artifactFor(f.plan, 1)),
      );
    assert.throws(
      () =>
        collectShardReports({
          plan: f.plan,
          projectDirectory: f.project,
          reportsDirectory: reports,
        }),
      /another shard identity/u,
    );
  } finally {
    f.cleanup();
  }
});

test("ordinary stub executions retain complete raw output on invalid JSON, nonzero exit and signal", () => {
  const f = fixture();
  try {
    for (const [name, raw, ending] of [
      ["invalid", "not JSON, retained exactly", "process.exit(0)"],
      ["nonzero", artifactFor(f.plan, 1).rawStdout, "process.exit(1)"],
      [
        "signal",
        artifactFor(f.plan, 1).rawStdout,
        'process.kill(process.pid, "SIGTERM")',
      ],
      ["pass", artifactFor(f.plan, 1).rawStdout, "process.exit(0)"],
    ]) {
      const binary = join(f.root, `${name}.mjs`);
      writeFileSync(
        binary,
        `#!/usr/bin/env node\nif(process.argv[2]==="--version"){console.log(${JSON.stringify(f.plan.compiler.version)});process.exit(0)}\nprocess.stdout.write(${JSON.stringify(raw)},()=>{${ending}});\n`,
      );
      chmodSync(binary, 0o755);
      const output = join(f.root, `${name}.json`);
      const run = () =>
        runShardCheck({
          plan: f.plan,
          index: 1,
          output,
          binary,
          projectDirectory: f.project,
        });
      if (name === "pass") assert.equal(run().total, 1);
      else assert.throws(run, /Aiken shards/u);
      const captured = JSON.parse(readFileSync(output, "utf8"));
      assert.equal(captured.rawStdout, raw);
      if (name === "signal") {
        assert.equal(captured.metadata.exitStatus, null);
        assert.equal(captured.metadata.signal, "SIGTERM");
      }
    }
  } finally {
    f.cleanup();
  }
});

test("plan CLI generates one fresh common uint32 seed and refuses duplicate or unknown options", () => {
  const f = fixture();
  try {
    const output = join(f.root, "plan.json");
    const result = runShardInvocation(
      ["--plan-shards", "2", "--output", output],
      { projectDirectory: f.project },
    );
    const plan = JSON.parse(readFileSync(output, "utf8"));
    assert.equal(result.seed, plan.seed);
    assert(plan.seed >= 0 && plan.seed <= 0xffffffff);
    assert.throws(
      () =>
        runShardInvocation(
          ["--plan-shards", "2", "--output", output, "--output", output],
          { projectDirectory: f.project },
        ),
      /duplicate/u,
    );
    assert.throws(
      () =>
        runShardInvocation(["--plan-shards", "2", "--unknown", output], {
          projectDirectory: f.project,
        }),
      /unknown/u,
    );
    assert.throws(
      () =>
        runShardInvocation(["--plan-shards", "2", "--output", output], {
          projectDirectory: f.project,
        }),
      /EEXIST/u,
    );
  } finally {
    f.cleanup();
  }
});
