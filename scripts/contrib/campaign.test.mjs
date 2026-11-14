import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { campaignPlan, pinCheck, runCampaign } from "./campaign.mjs";
import {
  definitionDigest,
  parseCampaign,
  stageCommand,
} from "./campaign-definition.mjs";
import { sha256 } from "./files.mjs";

// Campaigns run against a throwaway git checkout whose devnet-stack entry
// point is a small fake: each stage is a real child process under the real
// runProcess (deadline, process group, log), but nothing starts docker, a
// devnet or a journey. The resource registry lives in a private TMPDIR.

const scratch = realpathSync(mkdtempSync(join(tmpdir(), "midgard-campaign-")));
process.env.TMPDIR = join(scratch, "registry");
mkdirSync(process.env.TMPDIR, { mode: 0o700 });
process.on("exit", () => rmSync(scratch, { recursive: true, force: true }));

const repositoryRoot = fileURLToPath(new URL("../..", import.meta.url));
const env = Object.fromEntries(
  Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
);
const git = (cwd, ...args) =>
  execFileSync("git", args, { cwd, env, encoding: "utf8" }).trim();
const commitAll = (cwd, message) => {
  git(cwd, "add", "-A");
  git(
    cwd,
    "-c",
    "user.name=Fixture",
    "-c",
    "user.email=fixture@example.invalid",
    "-c",
    "core.hooksPath=/dev/null",
    "commit",
    "-qm",
    message,
  );
  return git(cwd, "rev-parse", "HEAD");
};

const FAKE_STACK = `const fs = require("node:fs");
const [mode, ...rest] = process.argv.slice(2);
if (mode === "record") {
  fs.appendFileSync(rest[0], rest[1] + "\\n");
  console.log("recorded " + rest[1]);
} else if (mode === "fail") {
  console.error("this stage fails");
  process.exit(7);
} else if (mode === "flaky") {
  fs.appendFileSync(rest[1], "flaky\\n");
  if (!fs.existsSync(rest[0])) {
    console.error("not yet");
    process.exit(1);
  }
} else if (mode === "hang") {
  process.on("SIGTERM", () => console.log("journaled on SIGTERM"));
  setInterval(() => {}, 1000);
  console.log("hanging");
} else if (mode === "up" || mode === "down") {
  if (rest[2] === "--slow")
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 700);
  else if (rest[2] && !fs.existsSync(rest[2])) {
    console.error(mode + " not yet");
    process.exit(1);
  }
  fs.appendFileSync(rest[0], rest[1] + "\\n");
} else if (mode === "env") {
  console.log(process.env.CAMPAIGN_SECRET ? "secret visible" : "no secret");
}
`;

const TRANSPORT =
  "demo/l1-node-transport/dist/native/midgard-l1-node-transport";

let fixtureIndex = 0;
// A checkout at one commit, with a ready blueprint and its build record.
const fixture = (t) => {
  const base = join(scratch, `fixture-${(fixtureIndex += 1)}`);
  const root = join(base, "checkout");
  mkdirSync(join(root, "demo/midgard-node-tools/dist"), { recursive: true });
  mkdirSync(join(root, "onchain/aiken"), { recursive: true });
  git(root, "init", "-q", "-b", "trunk");
  writeFileSync(join(root, "README"), "fixture\n");
  writeFileSync(
    join(root, ".gitignore"),
    "onchain/aiken/plutus.json.deployment.json\n",
  );
  writeFileSync(
    join(root, "demo/midgard-node-tools/dist/devnet-stack.js"),
    FAKE_STACK,
  );
  // The campaign's build of the transport binary the journeys start.
  mkdirSync(join(root, "demo/l1-node-transport/dist/native"), {
    recursive: true,
  });
  writeFileSync(join(root, TRANSPORT), "transport build\n");
  const head = commitAll(root, "base");
  // Untracked, as in a real checkout.
  writeFileSync(join(root, "onchain/aiken/plutus.json"), '{"validators":[]}\n');
  writeFileSync(
    join(root, "onchain/aiken/plutus.json.deployment.json"),
    '{"profile":"local-devnet-testing"}\n',
  );
  t.after(() => rmSync(base, { recursive: true, force: true }));
  return {
    root,
    head,
    base,
    directory: join(base, "campaign"),
    marks: join(base, "marks.txt"),
  };
};

const stack = (id, args, timeoutSeconds = 30) => ({
  id,
  kind: "devnet-stack",
  timeoutSeconds,
  args,
});
const teardown = (id, args) => ({
  ...stack(id, ["down", ...args]),
  always: true,
});

// A run directory as the watcher-journeys generator leaves it.
const journeyRun = (f, name = "journey-run") => {
  const runDir = join(f.base, name);
  mkdirSync(join(runDir, "config"), { recursive: true });
  mkdirSync(join(runDir, "work"), { recursive: true });
  writeFileSync(join(runDir, "run.env"), `MIDGARD_PHASE4_RUN_DIR=${runDir}\n`);
  writeFileSync(join(runDir, "config/config.json"), "{}\n");
  writeFileSync(join(runDir, "compose.host-paths.json"), "{}\n");
  return runDir;
};

const campaignFile = (f, stages, overrides = {}) => {
  const path = join(f.base, `campaign-${(fixtureIndex += 1)}.json`);
  writeFileSync(
    path,
    JSON.stringify({
      schema: "midgard-devnet-campaign/v1",
      id: "fixture-campaign",
      commit: f.head,
      deploymentProfile: "local-devnet-testing",
      directory: f.directory,
      stages,
      ...overrides,
    }),
  );
  return path;
};

const builds = [];
const fakeBuild = async (root, campaign, { steps }) => {
  builds.push(steps);
  return {
    status: "passed",
    steps: steps.map((step) => ({ ...step, status: "built" })),
  };
};
const run = (f, path, overrides = {}) =>
  runCampaign(f.root, path, {
    env: { ...env, CAMPAIGN_SECRET: "supersecret-campaign-value" },
    profileCheck: () => true,
    buildSteps: () => [{ step: "install" }],
    build: fakeBuild,
    graceMs: 300,
    ...overrides,
  });
const marks = (f) =>
  existsSync(f.marks) ? readFileSync(f.marks, "utf8").trim().split("\n") : [];
const receiptOf = (f) =>
  JSON.parse(readFileSync(join(f.directory, "campaign-receipt.json"), "utf8"));

const valid = {
  schema: "midgard-devnet-campaign/v1",
  id: "valid",
  commit: "a".repeat(40),
  deploymentProfile: "local-devnet-testing",
  directory: "/absolute/campaign",
  stages: [
    stack("stack-up", ["up", "--run-dir", "/absolute/stack"]),
    {
      id: "l1-up",
      kind: "phase4",
      action: "up",
      runDir: "/absolute/run",
      timeoutSeconds: 600,
    },
    {
      id: "journey",
      kind: "watcher-journeys",
      runDir: "/absolute/run",
      files: ["devnet/watcher-journeys/journey.test.ts"],
      env: { JOURNEY_SEED: "1" },
      timeoutSeconds: 3600,
    },
  ],
};
const refuses = (mutate, pattern) => {
  const value = structuredClone(valid);
  mutate(value);
  assert.throws(() => parseCampaign(value), pattern);
};

test("the shipped example campaign is valid", () => {
  const example = JSON.parse(
    readFileSync(
      resolve(
        repositoryRoot,
        "demo/midgard-node-tools/config/devnet-campaign.example.json",
      ),
      "utf8",
    ),
  );
  assert.equal(parseCampaign(example).stages.length, 9);
});

test("validation refuses unknown fields, bad values and kind mismatches", () => {
  assert.equal(parseCampaign(structuredClone(valid)).id, "valid");
  refuses((c) => (c.extra = 1), /campaign has unknown field extra/);
  refuses((c) => (c.schema = "v0"), /campaign.schema must be/);
  refuses((c) => (c.commit = "abc123"), /full 40-character commit/);
  refuses((c) => (c.commit = "A".repeat(40)), /full 40-character commit/);
  refuses(
    (c) => (c.directory = "relative/dir"),
    /directory must be an absolute/,
  );
  refuses((c) => (c.directory = "/a/../b"), /directory must be an absolute/);
  refuses((c) => (c.stages = []), /stages must be a nonempty array/);
  refuses(
    (c) => (c.stages[0].extra = true),
    /\(devnet-stack\) has unknown field extra/,
  );
  refuses((c) => (c.stages[0].runDir = "/x"), /unknown field runDir/);
  refuses(
    (c) => delete c.stages[0].timeoutSeconds,
    /timeoutSeconds is required|timeoutSeconds must be/,
  );
  for (const timeout of [0, -1, 1.5, "60", 2_147_484, null])
    refuses(
      (c) => (c.stages[0].timeoutSeconds = timeout),
      /timeoutSeconds must be/,
    );
  refuses((c) => (c.stages[0].kind = "shell"), /kind must be one of/);
  refuses((c) => (c.stages[0].args = []), /args must be a nonempty array/);
  refuses(
    (c) => (c.stages[0].args = ["up", 1]),
    /args must be a nonempty array/,
  );
  refuses(
    (c) => (c.stages[1].action = "restart"),
    /action must be one of up, down/,
  );
  refuses((c) => delete c.stages[1].runDir, /runDir is required/);
  refuses((c) => (c.stages[1].runDir = "run"), /runDir must be an absolute/);
  refuses(
    (c) => (c.stages[2].files = ["../escape.test.ts"]),
    /files must be distinct/,
  );
  refuses(
    (c) => (c.stages[2].files = ["devnet/other/x.test.ts"]),
    /files must be distinct/,
  );
  refuses(
    (c) => c.stages[2].files.push(c.stages[2].files[0]),
    /files must be distinct/,
  );
  for (const key of [
    "MIDGARD_REAL_BLUEPRINT_PATH",
    "MIDGARD_WATCHER_JOURNEY_RUN_DIR",
    "NETWORK",
    "MIDGARD_CONTRIB_RESOURCE_TOKENS",
  ])
    refuses((c) => (c.stages[2].env = { [key]: "x" }), /the campaign sets it/);
  refuses((c) => (c.stages[2].env = { "BAD-KEY": "x" }), /not a variable name/);
  refuses((c) => (c.stages[2].env = { SEED: 1 }), /must be a string/);
  refuses((c) => (c.stages[1].id = "stack-up"), /stage ids repeat: stack-up/);
  refuses((c) => (c.stages[0].id = "Stack Up"), /lowercase identifier/);
  // Every problem is reported at once.
  const value = structuredClone(valid);
  value.extra = 1;
  value.stages[0].timeoutSeconds = 0;
  assert.throws(
    () => parseCampaign(value),
    (error) =>
      /unknown field extra/.test(error.message) &&
      /stages\[0\]\.timeoutSeconds/.test(error.message),
  );
});

test("always is allowed only on a teardown stage and is part of its definition", () => {
  const withTeardowns = structuredClone(valid);
  withTeardowns.stages.push(
    {
      id: "l1-down",
      kind: "phase4",
      action: "down",
      runDir: "/absolute/run",
      timeoutSeconds: 600,
      always: true,
    },
    teardown("stack-down", ["--run-dir", "/absolute/stack", "--l1"]),
  );
  assert.equal(parseCampaign(withTeardowns).stages.length, 5);
  for (const value of [false, "true", 1])
    refuses(
      (c) =>
        (c.stages[0] = {
          ...teardown("down", ["--run-dir", "/x"]),
          always: value,
        }),
      /stages\[0\]\.always must be true; leave it out for an ordinary stage/,
    );
  refuses(
    (c) => (c.stages[0].always = true),
    /stages\[0\]\.always is allowed only on a teardown stage \(phase4 action down, or devnet-stack down\)/,
  );
  refuses(
    (c) => (c.stages[1].always = true),
    /stages\[1\]\.always is allowed only on a teardown stage/,
  );
  refuses(
    (c) => (c.stages[2].always = true),
    /\(watcher-journeys\) has unknown field always/,
  );
  // Marking a stage always reruns it: the digest covers the field.
  const down = teardown("down", ["--run-dir", "/x"]);
  const { always: _always, ...ordinary } = down;
  assert.notEqual(definitionDigest(down), definitionDigest(ordinary));
});

test("a checkout at another commit or with changes is refused, never moved", async (t) => {
  const f = fixture(t);
  const other = "b".repeat(40);
  const path = campaignFile(f, [stack("one", ["record", f.marks, "one"])], {
    commit: other,
  });
  await assert.rejects(
    run(f, path),
    new RegExp(
      `HEAD is ${f.head}, not the campaign commit ${other}.*worktree create --branch <name> --base ${other}`,
    ),
  );
  assert.equal(git(f.root, "rev-parse", "HEAD"), f.head);
  const plan = campaignPlan(f.root, path, {
    profileCheck: () => true,
    buildSteps: () => [],
  });
  assert.equal(plan.exitCode, 1);
  assert.match(plan.refusal, /never moves HEAD/);

  const pinned = campaignFile(f, [stack("one", ["record", f.marks, "one"])]);
  writeFileSync(join(f.root, "README"), "edited\n");
  await assert.rejects(run(f, pinned), /1 change\(s\).*README.*clean commit/);
  git(f.root, "checkout", "--", "README");
  writeFileSync(join(f.root, "stray.txt"), "new\n");
  await assert.rejects(run(f, pinned), /stray\.txt/);
  rmSync(join(f.root, "stray.txt"));
  assert.deepEqual(marks(f), []);
  assert.equal(existsSync(join(f.directory, "campaign-receipt.json")), false);
});

test("the generated deployment-profile file may differ only as that profile's build output", (t) => {
  const f = fixture(t);
  const generated = join(
    f.root,
    "demo/midgard-core/src/generated-deployment-profiles.ts",
  );
  mkdirSync(join(f.root, "demo/midgard-core/src"), { recursive: true });
  writeFileSync(generated, "preprod\n");
  f.head = commitAll(f.root, "generated");
  writeFileSync(generated, "local-devnet\n");
  const campaign = parseCampaign({ ...structuredClone(valid), commit: f.head });
  const git2 = (root, args) =>
    execFileSync("git", args, { cwd: root, env, encoding: "utf8" });
  assert.equal(
    pinCheck(f.root, campaign, { git: git2, profileCheck: () => true }).refusal,
    undefined,
  );
  assert.match(
    pinCheck(f.root, campaign, { git: git2, profileCheck: () => false })
      .refusal,
    /is not the generated file of profile local-devnet-testing/,
  );
});

test("stages run strictly in order into one receipt with per-stage logs", async (t) => {
  const f = fixture(t);
  builds.length = 0;
  const path = campaignFile(f, [
    stack("first", ["record", f.marks, "first"]),
    stack("second", ["record", f.marks, "second"]),
    stack("third", ["record", f.marks, "third"]),
  ]);
  const result = await run(f, path);
  assert.equal(result.status, "passed");
  assert.equal(result.exitCode, 0);
  assert.deepEqual(marks(f), ["first", "second", "third"]);
  assert.equal(builds.length, 1);
  const receipt = receiptOf(f);
  assert.equal(receipt.schema, "midgard-devnet-campaign-receipt/v1");
  assert.equal(receipt.commit, f.head);
  assert.deepEqual(
    receipt.stages.map((entry) => [entry.id, entry.status, entry.exitCode]),
    [
      ["first", "passed", 0],
      ["second", "passed", 0],
      ["third", "passed", 0],
    ],
  );
  const [first] = receipt.stages;
  assert.equal(first.kind, "devnet-stack");
  assert.equal(first.logPath, join(f.directory, "logs/first.log"));
  assert.match(readFileSync(first.logPath, "utf8"), /recorded first/);
  assert.equal(
    first.argv[1],
    join(f.root, "demo/midgard-node-tools/dist/devnet-stack.js"),
  );
  assert.equal(first.cwd, join(f.root, "demo/midgard-node-tools"));
  assert.ok(first.startedAt <= first.endedAt);
  // The blueprint and its build record are frozen and digested once.
  assert.equal(
    receipt.blueprint.sha256,
    sha256(readFileSync(join(f.root, "onchain/aiken/plutus.json"))),
  );
  assert.equal(
    readFileSync(join(f.directory, "plutus.json.deployment.json"), "utf8"),
    '{"profile":"local-devnet-testing"}\n',
  );
});

test("a stage past its deadline is killed after the grace period and recorded as failed", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [
    stack("hangs", ["hang"], 1),
    stack("after", ["record", f.marks, "after"]),
  ]);
  const result = await run(f, path);
  assert.equal(result.status, "failed");
  assert.equal(result.exitCode, 1);
  const [entry] = result.stages;
  assert.equal(entry.status, "failed");
  assert.equal(entry.timedOut, true);
  assert.match(entry.reason, /deadline exceeded 1000 ms/);
  // It ignored SIGTERM, so SIGKILL ended it after the grace period.
  assert.equal(entry.signal, "SIGKILL");
  assert.match(readFileSync(entry.logPath, "utf8"), /journaled on SIGTERM/);
  assert.deepEqual(marks(f), []);
});

test("a failed stage stops the campaign; a rerun resumes from the first stage not passed", async (t) => {
  const f = fixture(t);
  const ready = join(f.base, "ready");
  const stages = [
    stack("first", ["record", f.marks, "first"]),
    stack("flaky", ["flaky", ready, f.marks]),
    stack("last", ["record", f.marks, "last"]),
  ];
  const path = campaignFile(f, stages);
  const failed = await run(f, path);
  assert.equal(failed.exitCode, 1);
  assert.deepEqual(marks(f), ["first", "flaky"]);
  assert.deepEqual(
    failed.stages.map((entry) => [entry.id, entry.status]),
    [
      ["first", "passed"],
      ["flaky", "failed"],
    ],
  );

  writeFileSync(ready, "");
  const resumed = await run(f, path);
  assert.equal(resumed.exitCode, 0);
  assert.deepEqual(marks(f), ["first", "flaky", "flaky", "last"]);
  const retry = resumed.stages.find(
    (entry) => entry.id === "flaky" && entry.attempt === 2,
  );
  assert.equal(retry.status, "passed");
  // The failed attempt's log is kept beside the retry's.
  assert.equal(retry.logPath, join(f.directory, "logs/flaky.2.log"));
  assert.match(
    readFileSync(join(f.directory, "logs/flaky.log"), "utf8"),
    /not yet/,
  );
  assert.deepEqual(resumed.invocations.at(-1).skipped, ["first"]);

  // Everything passed: a rerun runs nothing.
  const again = await run(f, path);
  assert.equal(again.exitCode, 0);
  assert.deepEqual(marks(f), ["first", "flaky", "flaky", "last"]);

  // A changed stage definition reruns from that stage on.
  const changed = structuredClone(stages);
  changed[1].timeoutSeconds = 31;
  const edited = campaignFile(f, changed);
  const plan = campaignPlan(f.root, edited, {
    profileCheck: () => true,
    buildSteps: () => [],
  });
  assert.deepEqual(
    plan.stages.map((stage) => stage.action),
    ["skip: passed", "run", "run"],
  );
  const rerun = await run(f, edited);
  assert.equal(rerun.exitCode, 0);
  assert.deepEqual(marks(f), [
    "first",
    "flaky",
    "flaky",
    "last",
    "flaky",
    "last",
  ]);
});

test("a failed stage still runs the teardown block after it and exits with the failure", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [
    stack("first", ["up", f.marks, "first"]),
    stack("fails", ["fail"]),
    stack("skipped", ["record", f.marks, "skipped"]),
    teardown("stack-down", [f.marks, "stack-down"]),
    teardown("l1-down", [f.marks, "l1-down"]),
    stack("after", ["up", f.marks, "after"]),
    teardown("after-down", [f.marks, "after-down"]),
  ]);
  const result = await run(f, path);
  assert.equal(result.status, "failed");
  assert.equal(result.exitCode, 1);
  // Ordinary stages after the failure are skipped; the always-stages that
  // close its segment run; nothing after them runs.
  assert.deepEqual(marks(f), ["first", "stack-down", "l1-down"]);
  assert.deepEqual(
    result.stages.map((entry) => [entry.id, entry.status]),
    [
      ["first", "passed"],
      ["fails", "failed"],
      ["stack-down", "passed"],
      ["l1-down", "passed"],
    ],
  );
  assert.deepEqual(result.invocations.at(-1).stagesRun, [
    "first",
    "fails",
    "stack-down",
    "l1-down",
  ]);

  // A failed teardown does not stop the rest of its block.
  const g = fixture(t);
  const failing = campaignFile(g, [
    stack("up", ["up", g.marks, "up"]),
    teardown("stack-down", [g.marks, "stack-down", join(g.base, "never")]),
    teardown("l1-down", [g.marks, "l1-down"]),
    stack("after", ["up", g.marks, "after"]),
  ]);
  const second = await run(g, failing);
  assert.equal(second.exitCode, 1);
  assert.deepEqual(marks(g), ["up", "l1-down"]);
  assert.deepEqual(
    second.stages.map((entry) => [entry.id, entry.status]),
    [
      ["up", "passed"],
      ["stack-down", "failed"],
      ["l1-down", "passed"],
    ],
  );
});

test("a resume after a teardown restarts its segment's services, skips passed work and tears down again", async (t) => {
  const f = fixture(t);
  const ready = join(f.base, "ready");
  const stages = [
    stack("stack-up", ["up", f.marks, "stack-up"]),
    stack("work", ["record", f.marks, "work"]),
    stack("flaky", ["flaky", ready, f.marks]),
    teardown("stack-down", [f.marks, "stack-down"]),
  ];
  const path = campaignFile(f, stages);
  assert.equal((await run(f, path)).exitCode, 1);
  assert.deepEqual(marks(f), ["stack-up", "work", "flaky", "stack-down"]);

  writeFileSync(ready, "");
  const plan = campaignPlan(f.root, path, {
    profileCheck: () => true,
    buildSteps: () => [],
  });
  assert.deepEqual(
    plan.stages.map((stage) => [stage.id, stage.action]),
    [
      ["stack-up", "run: an always-stage stopped its services after it passed"],
      ["work", "skip: passed"],
      ["flaky", "run"],
      ["stack-down", "run"],
    ],
  );
  assert.equal(plan.stages[3].always, true);
  const resumed = await run(f, path);
  assert.equal(resumed.exitCode, 0);
  assert.deepEqual(marks(f), [
    "stack-up",
    "work",
    "flaky",
    "stack-down",
    "stack-up",
    "flaky",
    "stack-down",
  ]);
  assert.deepEqual(resumed.invocations.at(-1).skipped, ["work"]);

  // Everything passed, teardown last: a rerun runs nothing.
  assert.equal((await run(f, path)).exitCode, 0);
  assert.equal(marks(f).length, 7);

  // A teardown that failed is retried alone: nothing it would stop runs
  // again first.
  const g = fixture(t);
  const later = join(g.base, "later");
  const failing = campaignFile(g, [
    stack("stack-up", ["up", g.marks, "stack-up"]),
    teardown("stack-down", [g.marks, "stack-down", later]),
  ]);
  assert.equal((await run(g, failing)).exitCode, 1);
  writeFileSync(later, "");
  assert.equal((await run(g, failing)).exitCode, 0);
  assert.deepEqual(marks(g), ["stack-up", "stack-down"]);
});

test("cancellation stops the running stage, still runs the teardown, and leaves the campaign incomplete", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [
    stack("hangs", ["hang"], 60),
    stack("after", ["up", f.marks, "after"]),
    teardown("stack-down", [f.marks, "stack-down"]),
  ]);
  const until = async (predicate) => {
    const deadline = Date.now() + 10_000;
    while (!predicate()) {
      assert.ok(Date.now() < deadline, "the stage never started");
      await new Promise((done) => setTimeout(done, 20));
    }
  };
  const controller = new AbortController();
  const pending = run(f, path, { signal: controller.signal });
  const log = join(f.directory, "logs/hangs.log");
  await until(
    () => existsSync(log) && /hanging/.test(readFileSync(log, "utf8")),
  );
  controller.abort();
  const result = await pending;
  assert.equal(result.status, "incomplete");
  assert.equal(result.exitCode, 3);
  assert.deepEqual(
    result.stages.map((entry) => [entry.id, entry.status]),
    [
      ["hangs", "cancelled"],
      ["stack-down", "passed"],
    ],
  );
  assert.deepEqual(marks(f), ["stack-down"]);

  // Cancelled during a teardown: it finishes, and the next segment, which
  // started nothing, is neither run nor torn down.
  const g = fixture(t);
  const boundary = campaignFile(g, [
    stack("l1-up", ["up", g.marks, "l1-up"]),
    teardown("l1-down", [g.marks, "l1-down", "--slow"]),
    stack("stack-up", ["up", g.marks, "stack-up"]),
    teardown("stack-down", [g.marks, "stack-down"]),
  ]);
  const second = new AbortController();
  const running = run(g, boundary, { signal: second.signal });
  await until(() => existsSync(join(g.directory, "logs/l1-down.log")));
  second.abort();
  const stopped = await running;
  assert.equal(stopped.exitCode, 3);
  assert.deepEqual(
    stopped.stages.map((entry) => [entry.id, entry.status]),
    [
      ["l1-up", "passed"],
      ["l1-down", "passed"],
    ],
  );
  assert.deepEqual(marks(g), ["l1-up", "l1-down"]);
});

test("a receipt from another commit or profile is refused: a code change needs a new directory", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [stack("one", ["record", f.marks, "one"])]);
  assert.equal((await run(f, path)).exitCode, 0);
  writeFileSync(join(f.root, "README"), "a code change\n");
  const next = commitAll(f.root, "change");
  const moved = campaignFile(f, [stack("one", ["record", f.marks, "one"])], {
    commit: next,
  });
  await assert.rejects(
    run(f, moved),
    /another commit.*A code change needs a new campaign directory/,
  );
  const profile = campaignFile(f, [stack("one", ["record", f.marks, "one"])], {
    commit: next,
    deploymentProfile: "preprod-testing",
  });
  assert.throws(
    () => campaignPlan(f.root, profile, { profileCheck: () => true }),
    /commit and deploymentProfile.*new campaign directory/,
  );
  assert.deepEqual(marks(f), ["one"]);
});

test("a changed checkout blueprint is refused on resume", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [stack("fails", ["fail"])]);
  assert.equal((await run(f, path)).exitCode, 1);
  writeFileSync(
    join(f.root, "onchain/aiken/plutus.json"),
    '{"validators":[1]}\n',
  );
  await assert.rejects(
    run(f, path),
    /differs from the blueprint this campaign froze/,
  );
});

test("a campaign directory inside the checkout is refused", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [stack("one", ["record", f.marks, "one"])], {
    directory: join(f.root, "campaign"),
  });
  await assert.rejects(run(f, path), /inside the checkout/);
});

test("a watcher-journeys stage runs contrib test with the frozen blueprint and journey environment", async (t) => {
  const f = fixture(t);
  const runDir = journeyRun(f);
  const path = campaignFile(f, [
    {
      id: "deployment",
      kind: "watcher-journeys",
      runDir,
      files: [
        "devnet/watcher-journeys/configuration.test.ts",
        "devnet/watcher-journeys/deployment.test.ts",
      ],
      env: { JOURNEY_SEED: "hunter2-stage-value" },
      timeoutSeconds: 7200,
    },
  ]);
  const calls = [];
  const fakeRun = async (options) => {
    calls.push(options);
    writeFileSync(options.logPath, "journeys ran\n");
    const output = options.argv[options.argv.indexOf("--output") + 1];
    mkdirSync(resolve(output, ".."), { recursive: true });
    writeFileSync(
      output,
      JSON.stringify({
        status: "passed",
        exitCode: 0,
        path: "/registry/run/receipt.json",
        counts: { passed: 2, failed: 0, skipped: 0 },
      }),
    );
    return {
      exitCode: 0,
      signal: null,
      durationMs: 5,
      logPath: options.logPath,
    };
  };
  const result = await run(f, path, { run: fakeRun });
  assert.equal(result.exitCode, 0);
  const [call] = calls;
  assert.deepEqual(call.argv.slice(1), [
    join(f.root, "scripts/contrib.mjs"),
    "--root",
    f.root,
    "test",
    "--package",
    "midgard-node-tools",
    "--config",
    "vitest.watcher-journeys.config.ts",
    "--timeout-seconds",
    "7200",
    "--output",
    join(f.directory, "receipts/deployment.json"),
    "--file",
    "devnet/watcher-journeys/configuration.test.ts",
    "devnet/watcher-journeys/deployment.test.ts",
  ]);
  assert.equal(call.timeoutMs, 7_200_000);
  assert.equal(call.env.MIDGARD_DEPLOYMENT_PROFILE, "local-devnet-testing");
  assert.equal(call.env.NETWORK, "Custom");
  assert.equal(call.env.MIDGARD_DOTENV_MODE, "disabled");
  assert.equal(call.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR, runDir);
  assert.equal(
    call.env.MIDGARD_REAL_BLUEPRINT_PATH,
    join(f.directory, "plutus.json"),
  );
  assert.equal(call.env.JOURNEY_SEED, "hunter2-stage-value");
  assert.ok(
    call.env.MIDGARD_CONTRIB_RESOURCE_TOKENS,
    "the stage inherits the campaign's leases",
  );
  const [entry] = result.stages;
  assert.deepEqual(entry.childReceipt, {
    output: join(f.directory, "receipts/deployment.json"),
    receipt: "/registry/run/receipt.json",
    status: "passed",
    exitCode: 0,
    counts: { passed: 2, failed: 0, skipped: 0 },
  });
  assert.ok(entry.envKeys.includes("JOURNEY_SEED"));
  assert.ok(entry.envKeys.includes("MIDGARD_REAL_BLUEPRINT_PATH"));

  // Keys only: no environment value reaches the receipt.
  const text = readFileSync(join(f.directory, "campaign-receipt.json"), "utf8");
  assert.doesNotMatch(text, /hunter2-stage-value/);
  assert.doesNotMatch(text, /supersecret-campaign-value/);
});

test("a journey stage refuses a run directory the watcher-journeys generator did not prepare, by path, before running anything", async (t) => {
  const f = fixture(t);
  const runDir = join(f.base, "bare-run");
  mkdirSync(join(runDir, "config"), { recursive: true });
  writeFileSync(join(runDir, "config/config.json"), "{}\n");
  const path = campaignFile(f, [
    {
      id: "journey",
      kind: "watcher-journeys",
      runDir,
      files: ["devnet/watcher-journeys/journey.test.ts"],
      timeoutSeconds: 60,
    },
  ]);
  const plan = campaignPlan(f.root, path, {
    profileCheck: () => true,
    buildSteps: () => [],
  });
  assert.deepEqual(plan.stages[0].missingInputs, [
    join(runDir, "run.env"),
    join(runDir, "compose.host-paths.json"),
  ]);
  const calls = [];
  const result = await run(f, path, {
    run: async (options) => calls.push(options),
  });
  assert.equal(calls.length, 0, "no journey started");
  assert.equal(result.exitCode, 1);
  const [entry] = result.stages;
  assert.equal(entry.status, "failed");
  const generate = `MIDGARD_WATCHER_JOURNEY_RUN_DIR=${runDir} sh demo/midgard-node-tools/devnet/watcher-journeys/scripts/generate.sh`;
  assert.equal(
    entry.reason,
    `the journey run directory ${runDir} lacks ${join(runDir, "run.env")}, ${join(runDir, "compose.host-paths.json")}; generate the run first with \`${generate}\` and start its L1 services with a phase4 up stage`,
  );
  assert.match(readFileSync(entry.logPath, "utf8"), /lacks/);
});

test("a journey stage copies the campaign's transport binary into the run once, and refuses another build's", async (t) => {
  const f = fixture(t);
  const runDir = journeyRun(f);
  const path = campaignFile(f, [
    {
      id: "journey",
      kind: "watcher-journeys",
      runDir,
      files: ["devnet/watcher-journeys/journey.test.ts"],
      timeoutSeconds: 60,
    },
  ]);
  const binary = join(runDir, "work/midgard-l1-node-transport");
  const seen = [];
  const fakeRun = async (options) => {
    seen.push(readFileSync(binary, "utf8"));
    writeFileSync(options.logPath, "");
    const output = options.argv[options.argv.indexOf("--output") + 1];
    mkdirSync(resolve(output, ".."), { recursive: true });
    writeFileSync(output, JSON.stringify({ status: "failed", exitCode: 1 }));
    return { exitCode: 1, signal: null };
  };
  const first = await run(f, path, { run: fakeRun });
  assert.deepEqual(seen, ["transport build\n"]);
  assert.equal(statSync(binary).mode & 0o777, 0o755);
  assert.deepEqual(first.stages[0].transportBinary, {
    path: binary,
    sha256: sha256("transport build\n"),
    staged: "copied",
  });
  // A resume finds the same build already in place.
  const again = await run(f, path, { run: fakeRun });
  assert.equal(again.stages.at(-1).transportBinary.staged, "present");
  assert.equal(seen.length, 2);

  writeFileSync(binary, "another build\n");
  const refused = await run(f, path, { run: fakeRun });
  assert.equal(seen.length, 2, "no journey started");
  assert.equal(refused.exitCode, 1);
  assert.match(
    refused.stages.at(-1).reason,
    new RegExp(
      `${binary} is another build's transport binary \\(sha256 ${sha256("another build\n")}; this campaign built ${sha256("transport build\n")}\\)`,
    ),
  );
  assert.equal(readFileSync(binary, "utf8"), "another build\n");
});

test("a journey stage whose tests were skipped leaves the campaign incomplete", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [
    {
      id: "journey",
      kind: "watcher-journeys",
      runDir: journeyRun(f),
      files: ["devnet/watcher-journeys/journey.test.ts"],
      timeoutSeconds: 60,
    },
  ]);
  const fakeRun = async (options) => {
    writeFileSync(options.logPath, "");
    const output = options.argv[options.argv.indexOf("--output") + 1];
    mkdirSync(resolve(output, ".."), { recursive: true });
    writeFileSync(
      output,
      JSON.stringify({ status: "incomplete", exitCode: 3 }),
    );
    return { exitCode: 3, signal: null };
  };
  const result = await run(f, path, { run: fakeRun });
  assert.equal(result.status, "incomplete");
  assert.equal(result.exitCode, 3);
  assert.equal(result.stages[0].status, "incomplete");
});

test("the stage environment is passed to devnet-stack but never recorded", async (t) => {
  const f = fixture(t);
  const path = campaignFile(f, [stack("env", ["env"])]);
  const result = await run(f, path);
  assert.equal(result.exitCode, 0);
  assert.match(
    readFileSync(result.stages[0].logPath, "utf8"),
    /secret visible/,
  );
  assert.deepEqual(result.stages[0].envKeys, []);
  assert.doesNotMatch(
    readFileSync(join(f.directory, "campaign-receipt.json"), "utf8"),
    /supersecret-campaign-value/,
  );
});

test("phase4 stages run the run directory's up and down scripts", () => {
  const campaign = parseCampaign(structuredClone(valid));
  const command = stageCommand("/checkout", campaign, campaign.stages[1]);
  assert.deepEqual(command.argv, [
    "sh",
    "/checkout/demo/midgard-node-tools/devnet/phase4-process/scripts/up.sh",
    "/absolute/run",
  ]);
  assert.deepEqual(command.env, {});
});

test("the CLI plans a campaign without running it and refuses a mismatched commit", async (t) => {
  const f = fixture(t);
  const stages = [
    {
      id: "l1-down",
      kind: "phase4",
      action: "down",
      runDir: join(f.base, "run"),
      timeoutSeconds: 60,
    },
  ];
  const cli = (path) =>
    execFileSync(
      process.execPath,
      [
        join(repositoryRoot, "scripts/contrib.mjs"),
        "--root",
        f.root,
        "devnet",
        "campaign",
        "run",
        "--plan",
        "--input",
        path,
      ],
      { env, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
    );
  const plan = JSON.parse(cli(campaignFile(f, stages)));
  assert.equal(plan.schema, "midgard-devnet-campaign-plan/v1");
  assert.equal(plan.refusal, undefined);
  assert.deepEqual(plan.build, []);
  assert.equal(plan.stages[0].action, "run");
  assert.equal(existsSync(f.directory), false);
  assert.throws(
    () => cli(campaignFile(f, stages, { commit: "c".repeat(40) })),
    (error) => error.status === 1 && /never moves HEAD/.test(error.stdout),
  );
  assert.throws(
    () =>
      execFileSync(
        process.execPath,
        [
          join(repositoryRoot, "scripts/contrib.mjs"),
          "devnet",
          "campaign",
          "go",
        ],
        { env, stdio: "pipe" },
      ),
    (error) => error.status === 2,
  );
});
