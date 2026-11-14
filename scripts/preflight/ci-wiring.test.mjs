// CI and the local gate run one command per check: a workflow step runs a
// registry check by id (`node scripts/preflight.mjs --run <id>`), so the
// command lives only in the registry and the two cannot drift. This test
// fails when a step names a check preflight cannot run, a check that would
// only warn, a check from outside the repository root, or when a step spells
// out a registry check's command by hand instead of naming it.
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { loadYaml } from "../ci/lint-workflows.mjs";
import { buildRegistry, formatCommand } from "./registry.mjs";

const PREFLIGHT = /^node scripts\/preflight\.mjs(?=\s|$)/u;

/** Each step's run segments, with the directory the step runs in. */
const runSegments = (workflows) =>
  workflows.flatMap(({ file, workflow }) =>
    Object.entries(workflow.jobs ?? {}).flatMap(([job, body]) =>
      (body.steps ?? [])
        .filter((step) => typeof step.run === "string")
        .flatMap((step) =>
          step.run
            .split(/\n|&&/u)
            .map((segment) => segment.trim())
            .filter(Boolean)
            .map((segment) => ({
              where: `${file} ${job} "${step.name ?? "(unnamed)"}"`,
              directory:
                step["working-directory"] ??
                body.defaults?.run?.["working-directory"] ??
                workflow.defaults?.run?.["working-directory"] ??
                ".",
              segment,
            })),
        ),
    ),
  );

/**
 * A segment as it would read from the repository root, for a step whose
 * directory is not the root: `node <path>` and `pnpm run` gain the directory,
 * the way the registry spells them.
 */
const fromRoot = (directory, segment) => {
  const prefix = directory.replace(/^\.\/|\/$/gu, "");
  if (prefix === "" || prefix === ".") return segment;
  const [command, first, ...rest] = segment.split(/\s+/u);
  if (command === "node" && first !== undefined && !/^[-/]/u.test(first))
    return ["node", `${prefix}/${first}`, ...rest].join(" ");
  if (command === "pnpm" && first !== "--dir")
    return ["pnpm", "--dir", prefix, first, ...rest].join(" ");
  return segment;
};

/** Every way the workflows' check wiring disagrees with the registry. */
const ciWiringProblems = (registry, workflows) => {
  const checks = new Map(registry.checks.map((check) => [check.id, check]));
  const commands = new Map();
  for (const check of registry.checks) {
    if (check.internal !== undefined) continue;
    const steps = check.plan({ matched: [], full: true, advise: () => {} });
    // A step's env and working directory sit outside its run text, so the
    // bare argv is matched as well as preflight's display form.
    if (steps?.length === 1)
      for (const spelling of [
        formatCommand(steps[0]),
        formatCommand({ argv: steps[0].argv }),
      ])
        commands.set(spelling, check.id);
  }
  const problems = [];
  for (const { where, directory, segment } of runSegments(workflows)) {
    if (PREFLIGHT.test(segment)) {
      const ids = [...segment.matchAll(/--run\s+(\S+)/gu)].map(([, id]) => id);
      if (ids.length === 0) continue;
      if (directory !== "." && directory !== "./")
        problems.push(`${where}: runs preflight from ${directory}, not .`);
      for (const id of ids) {
        const check = checks.get(id);
        if (check === undefined) {
          problems.push(`${where}: --run ${id} names no registry check`);
          continue;
        }
        if (check.internal !== undefined)
          problems.push(`${where}: --run ${id} is an internal check`);
        const steps = check.plan({ matched: [], full: true, advise: () => {} });
        if (!steps?.length)
          problems.push(`${where}: --run ${id} plans no command`);
        if (check.warnOnly)
          problems.push(
            `${where}: --run ${id} is warn-only, so the registry does not see CI gating it`,
          );
      }
      continue;
    }
    const id =
      commands.get(segment) ?? commands.get(fromRoot(directory, segment));
    if (id !== undefined)
      problems.push(
        `${where}: spells out ${id}'s command; run it as node scripts/preflight.mjs --run ${id}`,
      );
  }
  return problems;
};

const fixtureRegistry = {
  checks: [
    {
      id: "gated",
      plan: () => [{ argv: ["node", "scripts/gated.mjs"], cwd: "." }],
    },
    {
      id: "warn",
      warnOnly: true,
      plan: () => [{ argv: ["node", "scripts/warn.mjs"], cwd: "." }],
    },
    { id: "empty", plan: () => null },
    { id: "merge", internal: "merge-tree", plan: () => [] },
  ],
};
const fixtureWorkflow = (steps, defaults) => [
  {
    file: "ci.yml",
    workflow: { jobs: { a: { ...(defaults && { defaults }), steps } } },
  },
];

test("a step that runs a gated check by id from the root is wired", () => {
  assert.deepEqual(
    ciWiringProblems(
      fixtureRegistry,
      fixtureWorkflow([
        { name: "x", run: "node scripts/preflight.mjs --run gated" },
        { name: "y", run: "node scripts/other.mjs" },
      ]),
    ),
    [],
  );
});

test("unknown, internal, planless, warn-only and misplaced ids are refused", () => {
  const problems = ciWiringProblems(
    fixtureRegistry,
    fixtureWorkflow(
      [
        {
          name: "x",
          run: "node scripts/preflight.mjs --run nope --run merge --run empty --run warn",
        },
        {
          name: "y",
          "working-directory": ".",
          run: "node scripts/preflight.mjs --run gated",
        },
        { name: "z", run: "node scripts/preflight.mjs --run gated" },
      ],
      { run: { "working-directory": "./onchain/aiken" } },
    ),
  );
  assert.deepEqual(problems, [
    'ci.yml a "x": runs preflight from ./onchain/aiken, not .',
    'ci.yml a "x": --run nope names no registry check',
    'ci.yml a "x": --run merge is an internal check',
    'ci.yml a "x": --run merge plans no command',
    'ci.yml a "x": --run empty plans no command',
    'ci.yml a "x": --run warn is warn-only, so the registry does not see CI gating it',
    'ci.yml a "z": runs preflight from ./onchain/aiken, not .',
  ]);
});

test("a step that spells out a registry command is refused", () => {
  assert.deepEqual(
    ciWiringProblems(
      fixtureRegistry,
      fixtureWorkflow([
        { name: "x", run: "echo start\nnode scripts/gated.mjs && echo done" },
        {
          name: "y",
          "working-directory": "./scripts",
          run: "node gated.mjs",
        },
      ]),
    ),
    [
      'ci.yml a "x": spells out gated\'s command; run it as node scripts/preflight.mjs --run gated',
      'ci.yml a "y": spells out gated\'s command; run it as node scripts/preflight.mjs --run gated',
    ],
  );
});

const repositoryRoot = fileURLToPath(new URL("../..", import.meta.url));
const yaml = loadYaml(repositoryRoot);

test(
  "the real workflows run registry checks only by id",
  {
    skip:
      yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
        ? "could not check: yaml absent (run `pnpm --dir demo install`)"
        : false,
  },
  () => {
    const directory = resolve(repositoryRoot, ".github/workflows");
    const workflows = readdirSync(directory)
      .filter((file) => /\.ya?ml$/u.test(file))
      .map((file) => ({
        file,
        workflow: yaml.parse(readFileSync(resolve(directory, file), "utf8")),
      }));
    const segments = runSegments(workflows).filter(({ segment }) =>
      /--run\s/u.test(segment),
    );
    assert.ok(segments.length > 0, "no workflow step runs a check by id");
    assert.deepEqual(
      ciWiringProblems(buildRegistry(repositoryRoot), workflows),
      [],
    );
  },
);

// The validation suites run the Aiken function evaluator on exports the
// `blueprint` job prefetches (demo/midgard-test-support/aiken-export-cache.js).
// A job that runs them restores the `aiken-exports` artifact, under which a
// miss fails instead of compiling, so no other job compiles an export.
const EVALUATOR_SUITES =
  /pnpm --dir demo\/midgard-validation (?:run )?test\b|--run tx-fuzz-reference\b/u;

test(
  "every job running the Aiken evaluator's suites restores the prefetched exports",
  {
    skip:
      yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
        ? "could not check: yaml absent (run `pnpm --dir demo install`)"
        : false,
  },
  () => {
    const directory = resolve(repositoryRoot, ".github/workflows");
    const consumers = readdirSync(directory)
      .filter((file) => /\.ya?ml$/u.test(file))
      .flatMap((file) => {
        const { jobs = {} } = yaml.parse(
          readFileSync(resolve(directory, file), "utf8"),
        );
        return Object.entries(jobs)
          .filter(([, body]) =>
            (body.steps ?? []).some((step) =>
              EVALUATOR_SUITES.test(step.run ?? ""),
            ),
          )
          .map(([job, body]) => ({ where: `${file} ${job}`, jobs, body }));
      });
    assert.ok(consumers.length > 0, "no job runs the evaluator's suites");
    for (const { where, jobs, body } of consumers) {
      const prefetch = jobs.blueprint?.steps ?? [];
      assert.deepEqual(
        {
          needsBlueprint: [body.needs ?? []].flat().includes("blueprint"),
          restores: (body.steps ?? []).some(
            (step) =>
              step.uses === "./.github/actions/demo-setup" &&
              step.with?.["aiken-exports"] === "true",
          ),
          prefetched: prefetch.some(
            (step) =>
              step.run ===
              "node demo/midgard-test-support/aiken-export-cache.js",
          ),
          shared: prefetch.some((step) => step.with?.name === "aiken-exports"),
        },
        {
          needsBlueprint: true,
          restores: true,
          prefetched: true,
          shared: true,
        },
        where,
      );
    }
  },
);

// The reverse direction: a check whose blocking status comes from the
// workflows warns only while no workflow runs it, so a new golden channel or
// gate left out of CI would sit silently warn-only. Every warn-only check is
// listed here with the reason it is not gated.
const WARN_ONLY_BY_DESIGN = new Map([
  ["merge-conflicts", "a predicted conflict is news, not a defect"],
  [
    "golden:script-decode-haskell-v1",
    "its producer needs the Haskell script-decode probe (MIDGARD_SCRIPT_DECODE_PROBE), which CI does not build",
  ],
]);

test("every warn-only check in the real registry is warn-only by design", () => {
  const warnOnly = buildRegistry(repositoryRoot)
    .checks.filter((check) => check.warnOnly)
    .map((check) => check.id)
    .sort();
  assert.deepEqual(warnOnly, [...WARN_ONLY_BY_DESIGN.keys()].sort());
});
