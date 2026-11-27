import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  durationTierProjects,
  SLOW_TIER_FILE_SECONDS,
  withDurationTier,
} from "../vitest.js";

/** A package with four test files, two projects and a duration table that
 * weighs one file of each project slow and does not know a fifth file. */
const fixture = (t) => {
  const packageDirectory = mkdtempSync(join(tmpdir(), "midgard-tier-test-"));
  t.after(() => rmSync(packageDirectory, { recursive: true, force: true }));
  mkdirSync(join(packageDirectory, "tests/support"), { recursive: true });
  mkdirSync(join(packageDirectory, "journeys"));
  for (const file of [
    "tests/quick.test.ts",
    "tests/long.test.ts",
    "tests/unknown.test.ts",
    "journeys/short.test.ts",
    "journeys/max-shape.test.ts",
  ])
    writeFileSync(join(packageDirectory, file), "");
  const tablePath = join(
    packageDirectory,
    "tests/support/ci-file-durations.json",
  );
  writeFileSync(
    tablePath,
    JSON.stringify({
      forksPerShard: 2,
      reservedSeconds: {},
      defaultSeconds: 5,
      files: {
        "tests/quick.test.ts": 2,
        "tests/long.test.ts": SLOW_TIER_FILE_SECONDS,
        "journeys/short.test.ts": SLOW_TIER_FILE_SECONDS - 0.1,
        "journeys/max-shape.test.ts": 900,
      },
    }),
  );
  const projects = [
    { test: { name: "unit", include: ["tests/**/*.test.ts"] } },
    {
      test: {
        name: "journeys",
        include: ["journeys/**/*.test.ts"],
        exclude: ["journeys/never.test.ts"],
      },
    },
  ];
  return { packageDirectory, tablePath, projects };
};

const withTier = (t, tier) => {
  const previous = process.env.MIDGARD_TEST_TIER;
  if (tier === undefined) delete process.env.MIDGARD_TEST_TIER;
  else process.env.MIDGARD_TEST_TIER = tier;
  t.after(() => {
    if (previous === undefined) delete process.env.MIDGARD_TEST_TIER;
    else process.env.MIDGARD_TEST_TIER = previous;
  });
};

test("without a tier, or with all, the projects are unchanged", (t) => {
  const { packageDirectory, tablePath, projects } = fixture(t);
  for (const tier of [undefined, "all"]) {
    withTier(t, tier);
    assert.equal(
      durationTierProjects(projects, { tablePath, packageDirectory }),
      projects,
    );
  }
});

test("the fast tier excludes exactly the files at or over the threshold", (t) => {
  const { packageDirectory, tablePath, projects } = fixture(t);
  withTier(t, "fast");
  const [unit, journeys] = durationTierProjects(projects, {
    tablePath,
    packageDirectory,
  });
  assert.deepEqual(unit.test.include, ["tests/**/*.test.ts"]);
  assert.ok(unit.test.exclude.includes("**/node_modules/**"), "keeps defaults");
  assert.deepEqual(journeys.test.exclude, [
    "journeys/never.test.ts",
    "./tests/long.test.ts",
    "./journeys/max-shape.test.ts",
  ]);
});

test("the slow tier includes exactly those files, project by project", (t) => {
  const { packageDirectory, tablePath, projects } = fixture(t);
  withTier(t, "slow");
  assert.deepEqual(
    durationTierProjects(projects, { tablePath, packageDirectory }).map(
      (project) => [project.test.name, project.test.include],
    ),
    [
      ["unit", ["./tests/long.test.ts"]],
      ["journeys", ["./journeys/max-shape.test.ts"]],
    ],
  );
  // A project with no slow file drops out; none at all leaves one empty one.
  assert.deepEqual(
    durationTierProjects(projects, {
      tablePath,
      packageDirectory,
      slowSeconds: 600,
    }).map((project) => project.test.name),
    ["journeys"],
  );
  assert.deepEqual(
    durationTierProjects(projects, {
      tablePath,
      packageDirectory,
      slowSeconds: 10_000,
    }).map((project) => [project.test.name, project.test.include]),
    [["unit", []]],
  );
});

test("an unknown tier refuses the config, and withDurationTier needs projects", (t) => {
  const { packageDirectory, tablePath, projects } = fixture(t);
  withTier(t, "quick");
  assert.throws(
    () => durationTierProjects(projects, { tablePath, packageDirectory }),
    /fast, slow or all/u,
  );
  withTier(t, "slow");
  assert.throws(
    () => withDurationTier({ test: {} }, { tablePath, packageDirectory }),
    /test\.projects/u,
  );
  const config = withDurationTier(
    { plugins: [], test: { projects } },
    { tablePath, packageDirectory },
  );
  assert.deepEqual(config.plugins, []);
  assert.equal(config.test.projects.length, 2);
});
