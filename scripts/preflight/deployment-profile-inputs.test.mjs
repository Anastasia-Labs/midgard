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
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { loadYaml } from "../ci/lint-workflows.mjs";
import { matchesFilter } from "../ci/workflow-path-filters.mjs";
import {
  DEPLOYMENT_PROFILE_GENERATOR,
  deploymentProfileInputs,
  matchesAny,
} from "./derive.mjs";
import { fullRunPaths } from "./registry.demo-checks.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const yaml = loadYaml(root);

const aikenCiPaths = () => {
  const { on } = yaml.parse(
    readFileSync(resolve(root, ".github/workflows/aiken-ci.yml"), "utf8"),
  );
  return { push: on.push.paths, pull_request: on.pull_request.paths };
};

test("a module the profile generator imports is a full-run input without a list edit", () => {
  const fixture = mkdtempSync(join(tmpdir(), "deployment-profile-inputs-"));
  try {
    const write = (path, source) => {
      mkdirSync(dirname(join(fixture, path)), { recursive: true });
      writeFileSync(join(fixture, path), source);
    };
    write(
      DEPLOYMENT_PROFILE_GENERATOR,
      'import { x } from "./deployment-profile-x.mjs";\n',
    );
    write(
      "demo/scripts/deployment-profile-x.mjs",
      'export { x } from "./lib/profile-helper.mjs";\n',
    );
    write("demo/scripts/lib/profile-helper.mjs", "export const x = 1;\n");
    const inputs = deploymentProfileInputs(fixture);
    assert.deepEqual(inputs, [
      DEPLOYMENT_PROFILE_GENERATOR,
      "demo/scripts/deployment-profile-x.mjs",
      "demo/scripts/lib/profile-helper.mjs",
    ]);
    const fullRun = fullRunPaths(fixture);
    for (const path of inputs) assert.ok(matchesAny(path, fullRun), path);
  } finally {
    rmSync(fixture, { recursive: true, force: true });
  }
});

test(
  "Aiken CI runs for every deployment-profile input and any new profile module",
  {
    skip: yaml === undefined && "install demo dependencies to parse workflows",
  },
  () => {
    for (const [event, paths] of Object.entries(aikenCiPaths()))
      for (const path of [
        ...deploymentProfileInputs(root),
        "demo/scripts/deployment-profile-new.mjs",
      ])
        assert.ok(matchesFilter(paths, path), `${event}: ${path}`);
  },
);

// --- superset of the replaced hand lists (#901) -------------------------------
//
// Kept for one release after the hand lists were replaced, then deleted: each
// list below is what a file named the deployment-profile modules by before
// the derivation, and the derived set must still cover every entry.

const PROFILE_MODULES = [
  "demo/scripts/deployment-profiles.mjs",
  "demo/scripts/deployment-profile-aiken.mjs",
  "demo/scripts/deployment-profile-identity.mjs",
  "demo/scripts/deployment-profile-da.mjs",
  "demo/scripts/deployment-profile-names.mjs",
  "demo/scripts/deployment-profile-budgets.mjs",
  "demo/scripts/deployment-profile-operator-economics.mjs",
];

const HAND_LISTS = {
  "scripts/preflight/registry.demo-checks.mjs FULL_RUN": PROFILE_MODULES,
  ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json deployment-profiles generators":
    PROFILE_MODULES,
  "demo/scripts/interactive-emulator.test.mjs producer tree": [
    ...PROFILE_MODULES,
    "demo/scripts/lib/blueprint-stamp.mjs",
    "demo/flake.lock",
    "scripts/lib/aiken.mjs",
  ],
};

const AIKEN_CI_HAND_LIST = [
  ...PROFILE_MODULES,
  "demo/scripts/deployment-profiles.test.mjs",
  "demo/scripts/deployment-profiles.retention.test.mjs",
  "demo/scripts/lib/vendored-imports.mjs",
];

test("the derived profile inputs cover every replaced hand list", () => {
  const inputs = new Set(deploymentProfileInputs(root));
  const fullRun = fullRunPaths(root);
  for (const [list, paths] of Object.entries(HAND_LISTS))
    for (const path of paths) {
      assert.ok(inputs.has(path), `${list}: ${path}`);
      assert.ok(matchesAny(path, fullRun), `${list}: ${path}`);
    }
});

test(
  "Aiken CI's globs cover its replaced hand list",
  {
    skip: yaml === undefined && "install demo dependencies to parse workflows",
  },
  () => {
    for (const [event, paths] of Object.entries(aikenCiPaths()))
      for (const path of AIKEN_CI_HAND_LIST)
        assert.ok(
          matchesFilter(paths, path),
          `.github/workflows/aiken-ci.yml ${event}.paths: ${path}`,
        );
  },
);
