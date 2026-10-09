import assert from "node:assert/strict";
import {
  existsSync,
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

import {
  globToRegExp,
  matchesAny,
  referencedFiles,
  workflowRunsCheck,
} from "./derive.mjs";
import { loadYaml } from "../ci/lint-workflows.mjs";
import {
  buildRegistry,
  FULL_RUN,
  IGNORED_PATHS,
  selectChecks,
  VERIFICATION_ONLY,
} from "./registry.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const registry = buildRegistry(root);
const byId = new Map(registry.checks.map((check) => [check.id, check]));
const selectedIds = (changed, options) =>
  selectChecks(registry, changed, options).selected.map(
    ({ check }) => check.id,
  );

test("SDK obligation follows completed full suites without changing the other lanes", () => {
  const position = (id) =>
    registry.checks.findIndex((check) => check.id === id);
  assert.ok(position("tx-preparation:sdk") > position("demo-test"));
  assert.ok(position("tx-preparation:sdk") > position("demo-test-db"));
  const steps = byId.get("demo-test").plan({
    matched: ["demo/lucid-midgard/src/x.ts", "demo/midgard-sdk/src/x.ts"],
    full: true,
  });
  for (const name of ["@al-ft/lucid-midgard", "@al-ft/midgard-sdk"]) {
    assert.deepEqual(steps.find((step) => step.sdkSuite === name)?.argv, [
      "pnpm",
      "--filter",
      name,
      "test",
    ]);
    assert.equal(steps.find((step) => step.sdkSuite === name).cwd, "demo");
  }
});

test("globs: ** spans directories, * and ? stay within one segment", () => {
  assert.ok(globToRegExp("a/**").test("a/b/c.ts"));
  assert.ok(globToRegExp("**/AGENTS.md").test("AGENTS.md"));
  assert.ok(globToRegExp("**/AGENTS.md").test("demo/x/AGENTS.md"));
  assert.ok(globToRegExp("a/**/*.ak").test("a/x.ak"));
  assert.ok(globToRegExp("a/**/*.ak").test("a/b/c/x.ak"));
  assert.ok(!globToRegExp("a/*.ak").test("a/b/x.ak"));
  assert.ok(!globToRegExp("a/?.ak").test("a/xy.ak"));
  assert.ok(globToRegExp("d/**/*.{ts,md}").test("d/p/r.md"));
  assert.ok(!globToRegExp("d/**/*.{ts,md}").test("d/p/r.mjs"));
  // Regex metacharacters in a literal path are literal.
  assert.ok(!globToRegExp("a.b").test("axb"));
  assert.throws(() => globToRegExp("a/{b"), /unterminated brace/u);
});

test("check ids are unique and every check can plan a run", () => {
  assert.equal(byId.size, registry.checks.length);
  for (const check of registry.checks) {
    assert.equal(typeof check.plan, "function", check.id);
    assert.ok(check.display.length > 0, check.id);
    assert.ok(
      check.always || check.triggers.length > 0,
      `${check.id} has no triggers`,
    );
  }
});

test("every script a check runs exists", () => {
  const referenced = new Set();
  for (const check of registry.checks) {
    for (const path of check.requiresFiles ?? []) {
      referenced.add(path);
    }
    const steps = check.plan({ matched: [], full: true, advise: () => {} });
    for (const step of steps ?? []) {
      for (const arg of step.argv) {
        if (/\.(mjs|sh)$/u.test(arg) && !arg.includes("*")) {
          referenced.add(
            resolve(root, step.cwd ?? ".", arg).slice(root.length + 1),
          );
        }
      }
    }
  }
  const missing = [...referenced].filter(
    (path) => !existsSync(resolve(root, path)),
  );
  assert.deepEqual(missing, []);
});

test("derived goldens read Aiken sources or the pinned builtin evaluator", () => {
  const goldens = registry.checks.filter((check) =>
    check.id.startsWith("golden:"),
  );
  assert.ok(goldens.length >= 10, `only ${String(goldens.length)} channels`);
  for (const check of goldens) {
    // This oracle evaluates textual UPLC directly; it has no Aiken module.
    if (check.id === "golden:cek-builtin-cardano-v1") {
      for (const path of [
        "onchain/aiken/scripts/pinned-compiler.mjs",
        "demo/midgard-validation/tests/fixtures/cek-builtin-cardano-v1.cases.mjs",
        "demo/midgard-validation/tests/fixtures/cek-builtin-cardano-v1.generated.json",
        "demo/midgard-validation/src/**",
      ]) {
        assert.ok(check.triggers.includes(path), `${check.id}: ${path}`);
      }
      continue;
    }
    assert.ok(
      check.triggers.some((path) => path.endsWith(".ak")),
      `${check.id} names no .ak file, so an Aiken edit cannot select it`,
    );
  }
  const ledgers = registry.checks.filter((check) =>
    check.id.startsWith("exec-ledger:"),
  );
  assert.ok(ledgers.length >= 7, `only ${String(ledgers.length)} ledgers`);
  for (const check of ledgers) {
    const sources = check.triggers.filter((path) => path.endsWith(".ak"));
    assert.ok(sources.length > 0, `${check.id} measures no module`);
    for (const path of sources) {
      assert.ok(existsSync(resolve(root, path)), `${check.id}: ${path}`);
    }
  }
});

test("consensus-profile.ts selects the profile-doc check", () => {
  const ids = selectedIds(["demo/midgard-core/src/consensus-profile.ts"]);
  assert.ok(ids.includes("docs:consensus-profile-v1"));
  assert.ok(ids.includes("demo-typecheck"));
  assert.ok(!ids.includes("aiken-fmt"));
});

test("an Aiken library edit selects format, focused tests, blueprint, ledgers and goldens", () => {
  const module = "onchain/aiken/lib/midgard/fraud-proofs/native-tx/codec.ak";
  const ids = selectedIds([module]);
  for (const id of ["aiken-fmt", "aiken-focused", "aiken-blueprint"]) {
    assert.ok(ids.includes(id), id);
  }
  assert.ok(ids.includes("exec-ledger:carriage"));
  // The suites that read the compiled blueprint are reached; nothing builds.
  assert.deepEqual(
    ids.filter((id) => id.startsWith("demo-")),
    ["demo-test", "demo-test-db"],
  );
  const steps = byId.get("demo-test-db").plan({ matched: [module] });
  assert.ok(
    steps.some(
      ({ argv }) =>
        argv.join(" ") ===
        `node scripts/contrib.mjs test --package midgard-node --related ${module}`,
    ),
    steps.map(({ argv }) => argv.join(" ")).join("\n"),
  );

  const golden =
    "onchain/aiken/lib/midgard/cek-core-step-goldens/identity.test.ak";
  assert.ok(selectedIds([golden]).includes("golden:cek-core-step-v1"));
});

test("a package change runs the tests it reaches in each package it can reach, as contrib test runs them", () => {
  const changed = "demo/midgard-core/src/canonical-json.ts";
  const ids = selectedIds([changed]);
  assert.ok(ids.includes("demo-test") && ids.includes("demo-test-db"));
  const steps = ["demo-test", "demo-test-db"].flatMap((id) =>
    byId.get(id).plan({ matched: [changed], full: false }),
  );
  // contrib runs Vitest suites; the plain node --test package runs its
  // script.
  const plain = steps.filter(({ argv }) => argv[0] === "pnpm");
  assert.deepEqual(
    plain.map(({ argv }) => argv.join(" ")),
    plain.length
      ? [
          "pnpm --dir demo --filter @al-ft/midgard-test-support run --if-present test",
        ]
      : [],
  );
  const packages = new Set(
    steps
      .filter(({ argv }) => argv[0] !== "pnpm")
      .map(({ argv }) => {
        assert.deepEqual(argv.slice(0, 4), [
          "node",
          "scripts/contrib.mjs",
          "test",
          "--package",
        ]);
        assert.deepEqual(argv.slice(5), ["--related", changed]);
        return argv[4];
      }),
  );
  // Its own package and every package that depends on it, at least.
  for (const name of [
    "@al-ft/midgard-core",
    "@al-ft/midgard-sdk",
    "midgard-node",
  ])
    assert.ok(packages.has(name), name);
  // A path neither in a package nor one Node CI runs the suites for does not
  // select them.
  assert.ok(!selectedIds(["docs/agents/domain.md"]).includes("demo-test"));
});

test("a file with a recorded module-size cap selects the cap's check", () => {
  const caps = JSON.parse(
    readFileSync(resolve(root, "demo/module-size-exceptions.json"), "utf8"),
  );
  const outside = caps.find(({ file }) => file.startsWith("../"));
  assert.ok(outside, "a cap outside demo/");
  for (const path of [
    "demo/module-size-exceptions.json",
    `demo/${caps[0].file}`,
    // `../<path>` names a repository file outside demo/.
    outside.file.slice("../".length),
  ])
    assert.ok(selectedIds([path]).includes("demo-script-tests"), path);
});

test("workflow, agent-doc and tooling edits select their owners' checks", () => {
  const workflow = selectedIds([".github/workflows/aiken-ci.yml"]);
  assert.ok(workflow.includes("workflow-lint"));
  assert.ok(workflow.includes("workflow-triggers"));
  const docs = selectedIds(["docs/agents/domain.md"]);
  assert.ok(docs.includes("agent-doc-links"));
  assert.ok(docs.includes("agent-enforcement-tags"));
  assert.ok(selectedIds([".githooks/pre-push"]).includes("repo-tooling-tests"));
  const helper = selectedIds(["onchain/aiken/scripts/run-focused-check.mjs"]);
  assert.ok(helper.includes("aiken-script-tests"));
  assert.ok(!helper.includes("repo-tooling-tests"));
});

test("a FULL_RUN path selects every check; the tracked blueprint selects nothing", () => {
  for (const path of [
    "onchain/aiken/aiken.toml",
    "demo/pnpm-lock.yaml",
    "scripts/preflight/derive.mjs",
  ]) {
    assert.ok(matchesAny(path, FULL_RUN), path);
    const selection = selectChecks(registry, [path]);
    assert.equal(selection.full, true);
    assert.equal(selection.selected.length, registry.checks.length);
  }
  const blueprint = selectChecks(registry, IGNORED_PATHS);
  assert.equal(blueprint.full, false);
  assert.deepEqual(blueprint.uncovered, []);
  assert.deepEqual(
    blueprint.selected.map(({ check }) => check.id),
    registry.checks.filter((check) => check.always).map((check) => check.id),
  );
});

test("verification-only edits exercise their tooling without repeating unchanged protocol suites", () => {
  const selection = selectChecks(registry, ["scripts/preflight/run.mjs"]);
  assert.equal(selection.full, false);
  const ids = selection.selected.map(({ check }) => check.id);
  assert.ok(ids.includes("repo-tooling-tests"));
  assert.ok(ids.includes("required-checks-doc"));
  assert.ok(!ids.includes("demo-test"));
  assert.ok(!ids.includes("tx-preparation:emulator"));
  for (const shared of [
    "scripts/preflight/derive.mjs",
    "scripts/preflight/probes.mjs",
    "scripts/contrib/process.mjs",
  ])
    assert.equal(selectChecks(registry, [shared]).full, true, shared);
  assert.equal(
    selectChecks(registry, ["scripts/preflight/run.mjs"], { full: true })
      .selected.length,
    registry.checks.length,
  );
});

test("verification-only exceptions retain unconditional Repo Tools CI coverage", () => {
  const yaml = loadYaml(root);
  assert.ok(yaml, "install demo root dependencies to verify CI coverage");
  const workflow = yaml.parse(
    readFileSync(resolve(root, ".github/workflows/repo-tools-ci.yml"), "utf8"),
  );
  assert.ok(Object.hasOwn(workflow.on, "pull_request"));
  assert.equal(
    workflow.on.pull_request,
    null,
    "Repo Tools must cover every PR without path/branch filters",
  );
  assert.equal(workflow.jobs["repo-tools"].if, undefined);
  // The step runs repo-tooling-tests by id, so CI runs the registry's own
  // command for it: every scripts/ test.
  assert.ok(
    workflow.jobs["repo-tools"].steps.some((step) =>
      workflowRunsCheck(step.run ?? "", "repo-tooling-tests"),
    ),
  );
  assert.deepEqual(
    byId
      .get("repo-tooling-tests")
      .plan({ full: true })
      .map((s) => s.argv),
    [["node", "--test", "scripts/**/*.test.mjs"]],
  );
  for (const path of VERIFICATION_ONLY)
    assert.ok(
      path === "scripts/preflight.mjs" || path.startsWith("scripts/preflight/"),
      path,
    );
});

test("--full and the kill switch reason force a full run; pre-push keeps only its slice", () => {
  const forced = selectChecks(registry, [], {
    full: true,
    fullReasons: ["MIDGARD_PREFLIGHT_FULL=1"],
  });
  assert.equal(forced.selected.length, registry.checks.length);
  assert.deepEqual(forced.fullReasons, ["MIDGARD_PREFLIGHT_FULL=1"]);
  const prePush = selectChecks(registry, ["demo/pnpm-lock.yaml"], {
    prePush: true,
  });
  assert.ok(prePush.selected.length > 0);
  assert.ok(prePush.selected.every(({ check }) => check.prePush));
});

test("a changed file no trigger names is reported as uncovered", () => {
  const selection = selectChecks(registry, ["some-new-top-level-file.txt"]);
  assert.deepEqual(selection.uncovered, ["some-new-top-level-file.txt"]);
});

// These used to depend on the contributor finding verification.md's manual table.
test("docs, specification and devnet edits select their required checks", () => {
  for (const [path, expected] of [
    [
      "docs-site/app/layout.tsx",
      ["docs-site-links", "docs-site-build", "docs-site-typecheck"],
    ],
    [
      "docs-site/pnpm-lock.yaml",
      ["docs-site-links", "docs-site-build", "docs-site-typecheck"],
    ],
    [
      "docs/spec/midgard-tx.md",
      ["docs-site-links", "docs-site-build", "docs-site-typecheck"],
    ],
    [
      "demo/lucid-midgard/src/index.ts",
      ["docs-site-build", "docs-site-typecheck"],
    ],
    [
      "demo/midgard-core/src/hex.ts",
      ["docs-site-build", "docs-site-typecheck"],
    ],
    [
      ".github/workflows/docs-site-ci.yml",
      ["docs-site-links", "docs-site-build", "docs-site-typecheck"],
    ],
    [".gitignore", ["docs-site-links"]],
    ["technical-spec/midgard.tex", ["spec-build"]],
    ["Makefile", ["spec-build"]],
    [".github/workflows/latex-ci.yml", ["spec-build"]],
    [
      "demo/midgard-node-tools/devnet/phase4-process/scripts/generate.sh",
      ["devnet-assets"],
    ],
  ]) {
    const ids = selectedIds([path]);
    for (const id of expected)
      assert.ok(ids.includes(id), `${path} must select ${id}`);
  }
  const unrelated = selectedIds(["demo/midgard-watcher/src/index.ts"]);
  for (const id of [
    "docs-site-build",
    "docs-site-typecheck",
    "spec-build",
    "devnet-assets",
  ]) {
    assert.ok(!unrelated.includes(id), `unrelated watcher edit selected ${id}`);
  }
});

test("new validation runs the documented commands with its own prerequisites", () => {
  const expected = [
    [
      "docs-site-links",
      [],
      [["node", "docs-site/scripts/check-docs-links.mjs"]],
    ],
    [
      "docs-site-build",
      ["node-modules", "docs-site-node-modules"],
      [["corepack", "pnpm", "run", "build"]],
    ],
    [
      "docs-site-typecheck",
      ["node-modules", "docs-site-node-modules"],
      [["corepack", "pnpm", "run", "types:check"]],
    ],
    ["spec-build", ["nix"], [["make", "spec"]]],
    [
      "devnet-assets",
      ["node-modules", "blueprint"],
      [
        [
          "node",
          "--test",
          "demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs",
          "demo/midgard-node-tools/devnet/phase4-process/tests/l1-follower-inputs.test.mjs",
          "demo/midgard-node-tools/devnet/phase4-process/tests/protocol-bootstrap-follower.test.mjs",
        ],
      ],
    ],
  ];
  for (const [id, capabilities, commands] of expected) {
    const check = byId.get(id);
    assert.ok(check, `missing ${id}`);
    assert.deepEqual(check.capabilities, capabilities, id);
    assert.deepEqual(
      check.plan({ full: true }).map((step) => step.argv),
      commands,
      id,
    );
    assert.equal(check.warnOnly, false, id);
    assert.equal(check.prePush, id === "docs-site-links", id);
  }
  assert.ok(
    registry.checks.findIndex((check) => check.id === "aiken-blueprint") <
      registry.checks.findIndex((check) => check.id === "devnet-assets"),
  );
  assert.ok(
    registry.checks.findIndex((check) => check.id === "docs-site-build") <
      registry.checks.findIndex((check) => check.id === "docs-site-typecheck"),
  );
});

test("building a blueprint cannot change generated preflight references", () => {
  const root = mkdtempSync(join(tmpdir(), "preflight-blueprint-references-"));
  try {
    mkdirSync(join(root, "onchain/aiken"), { recursive: true });
    writeFileSync(join(root, "tracked.json"), "{}");
    writeFileSync(
      join(root, "generator.mjs"),
      [
        '"tracked.json"',
        ...IGNORED_PATHS.map((path) => JSON.stringify(path)),
      ].join(";"),
    );
    assert.deepEqual(referencedFiles(root, "generator.mjs"), ["tracked.json"]);
    for (const path of IGNORED_PATHS) writeFileSync(join(root, path), "{}");
    assert.deepEqual(referencedFiles(root, "generator.mjs"), ["tracked.json"]);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test("workspace lint and its helper tests cover source, rules, and baseline moves", () => {
  const baseline = "demo/scripts/lib/eslint-plugin-midgard/baseline.json";
  for (const path of [
    baseline,
    "demo/eslint.config.mjs",
    "demo/midgard-node-tools/src/commands/stress-wallets/terminal-drain.ts",
  ]) {
    assert.ok(
      selectedIds([path]).includes("demo-lint"),
      `${path} must select workspace lint`,
    );
  }
  assert.ok(selectedIds([baseline]).includes("demo-script-tests"));
  assert.deepEqual(selectChecks(registry, [baseline]).uncovered, []);
  assert.ok(!selectedIds(["docs/agents/domain.md"]).includes("demo-lint"));
  for (const [id, argv] of [
    ["demo-lint", ["pnpm", "--dir", "demo", "run", "lint"]],
    [
      "demo-script-tests",
      [
        "node",
        "--test",
        "demo/scripts/lib/*.test.mjs",
        "demo/scripts/deployment-profiles.test.mjs",
        "demo/scripts/interactive-emulator.test.mjs",
      ],
    ],
  ]) {
    assert.deepEqual(
      byId
        .get(id)
        .plan({ full: true })
        .map((s) => s.argv),
      [argv],
    );
    assert.deepEqual(byId.get(id).capabilities, ["node-modules"]);
    assert.equal(byId.get(id).warnOnly, false);
  }
});

test("golden output manifests select every split Aiken artifact", () => {
  const channel = byId.get("golden:cek-core-step-v1");
  const outputs = channel.triggers.filter((path) =>
    path.startsWith("onchain/aiken/lib/midgard/cek-core-step-goldens/"),
  );
  assert.equal(outputs.length, 26);
  for (const path of outputs) {
    assert.ok(existsSync(resolve(root, path)), path);
    assert.ok(selectedIds([path]).includes(channel.id), path);
  }
});
