import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { globToRegExp, matchesAny, referencedFiles } from "./derive.mjs";
import {
  buildRegistry,
  FULL_RUN,
  IGNORED_PATHS,
  selectChecks,
} from "./registry.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const registry = buildRegistry(root);
const byId = new Map(registry.checks.map((check) => [check.id, check]));
const selectedIds = (changed, options) =>
  selectChecks(registry, changed, options).selected.map(
    ({ check }) => check.id,
  );

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

test("derived golden channels and ledgers read Aiken sources", () => {
  const goldens = registry.checks.filter((check) =>
    check.id.startsWith("golden:"),
  );
  assert.ok(goldens.length >= 10, `only ${String(goldens.length)} channels`);
  for (const check of goldens) {
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
  assert.ok(!ids.some((id) => id.startsWith("demo-")));

  const golden =
    "onchain/aiken/lib/midgard/cek-core-step-goldens/identity.test.ak";
  assert.ok(selectedIds([golden]).includes("golden:cek-core-step-v1"));
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
    "scripts/preflight/registry.mjs",
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
    ["demo-script-tests", ["node", "--test", "demo/scripts/lib/*.test.mjs"]],
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
