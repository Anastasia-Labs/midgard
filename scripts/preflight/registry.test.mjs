import assert from "node:assert/strict";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { globToRegExp, matchesAny } from "./derive.mjs";
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

  const golden = "onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak";
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
