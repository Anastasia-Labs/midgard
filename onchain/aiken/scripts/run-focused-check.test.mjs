import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const scriptsDirectory = dirname(fileURLToPath(import.meta.url));
const script = join(scriptsDirectory, "run-focused-check.mjs");
const projectDirectory = resolve(scriptsDirectory, "..");
const sourceRoots = ["lib", "validators"];

const collectModules = (directory, prefix, found) => {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      collectModules(
        join(directory, entry.name),
        `${prefix}${entry.name}/`,
        found,
      );
    } else if (entry.isFile() && entry.name.endsWith(".ak")) {
      found.push(`${prefix}${entry.name.slice(0, -".ak".length)}`);
    }
  }
  return found;
};

const sourceModules = sourceRoots
  .flatMap((root) => collectModules(join(projectDirectory, root), "", []))
  .sort();

// Aiken folds hyphens in a module's source path to underscores when it names the
// module, so `midgard/bounded-blob-v1.test.ak` reports as
// `midgard/bounded_blob_v1.test`.
const toAikenModule = (candidate) => candidate.replaceAll("-", "_");

const withStub = (callback) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-focused-check-"));
  const stub = join(directory, "stub-aiken.mjs");
  const log = join(directory, "invocations.log");
  writeFileSync(
    stub,
    `#!/usr/bin/env node
import { appendFileSync } from "node:fs";
const args = process.argv.slice(2);
const name = process.env.MIDGARD_STUB_MODULE ?? "";
const selectors = args.filter((_, index) => args[index - 1] === "-m");
appendFileSync(${JSON.stringify(log)}, selectors.join(" ") + "\\n");
const tests = selectors.map((selector) => ({
  title: selector.replace(/^.*\\{(.*)\\}$/u, "$1"),
  status: "pass",
}));
const summary = {
  total: tests.length,
  passed: tests.length,
  failed: 0,
  kind: { unit: tests.length, property: 0 },
};
process.stdout.write(
  JSON.stringify({ summary, modules: [{ name, summary, tests }] }),
);
`,
  );
  chmodSync(stub, 0o755);
  const run = (moduleName, testNames, stubModule) =>
    spawnSync(process.execPath, [script, moduleName, ...testNames], {
      encoding: "utf8",
      env: {
        ...process.env,
        MIDGARD_AIKEN_BIN: stub,
        MIDGARD_STUB_MODULE: stubModule ?? toAikenModule(moduleName),
      },
    });
  try {
    return callback(run, () =>
      readFileSync(log, "utf8").split("\n").filter(Boolean),
    );
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("every module in the tree is targetable", () => {
  withStub((run, invocations) => {
    assert.ok(sourceModules.length > 0);
    const rejected = [];
    for (const sourceModule of sourceModules) {
      for (const spelling of new Set([
        sourceModule,
        toAikenModule(sourceModule),
      ])) {
        const result = run(
          spelling,
          ["probe_test"],
          toAikenModule(sourceModule),
        );
        if (result.status !== 0) {
          rejected.push(`${spelling}: ${result.stderr.trim()}`);
        }
      }
    }
    assert.deepEqual(rejected, []);
    // Aiken splits an `-m` selector on its first ".", so the selector the script
    // builds is the module name truncated there; it must still name the module.
    const canonicalNames = new Set(sourceModules.map(toAikenModule));
    for (const selector of invocations()) {
      const modulePart = selector.slice(0, selector.lastIndexOf(".{"));
      assert.ok(
        [...canonicalNames].some((name) => name.startsWith(modulePart)),
        `selector ${selector} does not prefix any module`,
      );
    }
  });
});

test("a hyphenated module runs and its result is reported", () => {
  const hyphenated = sourceModules.find((candidate) => candidate.includes("-"));
  assert.ok(hyphenated, "expected at least one hyphenated module");
  withStub((run) => {
    const result = run(hyphenated, ["probe_test"]);
    assert.equal(result.status, 0, result.stderr);
    assert.equal(
      JSON.parse(result.stdout).modules[0].name,
      toAikenModule(hyphenated),
    );
  });
});

test("the pre-#524 module pattern rejected hyphenated modules", () => {
  const previousPattern = /^[a-z0-9_/]+$/u;
  assert.ok(
    sourceModules.some((candidate) => !previousPattern.test(candidate)),
    "expected modules the previous pattern could not express",
  );
});

test("a missing module fails closed with a diagnostic", () => {
  withStub((run) => {
    const result = run("midgard/no-such-module-v1.test", ["probe_test"]);
    assert.equal(result.status, 2);
    assert.match(
      result.stderr,
      /no module 'midgard\/no-such-module-v1\.test'/u,
    );
  });
});

test("a module outside the allowlist is rejected", () => {
  withStub((run) => {
    for (const rejected of ["Bad/Module", "../escape", "mod name", "mod;rm"]) {
      const result = run(rejected, ["probe_test"]);
      assert.equal(result.status, 2, `expected ${rejected} to be rejected`);
      assert.match(result.stderr, /^usage:/u);
    }
  });
});

test("results from another module fail closed", () => {
  const [first] = sourceModules;
  withStub((run) => {
    const result = run(first, ["probe_test"], "some/other_module");
    assert.equal(result.status, 1);
    assert.match(result.stderr, /expected results from module/u);
  });
});
