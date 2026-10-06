// No package script passes vitest a kebab-case multi-word flag without a value.
//
// vitest tells its argument parser which options are booleans by their
// camelCase names (3.0.7, and still 5.0.3), so `--disable-console-intercept`
// is not recognised as a boolean and takes the next argument as its value. pnpm appends a caller's
// arguments to the end of the script, so a script ending in that flag turns
// `pnpm test <file>` into a run of the whole package (measured 2026-09-28:
// midgard-node listed 270 files instead of 1). The camelCase spelling and the
// `--flag=value` form are parsed correctly.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// `--no-<flag>` negations are parsed correctly, so they are not matched.
const KEBAB_FLAG = /^--(?!no-)[a-z0-9]+(?:-[a-z0-9]+)+$/u;

/** The kebab-case multi-word flags without `=value` in a script's vitest commands. */
const swallowingFlags = (script) =>
  script
    .split(/&&|\|\||;/u)
    .filter((command) => /(?:^|\s)vitest(?:\s|$)/u.test(command))
    .flatMap((command) => command.trim().split(/\s+/u))
    .filter((token) => KEBAB_FLAG.test(token));

test("finds only kebab-case multi-word flags without a value", () => {
  assert.deepEqual(
    swallowingFlags(
      "export NODE_ENV='emulator' && vitest run --disable-console-intercept",
    ),
    ["--disable-console-intercept"],
  );
  assert.deepEqual(
    swallowingFlags("vitest run --pass-with-no-tests tests/a.test.ts"),
    ["--pass-with-no-tests"],
  );
  for (const safe of [
    "vitest run --disableConsoleIntercept",
    "vitest run --disable-console-intercept=true",
    "vitest run --no-file-parallelism --silent --reporter=dot",
    "vitest run --config vitest.bench.config.ts tests/x.bench.ts",
    "node --test-reporter=tap --experimental-vm-modules x.mjs",
    "tsc --no-emit-on-error && eslint --max-warnings=0 .",
  ]) {
    assert.deepEqual(swallowingFlags(safe), [], safe);
  }
});

test("no tracked package script passes vitest a flag that swallows an appended file", () => {
  const listed = spawnSync(
    "git",
    ["ls-files", "--", "package.json", "**/package.json"],
    { cwd: root, encoding: "utf8" },
  );
  assert.equal(listed.status, 0, listed.stderr);
  const manifests = listed.stdout.split("\n").filter(Boolean);
  assert.ok(
    manifests.includes("demo/midgard-node/package.json"),
    "the manifest list is not empty",
  );
  const offences = [];
  for (const manifest of manifests) {
    const scripts =
      JSON.parse(readFileSync(join(root, manifest), "utf8")).scripts ?? {};
    for (const [name, script] of Object.entries(scripts)) {
      for (const flag of swallowingFlags(script)) {
        offences.push(`${manifest} script "${name}": ${flag}`);
      }
    }
  }
  assert.deepEqual(
    offences,
    [],
    "vitest takes the argument after a kebab-case multi-word flag as its value, " +
      "so `pnpm <script> <file>` drops the file and runs every test. Write the flag " +
      "in camelCase (--disableConsoleIntercept) or with a value " +
      "(--disable-console-intercept=true).",
  );
});
