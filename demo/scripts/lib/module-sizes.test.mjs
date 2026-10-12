import "../check-module-size-exceptions.mjs";

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { ESLint, Linter } from "eslint";

test("the ESLint default admits 500 physical lines and refuses line 501", async () => {
  const cwd = fileURLToPath(new URL("../../", import.meta.url));
  const config = await new ESLint({ cwd }).calculateConfigForFile(
    "scripts/new-module.js",
  );
  const rule = config.rules["max-lines"];
  assert.equal(rule[0], 2);
  assert.equal(rule[1].max, 500);
  const linter = new Linter();
  const lint = (lines) =>
    linter.verify(`${Array(lines).fill("// counted line").join("\n")}\n`, {
      rules: { "max-lines": rule },
    });
  assert.deepEqual(lint(500), []);
  assert.equal(lint(501)[0].ruleId, "max-lines");
});

test("a module-size cap names a repository file by one spelling", async () => {
  const { canonicalExceptionPath, moduleSizeExceptionFailures } = await import(
    "../check-module-size-exceptions.mjs"
  );
  const demoRoot = "/repo/demo";
  assert.equal(canonicalExceptionPath(demoRoot, "a/b.ts"), "a/b.ts");
  assert.equal(
    canonicalExceptionPath(demoRoot, "../.agents/x.mjs"),
    "../.agents/x.mjs",
  );
  // Another spelling of the same file, or a file outside the repository, is
  // not the canonical one, so the validator refuses it.
  assert.equal(canonicalExceptionPath(demoRoot, "../demo/a/b.ts"), "a/b.ts");
  assert.equal(canonicalExceptionPath(demoRoot, "a/../a/b.ts"), "a/b.ts");
  assert.equal(canonicalExceptionPath(demoRoot, "../../etc/passwd"), null);
  assert.equal(canonicalExceptionPath(demoRoot, "/repo/demo/a/b.ts"), null);
  const body = `${Array(501).fill("x").join("\n")}\n`;
  const check = (exceptions) =>
    moduleSizeExceptionFailures(exceptions, { demoRoot, read: () => body });
  const reason = "kept";
  assert.deepEqual(
    check([
      { file: "a/b.ts", max: 501, reason },
      { file: "../.agents/x.mjs", max: 501, reason },
    ]),
    [],
  );
  for (const file of ["../demo/a/b.ts", "a/../a/b.ts", "../../etc/passwd"])
    assert.deepEqual(check([{ file, max: 501, reason }]), [
      `Invalid or duplicate exception: ${file}`,
    ]);
  assert.match(check([{ file: "a/b.ts", max: 502, reason }])[0], /actual 501/u);
});

test("ESLint caps each demo TS/JS entry and lints no other capped file", async () => {
  const cwd = fileURLToPath(new URL("../../", import.meta.url));
  const caps = JSON.parse(
    readFileSync(new URL("../../module-size-exceptions.json", import.meta.url)),
  );
  const eslint = new ESLint({ cwd });
  const others = caps.filter(({ file }) => !/\.[cm]?[jt]sx?$/u.test(file));
  assert.ok(others.length > 0, "a native or SQL cap");
  for (const { file } of others)
    assert.equal(await eslint.calculateConfigForFile(file), undefined, file);
  const demoScript = caps.find(
    ({ file }) => !file.startsWith("../") && file.endsWith(".ts"),
  );
  const config = await eslint.calculateConfigForFile(demoScript.file);
  assert.deepEqual(config.rules["max-lines"], [2, { max: demoScript.max }]);
});
