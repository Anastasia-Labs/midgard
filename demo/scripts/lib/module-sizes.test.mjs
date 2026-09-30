import "../check-module-size-exceptions.mjs";

import assert from "node:assert/strict";
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
