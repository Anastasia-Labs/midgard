import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { judgeNeeds } from "./check-needs-results.mjs";

const script = join(
  dirname(fileURLToPath(import.meta.url)),
  "check-needs-results.mjs",
);

const needs = (results) =>
  JSON.stringify(
    Object.fromEntries(
      Object.entries(results).map(([job, result]) => [
        job,
        { result, outputs: {} },
      ]),
    ),
  );

const run = (env, ...args) =>
  spawnSync(process.execPath, [script, ...args], {
    encoding: "utf8",
    env: { PATH: process.env.PATH, ...env },
  });

test("all-success needs pass", () => {
  assert.equal(
    judgeNeeds(needs({ a: "success", b: "success" }), ["success"]).ok,
    true,
  );
  assert.equal(
    run({ NEEDS: needs({ a: "success" }) }, "--allow", "success").status,
    0,
  );
});

test("failure, cancelled and skipped dependencies fail a success-only gate", () => {
  for (const bad of ["failure", "cancelled", "skipped"]) {
    const verdict = judgeNeeds(needs({ a: "success", b: bad }), ["success"]);
    assert.equal(verdict.ok, false, bad);
    assert.match(
      verdict.lines.join("\n"),
      new RegExp(`b finished '${bad}'`, "u"),
    );
  }
  assert.equal(
    run({ NEEDS: needs({ a: "failure" }) }, "--allow", "success").status,
    1,
  );
});

test("skipped passes only when explicitly allowed", () => {
  assert.equal(
    judgeNeeds(needs({ a: "skipped" }), ["success", "skipped"]).ok,
    true,
  );
  assert.equal(
    run({ NEEDS: needs({ a: "skipped" }) }, "--allow", "success,skipped")
      .status,
    0,
  );
});

test("the gate fails closed on a missing allowlist, an unacceptable allow value, or unusable NEEDS", () => {
  assert.equal(judgeNeeds(needs({ a: "success" }), []).ok, false);
  assert.equal(judgeNeeds(needs({ a: "failure" }), ["failure"]).ok, false);
  assert.equal(judgeNeeds(undefined, ["success"]).ok, false);
  assert.equal(judgeNeeds("not json", ["success"]).ok, false);
  assert.equal(judgeNeeds("[]", ["success"]).ok, false);
  assert.equal(judgeNeeds("null", ["success"]).ok, false);
  assert.equal(judgeNeeds("{}", ["success"]).ok, false);
  assert.equal(judgeNeeds(JSON.stringify({ a: {} }), ["success"]).ok, false);
  assert.equal(run({ NEEDS: needs({ a: "success" }) }).status, 1);
  assert.equal(run({}, "--allow", "success").status, 1);
});
