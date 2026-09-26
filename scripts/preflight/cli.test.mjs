import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { docsCommand, JSON_SCHEMA, main } from "../preflight.mjs";
import { REQUIRED_CHECKS_DOC, renderRequiredChecks } from "./docs.mjs";
import { buildRegistry } from "./registry.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

const capture = () => {
  const out = { stdout: "", stderr: "" };
  return {
    out,
    stdout: (text) => {
      out.stdout += text;
    },
    stderr: (text) => {
      out.stderr += text;
    },
  };
};

const allAvailable = {
  get: async (name) => ({ name, status: "available", detail: name }),
  invalidate: () => {},
};

test("the committed required-checks.md is the one the registry renders", () => {
  const committed = readFileSync(resolve(root, REQUIRED_CHECKS_DOC), "utf8");
  assert.equal(
    committed,
    renderRequiredChecks(buildRegistry(root)),
    "run: node scripts/preflight.mjs --write-docs",
  );
});

test("--check-docs fails on a stale or missing document and names the fix", () => {
  const scratch = mkdtempSync(join(tmpdir(), "preflight-docs-test-"));
  try {
    const path = join(scratch, "required-checks.md");
    writeFileSync(path, "# stale\n");
    const io = capture();
    assert.equal(docsCommand(root, "check", { ...io, path }), 1);
    assert.match(io.out.stderr, /--write-docs/u);
    const missing = capture();
    assert.equal(
      docsCommand(root, "check", { ...missing, path: join(scratch, "none") }),
      1,
    );
    assert.match(missing.out.stderr, /is missing/u);
    const write = capture();
    assert.equal(docsCommand(root, "write", { ...write, path }), 0);
    assert.equal(docsCommand(root, "check", { ...capture(), path }), 0);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});

test("usage errors exit 2", async () => {
  for (const argv of [
    ["--bogus"],
    ["--base"],
    ["--base", "--json"],
    ["--write-docs", "--check-docs"],
    ["--check-docs", "--json"],
    ["--strict", "--base", "refs/heads/no-such-branch-for-preflight"],
  ]) {
    const io = capture();
    assert.equal(await main(argv, { root, ...io }), 2, argv.join(" "));
    assert.equal(io.out.stdout, "", argv.join(" "));
  }
});

test("--json prints one document with the stable schema and nothing else", async () => {
  const io = capture();
  const exitCode = await main(["--strict", "--base", "HEAD", "--json"], {
    root,
    env: {},
    ...io,
    probes: allAvailable,
    runStep: async () => ({ status: 0, output: "" }),
  });
  const report = JSON.parse(io.out.stdout);
  assert.equal(report.schema, JSON_SCHEMA);
  assert.equal(report.strict, true);
  assert.deepEqual(report.changedFiles, []);
  assert.equal(report.exitCode, exitCode);
  assert.equal(exitCode, 0);
  const ids = report.checks.map((check) => check.id);
  assert.deepEqual(ids, ["merge-conflicts", "required-checks-doc"]);
  for (const check of report.checks) {
    assert.deepEqual(Object.keys(check).sort(), [
      "command",
      "durationMs",
      "id",
      "reason",
      "status",
    ]);
    assert.ok(["passed", "failed", "skipped", "warned"].includes(check.status));
  }
  assert.match(io.out.stderr, /Selection is not the final gate/u);
});

test("--list runs nothing and --json reports the plan separately", async () => {
  const io = capture();
  let ran = 0;
  const exitCode = await main(
    ["--strict", "--base", "HEAD", "--list", "--json", "--full"],
    {
      root,
      env: {},
      ...io,
      probes: allAvailable,
      runStep: async () => {
        ran += 1;
        return { status: 0, output: "" };
      },
    },
  );
  assert.equal(exitCode, 0);
  assert.equal(ran, 0);
  const report = JSON.parse(io.out.stdout);
  assert.deepEqual(report.checks, []);
  assert.equal(report.full, true);
  assert.deepEqual(report.fullReasons, ["--full"]);
  assert.equal(report.planned.length, buildRegistry(root).checks.length);
});

test("the kill switch forces a full run and a failed check exits 1 with its fix", async () => {
  const io = capture();
  const exitCode = await main(["--strict", "--base", "HEAD"], {
    root,
    env: { MIDGARD_PREFLIGHT_FULL: "1" },
    ...io,
    probes: allAvailable,
    runStep: async (_root, step) => ({
      status: step.argv.includes("--check-docs") ? 1 : 0,
      output: "",
    }),
  });
  assert.equal(exitCode, 1);
  assert.match(io.out.stdout, /full run: MIDGARD_PREFLIGHT_FULL=1/u);
  assert.match(io.out.stdout, /FAILED +required-checks-doc/u);
  assert.match(
    io.out.stdout,
    /fix: node scripts\/preflight\.mjs --write-docs/u,
  );
  assert.match(io.out.stdout, /preflight FAILED/u);
});
