import assert from "node:assert/strict";
import { mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { aikenImportClosure, indexAikenModules } from "./derive.mjs";
import { createProbeSet, probeTracedBlueprint } from "./probes.mjs";
import { buildRegistry } from "./registry.mjs";
import {
  TRACED_REFUSAL_RUNNER,
  tracedRefusalCheck,
  tracedRefusalPins,
} from "./registry.traced-refusals.mjs";
import { EXIT, planPreflight, runPreflight } from "./run.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const index = indexAikenModules(root);
const check = tracedRefusalCheck(root, index);
const TESTS = "demo/midgard-fault-proofs/tests";

// Read independently of the module: every file declaring a pin, by module.
const pinFiles = (() => {
  const byModule = new Map();
  for (const entry of readdirSync(resolve(root, TESTS), { recursive: true })
    .map(String)
    .filter((file) => /\.(?:ts|mts|js|mjs)$/u.test(file))) {
    const file = `${TESTS}/${entry}`;
    const source = readFileSync(resolve(root, file), "utf8");
    for (const [, module] of source.matchAll(/\brefusedBy:\s*"([^"]+)"/gu)) {
      byModule.set(module, new Set([...(byModule.get(module) ?? []), file]));
    }
  }
  return byModule;
})();
const allPinFiles = [...new Set([...pinFiles.values()].flatMap((f) => [...f]))];
const validatorFile = (module) => index.byAlias.get(module).file;

test("every pin names a validator module the index knows", () => {
  assert.ok(pinFiles.size > 0, "found no refusedBy pin");
  for (const module of pinFiles.keys())
    assert.ok(
      index.byAlias.get(module)?.file.includes("/validators/"),
      `${module} is not a validator module`,
    );
});

test("a changed validator selects exactly the pin files whose validator reaches it", () => {
  const file = validatorFile("correction_lock");
  const { files, unplaced } = tracedRefusalPins(root, [file], index);
  const expected = [...pinFiles]
    .filter(([module]) => aikenImportClosure(index, [module]).has(file))
    .flatMap(([, declaring]) => [...declaring]);
  assert.deepEqual(files, [...new Set(expected)].sort());
  assert.ok(files.length > 0 && files.length < allPinFiles.length);
  for (const pinned of pinFiles.get("correction_lock"))
    assert.ok(files.includes(pinned), pinned);
  assert.deepEqual(unplaced, []);
  assert.deepEqual(
    check.plan({ matched: [file], full: false, ciOwnsRuntime: true }),
    [{ argv: ["node", TRACED_REFUSAL_RUNNER, ...files], cwd: "." }],
  );
});

test("a changed library module selects the pin files of every validator importing it", () => {
  const lib = [...aikenImportClosure(index, ["state_queue_yields"])].find(
    (file) => file.includes("/lib/"),
  );
  assert.ok(lib, "state_queue_yields imports no library module");
  const { files } = tracedRefusalPins(root, [lib], index);
  for (const pinned of pinFiles.get("state_queue_yields"))
    assert.ok(files.includes(pinned), pinned);
  for (const file of files)
    assert.ok(
      [...pinFiles].some(
        ([module, declaring]) =>
          declaring.has(file) && aikenImportClosure(index, [module]).has(lib),
      ),
      `${file} pins no validator reaching ${lib}`,
    );
});

test("an Aiken test module selects nothing; a blueprint-wide input selects every pin file", () => {
  const testModule = index.modules.find((module) =>
    module.file.endsWith(".test.ak"),
  ).file;
  assert.deepEqual(tracedRefusalPins(root, [testModule], index), {
    files: [],
    unplaced: [],
  });
  assert.equal(
    check.plan({ matched: [testModule], full: false, ciOwnsRuntime: true }),
    null,
  );
  for (const path of [
    "onchain/aiken/aiken.toml",
    "onchain/aiken/env/preprod-testing.ak",
  ])
    assert.deepEqual(
      tracedRefusalPins(root, [path], index).files,
      [...allPinFiles].sort(),
      path,
    );
});

test("a changed pin file selects itself", () => {
  const [file] = pinFiles.get("state_queue_yields");
  assert.deepEqual(tracedRefusalPins(root, [file], index), {
    files: [file],
    unplaced: [],
  });
});

test("an off-chain input is handed to CI when it owns the runtime, and runs every pin when not", () => {
  const offChain = "demo/midgard-sdk/src/index.ts";
  assert.equal(check.handOff({ matched: [offChain], full: false }), true);
  // A full-scope run hands every pin over too, even with nothing matched.
  assert.equal(check.handOff({ matched: [], full: true }), true);
  assert.equal(
    check.plan({ matched: [], full: true, ciOwnsRuntime: true }),
    null,
  );
  assert.equal(
    check.plan({ matched: [offChain], full: false, ciOwnsRuntime: true }),
    null,
  );
  const validator = validatorFile("correction_lock");
  const [selected] = check.plan({
    matched: [offChain, validator],
    full: false,
    ciOwnsRuntime: true,
  });
  assert.ok(selected.argv.length > 2 && selected.argv.length < 200);
  assert.equal(check.handOff({ matched: [validator], full: false }), false);
  for (const options of [
    { matched: [offChain], full: false },
    { matched: [], full: true },
  ])
    assert.deepEqual(check.plan(options), [
      { argv: ["node", TRACED_REFUSAL_RUNNER], cwd: "." },
    ]);
});

test("preflight runs the reached pins for a validator change and leaves an off-chain change pending on CI", () => {
  const registry = buildRegistry(root);
  assert.ok(registry.runtimeWorkflow, "Node CI's runtime routing is unread");
  const traced = (plan) => ({
    planned: plan.planned.find(({ check: c }) => c.id === "traced-refusals"),
    hosted: plan.hosted.find(({ id }) => id === "traced-refusals"),
  });
  const validator = traced(
    planPreflight(registry, [validatorFile("correction_lock")], {
      runtimeOwner: "ci",
    }),
  );
  assert.equal(validator.hosted, undefined);
  assert.ok(validator.planned.steps[0].argv.length > 2);
  const offChain = traced(
    planPreflight(registry, ["demo/midgard-sdk/src/index.ts"], {
      runtimeOwner: "ci",
    }),
  );
  assert.equal(offChain.planned, undefined);
  assert.deepEqual(offChain.hosted, {
    id: "traced-refusals",
    workflow: registry.runtimeWorkflow.workflow,
    gate: registry.runtimeWorkflow.gate,
  });
  const local = traced(
    planPreflight(registry, ["demo/midgard-sdk/src/index.ts"], {
      runtimeOwner: "local",
    }),
  );
  assert.equal(local.hosted, undefined);
  assert.deepEqual(local.planned.steps[0].argv, [
    "node",
    TRACED_REFUSAL_RUNNER,
  ]);
});

test("a missing traced-blueprint capability skips the selected pins and exits 3", async () => {
  const registry = buildRegistry(root);
  const plan = planPreflight(registry, [validatorFile("correction_lock")], {
    runtimeOwner: "ci",
  });
  const only = {
    ...plan,
    planned: plan.planned.filter(({ check: c }) => c.id === "traced-refusals"),
  };
  assert.equal(only.planned.length, 1);
  const report = await runPreflight({
    root,
    base: "base",
    plan: only,
    probes: createProbeSet({
      root,
      overrides: {
        aiken: () => ({ name: "aiken", status: "available", detail: "aiken" }),
        "traced-blueprint": () => ({
          name: "traced-blueprint",
          status: "missing",
          detail: "blueprint stamp stale",
          fix: "pnpm --dir demo deployment:build preprod-testing",
        }),
      },
    }),
    log() {},
    runStep: async () => assert.fail("a skipped check must not run"),
  });
  assert.equal(report.exitCode, EXIT.skipped);
  assert.equal(report.results[0].status, "skipped");
  assert.match(report.results[0].reason, /traced-blueprint missing/u);
});

test("the traced-blueprint probe never reports an uncheckable blueprint as available", async () => {
  const empty = mkdtempSync(join(tmpdir(), "traced-blueprint-probe-"));
  try {
    const probe = await probeTracedBlueprint({ root: empty });
    assert.equal(probe.name, "traced-blueprint");
    assert.equal(probe.status, "unknown");
  } finally {
    rmSync(empty, { recursive: true, force: true });
  }
  assert.ok(createProbeSet({ root }).names.includes("traced-blueprint"));
});
