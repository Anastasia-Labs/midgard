// Verifies the canonical-V1 capability reconciliation artifact.
//
// Issue #529 (finding V-2 of #519). The published "17 pass" used to be produced
// by counting how many entries of this artifact's own `p2Tasks` map held the
// literal string "PASS", cross-checked against a `p2Summary` literal in this
// file and, externally, against two prose rows of a GOAL_SPEC markdown table.
// Nothing in that chain could observe a test result, so a task's promotion was
// a self-assertion: editing `p2Tasks` and the matching literal here promoted it.
//
// A P2 task may now be counted as PASS only when a runner reports that its
// executable witnesses passed. Each witness is additionally required to be one
// the *task manifest* declares as that task's focused verification, so the
// artifact under test cannot supply its own audit scope (finding V-6). The
// witnesses are executed by spawning each package's own Vitest CLI and, for the
// C20-6/C20-7 field-order claim that previously rested on the GOAL_SPEC table
// row, one `aiken check -e` invocation; every published count is derived from
// those reports alone.
//
// Issue #538 closed the coverage residual that left behind. Those witnesses are
// one focused verification per task, while the manifest declares every task's
// heavier per-task Aiken selectors as well; nothing replayed them, so the gate's
// scope was a strict subset of the scope its own authority declares. All of them
// now run, in one batched invocation of the patched fork — the compiler CI uses
// to execute the suite — and no task may be published PASS unless every selector
// the manifest binds to it was collected and passed.
//
// usage: node demo/scripts/verify-canonical-v1-capability-reconciliation.mjs

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { isPassStatus, statusRole } from "./lib/evidence-status.mjs";
import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenCompilerVersion,
  aikenModuleIndex,
  aikenModuleName,
  aikenPublishedCommand,
  aikenSelectorPattern,
  deriveAikenOutcome,
  deriveVitestOutcome,
  forkAikenBinary,
  runAikenCheck,
  runVitest,
  vitestPublishedCommand,
} from "./lib/runner-reports.mjs";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");
const aikenProjectDirectory = "onchain/aiken";

// Fixture mode runs one seeded defect and nothing else, so the negative
// self-tests at the bottom can spawn this very gate and observe its exit code.
runFixtureMode({
  argv: process.argv.slice(2),
  packageRoot: resolve(repositoryRoot, "demo/midgard-validation"),
});

const evidencePath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-capability-reconciliation-v1.json",
);
const evidence = JSON.parse(await readFile(evidencePath, "utf8"));

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-capability-reconciliation.v1",
);

// GOAL_SPEC §13.4 / §0 Integrity (owner amendment 2026-08-01): tracked
// sources are bound by path, not by byte hash. The content assertions below
// are what actually protect this reconciliation — they fail if the specific
// rows this evidence depends on are removed or renamed. A byte hash instead
// turned every unrelated edit to a 1,400-line spec into a red gate, which is
// exactly how this check came to be failing against a two-amendments-stale
// digest while the reconciliation it guards was still correct.
const sourceText = {};
for (const [name, source] of Object.entries(evidence.sources)) {
  const bytes = await readFile(resolve(repositoryRoot, source.path));
  sourceText[name] = bytes.toString("utf8");
}

// Statuses are classified by base role, so a decoration that records how a
// claim was established (`PASS (re-measured 2026-08-04 …)`) is governed by the
// same rules as a bare `PASS` instead of silently escaping them.
const controlStatuses = Object.values(evidence.controlPlane);
assert.equal(controlStatuses.length, 5);
assert.ok(
  controlStatuses.every((status) => isPassStatus(status)),
  "every control-plane status must classify as PASS",
);

const taskEntries = Object.entries(evidence.p2Tasks).filter(
  ([task]) => task !== "CG2",
);
const roleCount = (role) =>
  taskEntries.filter(([, disposition]) => statusRole(disposition) === role)
    .length;
const declaredPassTasks = taskEntries
  .filter(([, disposition]) => isPassStatus(disposition))
  .map(([task]) => task)
  .sort();

// ---------------------------------------------------------------------------
// The witnesses, and the external authority for them.
// ---------------------------------------------------------------------------

const taskManifest = JSON.parse(
  await readFile(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
    ),
    "utf8",
  ),
);
const manifestTaskById = new Map(
  (Array.isArray(taskManifest.tasks) ? taskManifest.tasks : []).map((task) => [
    task.id,
    task,
  ]),
);
const focusedCommandsOf = (taskId) => {
  const task = manifestTaskById.get(taskId);
  assert.ok(task !== undefined, `${taskId} has no task-manifest row`);
  const commands = Array.isArray(task.focusedCommands)
    ? task.focusedCommands
    : [];
  assert.ok(
    commands.length > 0,
    `${taskId} declares no focused verification in the task manifest`,
  );
  return commands;
};

// ---------------------------------------------------------------------------
// The manifest's own per-task Aiken selectors (issue #538).
//
// The witness sets above are one focused verification per PASS task. The task
// manifest declares far more: every PASS task also cites
// `node scripts/run-focused-check.mjs <module> <selector> ...` invocations —
// the heavier maximum_*/CEK/boundary selectors — which nothing replayed, so
// "the gate executes the task's verification" was true of a strict subset of
// what the manifest calls that task's verification. Compilation dominates an
// `aiken check` run, so the entire set costs one batched invocation.
//
// `plannedFocusedCommands` is deliberately NOT read: commit 6e805a78 quarantined
// those as planned, not-yet-executable citations, and replaying a planned
// citation would republish a plan as a measurement.
// ---------------------------------------------------------------------------

const FOCUSED_CHECK_SCRIPT = "scripts/run-focused-check.mjs";
const focusedCheckPattern = new RegExp(
  String.raw`node ${FOCUSED_CHECK_SCRIPT.replace("/", "\\/")}\s+(\S+)((?:\s+[a-z0-9_]+)+)$`,
  "u",
);

// Pure: turns one task's declared focused commands into the (module, selector)
// pairs a runner must be shown to have passed. It never silently drops a
// citation — a `run-focused-check` command it cannot parse, or one naming a
// module no source file compiles to, throws instead of shrinking the scope.
export const perTaskAikenCitations = ({ taskId, commands, moduleIndex }) => {
  const citations = [];
  for (const command of commands) {
    if (!command.includes(FOCUSED_CHECK_SCRIPT)) {
      continue;
    }
    const match = focusedCheckPattern.exec(command.trim());
    if (match === null) {
      throw new Error(
        `${taskId} declares a focused Aiken command this gate cannot parse into module and selectors, so its scope cannot be replayed: ${command}`,
      );
    }
    const [, citedModule, selectorList] = match;
    const module = moduleIndex.get(citedModule);
    if (module === undefined) {
      throw new Error(
        `${taskId} cites Aiken module ${citedModule}, which no source file under onchain/aiken/{lib,validators} compiles to`,
      );
    }
    for (const selector of selectorList.trim().split(/\s+/u)) {
      citations.push({
        task: taskId,
        module,
        // A citation that names the `midgard/x_v1` stem selects both that module
        // and its `midgard/x_v1.test` sibling, because `aiken check -m` matches
        // the module part as a prefix. Both spellings name the same source pair;
        // anything else is a module mismatch.
        modules: module.endsWith(".test")
          ? [module]
          : [module, `${module}.test`],
        selector,
      });
    }
  }
  return citations;
};

const witnessesByTask = evidence.p2TaskWitnesses;
assert.deepEqual(
  Object.keys(witnessesByTask).sort(),
  declaredPassTasks,
  "every task published as PASS — and only those — must declare executable witnesses",
);

const vitestPlan = new Map();
const aikenPlan = [];
const taskPlans = new Map();
for (const taskId of declaredPassTasks) {
  const witnesses = witnessesByTask[taskId];
  assert.ok(
    Array.isArray(witnesses) && witnesses.length > 0,
    `${taskId} is published PASS with no executable witness`,
  );
  const commands = focusedCommandsOf(taskId);
  const plan = { vitest: [], aiken: [] };
  for (const witness of witnesses) {
    if (witness.kind === "vitest") {
      const packageDirectory = witness.file.split("/").slice(0, 2).join("/");
      const testFile = witness.file.split("/").slice(2).join("/");
      // The manifest, not this artifact, decides what counts as the task's
      // focused verification.
      assert.ok(
        commands.some(
          (command) =>
            command.includes(`--dir ${packageDirectory} `) &&
            command.includes(testFile),
        ),
        `${taskId} witness ${witness.file} is not a focused verification the task manifest declares`,
      );
      if (!vitestPlan.has(witness.file)) {
        vitestPlan.set(witness.file, { packageDirectory, testFile });
      }
      plan.vitest.push(witness.file);
    } else if (witness.kind === "aiken") {
      const module = aikenModuleName(witness.module);
      assert.ok(
        Array.isArray(witness.selectors) && witness.selectors.length > 0,
        `${taskId} cites ${witness.module} with no selectors`,
      );
      for (const selector of witness.selectors) {
        assert.ok(
          /^[a-z0-9_]+$/u.test(selector),
          `selector ${selector} is not a focused-check-safe name`,
        );
        assert.ok(
          commands.some(
            (command) =>
              command.includes(module.replace(/\.test$/u, "")) &&
              command.includes(selector),
          ),
          `${taskId} witness ${module}.{${selector}} is not a focused verification the task manifest declares`,
        );
        aikenPlan.push({ module, selector });
        plan.aiken.push(`${module}.{${selector}}`);
      }
    } else {
      throw new Error(
        `unknown witness kind ${String(witness.kind)} for ${taskId}`,
      );
    }
  }
  taskPlans.set(taskId, plan);
}

const moduleIndex = aikenModuleIndex(
  resolve(repositoryRoot, aikenProjectDirectory),
);
const perTaskCitationsByTask = new Map(
  declaredPassTasks.map((taskId) => [
    taskId,
    perTaskAikenCitations({
      taskId,
      commands: focusedCommandsOf(taskId),
      moduleIndex,
    }),
  ]),
);
const perTaskCitations = [...perTaskCitationsByTask.values()].flat();
const perTaskDeclared = [];
const seenPerTaskPairs = new Set();
for (const citation of perTaskCitations) {
  const key = `${citation.module}#${citation.selector}`;
  if (!seenPerTaskPairs.has(key)) {
    seenPerTaskPairs.add(key);
    perTaskDeclared.push(citation);
  }
}
assert.ok(
  perTaskDeclared.length > 0,
  "the per-task Aiken plan is empty; the widened leg would measure nothing",
);

// ---------------------------------------------------------------------------
// Execute the witnesses. Every published count below comes from these reports.
// ---------------------------------------------------------------------------

const vitestResults = new Map();
for (const [file, { packageDirectory, testFile }] of vitestPlan) {
  const outcome = deriveVitestOutcome({
    label: `capability reconciliation ${file}`,
    ...runVitest({
      packageRoot: resolve(repositoryRoot, packageDirectory),
      testFile,
    }),
  });
  vitestResults.set(file, outcome);
}

const aikenSelectors = [...new Set(aikenPlan.map(({ selector }) => selector))];
const aikenOutcome = deriveAikenOutcome({
  label: "capability reconciliation on-chain witnesses",
  declared: aikenPlan,
  ...runAikenCheck({
    projectRoot: resolve(repositoryRoot, aikenProjectDirectory),
    selectors: aikenSelectors,
  }),
});
const measuredAikenSelectors = new Set(
  aikenOutcome.measured.map(
    ({ module, selector }) => `${module}.{${selector}}`,
  ),
);

// The widened leg. One batched invocation of the patched fork — the compiler
// `.github/workflows/aiken-ci.yml` uses to execute the suite — covers every
// per-task selector the manifest declares.
const forkBinary = forkAikenBinary();
const forkVersion = aikenCompilerVersion(forkBinary);
const perTaskSelectorPatterns = perTaskDeclared.map((citation) =>
  aikenSelectorPattern(citation),
);
const perTaskStartedAt = Date.now();
const perTaskOutcome = deriveAikenOutcome({
  label: "capability reconciliation per-task on-chain selectors",
  declared: perTaskDeclared,
  ...runAikenCheck({
    projectRoot: resolve(repositoryRoot, aikenProjectDirectory),
    selectors: perTaskSelectorPatterns,
    binary: forkBinary,
  }),
});
const perTaskWallSeconds = (Date.now() - perTaskStartedAt) / 1000;
assert.equal(
  perTaskOutcome.passed,
  perTaskDeclared.length,
  "every per-task Aiken selector the manifest declares must be measured as passing",
);
// Exact selection: `-m <module>.{<test>}` names one test, so a run that collected
// more than the declared set was not the run these counts describe.
assert.equal(
  perTaskOutcome.collected,
  perTaskDeclared.length,
  "the batched per-task run collected tests the manifest did not declare",
);
const measuredPerTaskSelectors = new Set(
  perTaskOutcome.measured.map(
    ({ module, selector }) => `${module}#${selector}`,
  ),
);
for (const [taskId, citations] of perTaskCitationsByTask) {
  const unmeasured = citations.filter(
    ({ module, selector }) =>
      !measuredPerTaskSelectors.has(`${module}#${selector}`),
  );
  assert.deepEqual(
    unmeasured,
    [],
    `${taskId} is published PASS but ${String(unmeasured.length)} of its manifest-declared Aiken selectors were not measured passing`,
  );
}

const measuredPassTasks = declaredPassTasks.filter((taskId) => {
  const plan = taskPlans.get(taskId);
  return (
    plan.vitest.every((file) => vitestResults.get(file)?.passed > 0) &&
    plan.aiken.every((identifier) => measuredAikenSelectors.has(identifier)) &&
    (perTaskCitationsByTask.get(taskId) ?? []).every(({ module, selector }) =>
      measuredPerTaskSelectors.has(`${module}#${selector}`),
    )
  );
});

assert.deepEqual(
  measuredPassTasks,
  declaredPassTasks,
  "a task may not be published PASS unless the runners passed every witness bound to it",
);
assert.deepEqual(evidence.p2Summary, {
  tasks: 22,
  // C20-6/C20-7 were promoted 2026-08-03. C20-2/C20-4/C20-5 and C27 were
  // promoted 2026-08-04 after exact focused boundary/protocol verification,
  // and C28 was promoted later the same day once complete content-addressed
  // CEK program material was authenticated and consumed by reference only.
  // CG2 stays OPEN while any P2 task is PARTIAL. The `pass` count is asserted
  // against the measured result below, not merely against this literal.
  pass: 17,
  partial: 5,
  open: 0,
  authoritativeConflict: 0,
  gate: "OPEN",
});
assert.equal(taskEntries.length, evidence.p2Summary.tasks);
assert.equal(
  measuredPassTasks.length,
  evidence.p2Summary.pass,
  "published pass count is not the number of tasks whose witnesses the runners passed",
);
assert.equal(roleCount("PASS"), evidence.p2Summary.pass);
assert.equal(roleCount("PARTIAL"), evidence.p2Summary.partial);
assert.equal(roleCount("OPEN"), evidence.p2Summary.open);
assert.equal(
  roleCount("AUTHORITATIVE_CONFLICT"),
  evidence.p2Summary.authoritativeConflict,
);
assert.deepEqual(
  evidence.p2WitnessCommands,
  [
    ...[...vitestPlan.values()].map((declaration) =>
      vitestPublishedCommand(declaration),
    ),
    aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
    }),
  ],
  "published witness commands drifted from the commands this gate executes",
);

// The widened coverage is a pin, checked against the runner result. Its *scope*
// is derived from the task manifest, never from this artifact, so the numbers
// below cannot be widened or narrowed by editing the file they live in.
const perTaskModules = [
  ...new Set(perTaskDeclared.map(({ module }) => module)),
].sort();
const perTaskByTask = Object.fromEntries(
  [...perTaskCitationsByTask]
    .filter(([, citations]) => citations.length > 0)
    .map(([taskId, citations]) => [taskId, citations.length]),
);
assert.deepEqual(
  evidence.p2PerTaskAikenCoverage.byTask,
  perTaskByTask,
  "published per-task Aiken citation counts drifted from the task manifest",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.tasks,
  Object.keys(perTaskByTask).length,
  "published per-task Aiken task count drifted from the measured plan",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.citations,
  perTaskCitations.length,
  "published per-task Aiken citation total drifted from the measured plan",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.selectors,
  perTaskDeclared.length,
  "published per-task Aiken selector total is not the number of distinct selectors executed",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.passed,
  perTaskOutcome.passed,
  "published per-task Aiken pass count is not the number the runner passed",
);
assert.deepEqual(
  evidence.p2PerTaskAikenCoverage.modules,
  perTaskModules,
  "published per-task Aiken module list drifted from the modules the runner collected from",
);
// No silent caps: anything the gate declines to execute must be named here, and
// an empty list is a claim that nothing was declined.
assert.deepEqual(
  evidence.p2PerTaskAikenCoverage.excluded,
  [],
  "the per-task leg publishes an exclusion list; it must be empty or the excluded selectors must be named",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.compiler,
  forkVersion,
  "published per-task compiler identity is not the compiler this gate spawned",
);
// The binary is a path that differs between this repository's CI runner and a
// contributor's machine, so what is published is the *selection rule* plus the
// version it produced — and the gate asserts it really spawned that selection.
assert.equal(
  evidence.p2PerTaskAikenCoverage.invocation.binaryEnv,
  "MIDGARD_FORK_AIKEN_BIN",
  "published per-task compiler-selection variable is not the one this gate reads",
);
assert.equal(
  forkBinary,
  process.env[evidence.p2PerTaskAikenCoverage.invocation.binaryEnv] ??
    evidence.p2PerTaskAikenCoverage.invocation.binaryDefault,
  "the compiler this gate spawned is not the one the published selection rule names",
);
assert.equal(
  evidence.p2PerTaskAikenCoverage.invocation.form,
  `cd ${aikenProjectDirectory} && $MIDGARD_FORK_AIKEN_BIN check -e -m <module>.{<selector>} ... --plain-numbers`,
  "published per-task invocation form is not the invocation this gate makes",
);

assert.match(
  sourceText.goalSpec,
  /\| C20-6 \| Field 6 native\/non-native script witnesses/u,
);
assert.match(
  sourceText.goalSpec,
  /\| C20-7 \| Field 7 vkey witnesses and exact signer identities/u,
);
assert.match(
  sourceText.canonicalTransaction,
  /5 mint, 6 script witnesses, 7 address witnesses, 8 redeemers/u,
);
assert.deepEqual(evidence.authorityConflict.tasks, ["C20-6", "C20-7"]);
assert.equal(evidence.authorityConflict.disposition, "RESOLVED");

assert.doesNotMatch(
  sourceText.validationAuxiliaryTypeScript,
  /TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineTypeScript,
  /transactionFieldPreimage|TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineDataTypeScript,
  /transactionFieldPreimage|TransactionFieldPreimageWitness/u,
);
assert.doesNotMatch(
  sourceText.validationMachineAiken,
  /TransactionFieldPreimageWitness \{ preimage_cbor: ByteArray \}/u,
);
assert.equal(evidence.wholePreimageFinding.status, "RESOLVED_ABI");
assert.deepEqual(evidence.wholePreimageFinding.removedFrom, [
  "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
  "demo/midgard-validation/src/validation-machine.ts",
  "demo/midgard-validation/src/validation-machine-data.ts",
  "onchain/aiken/lib/midgard/validation-machine-v1.ak",
]);

assert.ok(isPassStatus(evidence.acceptance.F10));
assert.ok(isPassStatus(evidence.acceptance.CG1));
assert.equal(statusRole(evidence.acceptance.CG2), "OPEN");
assert.equal(statusRole(evidence.p2Tasks.CG2), "OPEN");

// ---------------------------------------------------------------------------
// Negative self-tests: spawn this gate against seeded fixtures and require a
// non-zero exit carrying the specific diagnostic, plus passing controls so the
// rejections cannot be a gate that rejects everything.
// ---------------------------------------------------------------------------

// The widened leg's scope is built by `perTaskAikenCitations`, so a bug there
// shrinks what is measured without failing anything. These are pure and cost no
// runner time: a citation the builder cannot parse, or one naming a module that
// does not exist, must throw rather than silently reduce the plan.
assert.throws(
  () =>
    perTaskAikenCitations({
      taskId: "SELFTEST",
      commands: [
        "cd onchain/aiken && node scripts/run-focused-check.mjs midgard/validation_machine_v1 --all",
      ],
      moduleIndex,
    }),
  /cannot parse into module and selectors/u,
  "the per-task plan builder absorbed a focused command it could not parse",
);
assert.throws(
  () =>
    perTaskAikenCitations({
      taskId: "SELFTEST",
      commands: [
        "cd onchain/aiken && node scripts/run-focused-check.mjs midgard/no_such_module_v1 some_selector",
      ],
      moduleIndex,
    }),
  /which no source file under onchain\/aiken/u,
  "the per-task plan builder accepted a citation to a module that does not exist",
);
assert.deepEqual(
  perTaskAikenCitations({
    taskId: "SELFTEST",
    commands: [
      "cd onchain/aiken && aiken check --skip-tests",
      "cd onchain/aiken && node scripts/run-focused-check.mjs midgard/script_proof_v1 redeemer_purpose_tags_are_stable",
    ],
    moduleIndex,
  }),
  [
    {
      task: "SELFTEST",
      module: "midgard/script_proof_v1",
      modules: ["midgard/script_proof_v1", "midgard/script_proof_v1.test"],
      selector: "redeemer_purpose_tags_are_stable",
    },
  ],
  "the per-task plan builder did not produce the citation its input declares",
);

const runSelfTest = (flag) =>
  spawnSync(process.execPath, [fileURLToPath(import.meta.url), flag], {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
  });

for (const [flag, expectedDiagnostic] of [
  [
    "--vitest-fixture=failing",
    /ERR_FOCUSED_CHECK_FAILED: .*executes and fails/su,
  ],
  [
    "--vitest-fixture=zero-collection",
    /ERR_FOCUSED_CHECK_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  [
    "--vitest-fixture=skipped",
    /ERR_FOCUSED_CHECK_NOT_EXECUTED: .*never executed/su,
  ],
  [
    "--vitest-fixture=missing-file",
    /ERR_FOCUSED_CHECK_NO_FILES: .*matched no test file/su,
  ],
  [
    "--aiken-fixture=zero-collection",
    /ERR_AIKEN_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  ["--aiken-fixture=failing", /ERR_AIKEN_CHECK_FAILED: .*selftest_probe/su],
  // The widened leg is a batch, so it must fail closed on any one member.
  [
    "--aiken-fixture=batch-one-failing",
    /ERR_AIKEN_CHECK_FAILED: .*selftest_probe_two/su,
  ],
  [
    "--aiken-fixture=batch-zero-collection",
    /ERR_AIKEN_SELECTOR_NOT_COLLECTED: .*selftest_probe_absent/su,
  ],
  [
    "--aiken-fixture=batch-module-mismatch",
    /ERR_AIKEN_SELECTOR_MODULE_MISMATCH: .*selftest\/elsewhere\.test/su,
  ],
  // The widened leg names the compiler it ran under, so an unresolvable binary
  // must fail closed rather than fall through to whatever `aiken` is on PATH.
  [
    "--compiler-fixture=missing",
    /ERR_AIKEN_BINARY_UNAVAILABLE: .*MIDGARD_FORK_AIKEN_BIN/su,
  ],
]) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `capability reconciliation gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `capability reconciliation gate rejected ${flag} without its specific diagnostic`,
  );
}
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
  [
    "--aiken-fixture=batch-passing",
    /aiken fixture batch-passing: 3\/3 passed/u,
  ],
  [
    "--compiler-fixture=stub-version",
    /compiler fixture stub-version: 1\/1 passed/u,
  ],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `capability reconciliation gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

console.log(
  `canonical V1 capability reconciliation verified: ${taskEntries.length} P2 tasks, ${measuredPassTasks.length} pass measured from ${vitestResults.size} Vitest report(s) and ${aikenOutcome.collected} collected Aiken selector(s), CG2 open`,
);
console.log(
  `per-task Aiken coverage: ${perTaskOutcome.passed}/${perTaskDeclared.length} manifest-declared selector(s) across ${perTaskModules.length} module(s) and ${Object.keys(perTaskByTask).length} task(s) passed under ${forkVersion} in one batched invocation (${perTaskWallSeconds.toFixed(1)} s wall), 0 excluded`,
);
