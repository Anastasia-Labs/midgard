// Verifies the canonical-V1 capability reconciliation artifact.
//
// Issue #529 (finding V-2 of #519). The published "17 pass" used to be produced
// by counting how many entries of this artifact's own `p2Tasks` map held the
// literal string "PASS", cross-checked against a `p2Summary` literal in this
// file and, externally, against two prose rows of a GOAL_SPEC markdown table.
// Nothing in that chain could observe a test result, so a task's promotion was
// a self-assertion: editing `p2Tasks` and the matching literal here promoted it.
//
// A P2 task is counted as PASS only when a runner reports that the executable
// witness declared for that task passed. The witnesses use each package's own
// Vitest CLI and one `aiken check -e` invocation; every published count is
// derived from those reports rather than from a progress ledger.
//
// usage: node demo/scripts/verify-canonical-v1-capability-reconciliation.mjs

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readdir, readFile, stat } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { isPassStatus, statusRole } from "./lib/evidence-status.mjs";
import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenModuleName,
  aikenPublishedCommand,
  deriveAikenOutcome,
  deriveVitestOutcome,
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
  const absolutePath = resolve(repositoryRoot, source.path);
  // A directory binds every TypeScript module inside it, concatenated in
  // name order, so a module split does not loosen the content assertions.
  sourceText[name] = (await stat(absolutePath)).isDirectory()
    ? (
        await Promise.all(
          (await readdir(absolutePath))
            .filter((entry) => /\.(ts|ak)$/.test(entry))
            .sort()
            .map((entry) => readFile(resolve(absolutePath, entry), "utf8")),
        )
      ).join("\n")
    : (await readFile(absolutePath)).toString("utf8");
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
  const plan = { vitest: [], aiken: [] };
  for (const witness of witnesses) {
    if (witness.kind === "vitest") {
      const packageDirectory = witness.file.split("/").slice(0, 2).join("/");
      const testFile = witness.file.split("/").slice(2).join("/");
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

const measuredPassTasks = declaredPassTasks.filter((taskId) => {
  const plan = taskPlans.get(taskId);
  return (
    plan.vitest.every((file) => vitestResults.get(file)?.passed > 0) &&
    plan.aiken.every((identifier) => measuredAikenSelectors.has(identifier))
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
  // C26 and C29 were promoted 2026-08-18: C26 after the queue's 2026-08-06
  // owner promotion (e4335bbd) replayed green at bd833ff3, and C29 after its
  // all-PASS acceptance re-measurement at 8ef0e471.
  // C30, C31 and C32 were promoted on the owner ruling of 2026-08-18 on issue
  // #486, which authorized the C30-C33 promotion on the recorded measurements
  // (C33 was already PASS) and dispositioned the C32 mid-measurement row
  // repair as the measurement discipline working, requiring no replay. Their
  // witnesses are executed by this gate like every other task witness.
  // CG2 stays OPEN on its own owner pin, not on the PARTIAL count: the ruling
  // kept `acceptance.CG2`/`p2Tasks.CG2` OPEN until CG2's consolidated review,
  // which rides C21's two residuals. The `pass` count is asserted against the
  // measured result below, not merely against this literal.
  pass: 22,
  partial: 0,
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
  "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness.ts",
  "demo/midgard-validation/src/validation-machine",
  "demo/midgard-validation/src/validation-machine-data.ts",
  "onchain/aiken/lib/midgard/validation-machine",
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
