#!/usr/bin/env node

import { readFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const planPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-verification-plan-v1.json",
);
const packagePath = resolve(repoRoot, "demo/package.json");
const plan = JSON.parse(readFileSync(planPath, "utf8"));
const packageManifest = JSON.parse(readFileSync(packagePath, "utf8"));

const exactKeys = (value, path, expected) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${path}: expected object`);
  }
  const actual = Object.keys(value).sort();
  const sortedExpected = [...expected].sort();
  if (
    actual.length !== sortedExpected.length ||
    actual.some((key, index) => key !== sortedExpected[index])
  ) {
    throw new Error(`${path}: unexpected keys`);
  }
};

// Same exactness as exactKeys for required keys, plus a closed optional set.
const requiredAndOptionalKeys = (value, path, required, optional) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${path}: expected object`);
  }
  const allowed = new Set([...required, ...optional]);
  const unexpected = Object.keys(value).filter((key) => !allowed.has(key));
  const missing = required.filter((key) => !(key in value));
  if (unexpected.length > 0 || missing.length > 0) {
    throw new Error(`${path}: unexpected keys`);
  }
};

// GOAL_SPEC §13.1 requires the aggregate commands to run with bounded
// resources. A timeout may only ever be a positive integer of milliseconds
// inside these bounds: below a minute it would truncate ordinary commands, and
// above a day it would not bound anything a human waits on.
const MINIMUM_TIMEOUT_MS = 60_000;
const MAXIMUM_TIMEOUT_MS = 86_400_000;
const assertTimeoutMs = (value, path) => {
  if (
    !Number.isSafeInteger(value) ||
    value < MINIMUM_TIMEOUT_MS ||
    value > MAXIMUM_TIMEOUT_MS
  ) {
    throw new Error(
      `${path}: timeout must be an integer between ${MINIMUM_TIMEOUT_MS} and ${MAXIMUM_TIMEOUT_MS} milliseconds`,
    );
  }
};

exactKeys(plan, "$", ["execution", "phases", "schema", "version"]);
if (
  plan.schema !== "midgard.canonical-v1-goal-verification-plan.v1" ||
  plan.version !== 1
) {
  throw new Error("unexpected verification plan schema/version");
}
exactKeys(plan.execution, "$.execution", [
  "mode",
  "network",
  "resourceBounds",
  "stateChangingPhase",
  "stopOnFailure",
]);
exactKeys(plan.execution.resourceBounds, "$.execution.resourceBounds", [
  "defaultCommandTimeoutMs",
]);
assertTimeoutMs(
  plan.execution.resourceBounds.defaultCommandTimeoutMs,
  "$.execution.resourceBounds.defaultCommandTimeoutMs",
);
if (
  plan.execution.mode !== "serial" ||
  plan.execution.stopOnFailure !== true ||
  plan.execution.stateChangingPhase !== null ||
  plan.execution.network !== "preprod"
) {
  throw new Error(
    "verification execution policy is not strict Preprod serial with no published state-changing phase",
  );
}

const requiredPhases = [
  "static",
  "capability",
  "fault-proofs",
  "watcher",
  "local",
  "evidence",
  "all",
];
if (
  Object.keys(plan.phases).length !== requiredPhases.length ||
  requiredPhases.some((phase) => !(phase in plan.phases))
) {
  throw new Error(
    `plan must define exact phases: ${requiredPhases.join(", ")}`,
  );
}

const safeCwd = (cwd, path) => {
  if (typeof cwd !== "string" || cwd.length === 0) {
    throw new Error(`${path}: cwd must be non-empty`);
  }
  const absolute = resolve(repoRoot, cwd);
  const fromRoot = relative(repoRoot, absolute);
  if (
    fromRoot === ".." ||
    fromRoot.startsWith(`..${sep}`) ||
    fromRoot.startsWith(sep)
  ) {
    throw new Error(`${path}: cwd escapes repository`);
  }
};

const ids = [];
let exactAikenSelectors = 0;
let declaredTimeouts = 0;
for (const phaseName of requiredPhases) {
  const phase = plan.phases[phaseName];
  exactKeys(phase, `$.phases.${phaseName}`, [
    "commands",
    "dependsOn",
    "stateChanging",
  ]);
  if (
    !Array.isArray(phase.dependsOn) ||
    !phase.dependsOn.every(
      (dependency) =>
        typeof dependency === "string" && requiredPhases.includes(dependency),
    )
  ) {
    throw new Error(`${phaseName}: invalid dependency`);
  }
  if (typeof phase.stateChanging !== "boolean") {
    throw new Error(`${phaseName}: stateChanging must be boolean`);
  }
  if (phase.stateChanging !== false) {
    throw new Error(
      `${phaseName}: retired plan must contain no state-changing phase`,
    );
  }
  if (!Array.isArray(phase.commands)) {
    throw new Error(`${phaseName}: commands must be an array`);
  }
  phase.commands.forEach((command, index) => {
    const path = `$.phases.${phaseName}.commands[${index}]`;
    requiredAndOptionalKeys(
      command,
      path,
      ["argv", "cwd", "id"],
      ["timeoutMs"],
    );
    if ("timeoutMs" in command) {
      assertTimeoutMs(command.timeoutMs, `${path}.timeoutMs`);
      declaredTimeouts += 1;
    }
    if (typeof command.id !== "string" || command.id.length === 0) {
      throw new Error(`${path}: id must be non-empty`);
    }
    ids.push(command.id);
    safeCwd(command.cwd, path);
    if (
      !Array.isArray(command.argv) ||
      command.argv.length === 0 ||
      !command.argv.every(
        (argument) => typeof argument === "string" && argument.length > 0,
      )
    ) {
      throw new Error(`${path}: argv must contain non-empty strings`);
    }
    const serialized = command.argv.join(" ");
    for (const forbidden of [
      "--passWithNoTests",
      "|| true",
      "&& true",
      "; true",
      "--allow-incomplete",
    ]) {
      if (serialized.includes(forbidden)) {
        throw new Error(`${path}: forbidden command construct ${forbidden}`);
      }
    }
    if (command.argv.includes("onchain/aiken/scripts/run-focused-check.mjs")) {
      exactAikenSelectors += 1;
      if (command.argv.length !== 4) {
        throw new Error(
          `${path}: each guarded Aiken invocation must name exactly one test`,
        );
      }
    }
  });
}

if (new Set(ids).size !== ids.length) {
  throw new Error("verification command ids must be globally unique");
}
if (exactAikenSelectors < 2) {
  throw new Error("verification plan lacks exact guarded Aiken selectors");
}
if (
  JSON.stringify(plan.phases.local.dependsOn) !==
  JSON.stringify(["static", "capability", "fault-proofs", "watcher"])
) {
  throw new Error("local must run the four local phases in the required order");
}
if (
  JSON.stringify(plan.phases.all.dependsOn) !==
  JSON.stringify(["local", "evidence"])
) {
  throw new Error("all must run local then evidence");
}
const commandText = Object.values(plan.phases)
  .flatMap(({ commands }) => commands)
  .map(({ argv }) => argv.join(" "))
  .join("\n");
if ("goal:accept:testnet" in (packageManifest.scripts ?? {})) {
  throw new Error("goal:accept:testnet must remain retired while C79 is OPEN");
}
for (const required of [
  "nix develop ./demo --command bash -c node --version && pnpm --version",
  // A bare `aiken --version` is not required as its own plan command:
  // static-goal-policy already runs it and asserts the output equals the
  // compiler declared in aiken.toml, which is strictly stronger.
  //
  // `aiken fmt --check` is likewise not required here. Aiken CI owns format
  // enforcement and applies a trailing-space normalization for a known
  // v1.1.22 monadic-let artifact plus an exclusion for the two protected
  // lib files; a bare `--check` in this plan enforces a stricter rule than
  // the authority does and fails on 40+ files that CI accepts.
  "aiken check --skip-tests",
  "aiken check",
  "aiken build --env testnet",
  "pnpm --dir demo run build",
  "pnpm --dir demo run typecheck",
  "pnpm --dir demo run lint",
  "pnpm --dir demo run format-check",
  "pnpm --dir demo/midgard-core test",
  "pnpm --dir demo/midgard-validation test",
  "pnpm --dir demo/midgard-fault-proofs test",
  "pnpm --dir demo/midgard-sdk test",
  "pnpm --dir demo/da-committee-node test",
  "pnpm --dir demo/midgard-node test",
  "pnpm --dir demo/midgard-watcher test",
  "test:cardano-capability-p2-retained-da",
  "test:cardano-capability-p2:data-breadth",
  "verify-canonical-v1-goal-closure-self-test.mjs",
  "verify-canonical-v1-goal-static-policy.mjs",
  // §13.2 requires `make spec` from the repository root, skipped only when
  // technical-spec/ is unchanged. The plan carries no conditional command
  // shape, so the condition lives in this wrapper; the wrapper itself is
  // mandatory so the specification build cannot be quietly dropped.
  "canonical-v1-goal-conditional-make-spec.mjs",
  "git diff --check",
  "git status --short",
]) {
  if (!commandText.includes(required)) {
    throw new Error(
      `verification plan is missing required command: ${required}`,
    );
  }
}

const expectedScripts = {
  "goal:verify:static":
    "node scripts/run-canonical-v1-goal-verification.mjs static",
  "goal:verify:capability":
    "node scripts/run-canonical-v1-goal-verification.mjs capability",
  "goal:verify:fault-proofs":
    "node scripts/run-canonical-v1-goal-verification.mjs fault-proofs",
  "goal:verify:watcher":
    "node scripts/run-canonical-v1-goal-verification.mjs watcher",
  "goal:verify:local":
    "node scripts/run-canonical-v1-goal-verification.mjs local",
  "goal:verify:evidence":
    "node scripts/run-canonical-v1-goal-verification.mjs evidence",
  "goal:verify:all": "node scripts/run-canonical-v1-goal-verification.mjs all",
  // §13.1: F40 additionally provides the non-gating READY-task helper. It is
  // scheduling tooling, so it is required to exist but is not a plan phase and
  // no acceptance claim may cite it.
  "goal:tasks:ready": "node scripts/canonical-v1-goal-tasks-ready.mjs",
};
for (const [name, expected] of Object.entries(expectedScripts)) {
  if (packageManifest.scripts?.[name] !== expected) {
    throw new Error(`${name} must be exactly ${expected}`);
  }
}

process.stdout.write(
  `${JSON.stringify({
    schema: plan.schema,
    phases: requiredPhases.length,
    commands: ids.length,
    exactAikenSelectors,
    declaredTimeouts,
    defaultCommandTimeoutMs:
      plan.execution.resourceBounds.defaultCommandTimeoutMs,
    mode: plan.execution.mode,
    network: plan.execution.network,
    status: "PASS",
  })}\n`,
);
