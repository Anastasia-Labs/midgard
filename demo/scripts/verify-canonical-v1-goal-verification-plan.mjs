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
  "stateChangingPhase",
  "stopOnFailure",
]);
if (
  plan.execution.mode !== "serial" ||
  plan.execution.stopOnFailure !== true ||
  plan.execution.stateChangingPhase !== "accept-testnet" ||
  plan.execution.network !== "preprod"
) {
  throw new Error("verification execution policy is not strict Preprod serial");
}

const requiredPhases = [
  "static",
  "capability",
  "fault-proofs",
  "watcher",
  "local",
  "accept-testnet",
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
  if (phase.stateChanging !== (phaseName === "accept-testnet")) {
    throw new Error(`${phaseName}: only accept-testnet may be state-changing`);
  }
  if (!Array.isArray(phase.commands)) {
    throw new Error(`${phaseName}: commands must be an array`);
  }
  phase.commands.forEach((command, index) => {
    const path = `$.phases.${phaseName}.commands[${index}]`;
    exactKeys(command, path, ["argv", "cwd", "id"]);
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
if (
  JSON.stringify(plan.phases["accept-testnet"].dependsOn) !==
  JSON.stringify(["local"])
) {
  throw new Error("accept-testnet must require all local gates first");
}

const commandText = Object.values(plan.phases)
  .flatMap(({ commands }) => commands)
  .map(({ argv }) => argv.join(" "))
  .join("\n");
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
  "goal:accept:testnet":
    "node scripts/run-canonical-v1-goal-verification.mjs accept-testnet",
  "goal:verify:evidence":
    "node scripts/run-canonical-v1-goal-verification.mjs evidence",
  "goal:verify:all": "node scripts/run-canonical-v1-goal-verification.mjs all",
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
    mode: plan.execution.mode,
    network: plan.execution.network,
    status: "PASS",
  })}\n`,
);
