#!/usr/bin/env node

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

import { decodeCanonicalV1GoalClosure } from "./canonical-v1-goal-closure-v1.mjs";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const planPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-verification-plan-v1.json",
);
const closurePath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json",
);
const plan = JSON.parse(readFileSync(planPath, "utf8"));
const requestedPhase = process.argv[2];

if (
  process.argv.length !== 3 ||
  typeof requestedPhase !== "string" ||
  !(requestedPhase in plan.phases)
) {
  throw new Error(
    `usage: run-canonical-v1-goal-verification.mjs <${Object.keys(
      plan.phases,
    ).join("|")}>`,
  );
}
if (
  requestedPhase === "accept-testnet" &&
  process.env.MIDGARD_GOAL_ACCEPT_TESTNET !== "YES"
) {
  throw new Error(
    "state-changing Preprod acceptance requires MIDGARD_GOAL_ACCEPT_TESTNET=YES",
  );
}

// GOAL_SPEC §13.1: the aggregate commands run "with bounded resources". A
// command that hangs must fail the phase rather than occupy the machine
// forever, so every command gets a wall-clock bound: its own plan `timeoutMs`
// when it declares one, otherwise the plan's
// `execution.resourceBounds.defaultCommandTimeoutMs`. The bound exists to kill
// hangs, not to truncate known-slow gates — commands measured above the
// default (the full `aiken check`) carry an explicit larger bound in the plan.
// MIDGARD_GOAL_COMMAND_TIMEOUT_MS overrides both for one run.
const FALLBACK_COMMAND_TIMEOUT_MS = 3_600_000;
const parsePositiveInteger = (value, label) => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive integer of milliseconds`);
  }
  return parsed;
};
const timeoutOverrideMs =
  process.env.MIDGARD_GOAL_COMMAND_TIMEOUT_MS === undefined
    ? null
    : parsePositiveInteger(
        process.env.MIDGARD_GOAL_COMMAND_TIMEOUT_MS,
        "MIDGARD_GOAL_COMMAND_TIMEOUT_MS",
      );
const defaultTimeoutMs =
  plan.execution.resourceBounds?.defaultCommandTimeoutMs ??
  FALLBACK_COMMAND_TIMEOUT_MS;
const commandTimeoutMs = (command) =>
  timeoutOverrideMs ?? command.timeoutMs ?? defaultTimeoutMs;

// GOAL_SPEC §13.2 requires the evidence manifest to record command, exit code,
// test count, duration, revision and artifact identity. Recording rewrites a
// committed evidence file, so it is explicit: a plain verification run never
// mutates the manifest, and `--release` cleanliness is never disturbed by
// merely verifying.
const recordCommandResults =
  process.env.MIDGARD_GOAL_RECORD_COMMAND_RESULTS === "YES";
const recorded = [];

const currentRevision = () => {
  const result = spawnSync("git", ["rev-parse", "HEAD"], {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  });
  if (result.status !== 0) {
    throw new Error("unable to resolve verification revision");
  }
  return result.stdout.trim();
};

const absoluteCwd = (cwd) => {
  const absolute = resolve(repoRoot, cwd);
  const fromRoot = relative(repoRoot, absolute);
  if (
    fromRoot === ".." ||
    fromRoot.startsWith(`..${sep}`) ||
    fromRoot.startsWith(sep)
  ) {
    throw new Error(`command cwd escapes repository: ${cwd}`);
  }
  return absolute;
};

// Upsert by command id so re-running a phase replaces that command's result
// instead of accumulating duplicates (the closure verifier rejects duplicate
// ids). `testCount` stays null here: this runner streams child output straight
// to the terminal rather than capturing it, and the exact collected counts are
// asserted by the commands themselves — a focused runner or count-checking
// verifier exits nonzero when a suite collects the wrong number of tests.
// The closure manifest is a committed evidence artifact that is
// Prettier-formatted. Recording must not change its formatting contract, so
// serialize through Prettier when the workspace copy is importable and fall
// back to plain JSON with an explicit warning when it is not.
const serializeManifest = async (manifest) => {
  const json = `${JSON.stringify(manifest, null, 2)}\n`;
  try {
    const prettier = await import("prettier");
    return await prettier.format(json, {
      parser: "json",
      filepath: closurePath,
    });
  } catch {
    process.stderr.write(
      "prettier is unavailable; recorded command results use plain JSON formatting\n",
    );
    return json;
  }
};

const writeRecordedCommandResults = async () => {
  if (!recordCommandResults || recorded.length === 0) {
    return;
  }
  const manifest = JSON.parse(readFileSync(closurePath, "utf8"));
  const byId = new Map(
    manifest.commandResults.map((entry) => [entry.id, entry]),
  );
  for (const entry of recorded) {
    byId.set(entry.id, entry);
  }
  manifest.commandResults = [...byId.values()].sort((left, right) =>
    left.id < right.id ? -1 : left.id > right.id ? 1 : 0,
  );
  decodeCanonicalV1GoalClosure(manifest);
  writeFileSync(closurePath, await serializeManifest(manifest), "utf8");
  process.stdout.write(
    `${JSON.stringify({
      event: "command-results-recorded",
      path: relative(repoRoot, closurePath),
      recorded: recorded.length,
      total: manifest.commandResults.length,
    })}\n`,
  );
};

const completed = new Set();
const runPhase = async (phaseName) => {
  if (completed.has(phaseName)) {
    return;
  }
  const phase = plan.phases[phaseName];
  for (const dependency of phase.dependsOn) {
    await runPhase(dependency);
  }
  for (const command of phase.commands) {
    const timeoutMs = commandTimeoutMs(command);
    const startedAt = Date.now();
    process.stdout.write(
      `${JSON.stringify({
        event: "command-start",
        phase: phaseName,
        id: command.id,
        cwd: command.cwd,
        argv: command.argv,
        timeoutMs,
      })}\n`,
    );
    const result = spawnSync(command.argv[0], command.argv.slice(1), {
      cwd: absoluteCwd(command.cwd),
      env: process.env,
      stdio: "inherit",
      timeout: timeoutMs,
      killSignal: "SIGTERM",
    });
    const durationMs = Date.now() - startedAt;
    const timedOut = result.error?.code === "ETIMEDOUT";
    const exitCode =
      typeof result.status === "number"
        ? result.status
        : typeof result.signal === "string"
          ? 128
          : 1;
    const revision = currentRevision();
    const finishedAt = new Date().toISOString();
    process.stdout.write(
      `${JSON.stringify({
        event: "command-finish",
        phase: phaseName,
        id: command.id,
        command: command.argv,
        exitCode,
        timedOut,
        timeoutMs,
        durationMs,
        revision,
        finishedAt,
      })}\n`,
    );
    if (timedOut) {
      process.stderr.write(
        `command ${command.id} exceeded its ${String(timeoutMs)} ms bound and was terminated\n`,
      );
    }
    recorded.push({
      id: command.id,
      command: command.argv,
      exitCode,
      testCount: null,
      durationMs,
      revision,
      artifactIdentity: null,
      finishedAt,
    });
    if (exitCode !== 0) {
      await writeRecordedCommandResults();
      process.exit(exitCode);
    }
  }
  completed.add(phaseName);
};

await runPhase(requestedPhase);
await writeRecordedCommandResults();
