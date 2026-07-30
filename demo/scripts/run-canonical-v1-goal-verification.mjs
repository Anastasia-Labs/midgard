#!/usr/bin/env node

import { readFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const planPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-verification-plan-v1.json",
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

const completed = new Set();
const runPhase = (phaseName) => {
  if (completed.has(phaseName)) {
    return;
  }
  const phase = plan.phases[phaseName];
  for (const dependency of phase.dependsOn) {
    runPhase(dependency);
  }
  for (const command of phase.commands) {
    const startedAt = Date.now();
    process.stdout.write(
      `${JSON.stringify({
        event: "command-start",
        phase: phaseName,
        id: command.id,
        cwd: command.cwd,
        argv: command.argv,
      })}\n`,
    );
    const result = spawnSync(command.argv[0], command.argv.slice(1), {
      cwd: absoluteCwd(command.cwd),
      env: process.env,
      stdio: "inherit",
    });
    const durationMs = Date.now() - startedAt;
    const exitCode =
      typeof result.status === "number"
        ? result.status
        : typeof result.signal === "string"
          ? 128
          : 1;
    process.stdout.write(
      `${JSON.stringify({
        event: "command-finish",
        phase: phaseName,
        id: command.id,
        command: command.argv,
        exitCode,
        durationMs,
        revision: currentRevision(),
        finishedAt: new Date().toISOString(),
      })}\n`,
    );
    if (exitCode !== 0) {
      process.exit(exitCode);
    }
  }
  completed.add(phaseName);
};

runPhase(requestedPhase);
