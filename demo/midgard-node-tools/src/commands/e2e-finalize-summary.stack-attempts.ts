import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { parseE2EStep, type StepSummary } from "../e2e/runner.js";
import type { CleanRunGate, DbEvidence } from "../e2e/summary.js";
import { COMMAND_LOCK_CONFLICT_EXIT_CODE } from "../full-stack/process.js";

/**
 * The deployment steps' commands that create the on-chain identity
 * (deployment.ts): the hub-oracle nonce, the reference scripts, `init` and the
 * operator registration. An attach never runs them.
 */
export const STACK_DEPLOYMENT_COMMAND_IDS = [
  "nonce-create-or-resume",
  "references-publish",
  "initialize-submit",
  "operator-register-or-resume",
] as const;

const SOURCE = "e2e-stack";
// process.ts names each attempt `<command id>-<uuid>`, with a .log and a .json.
const ATTEMPT_FILE =
  /^(.+)-([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.(json|log)$/;

export type StackAttempts = {
  /** Attempt records whose command ran to an outcome. */
  readonly finished: readonly StepSummary[];
  /** Command ids of logs without a record: the controller stopped mid-command. */
  readonly unfinished: readonly string[];
  readonly malformed: readonly string[];
};

/** Reads `attempts/`, the per-command records `StackProcesses.command` writes. */
export async function readStackAttempts(
  runDirectory: string,
): Promise<StackAttempts> {
  const directory = join(runDirectory, "attempts");
  let names: string[];
  try {
    names = (await readdir(directory)).sort();
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT")
      return { finished: [], unfinished: [], malformed: [] };
    throw error;
  }
  const finished: StepSummary[] = [];
  const unfinished: string[] = [];
  const malformed: string[] = [];
  const recorded = new Set(names);
  for (const name of names) {
    const match = ATTEMPT_FILE.exec(name);
    if (match === null) {
      malformed.push(name);
      continue;
    }
    const [, id, attempt, extension] = match;
    if (extension === "log") {
      if (!recorded.has(`${id}-${attempt}.json`)) unfinished.push(id!);
      continue;
    }
    try {
      const summary = parseE2EStep(
        JSON.parse(await readFile(join(directory, name), "utf8")) as unknown,
        `stack attempt ${name}`,
      );
      if (summary.id !== id) throw new Error("attempt id differs");
      finished.push(summary);
    } catch {
      malformed.push(name);
    }
  }
  return { finished, unfinished, malformed };
}

/** The per-command lock refused the command, so it never ran (process.ts). */
const notStarted = (attempt: StepSummary) =>
  attempt.exitCode === COMMAND_LOCK_CONFLICT_EXIT_CODE;

/** Fresh mode: this run directory ran every command that creates the deployment. */
export function stackFreshDeploymentGate(attempts: StackAttempts): DbEvidence {
  const started = new Set([
    ...attempts.finished
      .filter((attempt) => !notStarted(attempt))
      .map((attempt) => attempt.id),
    ...attempts.unfinished,
  ]);
  const missing = STACK_DEPLOYMENT_COMMAND_IDS.filter((id) => !started.has(id));
  return {
    label: "stack_fresh_deployment",
    status: missing.length === 0 ? "satisfied" : "failed",
    source: SOURCE,
    details: {
      required: STACK_DEPLOYMENT_COMMAND_IDS.join(","),
      missing: missing.join(","),
    },
  };
}

const ids = (attempts: readonly StepSummary[]) =>
  attempts.map((attempt) => `${attempt.id}:${attempt.status}`).join(",");

/**
 * Fresh mode: a clean run finished every command it started. A stopped or
 * failed command is recovery evidence; the confirmed journal already shows
 * it was reconciled, so it reads as interrupted or failed, never blocked.
 */
export function stackAttemptQualityGate(attempts: StackAttempts): CleanRunGate {
  const ran = attempts.finished.filter((attempt) => !notStarted(attempt));
  const failed = ran.filter(
    (attempt) =>
      attempt.status === "failed" || attempt.status === "runner_error",
  );
  const interrupted = ran.filter(
    (attempt) => attempt.status === "timeout" || attempt.status === "signaled",
  );
  const status: CleanRunGate["status"] =
    interrupted.length > 0 || attempts.unfinished.length > 0
      ? "interrupted"
      : failed.length > 0 || attempts.malformed.length > 0
        ? "failed"
        : "satisfied";
  return {
    label: "stack_attempt_quality",
    status,
    source: SOURCE,
    details: {
      attempts: String(attempts.finished.length + attempts.unfinished.length),
      notStarted: String(attempts.finished.length - ran.length),
      failed: ids(failed),
      interrupted: ids(interrupted),
      unfinished: attempts.unfinished.join(","),
      malformed: attempts.malformed.join(","),
    },
  };
}
