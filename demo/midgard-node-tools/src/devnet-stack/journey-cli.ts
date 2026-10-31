import { readdirSync, readFileSync } from "node:fs";

import type { ExecResult } from "./exec.js";
import type { UserRole } from "./identities.js";

export const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

/** A failure no retry can repair: the journey found wrong behaviour. */
export class FatalJourneyError extends Error {}

/** A wait or a retried submission ran past its deadline. */
export class JourneyDeadlineError extends FatalJourneyError {}

/** One CLI invocation failed; its transcript says why. Retryable. */
export class CliFailure extends Error {
  constructor(
    message: string,
    readonly transcript: string | undefined,
  ) {
    super(message);
  }
}

/**
 * Why a failed CLI run cannot succeed when retried with the same arguments,
 * or undefined when it may: a Commander usage error (the arguments are
 * wrong, or `dist` predates an option the journey passes), or the transfer
 * journal refusing a submission id journaled with another intent.
 */
export const definitiveCliFailure = (output: string): string | undefined => {
  const usage =
    /^error: (?:unknown option|required option|unknown command|missing required argument|too many arguments|option '[^']*' argument missing).*$/mu.exec(
      output,
    );
  if (usage !== null) return usage[0];
  return /Submission ID \S+ belongs to a different transfer[^\n]*/u.exec(
    output,
  )?.[0];
};

export type CliRequest = {
  readonly user: UserRole | undefined;
  readonly args: readonly string[];
  readonly label: string;
  readonly timeoutMs: number;
  /** Set on submissions: the stable id this attempt runs under. */
  readonly submissionId?: string;
};

/** Runs one node CLI command to completion (production: `dist/index.js`). */
export type CliRunner = (request: CliRequest) => Promise<ExecResult>;

/** Marks a submission's CLI process so a later journey process can find it. */
export const SUBMISSION_MARKER_ENV = "MIDGARD_DEVNET_JOURNEY_SUBMISSION";
/**
 * The absolute journey directory of the run that started the process.
 * Submission ids repeat across runs (the run id is the run directory's
 * basename), so a process is only this run's when both markers match.
 */
export const SUBMISSION_RUN_ENV = "MIDGARD_DEVNET_JOURNEY_RUN";

/**
 * Processes of run `runKey` still running a CLI attempt for `submissionId`.
 * A SIGKILLed journey leaves its CLI child running; a rerun must not start a
 * second attempt of the same submission beside it.
 */
export const earlierAttempts = (
  runKey: string,
  submissionId: string,
): number[] => {
  let entries: string[];
  try {
    entries = readdirSync("/proc");
  } catch {
    return [];
  }
  const markers = [
    `${SUBMISSION_RUN_ENV}=${runKey}`,
    `${SUBMISSION_MARKER_ENV}=${submissionId}`,
  ];
  return entries
    .filter((entry) => /^\d+$/u.test(entry) && Number(entry) !== process.pid)
    .filter((entry) => {
      try {
        const environ = readFileSync(`/proc/${entry}/environ`, "utf8").split(
          "\0",
        );
        return markers.every((marker) => environ.includes(marker));
      } catch {
        return false;
      }
    })
    .map(Number);
};

/**
 * Lets an earlier attempt of `submissionId` in run `runKey` finish (it may be
 * mid-submission), then stops it: SIGTERM after `graceMs`, SIGKILL 30 s later.
 */
export const awaitEarlierAttempts = async (
  runKey: string,
  submissionId: string,
  graceMs: number,
  log: (message: string) => void,
  wait: (ms: number) => Promise<void> = sleep,
) => {
  const started = Date.now();
  let announced = false;
  let sent: NodeJS.Signals | undefined;
  for (;;) {
    const pids = earlierAttempts(runKey, submissionId);
    if (pids.length === 0) return;
    if (!announced) {
      announced = true;
      log(
        `waiting for earlier attempt ${pids.join(", ")} of ${submissionId} to finish`,
      );
    }
    const waited = Date.now() - started;
    const signal: NodeJS.Signals | undefined =
      waited >= graceMs + 30_000
        ? "SIGKILL"
        : waited >= graceMs
          ? "SIGTERM"
          : undefined;
    if (signal !== undefined && signal !== sent) {
      sent = signal;
      log(
        `stopping earlier attempt ${pids.join(", ")} of ${submissionId} with ${signal}`,
      );
      for (const pid of pids)
        try {
          process.kill(pid, signal);
        } catch {
          // Already gone.
        }
    }
    await wait(500);
  }
};

export const describeError = (error: unknown): string => {
  if (!(error instanceof Error)) return String(error);
  // fetch reports ECONNREFUSED, ECONNRESET and the like only on its cause.
  const code = (error.cause as { code?: unknown } | undefined)?.code;
  return typeof code === "string"
    ? `${error.message} (${code})`
    : error.message;
};

export const firstLine = (text: string) => text.split("\n", 1)[0] ?? text;
